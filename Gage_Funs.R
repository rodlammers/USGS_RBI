library(dataRetrieval)
library(dplyr)
library(leaflet)
library(purrr)

get_gages <- function(state, DA_min, DA_max, record_length_min, startDate, endDate, iv = FALSE){
  gages <- readNWISdata(parameterCd = "00060", stateCd = state, drainAreaMin = DA_min,
                        drainAreaMax = DA_max, service = "site", seriesCatalogOutput = TRUE) %>%
    filter(parm_cd == "00060",
           stat_cd == "00003",
           is.na(loc_web_ds)) %>%
    mutate(period = (as.Date(end_date) - as.Date(begin_date)) / 365,
           state = state) %>%
    filter(period >= record_length_min) %>%
    filter(as.Date(end_date) >= as.Date(startDate),
           as.Date(begin_date) <= as.Date(endDate))
  
  #if (iv){
    gages_inst <- readNWISdata(parameterCd = "00060", stateCd = state, drainAreaMin = DA_min,
                          drainAreaMax = DA_max, service = "site", seriesCatalogOutput = TRUE) %>%
      filter(parm_cd == "00060",
             data_type_cd == "uv",
             is.na(loc_web_ds)) %>%
      mutate(Instant = "Yes") %>%
      select(site_no, Instant)
    
    gages <- left_join(gages, gages_inst, by = "site_no") %>%
      mutate(Instant = if_else(is.na(Instant), "No", Instant))
  #}
  
  return(gages)
}

find_gages <- function(state = NULL, DA_min = 0, DA_max = 1e9, record_length_min = 0, startDate = "1800-01-01",
                       endDate = "2100-01-01", iv = FALSE){
  
  if (is.null(state)){
    state <- state.abb
  }
  
  if (is.na(DA_max)){DA_max <- 1e9}
  
  get_gages2 <- possibly(get_gages, NULL)
  gages <- lapply(state, get_gages2, DA_min, DA_max, record_length_min, startDate, endDate, iv)
  gages <- do.call("rbind", gages)
  
  if (is.null(gages)){
    
    return(print("There are no gages that match the provided criteria."))
  }
  if (nrow(gages) > 0){
    #Get other attributes
    attributes <- readNWISsite(gages$site_no) %>%
      select(site_no, drain_area_va)
    #nlcd06 <- read.csv("~/WORK/GAGES-II/conterm_lc06_basin.txt", colClasses = c("STAID" = "character"))
    gages <- left_join(gages, attributes, by = "site_no") #%>%
    #left_join(nlcd06, by = c("site_no" = "STAID"))
    
    # print(leaflet(data = gages) %>%
    #         #addTiles() %>%
    #         addProviderTiles(providers$OpenStreetMap) %>%
    #         addCircleMarkers(~dec_long_va, ~dec_lat_va,
    #                          color = "red", radius = 5, stroke = FALSE,
    #                          fillOpacity = 0.8, opacity = 0.8,
    #                          popup = paste("Station num:", gages$site_no, "<br>",
    #                                        "Station name:", gages$station_nm, "<br>",
    #                                        "Record (yr):", round(gages$period, 1), "<br>",
    #                                        "Start Date:", gages$begin_date, "<br>",
    #                                        "End Date:", gages$end_date, "<br>",
    #                                        "Drainage Area (mi2):", gages$drain_area_va)))
    # 
    return(gages)
  }else{
    print("There are no gages that match the provided criteria.")
  }
  
}