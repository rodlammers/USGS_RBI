#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinycssloaders)

data(fips_codes, package = "tigris")

states <- group_by(fips_codes, by = state) %>%
    summarize(state = first(state),
              state_code = first(state_code),
              state_name = first(state_name)) %>%
    filter(!(state_code %in% c(60, 66, 69, 74, 78)))

source("Gage_Funs.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("USGS Gage Locator and Flashiness Data"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        
        tabPanel("Gage Finder",
                 
                 h4("This tool can be used to find USGS gages with different attributes (e.g. states, drainage areas, dates with data). 
           Only gages with discharge data are returned."),
                 
                 br(),
                 
            sidebarLayout(
                sidebarPanel(
                    selectInput("state_nm",
                                "State (select one or more)",
                                choices = states$state,
                                multiple = TRUE),
                    
                    dateRangeInput("date_rng",
                                   "Date Range",
                                   start = "1900-01-01"),
                    
                    numericInput("DA_min",
                                 "Min. Drainage Area [sq. mi]",
                                 0),
                    
                    numericInput("DA_max",
                                 "Max Drainage Area [sq. mi]",
                                 NA),
                    
                    numericInput("length_min",
                                 "Min. record length [years]",
                                 0),
                    
                    sliderInput("urban_range",
                                "Range of % urbanization*",
                                min = 0,
                                max = 100,
                                value = c(0, 100),
                                step = 1),
                    
                    checkboxInput("inst_data",
                                  "Has 15-min data"),
                    
                    actionButton("run_gages",
                                 "Find Gages"),
                    
                    br(),
                    
                    br(),
                    
                    h4(a(href = "https://github.com/rodlammers/USGS_RBI", target="_blank", "Code"), "created by Rod Lammers (rodlammers@gmail.com)."),

                    br(),
                    p("*Percent urbanization for watersheds based on 2006 and 2001 (AK, HI, and PR) NLCD data. Annual precipitation data (see table) from 1971-2000 PRISM data. Data from the", a(href = "https://water.usgs.gov/GIS/metadata/usgswrd/XML/gagesII_Sept2011.xml", target = "_blank", "GAGES-II dataset.")),
                    
                    
                    width = 3
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                    leafletOutput("Gage_map", width = "100%", height = 600) %>% withSpinner(),
                    
                    dataTableOutput("gage_table")
                    
                )
            )
        ),
        
        tabPanel("Flashiness",
                 h4("This map shows flashiness data (computed using the", a(href="https://doi.org/10.1111/j.1752-1688.2004.tb01046.x", target="_blank", "Richards-Baker Flashiness Index"), 
                 "- or RBI) for USGS stream gages
                    using daily data from 2010-2019. Gages can be displayed by category (Low <= 0.2, 0.2 < Med <= 0.5, High > 0.5)
                    or by raw value. Note that some very high values of RBI are associated with tidal rivers and diversion channels that may have both positive
                    and negative discharges."),
                 
                 br(),
                 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("RBI_range",
                                     "Range of RBI to display",
                                     min = 0,
                                     max = 7.4,
                                     value = c(0, 7.4),
                                     step = 0.1),
                         
                         # sliderInput("DA_range",
                         #             "Range of drainage area to display [mi2]",
                         #             min = 0,
                         #             max = 1144500,
                         #             value = c(0, 1144500)),
                         
                         shinyWidgets::sliderTextInput("DA_range",
                                                       "Range of drainage area to display [mi2]",
                                                       choices = c(0, round(10 ^ seq(-2, 6, 0.5), 2), 1200000),
                                                       grid = TRUE,
                                                       selected = c(0, 1200000)),
                         
                         checkboxInput("DA_NA",
                                       "Include gages with unknown drainage areas?",
                                       value = TRUE),
                         
                         sliderInput("urban_range2",
                                     "Range of % urbanization*",
                                     min = 0,
                                     max = 100,
                                     value = c(0, 100),
                                     step = 1),
                         
                         radioButtons("map_type",
                                      "Map display",
                                      choices = c("Categories",
                                                  "Values")),
                         
                         br(),
                         
                         br(),
                         
                         h4(a(href = "https://github.com/rodlammers/USGS_RBI", target="_blank", "Code"), "created by Rod Lammers (rodlammers@gmail.com)."),

                         br(),
                         p("*Percent urbanization for watersheds based on 2006 and 2001 (AK, HI, and PR) NLCD data. Data from the", a(href = "https://water.usgs.gov/GIS/metadata/usgswrd/XML/gagesII_Sept2011.xml", target = "_blank", "GAGES-II dataset.")),
                         
                         width = 3
                         
                         #plotOutput("legend")
                         
                     ),
                     
                     mainPanel(
                         leafletOutput("RBI_map", width = "100%", height = 600) %>% withSpinner()
                     )
                 )
                 )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    #RBI Data and map
    files <- list.files("RBI_data", full.names = TRUE)
    data <- lapply(files, read.csv)
    data_all <- do.call("rbind", data) %>%
        mutate(site_no = as.character(site_no)) %>%
        rowwise() %>%
        mutate(site_no = if_else(nchar(site_no) == 7, paste0(0, site_no), site_no),
               class = factor(class, levels = c("High", "Med", "Low"))) %>%
        filter(RB > 0)
    
    #GAGES Data
    GAGES <- lapply(list.files("GAGESII_data", full.names = TRUE), read.csv, colClasses = c("STAID" = "character"))
    GAGES_comb <- left_join(GAGES[[2]], GAGES[[3]], by = "STAID")
    GAGES_AK <- GAGES[[1]]
    
    #combined urbanization data
    GAGES_urb <- rbind(select(GAGES_comb, STAID, "DEV" = DEVNLCD06),
                       select(GAGES_AK, STAID, "DEV" = DEVNLCD01))
    
    data_map <- eventReactive(c(input$DA_range, input$RBI_range, input$DA_NA, input$urban_range2), {
        
        if (input$DA_NA){
            data <- filter(data_all, drain_area_va >= as.numeric(input$DA_range[1]) | is.na(drain_area_va), 
                   drain_area_va <= as.numeric(input$DA_range[2]) | is.na(drain_area_va), 
                   RB >= input$RBI_range[1], RB <= input$RBI_range[2])
        }
        else if (!input$DA_NA){
            data <- filter(data_all, drain_area_va >= as.numeric(input$DA_range[1]), 
                           drain_area_va <= as.numeric(input$DA_range[2]), 
                           RB >= input$RBI_range[1], RB <= input$RBI_range[2])
        }
        
        data <- left_join(data, GAGES_urb, by = c("site_no" = "STAID"))
        if (diff(range(input$urban_range2)) != 100){
            #Only filter gages if urbanization range not adjusted
            data <- filter(data, DEV >= input$urban_range2[1], DEV <= input$urban_range2[2])            
            
        }
        
        return(data)
    })

    cols <- eventReactive(c(input$map_type, input$DA_range, input$RBI_range, input$DA_NA, input$urban_range2), {
        if (input$map_type == "Categories"){
            col_fun <- leaflet::colorFactor(palette = "viridis", domain = NULL, reverse = TRUE)
            col <- col_fun(data_map()$class)
        }else if (input$map_type == "Values"){
            vals <- cut(data_map()$RB, breaks = c(seq(0, 0.5, 0.1), 0.75, 1, 1.5, 2, 8), labels = c(seq(0.1, 0.5, 0.1), 0.75, 1, 1.5, 2, 8),
                        ordered_result = TRUE)
            #levels(vals) <-  c(seq(0, 0.5, 0.1), 0.75, 1, 1.5, 2, 4, 8)
            col_fun <- leaflet::colorFactor(palette = "viridis", domain = as.character(c(seq(0.1, 0.5, 0.1), 0.75, 1, 1.5, 2, 8)))
            col <- col_fun(vals)
        }
        return(list(col, col_fun))
    })
    
    legend_vals <- eventReactive(input$map_type, {
        if (input$map_type == "Categories"){
            legend <- factor(c("High", "Med", "Low"), levels = c("High", "Med", "Low"))
        }else if (input$map_type == "Values"){
            legend <- as.character(c(seq(0.1, 0.5, 0.1), 0.75, 1, 1.5, 2, 8))
            #legend <- 1:11
        }

        return(legend)
    })
    
    output$RBI_map <- leaflet::renderLeaflet({
        leaflet(data = data_map()) %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addCircleMarkers(~dec_long_va, ~dec_lat_va,
                             color = cols()[[1]], radius = 5, stroke = FALSE,
                             fillOpacity = 0.8, opacity = 0.8,
                             popup = paste("Station num:", data_map()$site_no, "<br>",
                                           "Station name:", data_map()$station_nm, "<br>",
                                           "RBI:", round(data_map()$RB, 2), "<br>",
                                           "Category:", data_map()$class, "<br>",
                                           "years:", round(data_map()$years), "<br>",
                                           "DA [sq. mi]:", round(data_map()$drain_area_va, 1), "<br>",
                                           "Urban area [%]:", round(data_map()$DEV))) %>%
            addLegend("bottomright", pal = cols()[[2]], values = legend_vals(),
                      title = "RBI",
                      opacity = 0.8)
    })
    
    ########################
    #Gage finder
    gages <- eventReactive(input$run_gages, {
        gage_data <- find_gages(state = input$state_nm, DA_min = input$DA_min, DA_max = input$DA_max, record_length_min = input$length_min,
                   startDate = input$date_rng[1], endDate = input$date_rng[2], iv = input$inst_data)
        validate(need(!is.character(gage_data), "There are no gages that match the provided criteria."))
        
        if (input$inst_data){
            gage_data <- filter(gage_data, Instant == "Yes")
        }
        
        #Add gages data
        if (input$state_nm %in% c("AK","HI","PR")){
            gage_data <- left_join(gage_data, GAGES_AK, by = c("site_no" = "STAID")) %>%
                rename("DEV" = DEVNLCD01) %>%
                mutate(PPTAVG_BASIN = NA)
        }else {
            gage_data <- left_join(gage_data, GAGES_comb, by = c("site_no" = "STAID")) %>%
                rename("DEV" = DEVNLCD06)
        }
        
        if (diff(range(input$urban_range)) != 100){
            #Only filter gages if urbanization range not adjusted
            gage_data <- filter(gage_data, DEV >= input$urban_range[1], DEV <= input$urban_range[2])            
            
        }

        
        return(gage_data)
    })
    
    output$Gage_map <- leaflet::renderLeaflet({
        leaflet(data = gages()) %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addCircleMarkers(~dec_long_va, ~dec_lat_va,
                             color = "red", radius = 5, stroke = FALSE,
                             fillOpacity = 0.8, opacity = 0.8,
                             popup = paste("Station num:", gages()$site_no, "<br>",
                                           "Station name:", gages()$station_nm, "<br>",
                                           "Record [yr]:", round(gages()$period, 1), "<br>",
                                           "Start Date:", gages()$begin_date, "<br>",
                                           "End Date:", gages()$end_date, "<br>",
                                           "Drainage Area [sq. mi]:", gages()$drain_area_va, "<br>",
                                           "Urban area [%]:", round(gages()$DEV), "<br>",
                                           "Annual Precip [in]:", round(gages()$PPTAVG_BASIN / 2.54), "<br>",
                                            "15-min data?:", gages()$Instant))

    })
    
    output$gage_table <- renderDataTable(
        gages() %>%
            select(site_no, "name" = station_nm, "n_year" = period, begin_date, end_date, "drain_area_mi2" = drain_area_va,
                   "15-min_data" = Instant, "Urban_%" = DEV, "Annual_precip_in" = PPTAVG_BASIN) %>%
            mutate(n_year = as.numeric(round(n_year, 1)),
                   Annual_precip_in = round(Annual_precip_in / 2.54))
    )
    # observeEvent(input$map_type, {
    #     output$legend <- renderPlot({
    #         plot.new()
    #         if (input$map_type == "Categories"){
    #             legend("top", legend = c("Low", "Med", "High"), pch = 16, col = viridis::viridis_pal()(3), cex = 1.5, bty = "n")
    #         } else if (input$map_type == "Values"){
    #             legend("top", legend = c(seq(0.1, 0.5, 0.1), 0.75, 1, 1.5, 2, 4, 8), 
    #                    pch = 16, col = viridis::viridis_pal()(11), cex = 1.5, bty = "n")
    #         }
    # 
    #     })
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
