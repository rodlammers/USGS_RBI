library(dataRetrieval)
library(dplyr)
library(tigris)

data(fips_codes, package = "tigris")

states <- group_by(fips_codes, by = state) %>%
  summarize(state = first(state),
            state_code = first(state_code),
            state_name = first(state_name)) %>%
  filter(!(state_code %in% c(60, 66, 69, 74, 78))) #remove some island territories

calc_parameters <- function(data){
  colnames(data)[c(3, 4)] <- c("Date", "Q")
  summary <- data %>%
    filter(!is.na(Q)) %>%#drop NAs
    summarize(site_no = first(site_no),
              RB = sum(abs(diff(Q))) / sum(Q),
              years = n() / 365,
              start_date = min(as.Date(Date)),
              end_date = max(as.Date(Date)),
              perc_missing = 1 - n() / (as.numeric(end_date - start_date) + 1))
}

get_data <- function(state_code, state){
  gages <- whatNWISsites(stateCd = state_code,
                  hasDataTypeCd = "dv",
                  parameterCd = c("00060"),
                  startDt = "2010-01-01",
                  endDt = "2019-12-31") %>%#,
      #startDT = "2004-01-01",
      #endDT = "2008-12-31") %>%
      filter(site_tp_cd == "ST")
  
  gage_attr <- readNWISsite(gages$site_no)
  
  flows <- lapply(gage_attr$site_no, readNWISdv, parameterCd = "00060", startDate = "2010-01-01", endDate = "2019-12-31")
  keep_flows <- which(sapply(flows, nrow) != 0)
  flows <- flows[keep_flows]
  
  RBI <- lapply(flows, calc_parameters)
  RBI <- do.call("rbind", RBI) %>%
    filter(!is.na(RBI),
           perc_missing < 0.1,
           years > 3) %>%
    left_join(gage_attr, by = "site_no") %>%
    mutate(class = as.factor(case_when(RB <= 0.2 ~ "Low",
                                       RB > 0.5 ~ "High",
                                       TRUE ~ "Med")),
           class = factor(class, levels(class)[c(1, 3, 2)]))
  
  write.csv(RBI, paste0("RBI_data/", state, ".csv"), row.names = FALSE)


}

for (i in 1:nrow(states)){
  get_data(state_code = states$state_code[i], state = states$state[i])
}
