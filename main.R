library(tidyverse)
library(broom)
library(lubridate)
library(car)
setwd('C:/Users/nhayden/OneDrive/work/Lake-Champlain-Monitoring')
rm(list = ls())
##==========================================================================================================================================
## Intro-
## Build the model with tests for Chloride, Chlorophyll_a, Dissolved_Oxygem, Dissolved_Silica, Iron, Magnesium, Potassium
## Sodium, Temperature, Total_Nitrogen, and Total_Phosphorus. 
## Chlorophyll_a will be used as an indicator of how much algae is present at the sample site.
##==========================================================================================================================================

raw_data <- read_csv("StationData.csv") %>% mutate(VisitDate = parse_date(VisitDate, "%m/%d/%Y")) %>% 
  mutate(Year = year(VisitDate)) 

## Aggregate median tests values by year.
agg_data <- raw_data %>% group_by(StationID, Station, Test, Year) %>% summarize(medianResult = median(Result, na.rm = T)) %>%
  ungroup()
## Nest data by station.  
by_station <- agg_data %>% group_by(StationID, Station) %>% nest()

fill_in_missing_years <- function(df) {
  df %>% complete(Test, Year)
}

by_station$data <- map(by_station$data, fill_in_missing_years)

test_correlations <- function (df) {
  pivot_tests <- df %>% spread(Test, medianResult)
  cor(pivot_tests %>% select(Chloride:Total_Phosphorus), use = "complete.obs") %>% as.data.frame() %>% select(Chlorophyll_a)  
}

add_rownames <- function(df) {
  df %>% mutate(test_name =rownames(df)) %>% select(test_name, names(df))
}

by_station <- by_station %>% mutate(chlorophyll_correlation = map(data,test_correlations)%>% map(add_rownames)) 
cors <- unnest(by_station, chlorophyll_correlation )

## Plot Correlations with Chlorophyll. 
plot <- ggplot(data = cors, aes(x = Chlorophyll_a)) + geom_histogram(binwidth = 0.1, color="black", fill="grey") + coord_cartesian(xlim = c(-1, 1))
plot + facet_wrap(~test_name, scale = "free", nrow = 3)

## Build linear models for Chlorophyll amount using Phosphorus. 
linear_model <- function (df) {
  df <- df  %>% spread(Test, medianResult)
  lm(Chlorophyll_a ~ Total_Phosphorus , data = df) %>% tidy()
}

by_station <- by_station %>% mutate(linear_model = map(by_station$data, linear_model)) 

## Plot the time series for Chlorophyll. 
time_series <- function(df) {
  df <- df %>% filter(Test == "Chlorophyll_a") %>% select(medianResult)
  ts(df, start = c(1992), frequency = 1 ) 
}

by_station <- by_station %>% mutate(ts_data = map(data, time_series))

par(mfrow = c(3,5))
for (i in 1:15) {
  plot(by_station$ts_data[[i]])
}  


