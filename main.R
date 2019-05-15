setwd('C:/Users/nhayden/OneDrive/work/Meetup')
library(tidyverse)
library(broom)
library(lubridate)

combined_data <- read_csv("DEC Data.csv") %>% mutate(VisitDate = parse_date(VisitDate, "%m/%d/%Y")) %>% 
  mutate(Year = year(VisitDate)) 
agg_combined <- combined_data %>% group_by(Test, Year) %>% summarize(medianResult = median(Result)) %>%
  ungroup() %>% complete(Test, Year)
ggplot(data = agg_combined , aes(x = Year, y = medianResult)) + geom_line() + facet_wrap(~Test, scales = "free_y", nrow = 3)

pivot_combined <- agg_combined %>% spread(Test, medianResult)
lm.fit <- lm(Chlorophyll_a ~ Total_Nitrogen + Dissolved_Oxygen, data = pivot_combined)
tidy(lm.fit)
