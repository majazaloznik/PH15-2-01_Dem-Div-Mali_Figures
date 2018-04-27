## Preliminaries ==============================================================
library(readr)
library(dplyr)
library(tidyr)
## Data import ================================================================

pyramids <- read_csv("data/pyramid.csv")

gdp_trend <- read_csv("data/Growth in GDP-Data Visualization_DemDiv.csv")

gdp_cumsum <- read_csv("data/GDP per Capita-Data Visualization_DemDiv.csv")

investment  <- read_csv("data/Investment-Data Visualization_DemDiv.csv")

hdi <- read_csv("data/HDI-Data Visualization_DemDiv.csv")

## Data clean =================================================================

# add proportions
pyramids %>% 
  mutate(scenario = ifelse(scenario == "2050-EE", "2050_EE", scenario)) %>% 
  mutate(scenario = ifelse(scenario == "2050 BAU", "2050_BAU", scenario)) %>% 
  mutate(age.group = ifelse(age.group == '5-9', '05-09', age.group)) %>% 
  spread(gender, population) %>% 
  group_by(scenario) %>% 
  mutate(total = sum(Female + Male),
        female.p = Female/total*100,
        male.p = Male/total*100) %>% 
  gather(gender, proportion , 6:7) %>% 
  select(age.group, scenario, gender, proportion) %>% 
  unite(group, gender, scenario) %>% 
  spread(group, proportion) %>% 
  mutate(age.group = ifelse(age.group == '05-09','5-9', age.group)) -> pyramids

# remove stray empty column
gdp_trend <- gdp_trend[1:4]

#rename columns
names(gdp_trend) <- c("year",
                      "Business_as_usual",
                      "Economic_Emphasis", 
                      "Comnibed_Econ_Educ_FP")
# delete by $1 billion
gdp_trend %>% 
  mutate_at(vars(2:4), funs( ./1000000000))  ->
  gdp_trend

# remove empty row
gdp_cumsum <- gdp_cumsum[1:4,]

# remane values
gdp_cumsum[2:4, 1] <-  c("2050 Business as usual",
                         "2050 Economic Emphasis", 
                         "2050 Comnibed Econ, Educ, FP")
gdp_cumsum <- separate(gdp_cumsum, X1, into = c("year", "thing"), extra = "merge")
gdp_cumsum <- separate(gdp_cumsum, thing, into = c("thing1", "thing2"), extra = "merge")
#rename columns
names(investment) <- c("year",
                      "Business_as_usual",
                      "Economic_Emphasis", 
                      "Comnibed_Econ_Educ_FP")

# hdi in 2013 is 182
hdi <- rbind(list("2013 Baseline", 182),
          hdi)
# remane values
hdi[2:4, 1] <-  c("2050 Business as usual",
                         "2050 Economic Emphasis", 
                         "2050 Comnibed Econ, Educ, FP")
hdi <- separate(hdi, X1, into = c("year", "thing"), extra = "merge")
hdi <- separate(hdi, thing, into = c("thing1", "thing2"), extra = "merge")

