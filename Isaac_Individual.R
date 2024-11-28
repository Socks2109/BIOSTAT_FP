HIV_TB <- read.csv("data/HIV_TB_INCIDENCE_PREVALENCE_DEATHS.csv")
Malaria <- read.csv("data/MALARIA_INCIDENCE_DEATHS.csv")

library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyr)
library(infer) # for hypothesis testing
library(countrycode)

valid_iso3 <- unique(countrycode::codelist$iso3c) # checks for valid iso3 codes

HIV_TB_filtered <- HIV_TB %>%
  filter(age_group_name == "All Ages") %>%
  filter(location_code %in% valid_iso3) %>%
  filter(metric == "Deaths") %>%
  filter(unit == "Number") %>%
  filter(sex_name == "Both sexes") %>%
  filter(year >= 2002)

Malaria_filtered <- Malaria %>%
  filter(age_group_name == "All Ages") %>%
  filter(location_code %in% valid_iso3) %>%
  filter(metric == "Deaths") %>%
  filter(unit == "Number") %>%
  filter(sex_name == "Both sexes") %>%
  filter(year >= 2002)

HIV_2002 <- HIV_TB_filtered %>%
  filter(year == 2002) %>%
  pull(mean)

HIV_2002 <- as.numeric(HIV_2002)

HIV_2013 <- HIV_TB_filtered %>%
  filter(year == 2013) %>%
  pull(mean)

HIV_2013 <- as.numeric(HIV_2013)

t_test_result <- t.test(
  x = HIV_2002,          # Observations for 2002
  y = HIV_2013,          # Observations for 2013
  alternative = "two.sided", 
  paired = TRUE,
  conf.level = 0.95
)

print(t_test_result)
