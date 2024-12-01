HIV_TB <- read.csv("data/HIV_TB_INCIDENCE_PREVALENCE_DEATHS.csv")
Malaria <- read.csv("data/MALARIA_INCIDENCE_DEATHS.csv")

library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyr)
library(infer)
library(countrycode)
library(gt)

# Research question: Is the mortality rate of HIV, TB, and Malaria
# between 2002 and 2013 significantly different in Vietnam compared to that in Somalia?

# Null Hypothesis (H₀): The mortality rate of HIV, TB, and Malaria
# between 2002 and 2013 in Vietnam is not significantly different compared to that in Somalia.

# Alternative Hypothesis (H₁): The cumulative mortality rate of HIV, TB,
# and Malaria between 2002 and 2013 in Vietnam is significantly different compared to that in Somalai.

# ==============================================================================

# Using the countrycode package, retrieve every country's ISO3 code, e.g: Australia = AUS
valid_iso3 <- unique(countrycode::codelist$iso3c)

# Perform data cleaning, filtering out invalid ISO3 codes, retrieving only "deaths" from "both sexes" in "all ages" from year "2002" and greater.
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

# HIV means for Somalia between the years 2002 and 2013
HIV_data_Somalia <- HIV_TB_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Somalia") %>%
  filter(cause_name == "HIV/AIDS") %>%
  pull("mean")

HIV_data_Somalia <- as.numeric(HIV_data_Somalia)

HIV_data_Vietnam <- HIV_TB_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Vietnam") %>%
  filter(cause_name == "HIV/AIDS") %>%
  pull("mean")

HIV_data_Vietnam <- as.numeric(HIV_data_Vietnam)

TB_data_Somalia <- HIV_TB_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Somalia") %>%
  filter(cause_name == "Tuberculosis") %>%
  pull("mean")

TB_data_Somalia <- as.numeric(TB_data_Somalia)

TB_data_Vietnam <- HIV_TB_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Vietnam") %>%
  filter(cause_name == "Tuberculosis") %>%
  pull("mean")

TB_data_Vietnam <- as.numeric(TB_data_Vietnam)

Malaria_data_Somalia <- Malaria_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Somalia") %>%
  filter(cause_name == "Malaria") %>%
  pull("mean")

Malaria_data_Somalia <- as.numeric(Malaria_data_Somalia)

Malaria_data_Vietnam <- Malaria_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Vietnam") %>%
  filter(cause_name == "Malaria") %>%
  pull("mean")

Malaria_data_Vietnam <- as.numeric(Malaria_data_Vietnam)

# ===================================================================

HIV_t_test <- t.test(
  x = HIV_data_Somalia,
  y = HIV_data_Vietnam,
  alternative = "two.sided", 
  paired = FALSE,
  conf.level = 0.95
)

HIV_t_test

TB_t_test <- t.test(
  x = TB_data_Somalia,
  y = TB_data_Vietnam,
  alternative = "two.sided", 
  paired = FALSE,
  conf.level = 0.95
)

TB_t_test

Malaria_t_test <- t.test(
  x = Malaria_data_Somalia,
  y = Malaria_data_Vietnam,
  alternative = "two.sided", 
  paired = FALSE,
  conf.level = 0.95
)

Malaria_t_test

# =================================================================

# Combine HIV, TB, and Malaria data into a tidy dataframe
years <- 2002:2013

# Create dataframes for each disease and country
HIV_Somalia <- data.frame(
  year = years,
  location = "Somalia",
  cause = "HIV/AIDS",
  rate = HIV_data_Somalia
)

HIV_Vietnam <- data.frame(
  year = years,
  location = "Vietnam",
  cause = "HIV/AIDS",
  rate = HIV_data_Vietnam
)

TB_Somalia <- data.frame(
  year = years,
  location = "Somalia",
  cause = "Tuberculosis",
  rate = TB_data_Somalia
)

TB_Vietnam <- data.frame(
  year = years,
  location = "Vietnam",
  cause = "Tuberculosis",
  rate = TB_data_Vietnam
)

Malaria_Somalia <- data.frame(
  year = years,
  location = "Somalia",
  cause = "Malaria",
  rate = Malaria_data_Somalia
)

Malaria_Vietnam <- data.frame(
  year = years,
  location = "Vietnam",
  cause = "Malaria",
  rate = Malaria_data_Vietnam
)

# Combine all dataframes into one
mortality_data <- bind_rows(HIV_Somalia, HIV_Vietnam, TB_Somalia, TB_Vietnam, Malaria_Somalia, Malaria_Vietnam)

# ===========================================================================

ggplot(mortality_data, aes(x = year, y = rate, color = location, linetype = cause)) +
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  labs(
    title = "Mortality Rates of HIV, TB, and Malaria in Somalia and Vietnam (2002–2013)",
    x = "Year",
    y = "Mortality Rate",
    color = "Country",
    linetype = "Cause of Death"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
