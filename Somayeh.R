HIV_TB <- read.csv("data/HIV_TB_INCIDENCE_PREVALENCE_DEATHS.csv")
Malaria <- read.csv("data/MALARIA_INCIDENCE_DEATHS.csv")

library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyr)
library(infer)
library(countrycode)
library(gt)

# Individual Portion

# Somayeh

# Research question: Does the mortalities caused by HIV, TB, and malaria in Vietnam, and Somalia differ in the years 2002 and 2013?
# Null Hypothesis (H₀): HIV, TB, and Malaria mortalities have not decreased in Vietnam, and Somalia significantly between the years 2002 and 2013.

# Alternative Hypothesis (H₁): HIV, TB, and Malaria deaths have decreased in Vietnam, and Somalaia significantly between the years 2002 and 2013.

# datasets are loaded above.
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


# HIV means for Somalia between the years 2002 and 2013

HIV_data_Somalia <- HIV_TB_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Somalia") %>%
  filter(cause_name == "HIV/AIDS") %>%
  select(year, mean)
early_period <- filter(HIV_data_Somalia, year <= 2007)
late_period <- filter(HIV_data_Somalia, year > 2007)
early_means <- as.numeric(early_period$mean)
late_means <- as.numeric(late_period$mean)
print(HIV_data_Somalia)
                      
# Perform an independent t-test
HIV_t_test <- t.test(early_means, late_means, alternative = "two.sided", conf.level = 0.95)
print(HIV_t_test)

# Results: Two Sample t-test, t-value = -4.0391, p-value = 0.008055
# alternative hypothesis: true mean difference is not equal to 0. Since the p-value < 0.05, we reject null hypothesis. Therefore, HIV deaths have decreased in Somalia between the years 2002 and 2013.



# TB means for Somalia in the years 2002 and 2013
TB_data_Somalia <- HIV_TB_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Somalia") %>%
  filter(cause_name == "Tuberculosis") %>%
  select(year, mean)
early_period <- filter(TB_data_Somalia, year <= 2007)
late_period <- filter(TB_data_Somalia, year > 2007)
early_means <- as.numeric(early_period$mean)
late_means <- as.numeric(late_period$mean)
print(TB_data_Somalia)

# Perform an independent t-test
TB_t_test <- t.test(early_means, late_means, alternative = "two.sided", conf.level = 0.95)
print(TB_t_test)

# Results: Two Sample t-test, t-value = -5.265, p-value = 0.0004222
# alternative hypothesis: true mean difference is not equal to 0. Since the p-value < 0.05, we reject null hypothesis. Therefore, TB deaths have decreased in Somalia between the years 2002 and 2013.


# Malaria means for Somalia in years 2002 and 2013
Malaria_data_Somalia <- Malaria_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Somalia") %>%
  filter(cause_name == "Malaria") %>%
  select(year, mean)
early_period <- filter(Malaria_data_Somalia, year <= 2007)
late_period <- filter(Malaria_data_Somalia, year > 2007)
early_means <- as.numeric(early_period$mean)
late_means <- as.numeric(late_period$mean)
print(Malaria_data_Somalia)

# Perform an independent t-test
Malaria_t_test <- t.test(early_means, late_means, alternative = "two.sided", conf.level = 0.95)
print(Malaria_t_test)


# Results: Two Sample t-test, t-value = 5.4488, p-value = 0.002031
# alternative hypothesis: true mean difference is not equal to 0. Since the p-value < 0.05, we reject null hypothesis. Therefore, Malaria deaths have decreased in Somalia between the years 2002 and 2013.

# Visualization
HIV_data_Somalia$mean <- as.numeric(HIV_data_Somalia$mean)
TB_data_Somalia$mean <- as.numeric(TB_data_Somalia$mean)

HIV_data_Somalia_sum <- sum(HIV_data_Somalia, na.rm = TRUE)
TB_data_Somalia_sum <- sum(TB_data_Somalia, na.rm = TRUE)
Malaria_data_Somalia_sum <- sum(Malaria_data_Somalia, na.rm = TRUE)

summary_data <- data.frame(
  condition = rep(c("HIV", "Malaria", "TB"), each = 2),
  year = rep(c(2002, 2013), times = 3),
  sum_value = c(HIV_data_Somalia_sum, TB_data_Somalia_sum, Malaria_data_Somalia_sum)
)

print(summary_data)

ggplot(summary_data, aes(x = condition, y = sum_value, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2002" = "cornflowerblue", "2013" = "darkslategray1")) +
  labs(
    title = "Sum of Deaths for HIV, Malaria, and TB (2002 vs. 2013)",
    x = "Disease",
    y = "Sum of Deaths",
    fill = "Year"
  ) +
  theme_minimal()




# HIV means for Vietnam in years 2002 and 2013
HIV_data_Vietnam <- HIV_TB_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Vietnam") %>%
  filter(cause_name == "HIV/AIDS") %>%
  select(year, mean)
early_period <- filter(HIV_data_Vietnam, year <= 2007)
late_period <- filter(HIV_data_Vietnam, year > 2007)
early_means <- as.numeric(early_period$mean)
late_means <- as.numeric(late_period$mean)
print(HIV_data_Vietnam)

# Perform an independent t-test
HIV_t_test <- t.test(early_means, late_means, alternative = "two.sided", conf.level = 0.95)
print(HIV_t_test)

# Results: Two Sample t-test, t-value = -1.8786, p-value = 0.1177
# alternative hypothesis: true mean difference is not equal to 0. Since the p-value > 0.05, we fail to reject null hypothesis. Therefore, HIV deaths have not decreased in Vietnam between the years 2002 and 2013.


# TB means for Vietnam in years 2002 and 2013
TB_data_Vietnam <- HIV_TB_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Vietnam") %>%
  filter(cause_name == "Tuberculosis") %>%
  select(year, mean)
early_period <- filter(TB_data_Vietnam, year <= 2007)
late_period <- filter(TB_data_Vietnam, year > 2007)
early_means <- as.numeric(early_period$mean)
late_means <- as.numeric(late_period$mean)
print(TB_data_Vietnam)

# Perform an independent t-test
TB_t_test <- t.test(early_means, late_means, alternative = "two.sided", conf.level = 0.95)
print(TB_t_test)

# Results: Two Sample t-test, t-value = 4.1684, p-value = 0.008347
# alternative hypothesis: true mean difference is not equal to 0. Since the p-value < 0.05, we reject null hypothesis. Therefore, TB deaths have decreased in Vietnam between the years 2002 and 2013.



# Malaria means for Vietnam in years 2002 and 2013
Malaria_data_Vietnam <- Malaria_filtered %>%
  filter(year >= 2002 & year <= 2013) %>%
  filter(location_name == "Vietnam") %>%
  filter(cause_name == "Malaria") %>%
  select(year, mean)
early_period <- filter(Malaria_data_Vietnam, year <= 2007)
late_period <- filter(Malaria_data_Vietnam, year > 2007)
early_means <- as.numeric(early_period$mean)
late_means <- as.numeric(late_period$mean)
print(Malaria_data_Vietnam)

# Perform an independent t-test
Malaria_t_test <- t.test(early_means, late_means, alternative = "two.sided", conf.level = 0.95)
print(Malaria_t_test)

# Results: Two Sample t-test, t-value = 5.5193, p-value = 0.0003262
# alternative hypothesis: true mean difference is not equal to 0. Since the p-value < 0.05, we reject null hypothesis. Therefore, Malaria deaths have decreased in Vietnam between the years 2002 and 2013.
# Visualization
HIV_data_Vietnam$mean <- as.numeric(HIV_data_Vietnam$mean)
TB_data_Vietnam$mean <- as.numeric(TB_data_Vietnam$mean)
Malaria_data_Vietnam$mean <- as.numeric(Malaria_data_Vietnam$mean)

HIV_data_Vietnam_sum <- sum(HIV_data_Vietnam, na.rm = TRUE)
TB_data_Vietnam_sum <- sum(TB_data_Vietnam, na.rm = TRUE)
Malaria_data_Vietnam_sum <- sum(Malaria_data_Vietnam, na.rm = TRUE)

summary_data <- data.frame(
  condition = rep(c("HIV", "Malaria", "TB"), each = 2),
  year = rep(c(2002, 2013), times = 3),
  sum_value = c(HIV_data_Vietnam_sum, TB_data_Vietnam_sum, Malaria_data_Vietnam_sum)
)

print(summary_data)

ggplot(summary_data, aes(x = condition, y = sum_value, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2002" = "cornflowerblue", "2013" = "darkslategray1")) +
  labs(
    title = "Sum of Deaths for HIV, Malaria, and TB in Vietnam (2002 vs. 2013)",
    x = "Disease",
    y = "Sum of Deaths",
    fill = "Year"
  ) +
  theme_minimal()
