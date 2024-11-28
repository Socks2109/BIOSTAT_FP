data <- read.csv("data/life_expectancy.csv")
library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyr)
# View(data)

# 2019 life expectancy (for checking)
LE_2019 <- data %>%
  select(location_name, val, year) %>%
  filter(year == 2019) %>%
  arrange(val)

# 2002 life expectancy (for checking)
LE_2002 <- data %>%
  select(location_name, val, year) %>%
  filter(year == 2002) %>%
  arrange(val)

# Combines the data and orders by 2002 LE
combined <- sqldf("SELECT a.location_name as Country,a.val as Life_Expectancy_2002,
                    b.val as Life_Expectancy_2019
                FROM LE_2002 as a
                JOIN LE_2019 as b
                WHERE a.location_name = b.location_name
                ORDER BY a.val ASC;
                ")

# Locks the countries so that ggplot can't auto arrange it
combined$Country <- factor(combined$Country, levels = combined$Country)

# Splits LE in 2002 and 2019 into two rows instead of columns
combined_long <- combined %>%
  pivot_longer(cols = starts_with("Life_Expectancy"), 
               names_to = "Year", 
               values_to = "Life_Expectancy") %>%
  mutate(Year = gsub("Life_Expectancy_", "", Year))


# Plots the LE sorted by 2002 LE
combined_long %>%
  ggplot(aes(x = Country, y = Life_Expectancy, group = Year, color = Year)) +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_color_manual(values = c("2002" = "red", "2019" = "blue")) +
  labs(color = "Year", y = "Life Expectancy",
       x = "Countries sorted by 2002 Life Expectancy",
       title = "Life Expectancy of Countries in 2002 and 2019")

