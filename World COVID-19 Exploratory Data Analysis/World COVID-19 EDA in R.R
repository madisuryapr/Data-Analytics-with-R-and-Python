# WORLD COVID-19 EXPLORATORY DATA ANALYSIS (EDA)

# Import Corresponding Packages
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(knitr)

# Data Import
world_covid19 <- read_excel(
  "D:/Data Analytics/R Programming/Portfolio/World COVID-19 Exploratory Data Analysis/World Corona Virus (COVID-19) Dataset.xlsx", 
  sheet = "cleansed_data")

# View The Data
world_covid19

#### Q1: How many locations are there within the table for each continent?
countries_per_continent <- world_covid19 %>% 
  group_by(continent) %>% 
    summarize(number_of_locations = n_distinct(location)) %>% 
  arrange(desc(number_of_locations))

#### Q2: What is the average number of reproduction rate for each location in Africa? 
#### Select Top 20 Locations
africa_average_reproduction_rate <- world_covid19 %>% 
  filter(continent == "Africa") %>% 
    group_by(location) %>% 
      summarize(average_reproduction_rate = mean(reproduction_rate)) %>% 
    mutate(across(where(is.numeric), round, 4)) %>% 
  arrange(desc(average_reproduction_rate)) %>% 
top_n(20)

#### Q3: Who are the 5 locations with highest total COVID-19 casualties 
#### that have been recorded for each location around Oceania in 2021?
total_casualties_oceania <- world_covid19 %>%
  filter(continent == "Oceania", year == 2021) %>%
    group_by(location) %>% 
      summarize(total_covid19_casualties = sum(new_deaths)) %>%
  arrange(desc(total_covid19_casualties)) %>% 
top_n(5)

#### Q4: What is the total number of COVID-19 tests 
#### that have been conducted for each location within Asian continent during 2022? 
#### Present only 15 locations with highest tests
total_tests_asia <- world_covid19 %>% 
  filter(
    continent == "Asia", year == 2022) %>%   
      group_by(location) %>%
        summarize(
          total_number_of_tests = sum(new_tests)) %>%
      arrange(
        desc(total_number_of_tests)) %>%
top_n(15)

#### Q5: Observe the average stringency index for 
#### Indonesia each month every year until 2022
Indonesia_average_stringency <- world_covid19 %>%
  filter(
    location == "Indonesia", year %in% c(2020, 2021, 2022)
  ) %>%
  group_by(
    year, month(date, label = TRUE)
  ) %>%
  summarize(average_stringency_index = 
              mean(stringency_index)) %>%
  mutate(
    across(where(is.numeric), round, 2)
  )

#### Q6: How is the trend of stringency index for The United Kingdom in 2022?
world_covid19 %>%
  filter(year == 2022, location == "United Kingdom") %>%
  select(location, year, date, stringency_index) %>%
  ggplot(
    aes(x = date, y = stringency_index)
  ) + geom_line(size = 1, color = "#CC274C") +
  labs(
    x = "Date Time",
    y = "Stringency Index",
    caption = "Source: Our world in Data"
  ) + theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "#3D4251"),
    rect = element_blank(),
    axis.ticks = element_line(colour = "#3D4251"),
    axis.text = element_text(color = "black")
  ) +
  scale_y_continuous(
    expand = c(0,0), 
    limits = c(0, 50),
    breaks = c(0, 10, 20, 30, 40, 50)
  )

#### Q7: How is the data distribution of COVID-19 New Cases in Japan during 2022?
world_covid19 %>%
  filter(location == "Japan", year == 2022) %>%
  select(
    location, year, date, stringency_index, new_cases
  ) %>% ggplot(
    aes(
      x = new_cases
    )
  ) + geom_histogram(fill = "#023246") +
  labs(
    x = "Number of New Cases",
    y = "Count Number",
    caption = "Source: Our World in Data"
  ) + 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "#3D4251"),
    rect = element_blank(),
    axis.ticks = element_line(colour = "#3D4251"),
    axis.text = element_text(color = "black")
  ) +
  scale_x_continuous(
    expand = c(0,0), 
    limits = c(0, 350000),
    breaks = c(0, 100000, 200000, 300000)
  ) +
  scale_y_continuous(
    expand = c(0,0), 
    limits = c(0,60), 
    breaks = c(15, 30, 45, 60)
  )

#### Q8: Who are the 10 highest average COVID-19 new cases in Asia during 2021?
world_covid19 %>%
  filter(continent == "Asia", year == 2021) %>%
  select(continent, location, date, year, new_cases) %>%
  group_by(location) %>%
  summarize(
    avg_new_cases = mean(new_cases)
  ) %>% 
  arrange(desc(avg_new_cases)) %>% 
  top_n(10) %>% 
  ggplot(
    aes(
      x = avg_new_cases, 
      y = reorder(location, + avg_new_cases), 
      color = avg_new_cases
    )
  ) + 
  geom_point(size = 6) +
  geom_segment(aes(xend = 0, yend = location), size = 2) +
  geom_text(
    aes(
      label = round(avg_new_cases, 0)), 
    color = "black", 
    size = 2.5, 
    position = "identity", hjust = -0.6) +
  labs(
    x = "Average New Cases",
    y = "Location",
    caption = "Source: Our World in Data"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "#3D4251"),
    rect = element_blank(),
    axis.ticks = element_line(colour = "#3D4251"),
    legend.title = element_blank(),
    axis.text = element_text(color = "black")
  ) +
  scale_x_continuous(
    expand = c(0,0), 
    limits = c(0,80000), 
    breaks = c(0, 20000, 40000, 60000, 80000)
  ) +
  scale_color_gradient2(
    mid = "#28B67E", high = "#1D4C4F"
  )