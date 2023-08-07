library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(stringr)
library(maps)

wa_pop <- read.csv(
  "https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-prison-jail-rates-1990-WA.csv"
)

print(names(wa_pop))


average_pop <- wa_pop %>%
  group_by(year) %>%
  summarize(avg_jail_pop = mean(total_pop))

average_pop

# This is for all years
#average_population <- wa_pop %>%
#  summarize(avg_population = mean(total_pop)) 

average_jail_pop_rate_1990 <- wa_pop %>%
  filter(year == min(year)) %>%
  summarise(mean(total_jail_pop_rate, na.rm = TRUE))

average_jail_pop_rate_2018 <- wa_pop %>%
  filter(year == max(year)) %>%
  summarise(mean(total_jail_pop_rate, na.rm = TRUE))

average_jail_pop_rate_1990
average_jail_pop_rate_2018

median_jail_pop_rate <- wa_pop %>%
  filter(total_jail_pop_rate == median(total_jail_pop_rate, na.rm = TRUE))

median_jail_pop_rate


highest_jail_pop_rate <- wa_pop %>%
  filter(total_jail_pop_rate == max(total_jail_pop_rate, na.rm = TRUE))
highest_jail_pop_rate

average_black_rate <- wa_pop %>%
  summarise(mean(black_jail_pop_rate, na.rm = TRUE))

average_white_rate <- wa_pop %>%
  summarise(mean(white_jail_pop_rate, na.rm = TRUE))
average_black_rate
average_white_rate


#King county and Whatcom
two_county <- wa_pop %>%
  filter(county_name %in% c("King County", "Whatcom County"))

chart1 <- ggplot(data = two_county, aes(x = year, y = black_jail_pop_rate, color = county_name)) +
  geom_line() +
  labs(title = "Trend of Black Jail Population in each County",
       subtitle = "(Data from 1990 to 2018)",
       x = "Year",
       y = "Black Jail Population",
       color = "County")+
  theme_minimal()
chart1


average_black_jail_pop_rate <- wa_pop %>%
  group_by(year) %>%
  summarize(avg_black_jail_pop_rate = mean(black_jail_pop_rate))
average_black_jail_pop_rate

average_white_jail_pop_rate <- wa_pop %>%
  group_by(year) %>%
  summarize(avg_white_jail_pop_rate = mean(white_jail_pop_rate))
average_white_jail_pop_rate

ggplot(average_black_jail_pop_rate, aes(x = year)) +
  geom_line(aes(y = avg_black_jail_pop_rate, color = "Black Jail Population")) +
  geom_line(aes(y = average_white_jail_pop_rate$avg_white_jail_pop_rate, color = "White Jail Population")) +
  scale_x_continuous(limits = c(2010, 2018)) +
  labs(title = "Average Population Rate in Jails by Year",
       x = "Year",
       y = "Average Jail Population Rate",
       subtitle = "Data from year 2010 to 2018")

chart2 <- ggplot(wa_pop, aes(x = white_jail_pop_rate, y = black_jail_pop_rate)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  labs(title = "White Jail Population vs Black Jail Population",
       x = "White Jail Population Rate",
       y = "Black Jail Population Rate")
chart2

#most recent year
wa_pop <- read.csv(
  "https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-prison-jail-rates-1990-WA.csv"
)

wa_pop <- wa_pop %>%
  filter(year == max(year))

wa_counties <- map_data("county", region = "washington")
wa_counties <- wa_counties %>%
  mutate(subregion = str_to_title(subregion) %>% str_c(" County"))

# Merge data
merged_data <- left_join(wa_counties, wa_pop, by = c("subregion" = "county_name"))


chart3 <- ggplot(data = merged_data, aes(x = long, y = lat, group = group, fill = total_jail_pop_rate)) +
  geom_polygon() +
  coord_fixed(1) +
  labs(title = "Total Jail Population Rate of All WA Counties in 2018",
       fill = "Total Jail Rate",
       x = "Longitude",
       y = "Latitude" ) +
  scale_fill_gradient(name = "Total Jail Rate", 
                      low = "yellow", high = "red",
  ) +
  theme_minimal()

print(chart3)