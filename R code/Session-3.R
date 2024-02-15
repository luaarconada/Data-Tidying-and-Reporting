
## ----------------------------------------------------------------------------------------------------------------------
library(nycflights13)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggThemeAssist)

#Load the data, 'flights' is contained on the package 'nycflights13'
alaska_flights <- flights %>% filter(carrier == "AS")

#Create a simple plot and use it to see how ggThemeAssist works.
ggplot(alaska_flights) +
  aes(x=dep_delay, y=arr_delay) +
  geom_point()
#We select the ggplot plot above and on the Addins menu we have 'ggplot Theme Assist' or something similar,
#which opens a shiny app to add this to my plot and it creates the code.

#We have this more complete plot
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point() + theme(axis.title = element_text(size = 13),
    plot.title = element_text(size = 12),
    plot.background = element_rect(fill = "white",
        linetype = "dashed")) +labs(x = "Departure", y = "Delay", subtitle = "Example plot")
## ----------------------------------------------------------------------------------------------------------------------
library(magrittr)

# Pipe basics
head(flights, n = 10)

flights %>% head(n = 10)
summary(head(flights))

#Let's do the code above, but with a pipeline
flights %>%
  head(n = 10, x = .) %>%
  summary()

## ----------------------------------------------------------------------------------------------------------------------
#We wanna see the first observations of the flights that arrive in Portland (PDX)
head(flights[flights$dest == "PDX", ])

portland_flights <- flights %>%
  filter(dest == "PDX")
portland_flights %>% head()
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We now wanna see the flights that leave JFK and arrive at BTV or SEA in the last 3 months of the year
btv_sea_flights_fall <- flights %>%
  filter(origin == "JFK",
        (dest == "BTV" | dest == "SEA"),
        month >= 10) #We can filter according to various expressions
#It works both with , and &; we like , because of aesthetic, we can press enter so we can see all the conditions better
btv_sea_flights_fall %>% head()
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We wanna see the flights that arrive at SEA, SFO, PDX, BTV or BDL.
many_airports <- flights %>%
  filter(dest %in% c("SEA", "SFO", "PDX", "BTV", "BDL"))
many_airports %>% head()
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We see a summary of the variable 'temp' in the 'weather' dataset which is contained on the package 'nycflights13'
summary_temp <- weather %>%
  summarize(mean = mean(temp, na.rm = TRUE), std_dev = sd(temp, na.rm = TRUE))
summary_temp

# Back and ford tibble to df
as_tibble(as.data.frame(weather))
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
# In base R:
?aggregate
?tapply

# Differences in structure
weather_grouped <- weather %>%
  group_by(month)
str(weather)
str(weather_grouped)

#Summary of the 'temp' variable grouped by month
summary_monthly_temp <- weather %>%
  group_by(month) %>%
  summarize(my_mean = mean(temp, na.rm = TRUE), my_std = sd(temp, na.rm = TRUE))
summary_monthly_temp
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
# One possibility (we group the flights by origin and obtain a table with the number of observations in each group)
flights %>% select(origin) %>% table()
#The asame
flights %>%
  select(origin) %>%
  table()


# The tidy option
by_origin <- flights %>%
  group_by(origin) %>%
  summarize(count = n()) #count is to count the number of observations
by_origin

#Other way
flights %>%
  count(origin)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We group by origin and month. 36 groups because 12 (month) x 3 (origin) 
flights_grouped <- flights %>%
  group_by(origin, month)
str(flights_grouped)

by_origin_monthly <- flights %>%
  group_by(origin, month) %>%
  summarize(count = n())
by_origin_monthly %>% print(n = 50)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We create a new variable 'temp_in_c' that computes the given temperature in degrees celsius
weather <- weather %>%
  mutate(temp_in_C = (temp - 32) / 1.8)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We compute the means of the temperature in C and in F grouped by months
summary_monthly_temp <- weather %>%
  group_by(month) %>%
  summarize(
    mean_temp_in_F = mean(temp, na.rm = TRUE),
    mean_temp_in_C = mean(temp_in_C, na.rm = TRUE)
  )
summary_monthly_temp
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We create a variable called 'gain'
flights <- flights %>%
  mutate(gain = dep_delay - arr_delay)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We compute the mean of all the variables
flights %>%
  summarise(across(where(is.numeric),function(x) mean(x, na.rm=TRUE)))
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We create a detailed summary of the variable 'gain': minimun, median, maximun, meadn, standard deviation, number of NA's
# and the first and third quantile
gain_summary <- flights %>%
  summarize(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )
gain_summary
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
ggplot(data = flights, mapping = aes(x = gain)) +
  geom_histogram(color = "white", bins = 20)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
ggplot(data = flights, mapping = aes(x = gain)) +
  geom_histogram(color = "white", bins = 20, fill = "lightblue") +
  theme(
    panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_line(linetype = "dotted"),
    panel.background = element_rect(fill = "wheat1")
  )
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We create 3 variables at the same time, using newly created variables in creating another at the same time
flights <- flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )

flights[1:10, 19:22]
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We group by destination, sum the number of observation and order by amount of flights
freq_dest <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(num_flights)
freq_dest
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#WE order by number of flights?
freq_dest %>%
  arrange(desc(num_flights)) #desc() is to have descending order
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We join the flights and airlines databases by the 'carrier' variable 
flights_joined <- flights %>%
  inner_join(airlines, by = "carrier")

flights[1:7, 18:22]
flights_joined[1:7, 18:23]
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We join flights and airports by the variables 'dest' and 'faa' which contain the same but different names
flights_with_airport_names <- flights %>%
  inner_join(airports, by = c("dest" = "faa")) # dest in flights, faa in airports
glimpse(flights_with_airport_names)
flights_with_airport_names[1:7, 17:27]
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
named_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  rename(airport_name = name) # rename(new = old)
named_dests
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
#We join flights and weather
flights_weather_joined <- flights %>%
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))
flights_weather_joined[1:7, 26:33]

# Investigate delays according to visibility
plot(dep_delay ~ visib, data = flights_weather_joined)

# Correlations of delay with weather variables
flights_weather_joined %>%
  select(dep_delay, visib, precip, wind_speed) %>%
  cor(use = "complete.obs")
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
joined_flights <- flights %>%
  inner_join(airlines, by = "carrier")
# View(joined_flights)
joined_flights[1:8, c(1:4, 20:23)]
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
glimpse(flights)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
flights %>%
  select(carrier, flight)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
flights_no_year <- flights %>% select(-year)
flights_no_year[1:4, ]
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
flight_arr_times <- flights %>% select(year:day, arr_time:arr_delay)
flight_arr_times
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
flights_reorder <- flights %>%
  select(year, month, day, hour, minute, time_hour, everything())
glimpse(flights_reorder)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
flights %>% select(starts_with("arr_"))
flights %>% select(ends_with("delay"))
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
flights %>% select(contains("time"))
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
flights_time_new <- flights %>%
  select(dep_time, arr_time) %>%
  rename(departure_time = dep_time, arrival_time = arr_time) # rename(new = old)
glimpse(flights_time_new)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
named_dests %>%
  top_n(n = 10, wt = num_flights) %>%
  arrange(desc(num_flights))

# Newer approach using slice_*()
named_dests %>%
  slice_max(n = 10, order_by = num_flights)
named_dests %>%
  slice_min(n = 10, order_by = num_flights)
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
library(readr)
dem_score <- read_csv("https://moderndive.com/data/dem_score.csv")
dem_score
## ----------------------------------------------------------------------------------------------------------------------

# using the GUI
library(readxl)
dem_score <- read_excel("/cloud/project/Session 3/dem_score.xlsx",
                        col_types = c("text", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric"))

## ----------------------------------------------------------------------------------------------------------------------
library(fivethirtyeight)
drinks

# Top 10 countries with more total_litres_of_pure_alcohol
drinks %>%
  slice_max(order_by = total_litres_of_pure_alcohol, n = 10)

## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
drinks_smaller <- drinks %>%
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia", "Spain", "Germany")) %>%
  select(-total_litres_of_pure_alcohol) %>%
  rename(beer = beer_servings, spirit = spirit_servings, wine = wine_servings)
drinks_smaller
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
drinks_smaller_tidy <- drinks_smaller %>%
  pivot_longer(
    names_to = "type",
    values_to = "servings",
    cols = beer:wine
  )
drinks_smaller_tidy
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
drinks_smaller %>%
  pivot_longer(
    names_to = "type",
    values_to = "servings",
    cols = c(beer, spirit, wine)
  )
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
ggplot(drinks_smaller_tidy, aes(x = country, y = servings, fill = type)) +
  geom_col(position = "dodge")
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
ggplot(drinks_smaller_tidy, aes(x = country, fill = type, y = servings)) +
  geom_col() +
  xlab("country")
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
ggplot(drinks_smaller_tidy, aes(x = country, fill = type, y = servings)) +
  geom_col(position = "fill") +
  xlab("proportion")
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
guat_dem <- dem_score %>%
  filter(country == "Guatemala")
guat_dem
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
guat_dem_tidy <- guat_dem %>%
  pivot_longer(
    names_to = "year",
    values_to = "democracy_score",
    cols = -country,
    names_transform = list(year = as.integer) # To avoid character vectors
  )
guat_dem_tidy
guat_dem_tidy$year
## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
ggplot(guat_dem_tidy, aes(x = year, y = democracy_score)) +
  geom_line() +
  labs(x = "Year", y = "Democracy Score")
## ----------------------------------------------------------------------------------------------------------------------
