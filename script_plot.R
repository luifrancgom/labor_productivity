# Libraries ----
library(rsdmx)
library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)

# Import ----
## This platform has reached the end of its life and will be 
## switched-off end of March 2024.
### Please use instead the new data dissemination platform 
### OECD Data Explorer: https://data-explorer.oecd.org/ 
#### https://stats.oecd.org/ > Productivity > 
#### Productivity and ULC – Annual, Total Economy >
#### Level of GDP per capita and productivity >
#### Level of GDP per capita and productivity
##### Country: ALL
##### Subject: GDP per hour worked
##### Measure: USD, constant prices, 2015 PPPs
##### Time: 1970 - 2022
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/PDB_LV/AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EU28+EU27_2020+EA19+G-7+OECD+NMEC+BRA+BGR+CHN+HRV+IND+IDN+PER+ROU+RUS+ZAF+BRIICS.T_GDPHRS.VPVOB/all?startTime=1970&endTime=2022" 
gdp_per_hour_worked_sdmx <- readSDMX(file = url)
location_labels <- read_csv(file = "001_local_data/location_country.csv")

# Tidy ----
gdp_per_hour_worked_tbl <- gdp_per_hour_worked_sdmx |> 
  as_tibble() |> 
  left_join(y = location_labels, by = join_by(LOCATION == LOCATION),
            relationship = "many-to-one") |> 
  clean_names()

gdp_per_hour_worked_tbl |> glimpse()

# Wrangle
gdp_per_hour_worked_tbl <- gdp_per_hour_worked_tbl |> 
  select(location, obs_time, 
         country, obs_value,
         obs_status) |> 
  mutate(obs_status = case_when(
    obs_status == "B" ~ "Break",
    obs_status == "E" ~ "Estimated value", 
    .default = NA),
         obs_time = as.integer(obs_time)) |> 
  filter(!(location %in% c("EU28", "EU27_2020", "EA19", "G-7")))

gdp_per_hour_worked_tbl |> glimpse()

# Visualize ----
## Static ----
g_static <- gdp_per_hour_worked_tbl |>
  ggplot(aes(x = obs_value, y = fct_reorder(country, obs_value))) + 
  geom_point(shape = 21, fill = "#E31A1C") + 
  geom_segment(aes(xend = 0, yend = fct_reorder(country, obs_value))) +
  scale_x_continuous(breaks = seq.int(from = 0, to = 140, by = 20),
                     limits = c(0, 140)) +
  labs(x = "USD, constant prices, 2015 PPPs",
       y = NULL,
       title = "GDP per hour worked",
       subtitle = "Year: {frame_time}",
       caption = str_glue("Source: OCDE.Stat - OECD Compendium of Productivity Indicators
                          Data extracted: 2024-02-29 21:13 UTC (GMT)")) + 
  theme(panel.border      = element_rect(fill = NA, 
                                         color = "black"),
        plot.background   = element_rect(fill = "#f3fcfc"),
        panel.background  = element_rect(fill = "#f3f7fc"),
        legend.background = element_rect(fill = "#f3fcfc"),
        plot.title        = element_text(face = "bold"),
        axis.title        = element_text(face = "bold"),
        legend.title      = element_text(face = "bold"),
        axis.text         = element_text(face = "bold"))

## Animate ----
g_static |> 
  ggsave(device = "png", 
         units = "px", 
         width = )

g_animate <- g_static +
  transition_time(obs_time)

# Save animation ----
animate(plot = g_animate,  
        renderer = gifski_renderer(file = "productivity_anim.gif"), 
        device = "png",
        units = "in",
        height = 8,
        width = 6,
        res = 150)