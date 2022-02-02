# tidy tuesday ------------------------------------------------------------

# Bee Colony Data
# Week 2, 2022

# libraries ---------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(streamgraph)
library(MetBrewer)
library(cartogram)
library(geojsonio)
library(sf)

# read data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-01-11')

colony <- tuesdata$colony
stressor <- tuesdata$stressor

# colony ------------------------------------------------------------------

# Mean colony size
mean_n <- 
  colony %>% summarize(mean_n = mean(colony_n, na.rm = T)) %>% pull()

# Bee colony size average by year
mean_by_yr <- 
  colony %>%
  rename(yr = year) %>% 
  group_by(yr) %>% 
  summarize(colony_n_mean = mean(colony_n, na.rm = T)) %>% 
  ungroup() 

# Bee colony size average per state
mean_by_state <- 
  colony %>%
  group_by(state) %>% 
  summarize(colony_n_mean = mean(colony_n, na.rm = T)) %>% 
  ungroup() %>% 
  filter(state != "Other States" & state != "United States") %>% 
  bind_rows(tibble(state = state.name)) %>% 
  group_by(state) %>% 
  mutate(colony_n_mean = ifelse(n() == 1, 0, colony_n_mean)) %>%  
  ungroup() %>% 
  distinct() %>% 
  bind_rows(tibble(state = "District of Columbia", colony_n_mean = 0)) %>% 
  arrange(state)

# USA in hexagons
spdf <- 
  geojson_read(
    paste0(
      "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/",
      "us_states_hexgrid.geojson.json"), 
    what = "sp") 

# Standardize state names
spdf@data <-
  spdf@data %>%
  mutate(state = str_remove(google_name, " \\(United States\\)"))

# Merge colony size with map data
spdf@data <- 
  spdf@data %>% 
  left_join(mean_by_state, by = "state")

spdf_sf <- st_as_sf(spdf)
spdf_proj <- st_transform(spdf_sf, crs = 27561)

# Compute a cartogram using bee colony size data
bee_cartogram <- cartogram_cont(spdf_proj, weight = "colony_n_mean", itermax = 5)

# Plot
library(tmap)
tm_shape(bee_cartogram) + 
  tm_polygons("colony_n_mean", style = "jenks") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

library(broom)
spdf_fortified <- tidy(spdf, region = "google_name")

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_sf, aes(x = long, y = lat, group = group), fill = "skyblue", color = "white") +
  geom_text(data = centers, aes(x = x, y = y, label = id)) +
  theme_void() +
  coord_map()

# stressor ----------------------------------------------------------------

# Definition of "stress_pct":
# Percent of colonies affected by stressors anytime during the quarter, colony
# can be affected by multiple stressors during same quarter.

# Annual stressor percentages by state
stressor %>% 
  group_by(year, state, stressor) %>% 
  summarize(stress_pct_avg = mean(stress_pct, na.rm = T)) %>% 
  ungroup()

# Annual stressor percentages
stressor %>% 
  group_by(year, stressor) %>% 
  summarize(stress_pct_avg = mean(stress_pct, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = stress_pct_avg, color = stressor)) +
  geom_point() +
  geom_line() + 
  theme_classic()

# Streamgraph
stressor %>% 
  group_by(year, stressor) %>% 
  summarize(stress_pct_avg = mean(stress_pct, na.rm = T)) %>% 
  ungroup() %>% 
  streamgraph(date = "year", key = "stressor", value = "stress_pct_avg") %>%
  sg_legend(show = TRUE, label = "Stressor: ")

# Stressors by quarter
stressor %>% 
  group_by(months, stressor) %>% 
  summarize(stress_pct_avg = mean(stress_pct, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = months, y = stress_pct_avg, color = stressor)) +
  geom_point() +
  geom_line() + 
  theme_classic()

# Stacked area graph
stressor_plot <- 
  stressor %>% 
  rename(yr = year) %>% 
  mutate(
    stressor = ifelse(str_detect(stressor, "^Dis"), "Diseases", stressor)) %>% 
  group_by(yr, months, stressor) %>% 
  summarize(stress_pct_avg = mean(stress_pct, na.rm = T)) %>% 
  ungroup() %>% 
  filter(stress_pct_avg != "NaN") %>% 
  mutate(
    quarter = 
      case_when(
        str_detect(months, "January") ~ ".0",
        str_detect(months, "April") ~ ".25",
        str_detect(months, "July") ~ ".5",
        str_detect(months, "October") ~ ".75",
        TRUE ~ NA_character_)) %>% 
  select(-months) %>% 
  mutate(year_q = as.numeric(paste0(yr, quarter))) %>% 
  arrange(year_q) %>% 
  #pivot_wider(names_from = quarter, values_from = stress_pct_avg) %>% 
  ggplot(aes(x = year_q, y = stress_pct_avg, fill = stressor)) +
  geom_area() +
  scale_fill_manual("Stressors", values = met.brewer("Morgenstern", 6)) +
  labs(x = "Year", y = "Percent of colonies affected") +
  scale_y_continuous(breaks = seq(25, 100, 25), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2015, 2021, 1), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100), expand = T) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank()) 

ggsave(
  plot = stressor_plot,
  filename = "2022_week02_bees.png",
  path = "output")

# text --------------------------------------------------------------------

# Jumping on the #TidyTuesday bandwagon with this week's Bee Colony Data from
# the USDA. I was mostly interested in visualizing the stressors measured on
# colonies, so created a stacked area graph using #rstats and the brand-new
# #MetBrewer Morgenstern colorblind-friendly palette to view the quarterly data
# over time. To view my source code for this viz, check out my github repo
# (link).
