### Import packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sf)

### Loading the data
# Import the dataset:
fires <- readr::read_csv("data/California_Fire_Incidents.csv")
fires[1020,32] <- as.POSIXct("2017-05-01")   # Manually correct dates which were 
fires[1262,32] <- as.POSIXct("2018-08-01")   # incorrectly recorded.

# Import shapefiles for mapping geodata:
# california <- st_as_sf(st_read("data/California_State_Border/california.dbf"))
county_map <- st_as_sf(st_read("data/California_County_Boundaries/cnty19_1.dbf", 
                               stringsAsFactors = FALSE))

# Add column "Frequency" (of fires) to 'county_map' dataframe:
county_map <- left_join(county_map,                             # Join this...
                        fires$Counties %>%                      # ...to this.
                          table() %>% 
                          as.data.frame(stringsAsFactors = FALSE) %>%
                          select(COUNTY_NAM = 1,   # Match colname in 'county_map'
                                 Frequency  = 2))  # New column to be inserted

# Add column "MajorFreq" (i.e. the same as above, but only major events):
county_map <- left_join(county_map,
                        fires[which(fires$MajorIncident == TRUE), "Counties"] %>%
                          table() %>% 
                          as.data.frame(stringsAsFactors = FALSE) %>%
                          select(COUNTY_NAM = 1,
                                 Major_freq = 2))

# Add column "Acres_burned":: 
county_map <- left_join(county_map,
                        fires[,c("Counties", "AcresBurned")] %>% 
                          group_by(Counties) %>% 
                          summarise(Acres_burned = sum(AcresBurned)) %>% 
                          select(COUNTY_NAM = 1,
                                 Acres_burned = 2))

# Create list of months from January 2013 - December 2019:
months <- seq(from = ym('2013-01'), 
              to = ym('2019-12'), 
              by = 'month') %>% 
  as.data.frame() ; colnames(months)[1] <- "month"

### Figure 1 - Fires by county
# Determine which colours are to be used in the gradient:
cols <- c("white", "lightgoldenrod1", "orange2", "orangered3", "firebrick4")
# Specify theme:
dust_theme <- ggthemr::ggthemr("dust", set_theme = FALSE)

# Create figure:
fires_by_county <- county_map %>%
  ggplot() +
  geom_sf(aes(fill = Major_freq)) +      # Frequency of manjor wildfire events.
  scale_fill_gradientn(name = "Freq.",
                       colors = cols,
                       breaks = c(0, 10, 20, 30),
                       labels = c(0, 10, 20, 30),
                       limits = c(0, 30),
                       na.value = "white",
                       guide = guide_colorbar(barheight = 10,
                                              draw.ulim = F,
                                              draw.llim = F,
                                              title.vjust = 3)) +
  labs(title = "Frequency of major wildfire events in California by county",
       subtitle = "2013 - 2019") +
  dust_theme$theme

# The plot above includes only the frequency of major wildfire events, as recorded 
# by CAL FIRE. To include all wildfire events, incl. minor events, use the following
# code instead:

# fires_by_county <- ggplot(data = county_map) +
#   geom_sf(aes(fill = Frequency)) +
#   scale_fill_gradientn(name = "Freq.",
#                        colors = cols,
#                        breaks = c(0, 50, 100, 150),
#                        labels = c(0, 50, 100, 150),
#                        limits = c(0, 150),
#                        na.value = "white",
#                        guide = guide_colorbar(barheight = 10,
#                                               draw.ulim = FALSE,
#                                               draw.llim = FALSE,
#                                               title.vjust = 3)) +
#   labs(title = "Frequency of wildfires in California by county",
#        subtitle = "2013 - 2019") +
#   dust_theme$theme

# Export plot to PDF:
ggsave("figures/fires_by_county.pdf",
       plot = fires_by_county)

# Create figure:
# km2_by_county <- 
  
ggplot(data = county_map) +
  geom_sf(aes(fill = Acres_burned * 0.004047)) +   # Convert to km²
  scale_fill_gradientn(name = "Area (km²)",
                       colors = cols,
                       na.value = "grey",
                       guide = guide_colorbar(barheight = 10,
                                              draw.ulim = FALSE,
                                              draw.llim = FALSE,
                                              title.vjust = 3,
                                              order = 2)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = subset(county_map, is.na(Acres_burned)),
          aes(fill = "grey")) +
  scale_fill_manual(name = NULL,
                    labels = "No data",
                    values = "grey",
                    guide = guide_legend(override.aes = list(linetype = 0))) +
  labs(title = "Area burned in wildfires in California by county (km²)",
       subtitle = "2013 - 2019") +
  dust_theme$theme

# Export plot to PDF.
ggsave("figures/km2_by_county.pdf",
       plot = km2_by_county)

### Figure 4 - Yearly comparison of fire season
# Create data frame for figure 4:

cols <- yarrr::piratepal(palette = "nemo")   # Set new colour palette

year_by_year <- fires %>% 
  group_by(month = floor_date(fires$Started,
                              unit='month')) %>% 
  summarise(n = n()) %>% 
  right_join(., months) %>% 
  mutate(year = year(month),
         month = month(month)) %>% 
  data.table::setnafill(fill = 0) %>% 
  ggplot(aes(x = month,
             y = n)) +
  geom_line(aes(col  = year)) +
  geom_area(aes(fill = year,
                group = year, 
                alpha = 0.5)) +
  scale_colour_gradientn(colours = cols) +
  scale_fill_gradientn(colours = cols) +
  facet_wrap(vars(year),
             ncol = 1,
             strip.position = "right") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "lightgrey",
                                          linetype = "longdash"),
        panel.background = element_rect(fill = "gray95"),
        axis.title.x = element_blank()) +
  labs(title = "Year by year comparison of the Californian fire season",
       subtitle = "2013 - 2019",
       y = "Wildfire frequency\n") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan.","Feb.", "March", "April", 
                                "May", "June", "July", "Aug.", 
                                "Sept.", "Oct.", "Nov.", "Dec."))


cols <- yarrr::piratepal(palette = "nemo")   # Set new colour palette

year_by_year <- fires %>% 
  group_by(month = floor_date(fires$Started,
                              unit='month')) %>% 
  summarise(n = n()) %>% 
  right_join(., months) %>% 
  mutate(year = year(month),
         month = month(month)) %>% 
  data.table::setnafill(fill = 0) %>% 
  ggplot(aes(x = month,
             y = n)) +
  geom_line(aes(col  = year)) +
  geom_area(aes(fill = year,
                group = year, 
                alpha = 0.5)) +
  scale_colour_gradientn(colours = cols) +
  scale_fill_gradientn(colours = cols) +
  facet_wrap(vars(year),
             ncol = 1,
             strip.position = "right") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "lightgrey",
                                          linetype = "longdash"),
        panel.background = element_rect(fill = "gray95"),
        axis.title.x = element_blank()) +
  labs(title = "Year by year comparison of the Californian fire season",
       subtitle = "2013 - 2019",
       y = "Wildfire frequency\n") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan.","Feb.", "March", "April", 
                                "May", "June", "July", "Aug.", 
                                "Sept.", "Oct.", "Nov.", "Dec."))

# Export plot to PDF:
ggsave("figures/year_bt_year.pdf",
       plot = year_by_year)

### Figure 3 - Line chart 
# Run this code after Fig. 4 to avoid issues related to ggthemr::theme_reset()).

fire_points <- fires %>% 
  group_by(month = floor_date(fires$Started, 
                              unit='month')) %>% 
  summarise(n = n()) %>% 
  right_join(., months) %>% 
  data.table::setnafill(fill = 0)

ggthemr::ggthemr("dust")   # Set theme.

fire_points %>% 
  ggplot(aes(x = month,
             y = n)) +
  geom_smooth(method = lm,
              col = "firebrick1") +
  geom_line() +
  geom_point(col="steelblue3") +
  scale_x_datetime(date_labels = c("Jan.\n2013", "Jan.\n2014", "Jan.\n2015", "Jan.\n2016",
                                   "Jan.\n2017", "Jan.\n2018", "Jan.\n2019", "Jan.\n2020"),
                   # date_labels = "%b\n%Y",
                   breaks = 'year') +
  labs(title = "Frequency of wildfires in California per month",
       subtitle = "2013 - 2019",
       y = "Frequency") +
  theme(axis.title.x = element_blank()) +
  geom_text(data = fire_points[which(fire_points$n == max(fire_points$n)),], 
            mapping = aes(label = "Jul. 2017",
                          fontface = 2),
            hjust = -0.2,
            size = 3.5)

# Export plot to PDF:
ggsave("figures/line_chart.pdf",
       plot = line_chart)

####

fire_points %>% 
  ggplot(aes(x = month,
             y = n)) +
  geom_smooth(method = lm,
              col = "firebrick1") +
  geom_line() +
  geom_point(col="steelblue3") +
  scale_x_datetime(date_labels = c("Jan.\n2020", "Jan.\n2013", "Jan.\n2014", "Jan.\n2015", "Jan.\n2016",
                                   "Jan.\n2017", "Jan.\n2018", "Jan.\n2019"),
                   breaks = 'year') +
  labs(title = "Frequency of wildfires in California per month",
       subtitle = "2013 - 2019",
       y = "Frequency") +
  theme(axis.title.x = element_blank()) +
  geom_text(data = fire_points[which(fire_points$n == max(fire_points$n)),], 
            mapping = aes(label = "Jul. 2017",
                          fontface = 2),
            hjust = -0.2,
            size = 3.5)
