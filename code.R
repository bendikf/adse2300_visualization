ggplot(data = county_map) +
  geom_sf(aes(fill = Acres_burned * 0.004047)) +
  scale_fill_gradientn(name = "Area (km²)",
                       colors = cols,
                       # breaks = c(0, 50, 100, 150),
                       # labels = c(0, 50, 100, 150),
                       # limits = c(0, 150),
                       na.value = "grey",
                       guide = guide_colorbar(barheight = 10,
                                              draw.ulim = F,
                                              draw.llim = F,
                                              title.vjust = 3,
                                              order = 2)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = subset(county_map, is.na(km2)),
          aes(fill = "grey")) +
  scale_fill_manual(name = NULL,
                    labels = "No data",
                    values = "grey",
                    guide = guide_legend(override.aes = list(linetype = 0))) +
  labs(title = "Area burned (km²) in wildfires in California by county",
       subtitle = "2013 - 2019")

# Create figure:

ggplot(data = california) +
  stat_density2d(data = fires,
                 aes(x = Longitude,
                     y = Latitude,
                     fill = ..density..),
                 geom = 'tile',
                 contour = F)

 
ggplot() +
  geom_sf(data = california) +
  coord_sf(data = fires[,c("Longitude", "Latitude")],
           crs = )

  
  geom_point(data = fires %>% 
               subset(Longitude > -122 & Longitude < -114 &
                        Latitude > 32 & Latitude < 42),
             aes(x = Longitude,
                 y = Latitude))


  
fire_points <- st_as_sf(st_read("data/Fires/California_Fire_Incidents-point.dbf"))

ggplot(data = california) +
  geom_sf() +
  geom_sf(data = fire_points) +
  coord_sf(xlim = c(-125, -114),
           ylim = c(32, 42),
           crs  = "+proj=longlat +datum=WGS84 +no_defs") +


          
          
          
          scale_fill_gradientn(name = "Freq.",
                       colors = cols,
                       breaks = c(0, 50, 100, 150),
                       labels = c(0, 50, 100, 150),
                       limits = c(0, 150),
                       na.value = "white",
                       guide = guide_colorbar(barheight = 10,
                                              draw.ulim = F,
                                              draw.llim = F,
                                              title.vjust = 3)) +
  labs(title = "Frequency of wildfires in California by county",
       subtitle = "2013 - 2019")
          
##################################################################

county_map %>%
  ggplot() +
  geom_sf(aes(fill = Major_freq)) +
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
       subtitle = "2013 - 2019")
          
          
          
# Add column "Frequency" (of fires) to 'county_map' dataframe:
county_map <- left_join(county_map,
                        fires[which(fires$MajorIncident == T), "Counties"] %>%
                          table() %>% 
                          as.data.frame(stringsAsFactors = FALSE) %>%
                          select(COUNTY_NAM = 1,
                                 Major_freq  = 2))

######################################################################################

cols <- yarrr::piratepal(palette = "nemo")

fires %>% 
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
       y = "Frequency\n") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan.","Feb.", "March", "April", 
                                "May", "June", "July", "Aug.", 
                                "Sept.", "Oct.", "Nov.", "Dec."))

fires_by_county <- county_map %>%
  ggplot() +
  geom_sf(aes(fill = Major_freq)) + 
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



















