### Spatial example
### from: https://menawhalen.github.io/WiDS_data_dive_2020/#the-data

##Testing Push 
source("packages.R")

# Illinois Zip Code Map ----------------------------------------------------
red_cross <- read_rds("data/Redcross.rds") %>% 
  as_tibble() %>% 
  clean_names()

# Forming Chicago Zip Map Tibble ------------------------------------------

#all spatial data files should be in neighborhood_shapefile directory! You need all of them!
il_map_dat <- readOGR(dsn="data/neighborhood_shapefiles/Neighborhoods_2012b.shp")
il_map_dat@data <- il_map_dat@data %>% 
  clean_names() %>% 
  mutate(pri_neigh= str_to_lower(pri_neigh))#read in chicago neighborhood shapefile 

il_map_dat@data$id <- rownames(il_map_dat@data)
il_points <- fortify(il_map_dat, region = "pri_neigh") %>% #fortify helps us turn the map into data points in a data frame 
  as_tibble() %>% 
  filter(id %in% red_cross$primary_neighborhood)

#creating mapping dataset 
il_map_df <- full_join(il_points, il_map_dat@data, by=c("id"= "pri_neigh")) %>% 
  as_tibble() 

neighborhood_raw_count <- red_cross %>% 
  mutate(year = year(date),
         primary_neighborhood = as.character(primary_neighborhood)) %>% 
  group_by(primary_neighborhood, year) %>% 
  summarise(incident_count = n()) %>% #counts the number of incidents in each community area in each year
  ungroup() %>% 
  filter(year == 2019) %>% #we just will look at 2019
  mutate(more_than_10 = case_when(incident_count <= 5 ~ "< 5",
                                  incident_count <= 10 ~ "5-10",
                                  TRUE ~ ">10")) #make discrete coloring scale that we don't end up using


# make the plot!
il_map_df %>% 
  left_join(neighborhood_raw_count, by = c("id" = "primary_neighborhood")) %>% 
  ggplot(aes(long, lat , group = group, fill= incident_count)) +
  scale_fill_gradient(
    name = "Incident Count", # changes legend title
    low = "blue",
    high = "red",
    space = "Lab") +
  geom_polygon() +
  geom_path(color = "black") +
  coord_quickmap() +
  theme_map() +  ggtitle('Incidents Per Chicago Neighborhood in 2019') 




###### Example using coordinates
nhood <- st_read(dsn="data/neighborhood_shapefiles/Neighborhoods_2012b.shp")

calls_311 <- read_csv('data/OLD_calls_311.csv' ) %>% 
  drop_na(longitude)

calls_311 <-  st_as_sf(calls_311, coords = c("longitude", "latitude"))

st_crs(calls_311) <- st_crs(nhood)

calls_311_nhood_data <- st_join(calls_311, nhood['PRI_NEIGH'], join = st_intersects)

#write_csv(calls_311_nhood_data, 'calls_311.csv')

