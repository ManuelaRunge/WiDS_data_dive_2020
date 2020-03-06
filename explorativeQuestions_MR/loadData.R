
##-------------------------------------
### LOAD AND CLEAN DATA
##-------------------------------------
list.files("data")
calls <- read_csv(file.path("data/calls_311.csv")) %>%  as_tibble() %>%   clean_names()
vacant <- read_csv(file.path("data/vacant.csv")) %>%  as_tibble() %>%   clean_names()
#benchmark<- read_csv(file.path("data/energy_benchmark.csv")) %>%  as_tibble() %>%   clean_names()
inspect <- read_csv(file.path("data/enviro_inspection.csv")) %>%  as_tibble() %>%   clean_names()
commun <- read_csv(file.path("data/chicago_community_data.csv")) %>%  as_tibble() %>%   clean_names()
#red_cross <- read_rds("data/Redcross.rds") %>%  as_tibble() %>%   clean_names()
red_cross <- read_csv(file.path("data/redcross.csv")) %>%  as_tibble() %>%   clean_names()

#### assign same names to same variables 
red_cross <- red_cross %>% rename(pri_neigh =  primary_neighborhood, sec_neigh = secondary_neighborhood)
red_cross$year = year(red_cross$date)
red_cross$month =month(red_cross$date)
red_cross$day =day(red_cross$date)
red_cross$daymon  = as.numeric( paste0(red_cross$month,".", red_cross$day))
red_cross$pri_neigh <- as.character(red_cross$pri_neigh)
## Checking and correcting variabels classes if needed 
class(red_cross$date)


##-------------------------------------
### SHAPEFILES AND SPATIAL DATA
##-------------------------------------
il_map_dat <- readOGR(dsn="data/neighborhood_shapefiles/Neighborhoods_2012b.shp")
### Clean il_map_dat
il_map_dat@data <- il_map_dat@data %>% 
  clean_names() %>% 
  mutate(pri_neigh= str_to_lower(pri_neigh))#read in chicago neighborhood shapefile 
il_map_dat@data$id <- rownames(il_map_dat@data)
### Fortify
il_points <- fortify(il_map_dat, region = "pri_neigh") %>% as_tibble()




