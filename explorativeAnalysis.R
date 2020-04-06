### -----------------------------------------------------------------------------------------------------------------------------------------
## WiDS_data_dive_2020
## Explorative questions
## Initial draft 
### -----------------------------------------------------------------------------------------------------------------------------------------
#  1. Are fires the most common incident? What other incidents happen within Chicago?
#  2. How often is the Red Cross responding to incidents? How do these patterns change over time? Do you see evidence for any spatial pattern?
#  3. How much financial assistance is given each day? How do these patterns change over time? Do you see evidence for any spatial pattern?
#  4. What variables and datasets appear to be most useful or interesting among the supplemental information?
### -----------------------------------------------------------------------------------------------------------------------------------------


### Load packages
##-------------------------------------
# PACKAGES
##-------------------------------------
require(tidyverse)
require(rgdal)
require(maptools)
require(rgeos)
require(maps)
require(mapproj)
require(ggthemes)
require(RCurl)
require(lubridate)
require(janitor)
require(hms)
require(kableExtra)
require(naniar) #for all you R nerds, here is the a new package that is great for handling missingness
require(sf)


##-------------------------------------
### LOAD AND CLEAN DATA
##-------------------------------------
list.files("data")
calls <- read_csv(file.path("data/calls_311.csv")) %>%  as_tibble() %>%   clean_names()
vacant <- read_csv(file.path("data/vacant.csv")) %>%  as_tibble() %>%   clean_names()
envir<- read_csv(file.path("data/enviro_inspection.csv")) %>%  as_tibble() %>%   clean_names()
inspect <- read_csv(file.path("data/enviro_inspection.csv")) %>%  as_tibble() %>%   clean_names()
commun <- read_csv(file.path("data/chicago_community_data.csv")) %>%  as_tibble() %>%   clean_names()
red_cross <- read_rds("data/Redcross.rds") %>%  as_tibble() %>%   clean_names()

#### assign same names to same variables 
red_cross <- red_cross %>% rename(pri_neigh =  primary_neighborhood, sec_neigh = secondary_neighborhood)
red_cross$year = year(red_cross$date)
red_cross$month =month(red_cross$date)
red_cross$day =day(red_cross$date)
red_cross$weekday <- weekdays(as.Date(red_cross$date))
red_cross$daymon  = as.numeric( paste0(red_cross$month,".", red_cross$day))
red_cross$pri_neigh <- as.character(red_cross$pri_neigh)
## Checking and correcting variabels classes if needed 
class(red_cross$date)

red_cross$pri_neigh <- tolower(red_cross$pri_neigh)
##-------------------------------------
### SHAPEFILES AND SPATIAL DATA
##-------------------------------------
il_map_dat <- readOGR(dsn="data/neighborhood_shapefiles/Neighborhoods_2012b.shp")
# plot(il_map_dat)
# ### Clean il_map_dat
 il_map_dat@data <- il_map_dat@data %>% 
   clean_names() %>% 
   mutate(pri_neigh= str_to_lower(pri_neigh))#read in chicago neighborhood shapefile 
 il_map_dat@data$id <- rownames(il_map_dat@data)
 plot(il_map_dat)
 ### Fortify
 il_points <- fortify(il_map_dat, region = "pri_neigh") %>% as_tibble()


library(raster)
il_shp <- shapefile("data/neighborhood_shapefiles/Neighborhoods_2012b.shp")
il_shp_f <- fortify(il_shp , region="PRI_NEIGH")
colnames(il_shp_f)[colnames(il_shp_f)=="id"] 		<- tolower("PRI_NEIGH")
il_shp_f$pri_neigh <- tolower(il_shp_f$pri_neigh)



### 0 - inspect data
missingness_info <- red_cross %>% miss_var_summary()


## ----------------------------------------------------------------
### 1. Are fires the most common incident?
###    What other incidents happen within Chicago?
## ----------------------------------------------------------------
red_cross %>%
  mutate(year = year(date), count=1) %>%
  group_by(incident_type) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_bar(aes(x = incident_type, y = count, fill = incident_type), stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title="",x = "", y = "", fill = "Type of incident" )

### Per year
red_cross %>%
  mutate(year = year(date), count=1) %>%
  group_by(year, incident_type) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_bar(aes(x = year, y = count), stat = "identity", position = "dodge") +
  facet_wrap(~incident_type, scales="free_y") +
  labs(
    title="",x = "", y = "Number of incidents")

red_cross %>%
  mutate(year = year(date), count=1) %>%
  group_by(year, incident_type) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_bar(aes(x = year, y = count, fill=incident_type), stat = "identity", position = "stack") +
  labs(
    title="",x = "", y = "Number of incidents")

### by neighbourhood
table(red_cross$pri_neigh)
length(unique(red_cross$pri_neigh))
sub <- red_cross %>% group_by(pri_neigh) %>% mutate(count=1) %>%  summarize(totalcount = sum(count)) %>% mutate(counthigh =ifelse(totalcount>=150,1,0))
  

red_cross %>%
  mutate(year = year(date), count=1) %>%
  group_by(year,  pri_neigh) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_bar(aes(x = reorder(pri_neigh, count), y = count, fill=year), stat = "identity", position = "stack") +
  labs(
    title="",x = "", y = "Number of incidents")+
  geom_hline(yintercept=0)



## ----------------------------------------------------------------
### 2. How often is the Red Cross responding to incidents?
###    How do these patterns change over time?
###    Do you see evidence for any spatial pattern?
## ----------------------------------------------------------------

dim(calls)
dim(red_cross)

mergevars <- colnames(calls)[colnames(calls) %in% colnames(red_cross)]
# calls$PRI_NEIGH
# red_cross$Primary.neighborhood
red_cross$response <- 1

calls2 <- merge(calls, red_cross, by =mergevars , all = TRUE)
dim(calls2)
calls2$response[is.na(calls2$response)] <- 0
table(calls2$response, exclude = NULL)
tapply(calls2$date, calls2$response, summary)



## ----------------------------------------------------------------
###  3. How much financial assistance is given each day?
###     How do these patterns change over time?
###     Do you see evidence for any spatial pattern?
## ----------------------------------------------------------------
summary(red_cross$assistance)

ggplot(data=red_cross)+ geom_point(aes(x=date, y=assistance, col=incident_type))
ggplot(data=red_cross)+ geom_line(aes(x=daymon, y=assistance,  col=as.factor(year)),se=F, span=0.3) 

## Create mapping dataset
il_points <- il_points %>% filter(id %in% red_cross$pri_neigh)
il_map_df <- full_join(il_points, il_map_dat@data, by = c("id" = "pri_neigh")) %>% as_tibble()

assistance <- red_cross %>% 
  group_by(pri_neigh, year) %>% 
  summarise(total_assistance = sum(assistance)) %>% ## sums the total amount of financial assistance per neighbourhood (all year)
  #filter(year==2019) %>%
  ungroup()

# make the plot!
il_map_df %>% 
  left_join(subset(assistance, !is.na(year)), by = c("id" = "pri_neigh")) %>% 
  ggplot(aes(long, lat , group = group, fill= total_assistance )) +
  geom_polygon() +
  geom_path(color = "black") +
  coord_quickmap() +
  theme_map() + 
  facet_wrap(~year) +
  labs(title='Sum of financial assistance per Chicago Neighborhood 2019') +
  customThemeNoFacet


## ----------------------------------------------------------------
###  4. What variables and datasets appear to be most useful or interesting among the supplemental information?
## ----------------------------------------------------------------




