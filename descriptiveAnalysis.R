### -----------------------------------------------------------------------------------------------------------------------------------------
## WiDS_data_dive_2020
## https://github.com/menawhalen/WiDS_data_dive_2020
## Below is a table of number of incidents per neighborhood from 2015-2019 that the Chicago/Northern Illinois Red Cross responded to. 
## There seems to be a few neighborhoods that have a lot of the incidents, those neighborhoods with over 150 incidents over the time period are shown in the simple bar chart. 
## Major Question: **What is going on in these neighborhoods?**
## Minor Question: With the first question looking into where these incidents are occuring, another questions the Red Cross is asking is **when these incidents are occuring?** 
## Look at seasonality of the dataset at a monthly, weekly, and/or daily level.
### -----------------------------------------------------------------------------------------------------------------------------------------


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

require(cowplot)  ## added for plotting themes

##-------------------------------------
### LOAD AND CLEAN DATA
##-------------------------------------
list.files("data")
calls <- read_csv(file.path("data/calls_311.csv")) %>%  as_tibble() %>%   clean_names()
vacant <- read_csv(file.path("data/vacant.csv")) %>%  as_tibble() %>%   clean_names()
envir<- read_csv(file.path("data/enviro_inspection.csv")) %>%  as_tibble() %>%   clean_names()
inspect <- read_csv(file.path("data/enviro_inspection.csv")) %>%  as_tibble() %>%   clean_names()
commun <- read_csv(file.path("data/chicago_community_data.csv")) %>%  as_tibble() %>%   clean_names()
#red_cross <- read_rds("data/Redcross.rds") %>%  as_tibble() %>%   clean_names()
red_cross <- read_csv(file.path("data/redcross.csv")) %>%  as_tibble() %>%   clean_names()

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



#### Custom objects
customTheme <- theme(
  strip.text.x = element_text(size = 16, face = "bold"),
  plot.title = element_text(size = 20, vjust = -1, hjust = 0),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 10),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 16),
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 16)
)

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
subHigh <- sub %>% filter(counthigh==1)  

red_cross %>%
  filter(pri_neigh %in% subHigh$pri_neigh) %>% 
  mutate(year = year(date), count=1) %>%
  group_by(year,  pri_neigh) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_bar(aes(x = reorder(pri_neigh, count), y = count, fill=year), stat = "identity", position = "stack") +
  labs(
    title="",x = "", y = "Number of incidents")

#### Inicdent type per neighbourood
red_cross %>%
  filter(pri_neigh %in% subHigh$pri_neigh) %>% 
  mutate(year = year(date), count=1) %>%
  group_by(incident_type,  pri_neigh) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_bar(aes(x = reorder(pri_neigh, count), y = count, fill=incident_type), stat = "identity", position = "stack") +
  labs(
    title="",x = "", y = "Number of incidents")

#### Seasonality
plotdat <- red_cross %>%
  filter(pri_neigh %in% subHigh$pri_neigh) %>% 
  mutate(year = year(date), count=1) %>%
  group_by( pri_neigh, month) %>%
  summarize(count=sum(count)) 

### Save plot separate per neighbourhood
for(i in unique(plotdat$pri_neigh)){
  #i =   unique(plotdat$pri_neigh)[8]

  p <- ggplot(data=subset(plotdat, pri_neigh==i)) + theme_cowplot() +
  geom_line(aes(x = as.factor(month), y = count, group=1), stat = "identity", position = "stack", size=1.3) +
  labs(title="",x = "", y = "Number of incidents")+
  facet_wrap(~pri_neigh)+customTheme

  ggsave(paste0("plot",i,".png"), plot = p, device = "png")
}

### Which incident type is seasonal ? 
table(red_cross$incident_type)
mainincidents <- c("Fire","Not eligible for services","No response needed","Flood")

#### per year and facets incident_type
red_cross %>%
  filter(pri_neigh %in% subHigh$pri_neigh & incident_type %in% mainincidents) %>% 
  mutate(year = year(date), count=1) %>%
  group_by( incident_type, month,year) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_line(aes(x = as.factor(month), y = count, col=as.factor(year), group=year), stat = "identity", position = "stack", size=1.3) +
  labs(
    title="",x = "", y = "Number of incidents")+
  facet_wrap(~incident_type, scales="free")

#### per neighbourhood and facets incident_type
red_cross %>%
  filter(pri_neigh %in% subHigh$pri_neigh & incident_type %in% mainincidents) %>% 
  mutate(year = year(date), count=1) %>%
  group_by( incident_type, month,pri_neigh) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_line(aes(x = as.factor(month), y = count, group=pri_neigh), stat = "identity", position = "stack", size=1.3) +
  labs(
    title="",x = "", y = "Number of incidents")+
  facet_wrap(~incident_type, scales="free")

#### per  incident_type, months aggregated for all years
red_cross %>%
  filter(!(pri_neigh %in% subHigh$pri_neigh) & incident_type %in% mainincidents) %>% 
  mutate(year = year(date), count=1) %>%
  group_by( incident_type, month) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_line(aes(x = as.factor(month), y = count, group=1), stat = "identity", position = "stack", size=1.3) +
  labs(
    title="",x = "", y = "Number of incidents")+
  facet_wrap(~incident_type, scales="free")

#### per  incident_type comparing high versus low incident neighborhoods
red_cross %>%
  mutate(subHigh = ifelse(pri_neigh %in% subHigh$pri_neigh,1,0)) 
  filter(incident_type %in% mainincidents) %>% 
  mutate(year = year(date), count=1) %>%
  group_by( incident_type, month) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_line(aes(x = as.factor(month), y = count, group=subHigh), stat = "identity", position = "stack", size=1.3) +
  labs(
    title="",x = "", y = "Number of incidents")+
  facet_wrap(~incident_type, scales="free")


##### per weekday
red_cross %>%
  filter(pri_neigh %in% subHigh$pri_neigh & incident_type %in% mainincidents) %>% 
  mutate(year = year(date), count=1) %>%
  group_by( incident_type, weekday) %>%
  summarize(count=sum(count)) %>%
  ggplot() + theme_calc() +
  geom_line(aes(x = as.factor(weekday), y = count, group=1), stat = "identity", position = "stack", size=1.3) +
  labs(
    title="",x = "", y = "Number of incidents")+
  facet_wrap(~incident_type, scales="free")

##### Generate Maps 
### by neighbourhood
table(red_cross$pri_neigh)
length(unique(red_cross$pri_neigh))
sub <- red_cross %>% filter(incident_type=="Fire") %>% group_by(pri_neigh, month) %>% mutate(count=1) %>%  
                 summarize(totalcount = sum(count)) %>% 
  mutate(counthigh =ifelse(totalcount>=150,1,0))
subHigh <- sub %>% filter(pri_neigh %in% subHigh$pri_neigh)  

## Create mapping dataset
il_points <- il_points %>% filter(id %in% red_cross$pri_neigh)
il_map_df <- full_join(il_points, il_map_dat@data, by = c("id" = "pri_neigh")) %>% as_tibble() %>% rename(pri_neigh = id)

 
tempDat.df = dplyr::left_join(il_map_df, subHigh, by="pri_neigh")
ggplot() + theme_clean() +
  geom_polygon(data =il_shp, 
               aes(x = long, y = lat, group = group),fill="#fff7bc", color = "grey", size = 0.35) +
  geom_polygon(data = subset(tempDat.df, (pri_neigh %in% subHigh$pri_neigh)), 
               aes(x = long, y = lat, group = group, fill = totalcount), color = "grey", size = 0.35) +
  labs(title='Number of incidents in high-incident areas') +
  theme_map()

#### Where are the neighbourhoods located with names (as reference only)
ggplot() + theme_clean() +
  geom_polygon(data = subset(tempDat.df, (pri_neigh %in% subHigh$pri_neigh)), 
               aes(x = long, y = lat, group = group, fill = pri_neigh), color = "grey", size = 0.35) 

