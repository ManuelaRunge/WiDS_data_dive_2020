red_cross<- read_csv("Redcross.csv")

missingness_info<-red_cross %>% miss_var_summary() #displays number and percent of NAs in each column 
table(red_cross$`Incident type`)

#--------------
hot_area = c('Englewood','Austin','Roseland','Garfield park','Humboldt park','Auburn gresham','North lawndale',
             'South shore','West pullman','New city','Grand crossing')
# extract the year and month
red_cross$year = as.numeric(format(red_cross$Date, "%Y"))
red_cross$month = as.numeric(format(red_cross$Date, "%m"))
dis = matrix(0,nrow = length(hot_area),ncol = 12)
for (i in hot_area){
  for (j in 1:12){
    dis[i,j]<- nrow(red_cross[which(red_cross$month==j & red_cross$`Primary neighborhood` == hot_area[i] & red_cross$`Incident type`=='Fire'),1:30])
  }
}
fire_amount_spe = rep(0,12)
for (i in 1:12){
  fire_amount_spe[i] =nrow(red_cross[which(red_cross$`Incident type`=='Fire'&red_cross$month == i&red_cross$`Primary neighborhood` %in% hot_area),1:30])
}
fire_amount_nospe = rep(0,12)
for (i in 1:12){
  fire_amount_nospe[i] =nrow(red_cross[which(red_cross$`Incident type`=='Fire'&red_cross$month == i&!red_cross$`Primary neighborhood` %in% hot_area),1:30])
}
plot(fire_amount_nospe,type = 'l', main = '<150 v.s. >150',xlab = 'month',,ylab = 'fire',ylim = c(0,max(fire_amount_nospe)))
abline(v=1:12, h = seq(0,250,50),col="gray", lty=3)
points(fire_amount_spe,type = 'l',col = 'red',xlab = 'month',,ylab = 'fire')