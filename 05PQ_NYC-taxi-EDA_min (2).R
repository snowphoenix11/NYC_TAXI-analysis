library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('alluvial') # visualisation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps

getwd()
setwd("C:/users/Singh/train.csv")
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
train <- as_tibble(read.csv('train.csv'))
summary(train)
glimpse(train)

table(train$vendor_id)

table(train$store_and_fwd_flag)
#' This flag indicates whether the trip record was held in vehicle memory before sending to the vendor 
#' because the vehicle did not have a connection to the server. Y=store and forward; N=not a store and forward trip.

table(train$passenger_count)

qplot(trip_duration, data=train, bins = 30)

t1 = train %>%
  filter(trip_duration < 10000)
qplot(trip_duration,data=t1, bins = 30)

train %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10() +
  scale_y_sqrt()




sum(is.na(train))

# Plot a map of NYC and overlay a managable number of pickup 
# coordinates to get a general overview of the locations and distances in question.
# For this visualisation we use the [leaflet](https://rstudio.github.io/leaflet/)
# package, which includes a variety of cool tools for interactive maps. 
# In this map you can zoom and pan through the pickup locations:


train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))


train %>%
  mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime)) + trip_duration) > 0) %>%
  select(check, pickup_datetime, dropoff_datetime, trip_duration) %>%
  group_by(check) %>%
  count()

set.seed(1234)
foo <- sample_n(train, 8e3)
leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                   color = "blue", fillOpacity = 0.3)


# investigate the variations together with the distributions of *passenger/_count* and *vendor/_id* by creating a multi-plot panel with different components:

train %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10() +
  scale_y_sqrt()

train %>%
  arrange(desc(trip_duration)) %>%
  select(trip_duration, pickup_datetime, dropoff_datetime, everything()) %>%
  head(10)

p1 <- train %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 120) +
  labs(x = "Pickup dates")
p2 <- train %>%
  ggplot(aes(dropoff_datetime)) +
  geom_histogram(fill = "blue", bins = 120) +
  labs(x = "Dropoff dates")
layout <- matrix(c(1,2),2,1,byrow=FALSE)
multiplot(p1, p2, layout=layout)
p1 <- 1; p2 <- 1

train %>%
  filter(pickup_datetime > ymd("2016-01-20") & pickup_datetime < ymd("2016-02-10")) %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 120)

#investigate the variations together with the distributions of *passenger_count*
# and *vendor_id* by creating a multi-plot panel with different components:

p1 <- train %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(vendor_id, fill = vendor_id)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(store_and_fwd_flag)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_y_log10()

p4 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE, week_start = 1)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")

p5 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,5),3,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1

#Check how many passengers travel in a taxi usually-

train %>%
  group_by(passenger_count) %>%
  count()

train %>%
  group_by(store_and_fwd_flag) %>%
  count()

#Check vendor-wise trips and the patterns for different weekdays, hours of the day, months, locations, trip durations.

p1 <- train %>%
  mutate(hpick = hour(pickup_datetime),
         Month = factor(month(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, Month) %>%
  count() %>%
  ggplot(aes(hpick, n, color = Month)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")
p2 <- train %>%
  mutate(hpick = hour(pickup_datetime),
         wday = factor(wday(pickup_datetime, label = TRUE, week_start = 1))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, color = wday)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")
layout <- matrix(c(1,2),2,1,byrow=FALSE)
multiplot(p1, p2, layout=layout)
p1 <- 1; p2 <- 1



p1 <- train %>%
  filter(pickup_longitude > -74.05 & pickup_longitude < -73.7) %>%
  ggplot(aes(pickup_longitude)) +
  geom_histogram(fill = "red", bins = 40)
p2 <- train %>%
  filter(dropoff_longitude > -74.05 & dropoff_longitude < -73.7) %>%
  ggplot(aes(dropoff_longitude)) +
  geom_histogram(fill = "blue", bins = 40)
p3 <- train %>%
  filter(pickup_latitude > 40.6 & pickup_latitude < 40.9) %>%
  ggplot(aes(pickup_latitude)) +
  geom_histogram(fill = "red", bins = 40)
p4 <- train %>%
  filter(dropoff_latitude > 40.6 & dropoff_latitude < 40.9) %>%
  ggplot(aes(dropoff_latitude)) +
  geom_histogram(fill = "blue", bins = 40)
layout <- matrix(c(1,2,3,4),2,2,byrow=FALSE)
multiplot(p1, p2, p3, p4, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1


#' Check vendor-wise trips and the patterns for different weekdays, 
#' hours of the day, months, locations, trip durations.


train %>%
  arrange(pickup_latitude) %>%
  select(pickup_latitude, pickup_longitude) %>%
  head(5)
train %>%
  arrange(desc(pickup_latitude)) %>%
  select(pickup_latitude, pickup_longitude) %>%
  head(5)
p1 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE, week_start = 1)) %>%
  group_by(wday, vendor_id) %>%
  summarise(median_duration = median(trip_duration)/60) %>%
  ggplot(aes(wday, median_duration, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Median trip duration [min]")
p2 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  summarise(median_duration = median(trip_duration)/60) %>%
  ggplot(aes(hpick, median_duration, color = vendor_id)) +
  geom_smooth(method = "loess", span = 1/2) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Median trip duration [min]") +
  theme(legend.position = "none")
layout <- matrix(c(1,2),2,1,byrow=FALSE)
multiplot(p1, p2, layout=layout)
p1 <- 1; p2 <- 1

# Check whether different numbers of passengers and/or 
#the different vendors are correlated with the duration of the trip. 


train %>%
  ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none") +
  facet_wrap(~ vendor_id) +
  labs(y = "Trip duration [s]", x = "Number of passengers")

train %>%
  ggplot(aes(trip_duration, fill = vendor_id)) +
  geom_density(position = "stack") +
  scale_x_log10()

train %>%
  group_by(vendor_id) %>%
  summarise(mean_duration = mean(trip_duration),
            median_duration = median(trip_duration))


#' ## Store and Forward vs *trip/_duration*
#' 
train %>%
  group_by(vendor_id, store_and_fwd_flag) %>%
  count()

train %>%
  filter(vendor_id == 1) %>%
  ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ store_and_fwd_flag) +
  theme(legend.position = "none") +
  labs(y = "Trip duration [s]", x = "Number of passengers") +
  ggtitle("Store_and_fwd_flag impact")

#'build new features from the existing ones - (date, month, wday, hour) 
#'derived from the *pickup/_datetime*. 
#'#' From the coordinates of the pickup and dropoff points calculate 
#'the direct *distance* between the two points, and 
#'compare it to our *trip/_durations*, using the *distCosine* function of the [geosphere].



jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

pick_coord <- train %>%
  select(pickup_longitude, pickup_latitude)

drop_coord <- train %>%
  select(dropoff_longitude, dropoff_latitude)
train$dist <- distCosine(pick_coord, drop_coord)
train$bearing = bearing(pick_coord, drop_coord)
train$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
train$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
train$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
train$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)

train <- train %>%
  mutate(speed = dist/trip_duration*3.6,
         date = date(pickup_datetime),
         month = month(pickup_datetime, label = TRUE),
         wday = wday(pickup_datetime, label = TRUE, week_start = 1),
         hour = hour(pickup_datetime),
         work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
         jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
         lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
         blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
  )

#'compute the average apparent velocity of the taxis, the average duration 
#'per day and hour, the average speed for these time bins


set.seed(4321)
train %>%
  sample_n(5e4) %>%
  ggplot(aes(dist, trip_duration)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")

train %>%
  filter(trip_duration < 3600 & trip_duration > 120) %>%
  filter(dist > 100 & dist < 100e3) %>%
  ggplot(aes(dist, trip_duration)) +
  geom_bin2d(bins = c(500,500)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")

train %>%
  filter(speed > 2 & speed < 1e2) %>%
  ggplot(aes(speed)) +
  geom_histogram(fill = "red", bins = 50) +
  labs(x = "Average speed [km/h] (direct distance)")

p1 <- train %>%
  group_by(wday, vendor_id) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(wday, median_speed, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Median speed [km/h]")

p2 <- train %>%
  group_by(hour, vendor_id) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(hour, median_speed, color = vendor_id)) +
  geom_smooth(method = "loess", span = 1/2) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Median speed [km/h]") +
  theme(legend.position = "none")

#'Create heatmap of speed over the week for hours.

p3 <- train %>%
  group_by(wday, hour) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(hour, wday, fill = median_speed)) +
  geom_tile() +
  labs(x = "Hour of the day", y = "Day of the week") +
  scale_fill_distiller(palette = "Spectral")
layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1

#' ## Bearing direction
#' 
#' If the direct *distance* is the magnitude of the trip vector 
#' then the *bearing* is its (initial) direction. 
#' Easily estimated through the *geosphere* package, 
#' it tells us whether our trip started out for instance in 
#' the direction of North-West or South-East. Here we visualise the *bearing* 
#' distribution and its relation to *trip/_duration*, direct *distance*, and *speed*:


p1 <- train %>%
  filter(dist < 1e5) %>%
  ggplot(aes(bearing)) +
  geom_histogram(fill = "red", bins = 75) +
  scale_x_continuous(breaks = seq(-180, 180, by = 45)) +
  labs(x = "Bearing")
p2 <- train %>%
  filter(dist < 1e5) %>%
  ggplot(aes(bearing, dist)) +
  geom_bin2d(bins = c(100,100)) +
  labs(x = "Bearing", y = "Direct distance") +
  scale_y_log10() +
  theme(legend.position = "none") +
  coord_polar() +
  scale_x_continuous(breaks = seq(-180, 180, by = 45))
p3 <- train %>%
  filter(trip_duration < 3600*22) %>%
  filter(dist < 1e5) %>%
  ggplot(aes(bearing, trip_duration)) +
  geom_bin2d(bins = c(100,100)) +
  scale_y_log10() +
  labs(x = "Bearing", y = "Trip duration") +
  coord_polar() +
  scale_x_continuous(breaks = seq(-180, 180, by = 45))
p4 <- train %>%
  filter(speed < 75 & dist < 1e5) %>%
  ggplot(aes(bearing, speed)) +
  geom_bin2d(bins = c(100,100)) +
  labs(x = "Bearing", y = "Speed") +
  coord_polar() +
  scale_x_continuous(breaks = seq(-180, 180, by = 45))
layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1


p1 <- train %>%
  ggplot(aes(jfk_dist_pick)) +
  geom_histogram(bins = 30, fill = "red") +
  scale_x_log10() +
  scale_y_sqrt() +
  geom_vline(xintercept = 2e3) +
  labs(x = "JFK pickup distance")
p2 <- train %>%
  ggplot(aes(jfk_dist_drop)) +
  geom_histogram(bins = 30, fill = "blue") +
  scale_x_log10() +
  scale_y_sqrt() +
  geom_vline(xintercept = 2e3) +
  labs(x = "JFK dropoff distance")
p3 <- train %>%
  ggplot(aes(lg_dist_pick)) +
  geom_histogram(bins = 30, fill = "red") +
  scale_x_log10() +
  scale_y_sqrt() +
  geom_vline(xintercept = 2e3) +
  labs(x = "La Guardia pickup distance")
p4 <- train %>%
  ggplot(aes(lg_dist_drop)) +
  geom_histogram(bins = 30, fill = "blue") +
  scale_x_log10() +
  scale_y_sqrt() +
  geom_vline(xintercept = 2e3) +
  labs(x = "La Guardia dropoff distance")
layout <- matrix(c(1,2,3,4),2,2,byrow=FALSE)
multiplot(p1, p2, p3, p4, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1

#define a JFK/La Guardia trip as having a pickup or dropoff distance of
#less than 2 km from the corresponding airport.
#' What are the *trip/_durations* of these journeys?

p1 <- train %>%
  filter(trip_duration < 23*3600) %>%
  ggplot(aes(jfk_trip, trip_duration, color = jfk_trip)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "JFK trip")
p2 <- train %>%
  filter(trip_duration < 23*3600) %>%
  ggplot(aes(lg_trip, trip_duration, color = lg_trip)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "La Guardia trip")
layout <- matrix(c(1,2),1,2,byrow=FALSE)
multiplot(p1, p2, layout=layout)
p1 <- 1; p2 <- 1

#' ### Longer than a day
#' 
#' We start with the few trips that pretend to have taken several days to complete:

day_plus_trips <- train %>%
  filter(trip_duration > 24*3600)
day_plus_trips %>% select(pickup_datetime, dropoff_datetime, speed)
ny_map <- as.tibble(map_data("state", region = "new york:manhattan"))
tpick <- day_plus_trips %>%
  select(lon = pickup_longitude, lat = pickup_latitude)
tdrop <- day_plus_trips %>%
  select(lon = dropoff_longitude, lat = dropoff_latitude)
p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1)

for (i in seq(1,nrow(tpick))){
  inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.75)
}
p1 + ggtitle("Longer than a day trips in relation to Manhattan")
p1 <- 1

#' Here we define day-long trips as taking between 22 and 24 hours, 
#' #which covers a small peak in our raw *trip_duration* distribution.


day_trips <- train %>%
  filter(trip_duration < 24*3600 & trip_duration > 22*3600)
day_trips %>% 
  arrange(desc(dist)) %>%
  select(dist, pickup_datetime, dropoff_datetime, speed) %>%
  head(5)
ny_map <- as.tibble(map_data("state", region = "new york:manhattan"))
set.seed(2017)
day_trips <- day_trips %>%
  sample_n(200)
tpick <- day_trips %>%
  select(lon = pickup_longitude, lat = pickup_latitude)
tdrop <- day_trips %>%
  select(lon = dropoff_longitude, lat = dropoff_latitude)
p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1)
for (i in seq(1,nrow(tpick))){
  inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.25)
}

p1 + ggtitle("Day-long trips in relation to Manhattan")
p1 <- 1

#short duration trips

min_trips <- train %>%
  filter(trip_duration < 5*60)
min_trips %>% 
  arrange(dist) %>%
  select(dist, pickup_datetime, dropoff_datetime, speed) %>%
  head(5)
zero_dist <- train %>%
  filter(near(dist,0))
nrow(zero_dist)
zero_dist %>%
  arrange(desc(trip_duration)) %>%
  select(trip_duration, pickup_datetime, dropoff_datetime, vendor_id) %>%
  head(5)
zero_dist %>%
  filter(trip_duration < 6000) %>%
  ggplot(aes(trip_duration, fill = vendor_id)) +
  geom_histogram(bins = 50) +
  scale_x_log10()

#### Short trips with above zero distance
#' After removing the zero-distance trips, those are the short rides with the highest average speed:

min_trips <- train %>%
  filter(trip_duration < 5*60 & dist > 0)

min_trips %>% 
  arrange(desc(speed)) %>%
  select(trip_duration, dist, pickup_datetime, speed) %>%
  head(10)

#plot the general distribution of distances within the sample-

ny_map <- as.tibble(map_data("state", region = "new york:manhattan"))
set.seed(1234)

foo <- min_trips %>%
  sample_n(600)

tpick <- foo %>%
  select(lon = pickup_longitude, lat = pickup_latitude)

tdrop <- foo %>%
  select(lon = dropoff_longitude, lat = dropoff_latitude)

p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1)

for (i in seq(1,nrow(tpick))){
  inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.25)
}

p1 + ggtitle("Minute-long trips in relation to Manhattan")
p1 <- 1

#Explore long distance Trips :
#with pickup or dropoff locations more than 300 km away from NYC (JFK airport)
long_dist <- train %>%
  filter( (jfk_dist_pick > 3e5) | (jfk_dist_drop > 3e5) )

long_dist_coord <- long_dist %>%
  select(lon = pickup_longitude, lat = pickup_latitude)

long_dist %>%
  select(id, jfk_dist_pick, jfk_dist_drop, dist, trip_duration, speed) %>%
  arrange(desc(jfk_dist_pick))

#plot zero-distance trips with more than a minute duration.

leaflet(long_dist_coord) %>%
  addTiles() %>%
  setView(-92.00, 41.0, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(popup = ~as.character(long_dist$dist), label = ~as.character(long_dist$id))

#Explore 0 distance Trips

train <- train %>%
  filter(trip_duration < 22*3600,
         dist > 0 | (near(dist, 0) & trip_duration < 60),
         jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5,
         trip_duration > 10,
         speed < 100)
