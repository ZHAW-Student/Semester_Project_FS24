# Preparation ####
# load libraries
library("XML")
library("leaflet")
library("sf")
library("tmap")
library("ggplot2")
library("tidyverse")
library("readr")
library("trajr")
library("yardstick")
library("caret") 

# read activities
myfiles <- list.files("activities/.", pattern = "*.gpx")

for (i in 1:length(myfiles)){
  filename <- paste0("act", i)
  wd <- paste0("activities/", myfiles[i])
  assign(filename, htmlTreeParse(file = wd, useInternalNodes = TRUE))
}


# function to parse activities and write into data frame
built_df <- function(activity) {
  # get coordinates
  coords <- xpathSApply(doc = activity, path = "//trkpt", fun = xmlAttrs)
  # get elevation
  elevation <- xpathSApply(doc = activity, path = "//trkpt/ele", fun = xmlValue)
  # get time
  time <- xpathApply(doc = activity, path = "//trkpt/time", fun = xmlValue)
  data.frame(
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    ts_POSIXct = ymd_hms(time, tz = "UTC"),
    elevation = as.numeric(elevation)
  )
}
# convert files to dataframe
act1_df <- built_df(act1)
act2_df <- built_df(act2)
act3_df <- built_df(act3)
act4_df <- built_df(act4)
act5_df <- built_df(act5)
act6_df <- built_df(act6)
act7_df <- built_df(act7)
act8_df <- built_df(act8)
act9_df <- built_df(act9)
act10_df <- built_df(act10)
act11_df <- built_df(act11)
act12_df <- built_df(act12)

# assign ID to single files
act1_df$ID <- "test_1"
act1_df$ID_text <- "test_Waedenswil_Reidbach_Zentrum"
act2_df$ID <- "test_2"
act2_df$ID_text <- "test_Waedenswil_Coop_Bahnhof"
act3_df$ID <- "test_3"
act3_df$ID_text <- "test_Waedenswil_Schloss_Mensa"
act4_df$ID <- "test_4"
act4_df$ID_text <- "test_Waedenswil_Gruental_Bahnhof1"
act5_df$ID <- "test_5"
act5_df$ID_text <- "test_Waedenswil_Gruental_Bahnhof2"
act6_df$ID <- "test_6"
act6_df$ID_text <- "test_Rueti_Home_Bahnhof_Coop_Home"
act7_df$ID <- "test_7"
act7_df$ID_text <- "test_test_Rapperswil_See_Bahnhof"
act8_df$ID <- "test_8"
act8_df$ID_text <- "test_Rueti_Home_Coop_Home"
act9_df$ID <- "test_9"
act9_df$ID_text <- "test_Neuhausen_Bahnhof_Rhein"
act10_df$ID <- "test_10"
act10_df$ID_text <- "test_Neuhausen_Rhein_Bahnhof"
act11_df$ID <- "test_11"
act11_df$ID_text <- "test_S-chanf"
act12_df$ID <- "test_12"
act12_df$ID_text <- "test_Regensdorf_Buero_Coop"


# function to convert df into sf object
df_to_sf <- function(df){
  st_as_sf(df, coords = c("lon", "lat"), crs = 4326 , remove = FALSE)
}

# combine all activitites to one data frame
test_activities_df <- rbind(act1_df, act2_df, act3_df, act4_df, act5_df, act6_df, act7_df, act8_df, act9_df, act10_df, act11_df, act12_df)

# turn data frame into sf object
test_activities_sf <- df_to_sf(test_activities_df)

# export sf object for setting attributes in GIS
export_test_activities <- st_write(test_activities_sf, "test_activities.shp")

# import csv with attributes and convert to sf ####
activities_classified <- read_delim("test_activities_attributiert.csv", ",")

activities_classified_sf <- df_to_sf(activities_classified)

# change crs of sf
activities_classified_sf <- st_transform(activities_classified_sf, crs = 2056)

activities_classified_sf <- activities_classified_sf |> 
  mutate(
    DateTime = as.POSIXct(ts_POSIXct),
    Attribute_factor = as.factor(Attribut)
  )

# Calculate attributes ####
## calculate time lag  ####
difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    timelag_sec = difftime_secs(lead(DateTime,), DateTime)
  )

## calculate distance between locations  ####
distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    steplenght = distance_by_element(lag(geometry), geometry)
  )

## Check plausibility of calculated parameters  ####
plot(activities_classified_sf$timelag_sec)
plot(activities_classified_sf$steplenght)
boxplot(activities_classified_sf$timelag_sec)
boxplot(activities_classified_sf$steplenght)
summary(activities_classified_sf$timelag_sec)
summary(activities_classified_sf$steplenght)

# based on this check, i will remove all timelag > 5 and steplenght > 5

outliers_timelag <- filter(activities_classified_sf, timelag_sec >= 5)
activities_classified_sf <- activities_classified_sf[which(activities_classified_sf$timelag_sec <= 5),]
outliers_steplenght <- filter(activities_classified_sf, steplenght >= 5)
activities_classified_sf <- activities_classified_sf[which(activities_classified_sf$steplenght <= 5),]



# plot again to make sure it's better
plot(activities_classified_sf$timelag_sec)
plot(activities_classified_sf$steplenght)
boxplot(activities_classified_sf$timelag_sec)
boxplot(activities_classified_sf$steplenght)

# Segmentation ####
## Preperation  ####
# Specify a temporal window for mean step
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), 
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  
  )
# calculate mean step
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

# calculate mean speed
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    speedMean = stepMean/timelag_sec
  )

# Explore mean step to define threashold 
hist(activities_classified_sf$stepMean)
boxplot(activities_classified_sf$stepMean)
summary(activities_classified_sf$stepMean)

# apply threshold
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(static = stepMean <  1.5)

# give segments an ID
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(segment_id = rle_id(static))|> 
  ungroup()

# single trajectories ####
## extract single trajectories####
traj1 <- filter(activities_classified_sf, ID == "test_1")
traj2 <- filter(activities_classified_sf, ID == "test_2")
traj3 <- filter(activities_classified_sf, ID == "test_3")
traj4 <- filter(activities_classified_sf, ID == "test_4")
traj5 <- filter(activities_classified_sf, ID == "test_5")
traj6 <- filter(activities_classified_sf, ID == "test_6")
traj7 <- filter(activities_classified_sf, ID == "test_7")
traj8 <- filter(activities_classified_sf, ID == "test_8")
traj9 <- filter(activities_classified_sf, ID == "test_9")
traj10 <- filter(activities_classified_sf, ID == "test_10")
traj11 <- filter(activities_classified_sf, ID == "test_11")
traj12 <- filter(activities_classified_sf, ID == "test_12")

## plot single trajectories with segmentation ####

traj_plot1 <- ggplot(traj1, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot2 <- ggplot(traj2, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot3 <- ggplot(traj3, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot4 <- ggplot(traj4, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot5 <- ggplot(traj5, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot6 <- ggplot(traj6, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot7 <- ggplot(traj7, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot8 <- ggplot(traj8, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot9 <- ggplot(traj9, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot10 <- ggplot(traj10, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot11 <- ggplot(traj11, aes(lat, lon, colour = segment_id)) +
  geom_point()
traj_plot12 <- ggplot(traj12, aes(lat, lon, colour = segment_id)) +
  geom_point()

## save plots ####
ggsave("traj_plots/traj_plot1.png", traj_plot1)
ggsave("traj_plots/traj_plot2.png", traj_plot2)
ggsave("traj_plots/traj_plot3.png", traj_plot3)
ggsave("traj_plots/traj_plot4.png", traj_plot4)
ggsave("traj_plots/traj_plot5.png", traj_plot5)
ggsave("traj_plots/traj_plot6.png", traj_plot6)
ggsave("traj_plots/traj_plot7.png", traj_plot7)
ggsave("traj_plots/traj_plot8.png", traj_plot8)
ggsave("traj_plots/traj_plot9.png", traj_plot9)
ggsave("traj_plots/traj_plot10.png", traj_plot10)
ggsave("traj_plots/traj_plot11.png", traj_plot11)
ggsave("traj_plots/traj_plot12.png", traj_plot12)


# Number of stops ####
# Stops per activity 
summary <- activities_classified_sf |> 
  group_by(ID) |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))

# stops per trajectory
stops_traj1 <- traj1 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE),
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj2 <- traj2 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE),
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj3 <- traj3 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj4 <- traj4 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE),
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj5 <- traj5 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE),
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj6 <- traj6 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj7 <- traj7 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj8 <- traj8 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj9 <- traj9 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj10 <- traj10 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE),
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj11 <- traj11 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))
stops_traj12 <- traj12 |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE),
            not_stops = sum(!static, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE))

plot(stops~Attribute_factor, data = summary)
plot(not_stops~Attribute_factor, data = summary)
summary$stop_ratio <- summary$not_stops/summary$stops
plot(stop_ratio~Attribute_factor, data = summary)

# Acceleration ####
acceleration <- function(s1, s2, t){
  as.numeric((s2-s1)/(t))
}

  
activities_classified_sf <- activities_classified_sf |> 
mutate(acceleration = 
    acceleration(lag(speedMean), speedMean, timelag_sec))

summary <- activities_classified_sf |> 
  group_by(ID) |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            stop_ratio = not_stops/stops,
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE),
            mean_acceleration = mean(acceleration, na.rm = TRUE),
            mean_lenght = length(segment_id)
            ) |> 
  ungroup()

# Export csv
# st_write(activities_classified_sf, "test_activities_with_attributes_korrigiert.csv")

activities_for_classification <- activities_classified_sf |> 
  mutate(combi_ID = paste(ID, segment_id, sep = "_"))
  

# decision tree for attribute based classification ####
## 1. based on mean speed ####
test_classification <- activities_for_classification |> 
  group_by(combi_ID) |> 
  summarize(mean_acceleration = mean(acceleration, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE),
            lenght = length(segment_id)
            ) |> 
  ungroup()

test_classification <- test_classification |> 
  mutate(travel = if_else(mean_speed > 1.7 & mean_speed < 4
                          & mean_step > 1.7 & mean_step < 4
                          & mean_acceleration < 0.003,  1 ,  0 ),
         recreation = if_else(travel %in% c( 0 ) 
                              & mean_speed > 1.1  & mean_speed < 1.7
                              & mean_step > 1.2 & mean_step < 1.7
                              & mean_acceleration < 0.003,  1 ,  0 ),
         shopping = if_else(travel %in% c( 0 ) 
                            & recreation %in% c( 0 )
                            & mean_speed > 5 |  mean_speed < 1.1
                            & mean_step > 5 | mean_step < 1.2
                            & mean_acceleration > 0.01
                              ,  1 ,  0 ))


test_classification <- test_classification |> 
  mutate(activity = if_else(shopping == 1, "shopping", 
                            if_else(recreation == 1, "recreation", "travel"),"NA"))
test_classification <- na.omit(test_classification)
test_classification <- test_classification |> 
  mutate(activity_factor = as.factor(activity)) 
  
  
test_activities_classified <- st_join(activities_classified_sf, test_classification, left = TRUE)

# Test classification ####
confus <-conf_mat(data = test_activities_classified, truth = Attribute_factor, estimate = activity_factor)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

# get statistics for confusion matrix
confusionMatrix(test_activities_classified$Attribute_factor, test_activities_classified$activity_factor)

