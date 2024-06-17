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
myfiles <- list.files("activities/activities_saskia.", pattern = "*.gpx")

for (i in 1:length(myfiles)){
  filename <- paste0("saskia_", i)
  wd <- paste0("activities/activities_saskia/", myfiles[i])
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
saskia_1_df <- built_df(saskia_1)
saskia_2_df <- built_df(saskia_2)
saskia_3_df <- built_df(saskia_3)
saskia_4_df <- built_df(saskia_4)
saskia_5_df <- built_df(saskia_5)
saskia_6_df <- built_df(saskia_6)
# saskia_7_df <- built_df(saskia_7)
# saskia_8_df <- built_df(saskia_8)
# saskia_9_df <- built_df(saskia_9)
# saskia_10_df <- built_df(saskia_10)
# saskia_11_df <- built_df(saskia_11)
# saskia_12_df <- built_df(saskia_12)

# assign ID to single files
saskia_1_df$ID <- "saskia__1"
saskia_1_df$ID_text <- "saskia_Pfaeffikon_Seedamm1"
saskia_2_df$ID <- "saskia__2"
saskia_2_df$ID_text <- "saskia_Pfaeffikon_Seedamm2"
saskia_3_df$ID <- "saskia__3"
saskia_3_df$ID_text <- "saskia_Regensdorf_Buero_Bahnhof"
saskia_4_df$ID <- "saskia__4"
saskia_4_df$ID_text <- "saskia_Rueti_Zentrum_Home"
saskia_5_df$ID <- "saskia__5"
saskia_5_df$ID_text <- "saskia_Rueti_Home_Bahnhof_Coop_Home"
saskia_6_df$ID <- "saskia__6"
saskia_6_df$ID_text <- "saskia_Waedenswil_Gruental_Bhf"

# saskia_7_df$ID <- "saskia__7"
# saskia_7_df$ID_text <- "saskia_Route"
# saskia_8_df$ID <- "saskia__8"
# saskia_8_df$ID_text <- "saskia_Route"
# saskia_9_df$ID <- "saskia__9"
# saskia_9_df$ID_text <- "saskia_Route"
# saskia_10_df$ID <- "saskia__10"
# saskia_10_df$ID_text <- "saskia_Route"
# saskia_11_df$ID <- "saskia__11"
# saskia_11_df$ID_text <- "saskia_Route"
# saskia_12_df$ID <- "saskia__12"
# saskia_12_df$ID_text <- "saskia_Route"


# function to convert df into sf object
df_to_sf <- function(df){
  st_as_sf(df, coords = c("lon", "lat"), crs = 4326 , remove = FALSE)
}

# combine all activitites to one data frame
activities_saskia_df <- rbind(saskia_1_df, saskia_2_df, saskia_3_df, saskia_4_df, saskia_5_df, saskia_6_df)

# turn data frame into sf object
activities_saskia_sf <- df_to_sf(activities_saskia_df)

tmap_mode("view")
tm_shape(activities_saskia_sf)+
  tm_dots(col="ID_text")

# export sf object for setting attributes in GIS
export_activities_saskia <- st_write(activities_saskia_sf, "activities_saskia.csv")

# import csv with attributes and convert to sf ####
activities_saskia_attributed <- read_delim("activities_saskia_attributiert.csv", ",")

activities_saskia_attributed_sf <- df_to_sf(activities_saskia_attributed)

# change crs of sf
activities_saskia_attributed_sf <- st_transform(activities_saskia_attributed_sf, crs = 2056)

activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
  mutate(
    DateTime = as.POSIXct(ts_POSIXct),
    Attribute_factor = as.factor(Attribut)
  )

tmap_mode("view")
tm_shape(activities_saskia_attributed_sf)+
  tm_dots(col="ID_text")
# Calculate attributes ####
## calculate time lag  ####
difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}

activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
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

activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
  group_by(ID) |>
  mutate(
    steplenght = distance_by_element(lag(geometry), geometry)
  )


## Check plausibility of calculated parameters  ####
plot(activities_saskia_attributed_sf$timelag_sec)
plot(activities_saskia_attributed_sf$steplenght)
boxplot(activities_saskia_attributed_sf$timelag_sec)
boxplot(activities_saskia_attributed_sf$steplenght)
summary(activities_saskia_attributed_sf$timelag_sec)
summary(activities_saskia_attributed_sf$steplenght)

# based on this check, i will remove all timelag > 5 and steplenght > 5
outliers_timelag <- filter(activities_saskia_attributed_sf, timelag_sec >= 5)
activities_saskia_attributed_sf <- activities_saskia_attributed_sf[which(activities_saskia_attributed_sf$timelag_sec <= 5),]
outliers_steplenght <- filter(activities_saskia_attributed_sf, steplenght >= 5)
activities_saskia_attributed_sf <- activities_saskia_attributed_sf[which(activities_saskia_attributed_sf$steplenght <= 5),]

# plot again to make sure it's better
plot(activities_saskia_attributed_sf$timelag_sec)
plot(activities_saskia_attributed_sf$steplenght)
boxplot(activities_saskia_attributed_sf$timelag_sec)
boxplot(activities_saskia_attributed_sf$steplenght)


# Segmentation ####
## Preperation  ####
# Specify a temporal window for mean step
activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
  group_by(ID) |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), 
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  
  )
# calculate mean step
activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
  group_by(ID) |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) 

# calculate mean speed
activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
  group_by(ID) |>
  mutate(
    speedMean = stepMean/timelag_sec
  )

# Explore mean step to define threashold 
hist(activities_saskia_attributed_sf$stepMean)
boxplot(activities_saskia_attributed_sf$stepMean)
summary(activities_saskia_attributed_sf$stepMean)

# apply threshold
activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
  group_by(ID) |>
  mutate(static = stepMean <  1.5)

# give segments an ID
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
  group_by(ID) |>
  mutate(segment_id = rle_id(static)) |> 
  ungroup()

# Acceleration ####
acceleration <- function(s1, s2, t){
  as.numeric((s2-s1)/(t))
}


activities_saskia_attributed_sf <- activities_saskia_attributed_sf |> 
  mutate(acceleration = 
           acceleration(lag(speedMean), speedMean, timelag_sec))

summary_saskia <- activities_saskia_attributed_sf |> 
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

activities_saskia_for_classification <- activities_saskia_attributed_sf |> 
  mutate(combi_ID = paste(ID, segment_id, sep = "_"))


# decision tree for attribute based classification ####
## 1. based on mean speed ####
saskia_classification <- activities_saskia_for_classification |> 
  group_by(combi_ID) |> 
  summarize(mean_acceleration = mean(acceleration, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE),
            lenght = length(segment_id)
  ) |> 
  ungroup()

saskia_classification <- saskia_classification |> 
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


saskia_classification <- saskia_classification |> 
  mutate(activity = if_else(shopping == 1, "shopping", 
                            if_else(recreation == 1, "recreation", "travel"),"NA"))
saskia_classification <- na.omit(saskia_classification)
saskia_classification <- saskia_classification |> 
  mutate(activity_factor = as.factor(activity)) 


saskia_activities_classified <- st_join(activities_saskia_attributed_sf, saskia_classification, left = TRUE)

# Export csv for other approaches
# st_write(saskia_activities_classified, "activities_saskia_with_attributes_classified.csv")

# Test classification ####
confus <-conf_mat(data = saskia_activities_classified, truth = Attribute_factor, estimate = activity_factor)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

confusionMatrix(saskia_activities_classified$Attribute_factor, saskia_activities_classified$activity_factor)
