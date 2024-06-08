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
  st_as_sf(df, coords = c("lat", "lon"), crs = 4326 , remove = FALSE)
}

# combine all activitites to one data frame
test_activities_df <- rbind(act1_df, act2_df, act3_df, act4_df, act5_df, act6_df, act7_df, act8_df, act9_df, act10_df, act11_df, act12_df)

# turn data frame into sf object
test_activities_sf <- df_to_sf(test_activities_df)

# change crs of sf
test_activities_sf <- st_transform(test_activities_sf, crs = 2056)

# export sf object for setting attributes in GIS
export_test_activities <- st_write(test_activities_sf, "test_activities.shp")

# import csv with attributes and convert to sf
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
# calculate time lag
difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    timelag_sec = difftime_secs(lead(DateTime,), DateTime)
  )

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    timelag_sec10 = difftime_secs(lead(DateTime, n = 10), DateTime)
  )

# calculate distance between locations
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
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    steplenght10 = distance_by_element(lag(geometry, n = 10), geometry)
  )

# calculate speed between locations
activities_classified_sf$speed <- activities_classified_sf$steplenght/activities_classified_sf$timelag

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    speed = steplenght/timelag_sec
  )
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    speed10 = steplenght10/timelag_sec10
  )

# Check plausibility of calculated parameters
plot(activities_classified_sf$timelag_sec)
plot(activities_classified_sf$steplenght)
plot(activities_classified_sf$speed)

# Segmentation ####

# Preperation
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

# Explore mean step to define threashold 
hist(activities_classified_sf$stepMean)
boxplot(activities_classified_sf$stepMean)
summary(activities_classified_sf$stepMean)

# apply threshold
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(static = stepMean < 2.527)

# give segments an ID
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(segment_id = rle_id(static))

# subset trajectories ####
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

# Number of stops per trajectory ####


# Sinuosity ####
TrajRediscretize(traj1, 2, simConstantSpeed = FALSE)
TrajSinuosity(traj1, compass.direction = NULL)

