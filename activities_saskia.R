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
saskia_7_df <- built_df(saskia_7)
saskia_8_df <- built_df(saskia_8)
saskia_9_df <- built_df(saskia_9)
saskia_10_df <- built_df(saskia_10)
saskia_11_df <- built_df(saskia_11)
saskia_12_df <- built_df(saskia_12)

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
saskia_7_df$ID <- "saskia__7"
saskia_7_df$ID_text <- "saskia_Route"
saskia_8_df$ID <- "saskia__8"
saskia_8_df$ID_text <- "saskia_Route"
saskia_9_df$ID <- "saskia__9"
saskia_9_df$ID_text <- "saskia_Route"
saskia_10_df$ID <- "saskia__10"
saskia_10_df$ID_text <- "saskia_Route"
saskia_11_df$ID <- "saskia__11"
saskia_11_df$ID_text <- "saskia_Route"
saskia_12_df$ID <- "saskia__12"
saskia_12_df$ID_text <- "saskia_Route"


# function to convert df into sf object
df_to_sf <- function(df){
  st_as_sf(df, coords = c("lat", "lon"), crs = 4326 , remove = FALSE)
}

# combine all activitites to one data frame
activities_saskia_df <- rbind(saskia_1_df, saskia_2_df, saskia_3_df, saskia_4_df, saskia_5_df, saskia_6_df)

# turn data frame into sf object
activities_saskia_sf <- df_to_sf(activities_saskia_df)


# export sf object for setting attributes in GIS
export_activities_saskia <- st_write(activities_saskia_sf, "activities_saskia.csv")
