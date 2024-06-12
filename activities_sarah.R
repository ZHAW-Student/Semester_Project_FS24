# Preparation ####
# load libraries
library("XML")
library("leaflet")
library("sf")
library("tmap")
library("ggplot2")
library("tidyverse")
library("readr")

# read activities
myfiles <- list.files("activities/activities_sarah", pattern = "*.gpx")

for (i in 1:length(myfiles)){
  filename <- paste0("sarah_", i)
  wd <- paste0("activities/activities_sarah/", myfiles[i])
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
sarah_1_df <- built_df(sarah_1)
sarah_2_df <- built_df(sarah_2)
sarah_3_df <- built_df(sarah_3)
sarah_4_df <- built_df(sarah_4)
sarah_5_df <- built_df(sarah_5)
sarah_6_df <- built_df(sarah_6)
sarah_7_df <- built_df(sarah_7)
sarah_8_df <- built_df(sarah_8)
sarah_9_df <- built_df(sarah_9)
sarah_10_df <- built_df(sarah_10)
sarah_11_df <- built_df(sarah_11)
sarah_12_df <- built_df(sarah_12)

# assign ID to single files
sarah_1_df$ID <- "sarah__1"
sarah_1_df$ID_text <- "sarah_Denner_Einkauf"
sarah_2_df$ID <- "sarah__2"
sarah_2_df$ID_text <- "sarah_Klafi_Feld"
sarah_3_df$ID <- "sarah__3"
sarah_3_df$ID_text <- "sarah_Klafi_von_Zug_Heim"
sarah_4_df$ID <- "sarah__4"
sarah_4_df$ID_text <- "sarah_Migros_groesserer_Einkauf"
sarah_5_df$ID <- "sarah__5"
sarah_5_df$ID_text <- "sarah_Migros_kleiner_Einkauf"
sarah_6_df$ID <- "sarah__6"
sarah_6_df$ID_text <- "sarah_Rosengarten_und_Shopping_Winti"
sarah_7_df$ID <- "sarah__7"
sarah_7_df$ID_text <- "sarah_Spaziergang_am_Morgen_Bub"
sarah_8_df$ID <- "sarah__8"
sarah_8_df$ID_text <- "sarah_Spaziergang_Klafi_kleine_Runde"
sarah_9_df$ID <- "sarah__9"
sarah_9_df$ID_text <- "sarah_WB_Heim"
sarah_10_df$ID <- "sarah__10"
sarah_10_df$ID_text <- "sarah_Zu_Bus_Klafi"
sarah_11_df$ID <- "sarah__11"
sarah_11_df$ID_text <- "sarah_Zum_Bus_Bub"
sarah_12_df$ID <- "sarah__12"
sarah_12_df$ID_text <- "sarah_Zum_Zug_Liestal"


# function to convert df into sf object
df_to_sf <- function(df){
  st_as_sf(df, coords = c("lon", "lat"), crs = 4326 , remove = FALSE)}

# combine all activitites to one data frame
activities_sarah_df <- rbind(sarah_1_df, sarah_2_df, sarah_3_df, sarah_4_df, sarah_5_df, sarah_6_df,sarah_7_df,sarah_8_df,sarah_9_df, sarah_10_df, sarah_11_df,sarah_12_df)

# turn data frame into sf object
activities_sarah_sf <- df_to_sf(activities_sarah_df)

tmap_mode("view")
tm_shape(activities_sarah_sf)+
  tm_dots(col="ID_text")

# export sf object for setting attributes in GIS
export_activities_sarah <- st_write(activities_sarah_sf, "activities_sarah.csv")
