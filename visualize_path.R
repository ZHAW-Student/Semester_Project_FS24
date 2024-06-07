# Quick visualization of movement data

library("XML")
library("leaflet")
library("sf")
library("tmap")
library("ggplot2")
library("tidyverse")

# read activities
myfiles <- list.files("activities/.", pattern = "*.gpx")

for (i in 1:12){
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

# run function with each activity
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

# function to convert df into sf object
df_to_sf <- function(df){
  st_as_sf(df, coords = c("lat", "lon"), crs = 4326 , remove = FALSE)
}

# run function on all activities
act1_sf <- df_to_sf(act1_df)
act2_sf <- df_to_sf(act2_df)
act3_sf <- df_to_sf(act3_df)
act4_sf <- df_to_sf(act4_df)
act5_sf <- df_to_sf(act5_df)
act6_sf <- df_to_sf(act6_df)
act7_sf <- df_to_sf(act7_df)
act8_sf <- df_to_sf(act8_df)
act9_sf <- df_to_sf(act9_df)
act10_sf <- df_to_sf(act10_df)
act11_sf <- df_to_sf(act11_df)
act12_sf <- df_to_sf(act12_df)

# DF zusammenfügen, crs ändern und exportieren

# Attributierung im GIS, dann Import und weiterverarbeitung

# plot
leaflet() |> 
  addTiles() |> 
  addPolylines(data = act1_sf, lat = ~lat, lng = ~lon, color = "blue", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act2_sf, lat = ~lat, lng = ~lon, color = "red", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act3_sf, lat = ~lat, lng = ~lon, color = "green", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act4_sf, lat = ~lat, lng = ~lon, color = "black", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act5_sf, lat = ~lat, lng = ~lon, color = "purple", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act6_sf, lat = ~lat, lng = ~lon, color = "yellow", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act7_sf, lat = ~lat, lng = ~lon, color = "orange", opacity = 0.8, weight = 3) |>   
  addPolylines(data = act8_sf, lat = ~lat, lng = ~lon, color = "darkgreen", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act9_sf, lat = ~lat, lng = ~lon, color = "lightblue", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act10_sf, lat = ~lat, lng = ~lon, color = "pink", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act11_sf, lat = ~lat, lng = ~lon, color = "grey", opacity = 0.8, weight = 3) |> 
  addPolylines(data = act12_sf, lat = ~lat, lng = ~lon, color = "turquoise", opacity = 0.8, weight = 3)
  