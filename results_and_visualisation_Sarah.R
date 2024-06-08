#my workspace
library("readr")
library("sf")
library("dplyr")
library("tmap")
library("ggplot2")
library("terra")
library("lubridate")

##Cama workflow ----

###read CAMA data ----

#what i need: buildings, roads, greenspace, versiegelte flächen

st_layers("SWISSTLM3D_2024_LV95_LN02.gpkg")#see all contents

nutzungsareal <- read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_areale_nutzungsareal")# read layers
bodenbedeckung <- read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_bb_bodenbedeckung")# read layers

unique(nutzungsareal$objektart)
unique(bodenbedeckung$objektart)

tmap_mode("view")
tm_shape(nutzungsareal)+
  tm_polygons(col="objektart")

tm_shape(bodenbedeckung)+
  tm_polygons(col="objektart")

### make hulls around areas where data was generated----

##Visualize confusion matrices ----

##visualize classified path ----