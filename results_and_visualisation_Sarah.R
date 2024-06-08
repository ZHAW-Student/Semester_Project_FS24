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


st_layers("Vektor_25_Waedi/226/SMV25_CHLV95LN02.gpkg")#see all contents


bodenbedeckung <- read_sf("Vektor_25_Waedi/226/SMV25_CHLV95LN02.gpkg","T65_DKM25_BODENBEDECKUNG")

nutzungsareal <-read_sf("Vektor_25_Waedi/226/SMV25_CHLV95LN02.gpkg","T64_DKM25_NUTZUNGSAREAL")

tmap_mode("view")
tm_shape(bodenbedeckung)+
  tm_polygons(col="OBJEKTART")+
  tm_shape(nutzungsareal)+
  tm_polygons(col="OBJEKTART")



##clip data----
st_layers("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg")#see all contents

gemeindegrenzen <- read_sf("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg","tlm_hoheitsgebiet")
tm_shape(gemeindegrenzen)+
  tm_polygons(col="name")

gemeindeselection <-gemeindegrenzen$name[(gemeindegrenzen$name %in% c("Andelfingen","Bubendorf","Kleinandelfingen","Liestal" ,"Neuhausen","Pfäffikon","Regensdorf","Rüti","St. Moritz","S-Chanf","Wädenswil","Zuoz"))]

a<-st_intersection(bodenbedeckung,)

tm_shape(borders)+
  tm_polygons(col="ort")+
    tm_shape(a)+
    tm_polygons(col="OBJEKTART")

###merge polygons---

#merge <-st_union(bodenbedeckung,nutzungsareal, is_coverage = TRUE)#

#find whether point is within a distance
st_is_within_distance()

### create number if point lies within boundaries of polygon ----

##Visualize confusion matrices ----

##visualize classified path ----