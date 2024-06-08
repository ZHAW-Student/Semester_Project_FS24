#my workspace
library("readr")
library("sf")
library("dplyr")
library("tmap")
library("ggplot2")
library("terra")

##Cama workflow ----

##read CAMA data and remove oversized files immediately----
###prepare clip data----
gemeindegrenzen <- read_sf("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg","tlm_hoheitsgebiet")

gemeindeselection <-gemeindegrenzen |> filter(name %in% c("Andelfingen","Bubendorf","Freienbach","Kleinandelfingen","Liestal" ,"Neuhausen am Rheinfall","Rapperswil-Jona","Regensdorf","Rüti","St. Moritz","S-chanf","Wädenswil","Winterthur","Zuoz"))

#tm_shape(gemeindeselection)+
  #tm_polygons(col="name")

rm(gemeindegrenzen)#free up memory


st_layers("SWISSTLM3D_2024_LV95_LN02.gpkg")#see all contents

gebaeude <-read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_bauten_gebaeude_footprint")
gebaeude_selection<-st_intersection(gebaeude,gemeindeselection)
rm(gebaeude)
st_write(bodenbedeckung_selection, dsn="CAMA_data/bodenbedeckung_selection.gpkg")


strassen <-read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_bauten_strassen_strasse")
strassen_selection<-st_intersection(strassen,gemeindeselection)
rm(strassen)


bodenbedeckung <- read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_bb_bodenbedeckung")
bodenbedeckung_selection<-st_intersection(bodenbedeckung,gemeindeselection)
rm(bodenbedeckung)
gc()#free up memory
st_write(bodenbedeckung_selection, dsn="CAMA_data/bodenbedeckung_selection.gpkg")

nutzungsareal <-read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_areale_nutzungsareal")
nutzungsareal_selection<-st_intersection(nutzungsareal,gemeindeselection)
rm(nutzungsareal)
st_write(nutzungsareal_selection, dsn="CAMA_data/nutzungsareal_selection.gpkg")


tm_shape(gemeindeselection)+
  tm_polygons(col="name")+
    tm_shape(a)+
    tm_polygons(col="OBJEKTART")+
  tm_shape(b)+
  tm_polygons(col="OBJEKTART")

###merge polygons---

merge <-st_union(bodenbedeckung,nutzungsareal, is_coverage = TRUE)#

#find whether point is within a distance
st_is_within_distance()

### create number if point lies within boundaries of polygon ----

##Visualize confusion matrices ----

##visualize classified path ----