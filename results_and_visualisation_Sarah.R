#my workspace
library("readr")
library("sf")
library("dplyr")
library("tmap")
library("ggplot2")
library("terra")
library("caret")
library("rpart")
library("yardstick")

#Cama workflow ----

##Prepare clip data----
gemeindegrenzen <- read_sf("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg","tlm_hoheitsgebiet")

gemeindeselection <-gemeindegrenzen |> filter(name %in% c("Andelfingen","Bubendorf","Freienbach","Kleinandelfingen","Liestal" ,"Neuhausen am Rheinfall","Rapperswil-Jona","Regensdorf","Rüti","St. Moritz","S-chanf","Wädenswil","Winterthur","Zuoz"))

tmap_mode("view")
tm_shape(gemeindeselection)+
  tm_polygons(col="name")
rm(gemeindegrenzen)#free up memory

##Reading, clipping and exporting clipped layers----
st_layers("SWISSTLM3D_2024_LV95_LN02.gpkg")#see all contents

gebaeude <-read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_bauten_gebaeude_footprint")
gebaeude_selection<-st_intersection(gebaeude,gemeindeselection)
rm(gebaeude)
st_write(gebaeude_selection, dsn="CAMA_data/gebaeude_selection.gpkg")

bodenbedeckung <- read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_bb_bodenbedeckung")
bodenbedeckung_selection<-st_intersection(bodenbedeckung,gemeindeselection)
rm(bodenbedeckung)
gc()#free up memory
st_write(bodenbedeckung_selection, dsn="CAMA_data/bodenbedeckung_selection.gpkg")

nutzungsareal <-read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_areale_nutzungsareal")
nutzungsareal_selection<-st_intersection(nutzungsareal,gemeindeselection)
rm(nutzungsareal)
st_write(nutzungsareal_selection, dsn="CAMA_data/nutzungsareal_selection.gpkg")

strassen <-read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_strassen_strasse")
strassen_selection<-st_intersection(strassen,gemeindeselection)
rm(strassen)
st_write(strassen_selection, dsn="CAMA_data/strassen_selection.gpkg")

oev <-read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_oev_haltestelle")
oev_selection<-st_intersection(oev,gemeindeselection)
rm(oev)
st_write(oev_selection, dsn="CAMA_data/oev_selection.gpkg")

##re-read exported data for other days ----
gebaeude_clip<-read_sf("CAMA_data/gebaeude_selection.gpkg")

bodenbedeckung_clip<-read_sf("CAMA_data/bodenbedeckung_selection.gpkg")

nutzungsareal_clip<-read_sf("CAMA_data/nutzungsareal_selection.gpkg")
strassen_clip<-read_sf("CAMA_data/strassen_selection.gpkg")
unique(strassen_clip$objektart)
#reduce roads to roads associated with recreational activities
strassen_recreational <-strassen_clip |> filter(objektart %in% c("Ausfahrt","Einfahrt","Zufahrt","3m Strasse","1m Weg" ,"2m Weg","1m Wegfragment","2m Wegfragment"))
rm(strassen_clip)

oev_clip<-read_sf("CAMA_data/oev_selection.gpkg")

##Merge polygons---
merge <-st_union(bodenbedeckung_clip,nutzungsareal_clip, is_coverage = TRUE)#

#find whether point is within a distance
st_is_within_distance(points,camalayer,distance=2)

## create number if point lies within boundaries of object ----
#i am bad at if else statements, but this could work

#Visualize confusion matrices ----
#source:https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package

testmat<-matrix(c(15,44,23,32),nrow=2,dimnames = list(c("0","1"), c("0","1")))

confus <-conf_mat(testmat)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

#Visualize classified path ----

#CART workflow ----
#http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/#classification-trees
model <-rpart(klasse~ var+var+var, data=)
plot(model)

# Make predictions on the test data
predicted.classes <- model %>% 
  predict(test.data, type = "class")
head(predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes == test.data$diabetes)