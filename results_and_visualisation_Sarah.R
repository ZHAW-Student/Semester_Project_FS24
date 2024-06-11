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
library("stringr")

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


##re-read exported data and remove unnecessary columns ----
gebaeude_clip<-read_sf("CAMA_data/gebaeude_selection.gpkg")
gebaeude_clip <- gebaeude_clip[,c(1,10,40)]

bodenbedeckung_clip<-read_sf("CAMA_data/bodenbedeckung_selection.gpkg")
bodenbedeckung_clip<-bodenbedeckung_clip[,c(1,10,37)]
bodenbedeckung_clip <-st_as_sf(bodenbedeckung_clip)

nutzungsareal_clip<-read_sf("CAMA_data/nutzungsareal_selection.gpkg")
nutzungsareal_clip<-nutzungsareal_clip[,c(1,11,39)]

strassen_clip<-read_sf("CAMA_data/strassen_selection.gpkg")
#reduce roads to roads associated with recreational activities
strassen_recreational <-strassen_clip |> filter(objektart %in% c("Ausfahrt","Einfahrt","Zufahrt","3m Strasse","1m Weg" ,"2m Weg","1m Wegfragment","2m Wegfragment"))
rm(strassen_clip)
strassen_recreational<-strassen_recreational[,c(1,10,51)]

oev_clip<-read_sf("CAMA_data/oev_selection.gpkg")
colnames(oev_clip)
oev_clip<-oev_clip[,c(1,10,40)]

#read data
activities_classified_sf <-read_sf("test_activities_attributiert.gpkg")

activities_classified_sf <- st_transform(activities_classified_sf, crs = 2056)
#create index
activities_classified_sf$id_cama<- 1:nrow(activities_classified_sf)
  
##(Bodenbedeckung)find whether point is within a distance----
bodenbedeckung_test <- st_is_within_distance(x = activities_classified_sf, y = bodenbedeckung_clip, dist = units::set_units(20, "m"), remove_self = FALSE)

bodenbedeckung_test[1]

overlap_test <-function(output_list){
  list_df<-as.data.frame(do.call(rbind, output_list))
  list_df$test<-TRUE
  list_df$id_cama <-rownames(list_df)
  list_df$id_cama <-as.numeric(str_sub(list_df$id_cama,4))
  list_df <- subset(list_df, select = c(test, id_cama))}

tests<-overlap_test(test)
activities_classified_sf<-left_join(activities_classified_sf,tests,by="id_cama")
activities_classified_sf<-activities_classified_sf |> rename(bodenbedeckung = test)


st_write(activities_classified_sf, dsn="CAMA_data/activities cama.gpkg")

## create number if point lies within boundaries of object ----


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