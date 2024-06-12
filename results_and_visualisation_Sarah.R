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
bodenbedeckung_clip <-st_as_sf(bodenbedeckung_clip)


bodenbedeckung_clip<-read_sf("CAMA_data/bodenbedeckung_selection.gpkg")
bodenbedeckung_clip<-bodenbedeckung_clip[,c(1,10,37)]

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

##read activity data ----
activities_classified_sf <-read_sf("test_activities_attributiert.gpkg")

activities_classified_sf <- st_transform(activities_classified_sf, crs = 2056)

##Buffer -----
###(Bodenbedeckung)find whether point is within a distance----
bodenbuf <-st_buffer(bodenbedeckung_clip, dist=10)

activities_classified_sf<-st_join(activities_classified_sf, bodenbuf, join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_boden= objektart)
rm(bodenbuf)

###(Nutzungsareal)find whether point is within a distance----
nutzungsbuf <-st_buffer(nutzungsareal_clip, dist=10)

activities_classified_sf<-st_join(activities_classified_sf, nutzungsbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_nutzung= objektart)
rm(nutzungsbuf)

###(Strassen)find whether point is within a distance----
strassenbuf <-st_buffer(strassen_recreational, dist=10)

activities_classified_sf<-st_join(activities_classified_sf, strassenbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_strassen= objektart)
rm(strassenbuf)

###(Oev)find whether point is within a distance----
oevbuf <-st_buffer(oev_clip, dist=50)

activities_classified_sf<-st_join(activities_classified_sf, oevbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_oev= objektart)
rm(oevbuf)

###(Gebauede)find whether point is within a distance----
activities_classified_sf<-st_join(activities_classified_sf, gebaeude_clip , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_geb= objektart)

colnames(activities_classified_sf)
activities_classified_sf = subset(activities_classified_sf, select = -c(uuid.x...9,uuid.y...11, uuid.x...13,uuid.y...15,uuid ))

st_write(activities_classified_sf, dsn="CAMA_data/activities cama_objects.gpkg")

##reread data ----
activities_with_objects<-read_sf("CAMA_data/activities cama_objects.gpkg")

activities_with_objects$recreation_b <- if_else(is.na(activities_with_objects$obj_boden == TRUE) , FALSE, TRUE)

activities_with_objects$recreation_n <- if_else(is.na(activities_with_objects$obj_nutzung == TRUE) , FALSE, TRUE)

activities_with_objects$recreation_s <- if_else(is.na(activities_with_objects$obj_strassen == TRUE) , FALSE, TRUE)

activities_with_objects<-activities_with_objects |> 
  mutate(recreation = case_when(recreation_b == TRUE ~ "TRUE", recreation_n == TRUE ~ "TRUE", recreation_s == TRUE ~ "TRUE"))

activities_with_objects$recreation[is.na(activities_with_objects$recreation)] <- "FALSE" 

activities_with_objects$recreation<-as.logical(activities_with_objects$recreation)

activities_with_objects$oev <- if_else(is.na(activities_with_objects$obj_oev== TRUE) , FALSE, TRUE)

activities_with_objects$gebaeude <- if_else(is.na(activities_with_objects$obj_geb) == TRUE , FALSE, TRUE)    
activities_with_objects<-activities_with_objects[,c(1:7,13,17:19)]

##Classify
test_classification <- activities_with_objects |> 
  mutate(activity = if_else(gebaeude == TRUE, "shopping", 
                  if_else(recreation == TRUE, "recreation",
                  if_else(oev == TRUE, "travel", "NA"),"NA")))#anpassen conf- matrix


test_classification <- test_classification |> 
  mutate(activity_factor = as.factor(activity)) 
test_classification <- test_classification |> 
  mutate(Attribute_factor = as.factor(Attribut))

#Visualize confusion matrices ----
#source:https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
na.omit()
confus <-conf_mat(data = test_classification, truth = Attribute_factor, estimate = activity_factor)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

#Visualize classified path ----

#CART workflow ----
#http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/#classification-trees 
#and r book

##import movement attributes ----
activities_attributes <-read_csv("test_activities_with_attributes_korrigiert.csv")
activities_attributes$ts_POSIXct <-as.POSIXct(activities_attributes$ts_POSIXct)

activities_attributes_sf <-st_as_sf(activities_attributes,coords = c("lon","lat"), crs = 4326 , remove = FALSE) #anders herum reingelesen

activities_attributes_sf <- st_transform(activities_attributes_sf, crs = 2056)

activities_full<-st_join(activities_attributes_sf,activities_with_objects)

model_full <-rpart(Attribute_factor~ speedMean+stepMean+acceleration+ recreation + oev + gebaeude, data=activities_full)
plot(model_full)
text(model_full, cex=0.8,use.n = TRUE, xpd = TRUE)
model_full$cptable
plotcp(model_full)

# Make predictions on the test data
predicted.classes <- model_full |>  
  predict(test.data, type = "class")
head(predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes == test.data$diabetes)

### Confusion matrix for test- data ---
confus <-conf_mat(data = test_classification, truth = Attribute_factor, estimate = activity_factor)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")