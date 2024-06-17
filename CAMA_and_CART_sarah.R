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

#Cama analysis ----

##Prepare clip data----
gemeindegrenzen <- read_sf("swissBOUNDARIES3D_1_5_LV95_LN02.gpkg","tlm_hoheitsgebiet")

gemeindeselection <-gemeindegrenzen |> filter(name %in% c("Andelfingen","Bubendorf","Freienbach","Kleinandelfingen","Liestal" ,"Neuhausen am Rheinfall","Rapperswil-Jona","Regensdorf","Rüti","St. Moritz","S-chanf","Wädenswil","Winterthur","Zuoz"))

tmap_mode("view")
tm_shape(gemeindeselection)+
  tm_polygons(col="name")
rm(gemeindegrenzen)#free up memory

##Reading, clipping and exporting clipped layers----
st_layers("SWISSTLM3D_2024_LV95_LN02.gpkg")#see all contents


################### Warning! This does take some time.  

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


##Re-read clipped data, remove columns ----
gebaeude_clip<-read_sf("CAMA_data/gebaeude_selection.gpkg")
gebaeude_clip <- gebaeude_clip[,c(1,10,40)]

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
oev_clip<-oev_clip[,c(1,10,40)]

##Create Buffers ----
bodenbuf <-st_buffer(bodenbedeckung_clip, dist=10)
rm(bodenbedeckung_clip)
nutzungsbuf <-st_buffer(nutzungsareal_clip, dist=10)
rm(nutzungsareal_clip)
strassenbuf <-st_buffer(strassen_recreational, dist=10)
rm(strassen_recreational)
oevbuf <-st_buffer(oev_clip, dist=50)
rm(oev_clip)

##Workflow with Saskia's training- data ----
###Read activity data ----
activities_classified_sf <-read_sf("CAMA_data/test_activities_attributiert.gpkg")

activities_classified_sf <- st_transform(activities_classified_sf, crs = 2056)

###Join with objects -----
activities_classified_sf<-st_join(activities_classified_sf, bodenbuf, join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_boden= objektart)
rm(bodenbuf)

activities_classified_sf<-st_join(activities_classified_sf, nutzungsbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_nutzung= objektart)
rm(nutzungsbuf)

activities_classified_sf<-st_join(activities_classified_sf, strassenbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_strassen= objektart)
rm(strassenbuf)

activities_classified_sf<-st_join(activities_classified_sf, oevbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_oev= objektart)
rm(oevbuf)

activities_classified_sf<-st_join(activities_classified_sf, gebaeude_clip , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_geb= objektart)
rm(gebaeude_clip)

activities_classified_sf = subset(activities_classified_sf, select = -c(uuid.x...9,uuid.y...11, uuid.x...13,uuid.y...15,uuid ))

st_write(activities_classified_sf, dsn="CAMA_data/activities cama_objects.gpkg")

###Reread object-data ----
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

###Classification ----
test_classification <- activities_with_objects |> 
  mutate(activity = if_else(gebaeude == TRUE, "shopping", 
                    if_else(oev == TRUE, "travel",
                    if_else(recreation == TRUE, "recreation", "travel"),NA)))

test_classification <- test_classification |> 
  mutate(activity_factor = as.factor(activity)) 
test_classification <- test_classification |> 
  mutate(Attribute_factor = as.factor(Attribut))

st_write(test_classification, dsn="CAMA_data/ cama_classification_results_saskia_training.gpkg")#export classification results

###Confusion matrix ----
test_classification <-na.omit(test_classification)
confus <-conf_mat(data = test_classification, truth = Attribute_factor, estimate = activity_factor)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

### extract single trajectories----
traj1 <- filter(test_classification, ID == "test_1")
traj2 <- filter(test_classification, ID == "test_2")
traj3 <- filter(test_classification, ID == "test_3")
traj4 <- filter(test_classification, ID == "test_4")
traj5 <- filter(test_classification, ID == "test_5")
traj6 <- filter(test_classification, ID == "test_6")
traj7 <- filter(test_classification, ID == "test_7")
traj8 <- filter(test_classification, ID == "test_8")
traj9 <- filter(test_classification, ID == "test_9")
traj10 <- filter(test_classification, ID == "test_10")
traj11 <- filter(test_classification, ID == "test_11")
traj12 <- filter(test_classification, ID == "test_12")

### plot single trajectories with segmentation----

traj_plot1 <- ggplot(traj1, aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot2 <- ggplot(traj2,  aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot3 <- ggplot(traj3, aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot4 <- ggplot(traj4,  aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot5 <- ggplot(traj5, aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot6 <- ggplot(traj6, aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot7 <- ggplot(traj7,  aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot8 <- ggplot(traj8,  aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut)) 

traj_plot9 <- ggplot(traj9,  aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot10 <- ggplot(traj10,   aes(lat, lon, colour = activity)) +
geom_point(aes(shape=Attribut))

traj_plot11 <- ggplot(traj11,  aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))
traj_plot12 <- ggplot(traj12,  aes(lat, lon, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot1
traj_plot2
traj_plot3
traj_plot4
traj_plot5
traj_plot6
traj_plot7
traj_plot8
traj_plot9
traj_plot10
traj_plot11
traj_plot12

##Workflow with Sarah's test- data ----
###read activity data  ----
activities_classified_sf <-read_sf("CAMA_data/activities_sarah_classified.gpkg")

activities_classified_sf <- st_transform(activities_classified_sf, crs = 2056)

###Join with objects -----

activities_classified_sf<-st_join(activities_classified_sf, bodenbuf, join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_boden= objektart)
rm(bodenbuf)

activities_classified_sf<-st_join(activities_classified_sf, nutzungsbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_nutzung= objektart)
rm(nutzungsbuf)

activities_classified_sf<-st_join(activities_classified_sf, strassenbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_strassen= objektart)
rm(strassenbuf)

activities_classified_sf<-st_join(activities_classified_sf, oevbuf , join=st_within,left=TRUE, largest=TRUE)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_oev= objektart)
rm(oevbuf)

activities_classified_sf<-st_join(activities_classified_sf, gebaeude_clip , join=st_within,left=TRUE, largest=TRUE)
rm(gebaeude_clip)

activities_classified_sf<-activities_classified_sf |> 
  rename(obj_geb= objektart)

activities_classified_sf = subset(activities_classified_sf, select = -c(uuid.x...9,uuid.y...11, uuid.x...13,uuid.y...15,uuid ))

st_write(activities_classified_sf, dsn="CAMA_data/activities cama_objects_sarah_test.gpkg")

###Reread object-data ----
activities_with_objects<-read_sf("CAMA_data/activities cama_objects_sarah_test.gpkg")

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

###Classification ----
classification <- activities_with_objects |> 
  mutate(activity = if_else(gebaeude == TRUE, "shopping", 
                    if_else(oev == TRUE, "travel",
                    if_else(recreation == TRUE, "recreation", "travel"),NA)))


classification <- classification |> 
  mutate(activity_factor = as.factor(activity)) 
classification <-classification |> 
  mutate(Attribute_factor = as.factor(Attribut))

st_write(classification, dsn="CAMA_data/ cama_classification_results_sarah.gpkg")#export classification results

###Confusion matrix ----
classification<-na.omit(classification)
confus <-conf_mat(data = classification, truth = Attribute_factor, estimate = activity_factor)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

### extract single trajectories----
traj1 <- filter(classification, ID == "sarah__1")
traj2 <- filter(classification, ID == "sarah__2")
traj3 <- filter(classification, ID == "sarah__3")
traj4 <- filter(classification, ID == "sarah__4")
traj5 <- filter(classification, ID == "sarah__5")
traj6 <- filter(classification, ID == "sarah__6")
traj7 <- filter(classification, ID == "sarah__7")
traj8 <- filter(classification, ID == "sarah__8")
traj9 <- filter(classification, ID == "sarah__9")
traj10 <- filter(classification, ID == "sarah__10")
traj11 <- filter(classification, ID == "sarah__11")
traj12 <- filter(classification, ID == "sarah__12")

### plot single trajectories with segmentation----

traj_plot1 <- ggplot(traj1, aes( lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot2 <- ggplot(traj2,  aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot3 <- ggplot(traj3, aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot4 <- ggplot(traj4,  aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot5 <- ggplot(traj5, aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot6 <- ggplot(traj6, aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot7 <- ggplot(traj7,  aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot8 <- ggplot(traj8,  aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut)) 

traj_plot9 <- ggplot(traj9,  aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot10 <- ggplot(traj10,   aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot11 <- ggplot(traj11,  aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))
traj_plot12 <- ggplot(traj12,  aes(lon, lat, colour = activity)) +
  geom_point(aes(shape=Attribut))

traj_plot1
traj_plot2
traj_plot3
traj_plot4
traj_plot5
traj_plot6
traj_plot7
traj_plot8
traj_plot9
traj_plot10
traj_plot11
traj_plot12

#CART analysis ----
#http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/#classification-trees 
#and r book

##Workflow based on Saskia's training- data ----
###Import movement attributes and results of CAMA analysis  ----
activities_attributes <-read_csv("test_activities_with_attributes_korrigiert.csv")
activities_attributes$ts_POSIXct <-as.POSIXct(activities_attributes$ts_POSIXct)

activities_attributes_sf <-st_as_sf(activities_attributes,coords = c("lon","lat"), crs = 4326 , remove = FALSE) #anders herum reingelesen wegen lat lon Fehler

activities_attributes_sf <- st_transform(activities_attributes_sf, crs = 2056)

cama_class_saskia_train<-read_sf("CAMA_data/ cama_classification_results_saskia_training.gpkg")

###Join data from CAMA and walking- attributes -----
activities_sas_train<-st_join(activities_attributes_sf,cama_class_saskia_train)

activities_sas_train$Attribute_factor.x <-as.factor(activities_sas_train$Attribute_factor.x)

###Create tree ----
model_sas_train <-rpart(Attribute_factor.x~ speedMean+stepMean+acceleration+ recreation + oev + gebaeude, data=activities_sas_train, method= "class")

### Visualize tree ----
plot(model_sas_train)
text(model_sas_train, cex=0.8,use.n = TRUE, xpd = TRUE)

###See wheter tree needs to be pruned ----
model_sas_train$cptable
plotcp(model_sas_train)#no pruning needed

###Make prediction on original data for confusion matrix----
pred_model_sas_train<- predict(model_sas_train, type="class")
activities_sas_train$pred<-pred_model_sas_train

### Confusion matrix for training- data ---
confus <-conf_mat(data =activities_sas_train, truth = Attribute_factor.x, estimate = pred)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

### Compute model accuracy rate on Sakia's training data----
mean(pred_model_sas_train == activities_sas_train$Attribute_factor.x)

##Make predictions on the test data from Saskia ----
pred_model_sas_test<- model_sas_train |>  
  predict(test.data, type = "class")

activities_sas_train$pred_test_sas <-pred_model_sas_test

### Confusion matrix for Sakia's test- data  ---
confus_test_sas <-conf_mat(data = activities_sas_train, truth = Attribute_factor.x, estimate = pred_test_sas)

autoplot(confus_test_sas, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

### Compute model accuracy rate on Sakia's test data----
mean(pred_model_sas_test == activities_sas_train$Attribute_factor.x)


##Make predictions on the test data from Sarah ----
activities_attributes_sarah <-read_csv("test_activities_with_attributes_korrigiert.csv")
activities_attributes_sarah$ts_POSIXct <-as.POSIXct(activities_attributes_sarah$ts_POSIXct)

activities_attributes_sarah <-st_as_sf(activities_attributes_sarah,coords = c("lon","lat"), crs = 4326 , remove = FALSE) #anders herum reingelesen wegen lat lon Fehler

activities_attributes_sarah <- st_transform(activities_attributes_sarah, crs = 2056)

cama_class_sarah_test<-read_sf("CAMA_data/ cama_classification_results_saskia_training.gpkg")

###Join data from CAMA and walking- attributes -----
activities_sar_test<-st_join(activities_attributes_sarah,cama_class_sarah_test)

activities_sar_test$Attribute_factor.x <-as.factor(activities_sar_test$Attribute_factor.x)

pred_model_sar_test<- model_sas_train |>  
  predict(activities_sar_test, type = "class")

activities_sar_test$pred_test_sar <-pred_model_sar_test

### Confusion matrix for Sarah's test- data  ---
confus_sar_test <-conf_mat(data = activities_sar_test, truth = Attribute_factor.x, estimate = pred_test_sar)

autoplot(confus_sar_test, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

### Compute model accuracy rate on Sarah's test data----
mean(pred_model_sar_test == activities_sar_train$Attribute_factor.x)


#Visualize confusion matrices ----
#source:https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package