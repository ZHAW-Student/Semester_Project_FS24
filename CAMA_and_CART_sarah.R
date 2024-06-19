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

gemeindeselection <-gemeindegrenzen |>
                  filter(name %in% c("Andelfingen","Bubendorf","Freienbach" 
                                     ,"Kleinandelfingen","Liestal" ,"Neuhausen am
                                     Rheinfall","Rapperswil-Jona","Regensdorf","
                                     Rüti","St. Moritz","S-chanf","Wädenswil"
                                     ,"Winterthur","Zuoz"))

##Reading, clipping and exporting clipped layers----
st_layers("SWISSTLM3D_2024_LV95_LN02.gpkg")#see all contents of geopackage

################### Warning! This does take some time.  

gebaeude <-read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_bauten_gebaeude_footprint")
gebaeude_selection<-st_intersection(gebaeude,gemeindeselection)
rm(gebaeude)
st_write(gebaeude_selection, dsn="CAMA_data/gebaeude_selection.gpkg")

bodenbedeckung <- read_sf("SWISSTLM3D_2024_LV95_LN02.gpkg","tlm_bb_bodenbedeckung")
bodenbedeckung_selection<-st_intersection(bodenbedeckung,gemeindeselection)
rm(bodenbedeckung)
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


##Re-read clipped data, remove not- needed columns and filter if required ----
gebaeude_clip<-read_sf("CAMA_data/gebaeude_selection.gpkg")
gebaeude_clip <- gebaeude_clip[,c(1,10,40)]

bodenbedeckung_clip<-read_sf("CAMA_data/bodenbedeckung_selection.gpkg")
bodenbedeckung_clip<-bodenbedeckung_clip[,c(1,10,37)]

nutzungsareal_clip<-read_sf("CAMA_data/nutzungsareal_selection.gpkg")
nutzungsareal_clip<-nutzungsareal_clip[,c(1,11,39)]

strassen_clip<-read_sf("CAMA_data/strassen_selection.gpkg")
#reduce roads to roads associated with recreational activities
strassen_recreational <-strassen_clip |> 
                      filter(objektart %in% c("Ausfahrt","Einfahrt","Zufahrt",
                                              "3m Strasse","1m Weg" ,"2m Weg",
                                              "1m Wegfragment","2m Wegfragment"))
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
sas_tra_activities_classified_sf <-read_sf("CAMA_data/test_activities_attributiert.gpkg")

sas_tra_activities_classified_sf <- st_transform(sas_tra_activities_classified_sf, crs = 2056)

###Join with objects -----
sas_tra_activities_classified_sf<-st_join(sas_tra_activities_classified_sf, bodenbuf,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tra_activities_classified_sf<-sas_tra_activities_classified_sf |> 
  rename(obj_boden= objektart)

sas_tra_activities_classified_sf<-st_join(sas_tra_activities_classified_sf, nutzungsbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tra_activities_classified_sf<-sas_tra_activities_classified_sf |> 
  rename(obj_nutzung= objektart)

sas_tra_activities_classified_sf<-st_join(sas_tra_activities_classified_sf, strassenbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tra_activities_classified_sf<-sas_tra_activities_classified_sf |> 
  rename(obj_strassen= objektart)

sas_tra_activities_classified_sf<-st_join(sas_tra_activities_classified_sf, oevbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tra_activities_classified_sf<-sas_tra_activities_classified_sf |> 
  rename(obj_oev= objektart)

sas_tra_activities_classified_sf<-st_join(sas_tra_activities_classified_sf, gebaeude_clip ,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tra_activities_classified_sf<-sas_tra_activities_classified_sf |> 
  rename(obj_geb= objektart)

sas_tra_activities_classified_sf = subset(sas_tra_activities_classified_sf,
                                  select = -c(uuid.x...9,uuid.y...11, uuid.x...13, uuid.y...15,uuid ))

###Create presence/absence information for objects ----
sas_tra_activities_classified_sf$recreation_b <- if_else(is.na(sas_tra_activities_classified_sf$obj_boden == TRUE) , FALSE, TRUE)

sas_tra_activities_classified_sf$recreation_n <- if_else(is.na(sas_tra_activities_classified_sf$obj_nutzung == TRUE) , FALSE, TRUE)

sas_tra_activities_classified_sf$recreation_s <- if_else(is.na(sas_tra_activities_classified_sf$obj_strassen == TRUE) , FALSE, TRUE)

sas_tra_activities_classified_sf<-sas_tra_activities_classified_sf |> 
  mutate(recreation = case_when(recreation_b == TRUE ~ "TRUE", 
                                recreation_n == TRUE ~ "TRUE", 
                                recreation_s == TRUE ~ "TRUE"))

sas_tra_activities_classified_sf$recreation[is.na(sas_tra_activities_classified_sf$recreation)] <- "FALSE" 

sas_tra_activities_classified_sf$recreation<-as.logical(sas_tra_activities_classified_sf$recreation)

sas_tra_activities_classified_sf$oev <- if_else(is.na(sas_tra_activities_classified_sf$obj_oev== TRUE)
                                       , FALSE, TRUE)

sas_tra_activities_classified_sf$gebaeude <- if_else(is.na(sas_tra_activities_classified_sf$obj_geb)
                                            == TRUE , FALSE, TRUE)    
sas_tra_activities_classified_sf<-sas_tra_activities_classified_sf[,c(1:7,13,17:19)]

### Classification ----
sas_tra_classification <- sas_tra_activities_classified_sf |> 
  mutate(activity = if_else(gebaeude == TRUE, "shopping", 
                    if_else(oev == TRUE, "travel",
                    if_else(recreation == TRUE, "recreation", "travel"),NA)))

sas_tra_classification <- sas_tra_classification |> 
  mutate(activity_factor = as.factor(activity)) 
sas_tra_classification <- sas_tra_classification |> 
  mutate(Attribute_factor = as.factor(Attribut))#change character to factor for confusion matrix

#st_write(sas_tra_test_classification, dsn="CAMA_data/ cama_classification_results_saskia_training.gpkg")#export classification results

###Confusion matrix ----
sas_tra_classification <-na.omit(sas_tra_classification)
confus_sas_tra_cama <-conf_mat(data = sas_tra_classification, truth = Attribute_factor, 
                  estimate = activity_factor)

autoplot(confus_sas_tra_cama, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

###Compute model accuracy ----
confusionMatrix(sas_tra_classification$Attribute_factor, sas_tra_classification$activity_factor)


##Workflow with Saskia's test- data ----
###read activity data  ----
sas_tes_activities_classified_sf <-read_csv("CAMA_data/activities_saskia_attributiert.csv")
sas_tes_activities_classified_sf$ts_POSIXct <-as.POSIXct(sas_tes_activities_classified_sf$ts_POSIXct)

sas_tes_activities_classified_sf <-st_as_sf(sas_tes_activities_classified_sf, 
                          coords = c("lon", "lat"), crs = 4326 , remove = FALSE) 

sas_tes_activities_classified_sf <- st_transform(sas_tes_activities_classified_sf, crs = 2056)

###Join with objects -----
sas_tes_activities_classified_sf<-st_join(sas_tes_activities_classified_sf, bodenbuf,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tes_activities_classified_sf<-sas_tes_activities_classified_sf |> 
  rename(obj_boden= objektart)

sas_tes_activities_classified_sf<-st_join(sas_tes_activities_classified_sf, nutzungsbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tes_activities_classified_sf<-sas_tes_activities_classified_sf |> 
  rename(obj_nutzung= objektart)

sas_tes_activities_classified_sf<-st_join(sas_tes_activities_classified_sf, strassenbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tes_activities_classified_sf<-sas_tes_activities_classified_sf |> 
  rename(obj_strassen= objektart)

sas_tes_activities_classified_sf<-st_join(sas_tes_activities_classified_sf, oevbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tes_activities_classified_sf<-sas_tes_activities_classified_sf |> 
  rename(obj_oev= objektart)

sas_tes_activities_classified_sf<-st_join(sas_tes_activities_classified_sf, gebaeude_clip ,
                                  join=st_within,left=TRUE, largest=TRUE)

sas_tes_activities_classified_sf<-sas_tes_activities_classified_sf |> 
  rename(obj_geb= objektart)

sas_tes_activities_classified_sf = subset(sas_tes_activities_classified_sf,
                          select = -c(fid,uuid.x...10,uuid.y...12, uuid.x...14,
                                      uuid.y...16,uuid))

###Create presence/absence information for objects ----
sas_tes_activities_classified_sf$recreation_b <- if_else(is.na(sas_tes_activities_classified_sf$obj_boden == TRUE) , FALSE, TRUE)

sas_tes_activities_classified_sf$recreation_n <- if_else(is.na(sas_tes_activities_classified_sf$obj_nutzung == TRUE) , FALSE, TRUE)

sas_tes_activities_classified_sf$recreation_s <- if_else(is.na(sas_tes_activities_classified_sf$obj_strassen == TRUE) , FALSE, TRUE)

sas_tes_activities_classified_sf<-sas_tes_activities_classified_sf |> 
  mutate(recreation = case_when(recreation_b == TRUE ~ "TRUE",
                                recreation_n == TRUE ~ "TRUE", 
                                recreation_s == TRUE ~ "TRUE"))

sas_tes_activities_classified_sf$recreation[is.na(sas_tes_activities_classified_sf$recreation)] <- "FALSE" 

sas_tes_activities_classified_sf$recreation<-as.logical(sas_tes_activities_classified_sf$recreation)

sas_tes_activities_classified_sf$oev <- if_else(is.na(sas_tes_activities_classified_sf$obj_oev== TRUE) , FALSE, TRUE)

sas_tes_activities_classified_sf$gebaeude <- if_else(is.na(sas_tes_activities_classified_sf$obj_geb) == TRUE , FALSE, TRUE)    
sas_tes_activities_classified_sf<-sas_tes_activities_classified_sf[,c(1:7,13,17:19)]

###Classification ----
sas_test_classification <- sas_tes_activities_with_objects |> 
  mutate(activity = if_else(gebaeude == TRUE, "shopping", 
                            if_else(oev == TRUE, "travel",
                            if_else(recreation == TRUE, "recreation", "travel"),NA)))


sas_test_classification <- sas_test_classification |> 
  mutate(activity_factor = as.factor(activity)) 
sas_test_classification <-sas_test_classification |> 
  mutate(Attribute_factor = as.factor(Attribut))#change character to factor for confusion matrix

#st_write(sas_test_classification, dsn="CAMA_data/ cama__results_saskia_test.gpkg")#export classification results

###Confusion matrix ----
sas_test_classification<-na.omit(sas_test_classification)
confus_sas_tes_cama <-conf_mat(data = sas_test_classification, truth = Attribute_factor,
                  estimate = activity_factor)

autoplot(confus_sas_tes, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

###Compute model accuracy ----
confusionMatrix(sas_test_classification$Attribute_factor, sas_test_classification$activity_factor)

##Workflow with Sarah's test- data ----
###Read activity data  ----
sar_tes_activities_classified_sf <-read_sf("CAMA_data/activities_sarah_classified.gpkg")

sar_tes_activities_classified_sf <- st_transform(sar_tes_activities_classified_sf, crs = 2056)

###Join with objects -----
sar_tes_activities_classified_sf<-st_join(sar_tes_activities_classified_sf, bodenbuf,
                                  join=st_within,left=TRUE, largest=TRUE)

sar_tes_activities_classified_sf<-sar_tes_activities_classified_sf |> 
  rename(obj_boden= objektart)

sar_tes_activities_classified_sf<-st_join(sar_tes_activities_classified_sf, nutzungsbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sar_tes_activities_classified_sf<-sar_tes_activities_classified_sf |> 
  rename(obj_nutzung= objektart)

sar_tes_activities_classified_sf<-st_join(sar_tes_activities_classified_sf, strassenbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sar_tes_activities_classified_sf<-sar_tes_activities_classified_sf |> 
  rename(obj_strassen= objektart)

sar_tes_activities_classified_sf<-st_join(sar_tes_activities_classified_sf, oevbuf ,
                                  join=st_within,left=TRUE, largest=TRUE)

sar_tes_activities_classified_sf<-sar_tes_activities_classified_sf |> 
  rename(obj_oev= objektart)

sar_tes_activities_classified_sf<-st_join(sar_tes_activities_classified_sf, gebaeude_clip ,
                                  join=st_within,left=TRUE, largest=TRUE)

sar_tes_activities_classified_sf<-sar_tes_activities_classified_sf |> 
  rename(obj_geb= objektart)

sar_tes_activities_classified_sf = subset(sar_tes_activities_classified_sf, 
                                  select = -c(uuid.x...9,uuid.y...11, uuid.x...13,
                                              uuid.y...15,uuid ))

###Create presence/absence information for objects ----
sar_tes_activities_classified_sf$recreation_b <- if_else(is.na(sar_tes_activities_classified_sf$obj_boden == TRUE) , FALSE, TRUE)

sar_tes_activities_classified_sf$recreation_n <- if_else(is.na(sar_tes_activities_classified_sf$obj_nutzung == TRUE) , FALSE, TRUE)

sar_tes_activities_classified_sf$recreation_s <- if_else(is.na(sar_tes_activities_classified_sf$obj_strassen == TRUE) , FALSE, TRUE)

sar_tes_activities_classified_sf<-sar_tes_activities_classified_sf |> 
  mutate(recreation = case_when(recreation_b == TRUE ~ "TRUE", 
                                recreation_n == TRUE ~ "TRUE", 
                                recreation_s == TRUE ~ "TRUE"))

sar_tes_activities_classified_sf$recreation[is.na(sar_tes_activities_classified_sf$recreation)] <- "FALSE" 

sar_tes_activities_classified_sf$recreation<-as.logical(sar_tes_activities_classified_sf$recreation)

sar_tes_activities_classified_sf$oev <- if_else(is.na(sar_tes_activities_classified_sf$obj_oev== TRUE) , FALSE, TRUE)

sar_tes_activities_classified_sf$gebaeude <- if_else(is.na(sar_tes_activities_classified_sf$obj_geb) == TRUE , FALSE, TRUE)    
sar_tes_activities_classified_sf<-sar_tes_activities_classified_sf[,c(1:7,13,17:19)]

###Classification ----
sar_test_classification <- sar_tes_activities_classified_sf |> 
  mutate(activity = if_else(gebaeude == TRUE, "shopping", 
                    if_else(oev == TRUE, "travel",
                    if_else(recreation == TRUE, "recreation", "travel"),NA)))


sar_test_classification <- sar_test_classification |> 
  mutate(activity_factor = as.factor(activity)) 
sar_test_classification <-sar_test_classification |> 
  mutate(Attribute_factor = as.factor(Attribut))#change character to factor for confusion matrix

st_write(sar_test_classification, dsn="CAMA_data/ cama_classification_results_sarah.gpkg")#export sar_test_classification results

###Confusion matrix ----
sar_test_classification<-na.omit(sar_test_classification)
confus_sar_tes_cama <-conf_mat(data = sar_test_classification, truth = Attribute_factor, estimate = activity_factor)

autoplot(confus_sar_tes_cama, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

###Compute model accuracy ----
confusionMatrix(sar_test_classification$Attribute_factor, sar_test_classification$activity_factor)

#CART analysis ----
##Workflow based on Saskia's training- data ----
###Import movement attributes and results of CAMA analysis and transform them into spatial objects ----
activities_attributes <-read_csv("test_activities_with_attributes_new.csv")
activities_attributes$ts_POSIXct <-as.POSIXct(activities_attributes$ts_POSIXct)

activities_attributes_sf <-st_as_sf(activities_attributes,
                                    coords = c("lon","lat"), crs = 4326 ,
                                    remove = FALSE) 

activities_attributes_sf <- st_transform(activities_attributes_sf, crs = 2056)


activities_attributes_sf  <-activities_attributes_sf [,c(1:26,30:32)]
#remove non-required columns

###Join data from CAMA and walking- attributes -----
activities_sas_train<-st_join(activities_attributes_sf,sas_tra_classification)

activities_sas_train$Attribute_factor.x <-as.factor(activities_sas_train$Attribute_factor.x)

###Create tree ----
set.seed(6832)
model_sas_train <-rpart(Attribute_factor.x~ speedMean+stepMean+acceleration+ recreation + oev + gebaeude, data=activities_sas_train, method= "class")

### Visualize tree ----
plot(model_sas_train)
text(model_sas_train, cex=0.8,use.n = TRUE, xpd = TRUE)

###See whether tree needs to be pruned ----
model_sas_train$cptable
#plotcp(model_sas_train)#no pruning needed

###Make prediction on original data for confusion matrix----
pred_model_sas_train<- predict(model_sas_train, type="class")
activities_sas_train$pred<-pred_model_sas_train

### Confusion matrix for training- data ---
confus <-conf_mat(data =activities_sas_train, truth = Attribute_factor.x,
                  estimate = pred)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

### Compute model accuracy rate on Sakia's training data----
confusionMatrix(activities_sas_train$Attribute_factor.x, activities_sas_train$pred)

##Make predictions on the test data from Saskia ----
###Import movement attributes and results of CAMA analysis and transform them into spatial objects ----

activities_attributes_sas_test <-read_csv("activities_saskia_with_attributes_classified_new.csv")
activities_attributes_sas_test$ts_POSIXct <-as.POSIXct(activities_attributes_sas_test$ts_POSIXct)

activities_attributes_sf_sas_test <-st_as_sf(activities_attributes_sas_test,
                                  coords = c("lon","lat"), crs = 4326 ,
                                  remove = FALSE)

activities_attributes_sf_sas_test  <- st_transform(activities_attributes_sf_sas_test , crs = 2056)

activities_attributes_sf_sas_test <-activities_attributes_sf_sas_test[,c(1:27,31:33)]##remove non-required columns

###Join data from CAMA and walking- attributes -----
activities_sas_test<-st_join(activities_attributes_sf_sas_test ,sas_test_classification)

activities_sas_test$Attribute_factor.x <-as.factor(activities_sas_test$Attribute_factor.x)

pred_model_sas_test<- model_sas_train |>  
  predict(activities_sas_test, type = "class")

activities_sas_test$pred <-pred_model_sas_test

### Confusion matrix for Sakia's test- data  ---
confus_test_sas <-conf_mat(data = activities_sas_test,
                           truth = Attribute_factor.x, estimate = pred)

autoplot(confus_test_sas, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

### Compute model accuracy rate on Sakia's test data----
confusionMatrix(activities_sas_test$Attribute_factor.x, activities_sas_test$pred)

##Make predictions on the test data from Sarah ----
###Import movement attributes and results of CAMA analysis and transform them into spatial objects

activities_attributes_sarah <-read_csv("activities_sarah_with_attributes_classified_new.csv")
activities_attributes_sarah$ts_POSIXct <-as.POSIXct(activities_attributes_sarah$ts_POSIXct)

activities_attributes_sarah_sf <-st_as_sf(activities_attributes_sarah,
                                          coords = c("lon","lat"),
                                          crs = 4326 , remove = FALSE) 

activities_attributes_sarah_sf <- st_transform(activities_attributes_sarah_sf,
                                               crs = 2056)

activities_attributes_sarah_sf <-activities_attributes_sarah_sf[,c(1:27,31:33)]##remove non-required columns

###Join data from CAMA and walking- attributes -----
activities_sar_test<-st_join(activities_attributes_sarah_sf,sar_test_classification)

activities_sar_test$Attribute_factor.x <-as.factor(activities_sar_test$Attribute_factor.x)

pred_model_sar_test<- model_sas_train |>  
  predict(activities_sar_test, type = "class")

activities_sar_test$pred <-pred_model_sar_test

### Confusion matrix for Sarah's test- data  ---
confus_sar_test <-conf_mat(data = activities_sar_test, 
                           truth = Attribute_factor.x, estimate = pred)

autoplot(confus_sar_test, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

### Compute model accuracy rate on Sarah's test data----
confusionMatrix(activities_sar_test$Attribute_factor.x, activities_sar_test$pred)
