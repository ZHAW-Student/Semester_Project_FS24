#friedhof für code


##Version Vektor 25----
st_layers("Vektor_25_Waedi/226/SMV25_CHLV95LN02.gpkg")#see all contents

bodenbedeckung <- read_sf("Vektor_25_Waedi/226/SMV25_CHLV95LN02.gpkg","T65_DKM25_BODENBEDECKUNG")

nutzungsareal <-read_sf("Vektor_25_Waedi/226/SMV25_CHLV95LN02.gpkg","T64_DKM25_NUTZUNGSAREAL")

tmap_mode("view")
tm_shape(bodenbedeckung)+
  tm_polygons(col="OBJEKTART")+
  tm_shape(nutzungsareal)+
  tm_polygons(col="OBJEKTART")



### make hulls around areas where data was generated----
waedi_lat <- c(1231856.6,1232486.7,1230502.2,1229749.7)
waedi_lon <- c(2691660.9,2693090.5,2695714.7,2693701.9)

waedi_data <-data.frame(lat=waedi_lat,lon=waedi_lon)
waedi_data$ort <-"Waedi"

klafi_lat <-c(1273987.7, 1274015.2, 1271776.6, 1271973.8)
klafi_lon <-c(2692531.2, 2695650.7,	2694948.8, 2692288.1)

klafi_data <-data.frame(lat=klafi_lat,lon=klafi_lon)
klafi_data$ort <-"Klafi"

all_data <-rbind(waedi_data,klafi_data)


borders <- all_data |> 
  st_as_sf(coords = c("lon", "lat"),crs=2056) |> 
  group_by(ort) |> 
  summarize(geometry = st_union(geometry)) |> 
  st_convex_hull()
#source:https://stackoverflow.com/questions/69638192/draw-polygons-around-coordinates-in-r


tmap_mode("view")
tm_shape(borders)+
  tm_polygons(col="ort")


## read Saskia's data ----
df_to_sf <- function(df){
  st_as_sf(df, coords = c("lat", "lon"), crs = 4326 , remove = FALSE)}
activities_classified <- read_delim("test_activities_attributiert.csv", ",")
activities_classified_sf <- df_to_sf(activities_classified)

#create index
activities_classified_sf$id_cama<- 1:nrow(activities_classified_sf)

#alternative with buffer
bodenbuf <-st_buffer(bodenbedeckung_clip, dist=10)
activities_classified_sf$boden <-st_within(activities_classified_sf,bodenbuf, perpared = TRUE)  




bodenbedeckung_test[1]

overlap_test <-function(output_list){
  list_df<-as.data.frame(do.call(rbind, output_list))
  list_df$test<-TRUE
  list_df$id_cama <-rownames(list_df)
  list_df <- subset(list_df, select = c(test, id_cama))
  list_df$id_cama <-as.numeric(str_sub(list_df$id_cama,4))}



bodenbedeckung_test <- st_is_within_distance(x = activities_classified_sf, y = bodenbedeckung_clip, dist = units::set_units(20, "m"), remove_self = FALSE)


##messy attempts to get the code to work

a<-list_df<-as.data.frame(do.call(rbind, bodenbedeckung_test,optional=TRUE)

o<-do.call(rbind.data.frame, Map(cbind, V1 = bodenbedeckung_test))
k<-bodenbedeckung_test |> map_df(as_tibble)


tibble::enframe(bodenbedeckung_test, name = 'ID', value = 'V1') %>% tidyr::unnest(V1)

g<-data.frame(t(data.frame(t(sapply(bodenbedeckung_test,c)))))
colnames(g)[1]="test"
g[g=="integer(0)"]<-NA
g<-as.numeric(unlist(g))

list_df<-as.data.frame(do.call(cbind, bodenbedeckung_test))

list_df<-as.data.frame(do.call(rbind, bodenbedeckung_test) )
str(bodenbedeckung_test)
e<-data.frame(matrix(unlist(bodenbedeckung_test), nrow=13636, byrow=T))
r<-do.call(rbind.data.frame, bodenbedeckung_test)

t<- data.frame(matrix(unlist(bodenbedeckung_test), nrow=length(bodenbedeckung_test), byrow=TRUE))

s<- data.frame(matrix(unlist(bodenbedeckung_test), nrow=length(bodenbedeckung_test), byrow=TRUE))

w<-data.frame(lapply(bodenbedeckung_test, \(x) ifelse(is.null(x), NA, x)), a = "a")



q<-do.call(rbind.data.frame, lapply(bodenbedeckung_test, function(i)replace(i, length(i) == NULL, NA)))

tests<-overlap_test(bodenbedeckung_test)

activities_classified_sf<-left_join(activities_classified_sf,tests,by="id_cama")
activities_classified_sf<-activities_classified_sf |> rename(bodenbedeckung = test)

bodenbedeckung_test <- st_is_within_distance(x = activities_classified_sf, y = bodenbedeckung_clip, dist = units::set_units(20, "m"), remove_self = FALSE)

#alternative with buffer



activities_with_objects$recreation <- 
  ifelse(activities_with_objects$recreation_b == "", TRUE, 
         ifelse(activities_with_objects$recreation_n == "" , TRUE,
                ifelse(activities_with_objects$recreation_s  == "", TRUE, FALSE)))

activities_with_objects$recreation_ <- if_else(activities_with_objects$obj_strassen == ""| activities_with_objects$obj_nutzung == "" |
                                                 activities_with_objects$obj_boden == ""|
                                                 activities_with_objects$obj_strassen == ""&
                                                 activities_with_objects$obj_nutzung == ""|
                                                 activities_with_objects$obj_nutzung == "" &
                                                 activities_with_objects$obj_boden == ""| 
                                                 activities_with_objects$obj_strassen == ""&
                                                 activities_with_objects$obj_boden == ""|
                                                 activities_with_objects$obj_strassen == ""&
                                                 activities_with_objects$obj_boden == ""&
                                                 activities_with_objects$obj_boden == "",
                                               0, 1)


activities_with_objects$recreationö <-is.na(activities_with_objects$obj_boden & activities_with_objects$obj_nutzung & activities_with_objects$obj_strassen)

if_else(activities_with_objects$obj_nutzung == "" , TRUE, if_else(activities_with_objects$obj_boden == "" , TRUE, FALSE) ))


activities_with_objects$recreation_ <- if_else(activities_with_objects$obj_strassen == "" , TRUE, 
                                               if_else(activities_with_objects$obj_nutzung == "" , TRUE, if_else(activities_with_objects$obj_boden == "" , TRUE, FALSE) ))



activities_with_objects$Den<-ifelse (is.na(activities_with_objects$obj_boden) | is.na(activities_with_objects$obj_nutzung) | is.na(activities_with_objects$obj_strassen), "yes", "no")




activities_with_objects$recreation<-if_else( any ( NA %in% c(activities_with_objects$recreation_b,activities_with_objects$recreation_n,activities_with_objects$recreation_s)),TRUE, FALSE)



a<-left_join(activities_with_objects, activities_attributes, by=c("lat","lon","elevation","ID"))

class(activities_with_objects)

s<-merge(activities_attributes_sf, activities_with_objects,by = c("lat", "lon"))