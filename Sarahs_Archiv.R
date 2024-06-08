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