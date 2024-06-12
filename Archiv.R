# Archiv
# act1 <- htmlTreeParse(file = "activities/11034695746.gpx", useInternalNodes = TRUE)
# act2 <- htmlTreeParse(file = "activities/11034777563.gpx", useInternalNodes = TRUE)
# act3 <- htmlTreeParse(file = "activities/11040701159.gpx", useInternalNodes = TRUE)
# act4 <- htmlTreeParse(file = "activities/11041610041.gpx", useInternalNodes = TRUE)
# act5 <- htmlTreeParse(file = "activities/11090207888.gpx", useInternalNodes = TRUE)
# act6 <- htmlTreeParse(file = "activities/11116939487.gpx", useInternalNodes = TRUE)
# act7 <- htmlTreeParse(file = "activities/11126064758.gpx", useInternalNodes = TRUE)
# act8 <- htmlTreeParse(file = "activities/11134866767.gpx", useInternalNodes = TRUE)
# act9 <- htmlTreeParse(file = "activities/11163006442.gpx", useInternalNodes = TRUE)
# act10 <- htmlTreeParse(file = "activities/11163561276.gpx", useInternalNodes = TRUE)
# act11 <- htmlTreeParse(file = "activities/11180057399.gpx", useInternalNodes = TRUE)
# act12 <- htmlTreeParse(file = "activities/11238978781.gpx", useInternalNodes = TRUE)

# Calculate steplength and speed for 10 sec ####
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    steplenght10 = distance_by_element(lag(geometry, n = 10), geometry)
  )

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    timelag_sec10 = difftime_secs(lead(DateTime, n = 10), DateTime)
  )
# calculate speed between locations
activities_classified_sf$speed <- activities_classified_sf$steplenght/activities_classified_sf$timelag_sec
activities_classified_sf$speed <- activities_classified_sf$steplenght10/activities_classified_sf$timelag_sec10

activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    speed = steplenght/timelag_sec
  )
activities_classified_sf <- activities_classified_sf |> 
  group_by(ID) |>
  mutate(
    speed10 = steplenght10/timelag_sec10
  )

# decision tree for attribute based classification ####
## 1. based on mean speed ####
test_classification <- activities_classified_sf |> 
  group_by(segment_id) |> 
  summarize(mean_acceleration = mean(acceleration, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE),
            lenght = length(segment_id)
  ) |> 
  ungroup()

test_classification <- test_classification |> 
  mutate(shopping = if_else(mean_speed > 0.9 | is.na(mean_speed) & mean_acceleration > 0.1 & mean_step > 2.8 ,  0 ,  1 ),
         recreation = if_else(shopping %in% c( 0 ) & mean_step < 2.6 & mean_acceleration < 0.012 & mean_speed < 2.6 ,  1 ,  0 ),
         travel = if_else(shopping %in% c( 0 ) & recreation %in% c( 0 ) & !is.na(mean_step),  1 ,  0 ))

test_classification <- test_classification |> 
  mutate(activity = if_else(shopping == 1, "shopping", 
                            if_else(recreation == 1, "recreation", "travel"),"NA"))
test_classification <- na.omit(test_classification)
test_classification <- test_classification |> 
  mutate(activity_factor = as.factor(activity)) 


test_activities_classified <- st_join(activities_classified_sf, test_classification, left = TRUE)


# test_classification <- test_classification |> 
#  mutate(shopping = if_else(mean_speed > 2.7 & mean_acceleration < 0.001 & mean_step < 2,  1 ,  0 ),
#        recreation = if_else(shopping %in% c( 0 ) & mean_step < 2.7 & mean_acceleration < 0.001 & mean_speed < 2.7 ,  1 ,  0 ),
#        travel = if_else(shopping %in% c( 0 ) & recreation %in% c( 0 ),  1 ,  0 ))