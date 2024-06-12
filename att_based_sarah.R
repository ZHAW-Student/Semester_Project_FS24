# import csv with attributes and convert to sf ####
activities_sarah_attributed <- read_delim("activities_sarah_attributiert.csv", ",")

activities_sarah_attributed_sf <- df_to_sf(activities_sarah_attributed)

# change crs of sf
activities_sarah_attributed_sf <- st_transform(activities_sarah_attributed_sf, crs = 2056)

activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  mutate(
    DateTime = as.POSIXct(ts_POSIXct),
    Attribute_factor = as.factor(Attribut)
  )


# Calculate attributes ####
## calculate time lag  ####
difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}

activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  group_by(ID) |>
  mutate(
    timelag_sec = difftime_secs(lead(DateTime,), DateTime)
  )

## calculate distance between locations  ####
distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  group_by(ID) |>
  mutate(
    steplenght = distance_by_element(lag(geometry), geometry)
  )


## Check plausibility of calculated parameters  ####
plot(activities_sarah_attributed_sf$timelag_sec)
plot(activities_sarah_attributed_sf$steplenght)
boxplot(activities_sarah_attributed_sf$timelag_sec)
boxplot(activities_sarah_attributed_sf$steplenght)
summary(activities_sarah_attributed_sf$timelag_sec)
summary(activities_sarah_attributed_sf$steplenght)

# based on this check, i will remove all timelag > 5 and steplenght > 5
outliers_timelag <- filter(activities_sarah_attributed_sf, timelag_sec >= 5)
activities_sarah_attributed_sf <- activities_sarah_attributed_sf[which(activities_sarah_attributed_sf$timelag_sec <= 5),]
outliers_steplenght <- filter(activities_sarah_attributed_sf, steplenght >= 5)
activities_sarah_attributed_sf <- activities_sarah_attributed_sf[which(activities_sarah_attributed_sf$steplenght <= 5),]

# plot again to make sure it's better
plot(activities_sarah_attributed_sf$timelag_sec)
plot(activities_sarah_attributed_sf$steplenght)
boxplot(activities_sarah_attributed_sf$timelag_sec)
boxplot(activities_sarah_attributed_sf$steplenght)


# Segmentation ####
## Preperation  ####
# Specify a temporal window for mean step
activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  group_by(ID) |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), 
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  
  )
# calculate mean step
activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  group_by(ID) |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) 

# calculate mean speed
activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  group_by(ID) |>
  mutate(
    speedMean = stepMean/timelag_sec
  )

# Explore mean step to define threashold 
hist(activities_sarah_attributed_sf$stepMean)
boxplot(activities_sarah_attributed_sf$stepMean)
summary(activities_sarah_attributed_sf$stepMean)

# apply threshold
activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  group_by(ID) |>
  mutate(static = stepMean <  1.5)

# give segments an ID
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  group_by(ID) |>
  mutate(segment_id = rle_id(static)) |> 
  ungroup()

# Acceleration ####
acceleration <- function(s1, s2, t){
  as.numeric((s2-s1)/(t))
}


activities_sarah_attributed_sf <- activities_sarah_attributed_sf |> 
  mutate(acceleration = 
           acceleration(lag(speedMean), speedMean, timelag_sec))

summary_sarah <- activities_sarah_attributed_sf |> 
  group_by(ID) |> 
  group_by(Attribute_factor) |> 
  summarize(stops = sum(static, na.rm = TRUE), 
            not_stops = sum(!static, na.rm = TRUE),
            stop_ratio = not_stops/stops,
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE),
            mean_acceleration = mean(acceleration, na.rm = TRUE),
            mean_lenght = length(segment_id)
  ) |> 
  ungroup()

activities_sarah_for_classification <- activities_sarah_attributed_sf |> 
  mutate(combi_ID = paste(ID, segment_id, sep = "_"))


# decision tree for attribute based classification ####
## 1. based on mean speed ####
sarah_classification <- activities_sarah_for_classification |> 
  group_by(combi_ID) |> 
  summarize(mean_acceleration = mean(acceleration, na.rm = TRUE),
            mean_speed = mean(speedMean, na.rm =TRUE),
            mean_step = mean(stepMean, na.rm = TRUE),
            lenght = length(segment_id)
  ) |> 
  ungroup()

sarah_classification <- sarah_classification |> 
  mutate(travel = if_else(mean_speed > 1.7 & mean_speed < 4
                          & mean_step > 1.7 & mean_step < 4
                          & mean_acceleration < 0.003,  1 ,  0 ),
         recreation = if_else(travel %in% c( 0 ) 
                              & mean_speed > 1.1  & mean_speed < 1.7
                              & mean_step > 1.2 & mean_step < 1.7
                              & mean_acceleration < 0.003,  1 ,  0 ),
         shopping = if_else(travel %in% c( 0 ) 
                            & recreation %in% c( 0 )
                            & mean_speed > 5 |  mean_speed < 1.1
                            & mean_step > 5 | mean_step < 1.2
                            & mean_acceleration > 0.01
                            ,  1 ,  0 ))


sarah_classification <- sarah_classification |> 
  mutate(activity = if_else(shopping == 1, "shopping", 
                            if_else(recreation == 1, "recreation", "travel"),"NA"))
sarah_classification <- na.omit(sarah_classification)
sarah_classification <- sarah_classification |> 
  mutate(activity_factor = as.factor(activity)) 


sarah_activities_classified <- st_join(activities_sarah_attributed_sf, sarah_classification, left = TRUE)

# Test classification ####
confus <-conf_mat(data = sarah_activities_classified, truth = Attribute_factor, estimate = activity_factor)

autoplot(confus, type="heatmap")+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
  theme(legend.position = "right")+
  labs(fill="frequency")

confusionMatrix(sarah_activities_classified$Attribute_factor, sarah_activities_classified$activity_factor)
