#The purpose of this is to demonstrate the method using some simulated data.

#First, simulate GPS trajectories in San Diego County.
#We don't snap to a road, so they can be anywhere.

#To have something to sample from, get the geometry for San Diego County
library(tidycensus)
library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
# sd_county_geo=tidycensus::get_acs(
#     geography = "county", 
#     year=2020,  
#     cache_table = TRUE,
#     state = "CA",
#     county =  c("073"), 
#     keep_geo_vars = FALSE, 
#     output = "wide",  
#     survey = "acs5", 
#     variables = "B01001_001",
#     geometry = TRUE   
#     ) %>% 
#   st_transform(4326)
# 
# sd_county_geo %>% mapview()
# 
# sample_point=sd_county_geo %>% 
#   st_sample(size=c(5,5)) %>% 
#   st_as_sf()

#This works, but perhaps we can pick 3 spots within San Diego and go from there
library(ggmap)
pt1_sd_zoo = as_tibble("San Diego Zoo") %>% 
  rename(address = value) %>% 
  mutate_geocode(address,force=T) %>% 
  mutate(study_id=1)

pt2_ucsd=as_tibble("University of California, San Diego") %>% 
  rename(address = value) %>% 
  mutate_geocode(address,force=T) %>% 
  mutate(study_id=2)

pt3_presidio_park=as_tibble("Presidio Park, San Diego") %>% 
  rename(address = value) %>% 
  mutate_geocode(address,force=T) %>% 
  mutate(study_id=3)


#now generate tracks from these points
#how many minutes in a week?

# Generate GPS tracks from points of origin------
## Create one GPS trace without a function------
n_days=3
n_min=60*24*n_days
#Comment: a week makes the data too big. Use 3 days instead
gps_trace_1=pt1_sd_zoo %>% 
  uncount(n_min) %>% 
  rename(lon_origin=lon,
         lat_origin=lat) %>% 
  mutate(
    #every day, I want them to leave home (the origin)
    #again, so I need to also calculate
    #the nth minute in a given day
    minute_in_data=row_number(),
    line_id = row_number(),#same as minute_in_data in this case
    day_in_data=round(minute_in_data/(60*24),digits=0),
    
    #to make the data more realistic, let's add a date as well
    #Say the data begins at 8 am on June 18, 2023, as
    #that is the day I'm using for demo LST data
    date_time_index=ymd_h(2023061808), #everybody starts on the same day
    date_time=date_time_index+minutes(minute_in_data),
    
    #how much time has elapsed between this point and the next one.
    #It is of course 1 here but it might not always be
    t_elapsed_to_next = lead(date_time)-date_time,
    t_elapsed_to_next_units = units(t_elapsed_to_next),#all minutes.
    t_elapsed_to_next_m = as.numeric(t_elapsed_to_next)
  ) %>% 
  
  group_by(day_in_data) %>% 
  mutate(minute_in_day=row_number()) %>%   #the nth minute in the day
  ungroup() %>% 
  #Now calculate distance traveled every minute
  mutate(
    #distance traveled east and north per minute.
    #start with km and then convert to degrees (roughly)
    #Let's go with uniform distribution
    #Bias towards going northeast to avoid going into the ocean
    
    dist_trav_e_km_per_min=runif(n=n(),min=-.01,max=.02),
    dist_trav_e_deg_per_min=dist_trav_e_km_per_min/111,
    
    dist_trav_n_km_per_min=runif(n=n(),min=-.01,max=.02),
    dist_trav_n_deg_per_min=dist_trav_n_km_per_min/111,
    ) %>% 
  
  #I should use cumulative sum so that the path stays
  #less erratic and moves relative to its previous observation
  #and do this by day so that each day gets its own trajectory
  #for the person
  group_by(day_in_data) %>% 
  mutate(
    dist_trav_e_deg_cum_from_origin=cumsum(dist_trav_n_deg_per_min),
    dist_trav_n_deg_cum_from_origin=cumsum(dist_trav_n_deg_per_min)
  ) %>% 
  ungroup() %>% 
  mutate(
    lon=lon_origin+dist_trav_e_deg_cum_from_origin,
    lat=lat_origin+dist_trav_n_deg_cum_from_origin
  ) %>%   #convert to sf
  st_as_sf(coords = c("lon", "lat"),crs = 4326)

#checks
gps_trace_1 %>% 
  filter(day_in_data==1) %>% 
  mapview(zcol="day_in_data")
gps_trace_1 %>% View()

#okay, a straight line. not realistic
#but good for demo for sample points

## Write a function to create two more GPS traces-----
create_gps_trace=function(df){
  df %>% 
    uncount(n_min) %>% 
    rename(lon_origin=lon,
           lat_origin=lat) %>% 
    mutate(
      minute_in_data=row_number(),
      line_id = row_number(),#same as minute_in_data in this case
      day_in_data=round(minute_in_data/(60*24),digits=0),
      
      #to make the data more realistic, let's add a date as well
      #Say the data begins at 8 am on June 18, 2023, as
      #that is the day I'm using for demo LST data
      date_time_index=ymd_h(2023061808), #everybody starts on the same day
      date_time=date_time_index+minutes(minute_in_data),
      
      #how much time has elapsed between this point and the next one.
      #It is of course 1 here but it might not always be
      t_elapsed_to_next = lead(date_time)-date_time,
      t_elapsed_to_next_units = units(t_elapsed_to_next),#all minutes.
      t_elapsed_to_next_m = as.numeric(t_elapsed_to_next)
      
    ) %>% 
    group_by(day_in_data) %>% 
    mutate(minute_in_day=row_number()) %>%   #the nth minute in the day
    ungroup() %>% 
    #Now calculate distance traveled every minute
    mutate(

      dist_trav_e_km_per_min=runif(n=n(),min=-.01,max=.02),
      dist_trav_e_deg_per_min=dist_trav_e_km_per_min/111,
      
      dist_trav_n_km_per_min=runif(n=n(),min=-.01,max=.02),
      dist_trav_n_deg_per_min=dist_trav_n_km_per_min/111,
      

    ) %>% 
    

    group_by(day_in_data) %>% 
    mutate(
      dist_trav_e_deg_cum_from_origin=cumsum(dist_trav_n_deg_per_min),
      dist_trav_n_deg_cum_from_origin=cumsum(dist_trav_n_deg_per_min)
    ) %>% 
    ungroup() %>% 
    mutate(
      lon=lon_origin+dist_trav_e_deg_cum_from_origin,
      lat=lat_origin+dist_trav_n_deg_cum_from_origin
    ) %>%   #convert to sf
    st_as_sf(coords = c("lon", "lat"),crs = 4326)
}

## Create two more GPS traces------
gps_trace_2=pt2_ucsd %>% 
  create_gps_trace()

gps_trace_2 %>% mapview()

gps_trace_3=pt3_presidio_park %>% 
  create_gps_trace()

gps_trace_3 %>% mapview()


## Combine the GPS traces----
gps_traces=gps_trace_1 %>% 
  bind_rows(
    gps_trace_2,
    gps_trace_3
  )

#gps_traces %>% mapview(zcol="study_id")
nrow(gps_traces)

#Create a look-up for the time elapsed variables for later linking
lookup_duration_study_id_line_id_demo = gps_traces %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(study_id, line_id, starts_with("t_elapsed"))

lookup_duration_study_id_line_id_demo
#Connect the points in time to create trajectories----
#Trajectory meaning line segments connecting each GPS point
## Create one trajectory----
traj_1=gps_trace_1 %>% 
  #for a given observation, define start and end points
  #Because I converted them to sf object, access the geometry data like this
  mutate(
    #In this demo, line_id and minute_in_data will be the same
    #but to be consistent with my languag elsewhere, I'm creating
    #a "line_id"
    line_id = row_number(),#an id for each "line segment"
    
    lon_start=st_coordinates(geometry)[,1],
    lat_start=st_coordinates(geometry)[,2],
    lon_end = lead(lon_start),
    lat_end = lead(lat_start)
  ) %>% 
  ungroup() %>% 
  st_set_geometry(NULL) %>% 
  #exclude the last observation, which has no "lead", and will be missing.
  filter(is.na(lon_end)==FALSE) %>% 
  #remove the _origin coordinates, as it will mess up the pivot_longer
  dplyr::select(-contains("_origin")) %>% 
  #Now make the data long form so that each point has two observations
  pivot_longer(
    #a tidy-select to pick the variables to pivot longer.
    cols = c(contains("lon"), contains("lat")),
    #value goes to "lon/lat", and time goes to "_start/end"
    names_to = c(".value", "time"),
    names_sep = "_"#the separator for the column name
  ) %>% 
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>% 
  group_by(study_id,
           line_id,
           minute_in_data) %>% 
  #see Edzer's answer here:https://github.com/r-spatial/sf/issues/851
  #do_union=FALSE is needed. 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_as_sf() %>% 
  ungroup() %>% 
  mutate(
    line_length_m = as.numeric(st_length(geometry)),
    line_length_mi = line_length_m/1609.34
  )

summary(traj_1$line_length_mi)
traj_1 %>% 
  mapview(zcol="line_length_m")

#A function to create trajectories by study id.
#In theory, this should be possible without a function,
#but I found that it's faster when the data are large.

#The function only has one argument: the study id
names(gps_traces)
trajectories_fun_demo = function(study_id_val){
  trajectories_out = gps_traces %>% 
    filter(study_id==study_id_val) %>% #filter to the study id defined by the argument
    #for a given observation, define start and end points
    #Because I converted them to sf object, access the geometry data like this
    mutate(
      #In this demo, line_id and minute_in_data will be the same
      #but to be consistent with my languag elsewhere, I'm creating
      #a "line_id"
      line_id = row_number(),#an id for each "line segment"
      lon_start=st_coordinates(geometry)[,1],
      lat_start=st_coordinates(geometry)[,2],
      lon_end = lead(lon_start),
      lat_end = lead(lat_start)
    ) %>% 
    ungroup() %>% 
    st_set_geometry(NULL) %>% 
    #exclude the last observation, which has no "lead", and will be missing.
    filter(is.na(lon_end)==FALSE) %>% 
    #remove the _origin coordinates, as it will mess up the pivot_longer
    dplyr::select(-contains("_origin")) %>% 
    #Now make the data long form so that each point has two observations
    pivot_longer(
      #a tidy-select to pick the variables to pivot longer.
      cols = c(contains("lon"), contains("lat")),
      #value goes to "lon/lat", and time goes to "_start/end"
      names_to = c(".value", "time"),
      names_sep = "_"#the separator for the column name
    ) %>% 
    st_as_sf(coords = c("lon", "lat"), crs=4326) %>% 
    group_by(study_id,
             line_id,
             minute_in_data) %>% 
    #see Edzer's answer here:https://github.com/r-spatial/sf/issues/851
    #do_union=FALSE is needed. 
    summarise(do_union=FALSE) %>% 
    st_cast("LINESTRING") %>% 
    st_as_sf() %>% 
    ungroup() %>% 
    mutate(
      line_length_m = as.numeric(st_length(geometry)),
      line_length_mi = line_length_m/1609.34
    )
}

#Test function on study id 2
traj_2_test_fun=trajectories_fun_demo(2)

#worked
traj_2_test_fun %>% mapview()

## Create all trajectories--------
#Run the function over all 3 participants and stack results on top of one another
#use map_dfr() syntax. Need a list of study-ids. 
#In this case, it's simply 1:3, but here's a general way to get that list.
study_id_list = gps_traces %>% 
  group_by(study_id) %>% 
  summarise(n=n()) %>% 
  pull(study_id)

traj_demo = study_id_list %>% 
  map_dfr(trajectories_fun_demo)

#map the trajectories of the three participants
traj_demo %>% mapview(zcol="study_id")

n_distinct(traj_demo$study_id)
n_distinct(traj_demo$line_id)



