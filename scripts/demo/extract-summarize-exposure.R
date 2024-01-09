#Measure exposure in the simulated trajectories
#Filename: extract-summarize-exposure.R
#Continued from
#source(here("scripts", "demo","sim-trajectories.R"))
#Revised Jan 9, 2024 to simplify weighted mean calculations

library(here)
library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(raster)
library(tidyterra)
library(Hmisc)#for weighted variance


#Assess exposure in the buffers created in 
source(here("scripts", "demo","create-buffers-around-traj.R"))


#traj_demo_buff %>% mapview(zcol="study_id")

#Then extract the lst data within the buffer


# Extract exposure data------
## Test on one study ID----
#Okay, now find the time-weighted average exposure in the buffer
#Use lst_20230618 for the exposure layer, created here
source(here("scripts", "demo","read-lst-demo.R"))
lst_20230618#a nice complete image
lst_20230618 %>% plot()

#Use terra's extract function to summarize the exposure values within the buffer
#Demo first and then write a function
#Extract LST data in this buffer
#Takes about 5 minutes on my comp per ID

study_id_1=traj_demo_buff_1 %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(study_id) %>% 
  distinct() %>% 
  pull()

#test process on study id 1
traj_extract_e_test=lst_20230618 %>% 
  terra::extract(
    traj_demo_buff_1, 
    na.rm=TRUE,#added Oct 21, 2022: important as missings were being coded as zero, I think
    weights = TRUE#print the proportion of the pixel covered by the polygon
  ) %>% 
  as_tibble() %>% #uses tidyterra to convert to tibble
  rename(
    line_id = ID,#rename this to line id for that person
    e=2#second column is the exposure.
  ) 

#examine output
traj_extract_e_test %>% 
  print(n=50)

#Revising this January 9, 2023 to simplify
#using the weighted.mean() function

#every row corresponds to a raster cell. Line id repeats because a given line id
#might cover more than one raster cell.
#The weight is the proportion of the pixel covered by the buffer for that line ID

#summarize by line ID now

lookup_duration_study_id_line_id_demo

traj_extract_by_line_id_test=traj_extract_e_test %>% 
  group_by(line_id) %>%
  summarise(
    #Jan 9, 2024 use R's built-in weighted.mean() function
    #instead of calculating weighted average manually
    e=weighted.mean(
      x=e,
      w=weight,
      na.rm=T),
    #These weights are based on the areal overlap, not time
    sum_of_weights_area=sum(weight,na.rm=T),
    n_pixel = n() # number of observations corresponds to number of pixels per line segment
  ) %>% 
  ungroup() %>% 
  mutate(study_id=study_id_1) %>% #add this so it can be linked
  #now link in the time weight
  left_join(lookup_duration_study_id_line_id_demo,by=c("study_id","line_id")) %>% 
  mutate(
    area_time_weight=sum_of_weights_area*t_elapsed_to_next_m,
    e_name = "lst", #this could be dynamic in the function. the name of the exposure
    #converting land-surface temperature from kelvin to celsius
    e = case_when(
      e_name== "lst" ~ e-273.15,
      TRUE ~ e
    ))
    


#Done with the test. What's the distribution of the exposure for this study id?
summary(traj_extract_by_line_id_test$e)
traj_extract_by_line_id_test %>% 
  ggplot(aes(e))+
  geom_histogram()

## Write function to extract from all study ids----
traj_extract_e_demo=function(study_id_val){
  #filter the trajectory buffer to the corresponding study id
  traj_demo_buff_id = traj_demo_buff %>% #excluding high speed
    filter(study_id==study_id_val)
  
  traj_extract_e_obj=lst_20230618 %>% 
    terra::extract(
      traj_demo_buff_id, #object just created above
      na.rm=TRUE, 
      weights = TRUE 
    ) %>% 
    as_tibble() %>%  
    rename(
      line_id = ID,#rename this to line id for that person
      e=2#second column is the exposure.
    ) 
  
  traj_extract_e_obj_by_line_id=traj_extract_e_obj %>% 
    group_by(line_id) %>%
      summarise(
        #Jan 9, 2024 use R's built-in weighted.mean() function
        #instead of calculating weighted average manually
        e=weighted.mean(
          x=e,
          w=weight,
          na.rm=T),
        #These weights are based on the areal overlap, not time
        sum_of_weights_area=sum(weight,na.rm=T),
        n_pixel = n() # number of observations corresponds to number of pixels per line segment
      ) %>% 
      ungroup() %>% 
      mutate(study_id=study_id_val) %>% #add this so it can be linked
      #now link in the time weight
      left_join(lookup_duration_study_id_line_id_demo,by=c("study_id","line_id")) %>% 
      mutate(
        #calculate a weight that considers both area overlap and time
        area_time_weight=sum_of_weights_area*t_elapsed_to_next_m,
        e_name = "lst", #this could be dynamic in the function. the name of the exposure
        #converting land-surface temperature from kelvin to celsius
        e = case_when(
          e_name== "lst" ~ e-273.15,
          TRUE ~ e
        ))
  
}


#Caution: this step takes ~5-15 mins
traj_extract_wrangle_test_fun=traj_extract_e_demo(1)

#check it out for one study ID
traj_extract_wrangle_test_fun
summary(traj_extract_wrangle_test_fun$e)

## summarize for this one study ID----
traj_extract_wrangle_test_fun %>% 
    filter(is.na(area_time_weight)==F) %>% 
    group_by(study_id) %>% 
    summarise(
      e_m=weighted.mean(
        x=e,
        w=area_time_weight,
        na.rm=T),
      #generate a weighted standard deviation using Hmisc package
      #https://stackoverflow.com/questions/10049402
      e_var=Hmisc::wtd.var(x=e,weights=area_time_weight,normwt = TRUE),
      e_min=min(e, na.rm=T),
      e_max=max(e, na.rm=T),
      e_med=median(e, na.rm=T),
      n_line = n(), #keep track of how many line segments
    ) %>% 
    ungroup() %>% 
    mutate(e_sd=sqrt(e_var))#SD is square root of variance


### Run function over all study IDs-----
#Caution: this step takes some time - about 10 mins on my Macbook Pro
traj_extract_df_demo = study_id_list %>% 
  map_dfr(traj_extract_e_demo)

#checks
traj_extract_df_demo
n_distinct(traj_extract_df_demo$study_id)
n_distinct(traj_extract_df_demo$line_id)
names(traj_extract_df_demo)
summary(traj_extract_df_demo$t_elapsed_to_next)
summary(traj_extract_df_demo$t_elapsed_to_next_m)

## Summarize by study ID---------
traj_summary_s_id_demo=traj_extract_df_demo %>% 
  filter(is.na(area_time_weight)==F) %>% 
  group_by(study_id) %>% 
  summarise(
    e_m=weighted.mean(
      x=e,
      w=area_time_weight,
      na.rm=T),
    #generate a weighted standard deviation using Hmisc package
    #https://stackoverflow.com/questions/10049402
    e_var=Hmisc::wtd.var(x=e,weights=area_time_weight,normwt = TRUE),
    e_min=min(e,na.rm=T),
    e_max=max(e,na.rm=T),
    e_med=median(e,na.rm=T),
    n_line = n(), #keep track of how many line segments
  ) %>% 
  ungroup() %>% 
  mutate(e_sd=sqrt(e_var))

traj_summary_s_id_demo
