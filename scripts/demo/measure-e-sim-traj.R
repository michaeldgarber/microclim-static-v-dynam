#Measure exposure in the simulated trajectories
#Continued from
#source(here("scripts", "demo","sim-trajectories.R"))

library(here)
library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(raster)
library(tidyterra)

traj_demo
#Assess exposure in these trajectories
#Use the existing data you have as a placeholder. Then load the data where
#you know exactly how it was loaded.

#First create buffers around the trajectories

#Then extract the lst data within the buffer

# Create buffers around trajectories----
#Could simply do this, but in my experience it's kind of slow
st_crs(traj_demo) #check coordinate system. meters.
#no_fun for without a function
traj_demo_buff_no_fun=traj_demo %>% 
  st_buffer(200)


#so use a function instead
traj_demo_buff_fun = function(study_id_val){
  traj_buff_200m_out = traj_demo %>%
    dplyr::select(study_id, line_id, geometry) %>% 
    filter(study_id==study_id_val) %>% 
    st_buffer(200)
}

#This still takes a while. Test one of them
traj_demo_buff_1=traj_demo_buff_fun(1)
traj_demo_buff_1 %>% 
  slice(1:100) %>% 
  mapview()#worked

#study id list created previously:
study_id_list = gps_traces %>% 
  group_by(study_id) %>% 
  summarise(n=n()) %>% 
  pull(study_id)

#Run the function over all study IDs to create buffers around everyone's trajectory
traj_demo_buff = study_id_list %>% 
  map_dfr(traj_demo_buff_fun)

#
traj_demo_buff

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
#every row corresponds to a raster cell. Line id repeats because a given line id
#might cover more than one raster cell.
#The weight is the proportion of the pixel covered by the buffer for that line ID

#now a new object in which we calculate new weights among non-missing exposure values.
#Perhaps not an issue with this demo, but sometimes the raster files have missings,
#so good to include this
traj_extract_e_weights_by_line_id_no_e_miss_test  = traj_extract_e_test  %>% 
  filter(is.na(e)==FALSE) %>% 
  group_by(line_id) %>% 
  summarise(sum_of_weights_no_miss=sum(weight,na.rm=TRUE)) %>% #sum of weights
  ungroup()

traj_extract_e_weights_by_line_id_no_e_miss_test %>% 
  print(n=200)

#to weight the elapsed time, what proportion of the line segment
#is missing?
traj_extract_prop_non_miss_test=traj_extract_e_test %>% 
  group_by(line_id) %>% 
  summarise(sum_weights_original=sum(weight,na.rm=TRUE)) %>% #sum of weights
  ungroup() %>% 
  left_join(traj_extract_e_weights_by_line_id_no_e_miss_test, by = "line_id") %>% 
  mutate(prop_non_miss=sum_of_weights_no_miss/sum_weights_original)

#proportion non-missing is all 1, which is good, but may not always be true
summary(traj_extract_prop_non_miss_test$prop_non_miss)

#Continue

#and now we can continue
traj_extract_wrangle_test = traj_extract_e_test %>% 
  #we are within study id so this works
  left_join(traj_extract_e_weights_by_line_id_no_e_miss_test, by ="line_id") %>% 
  #sum over line_id and calculate a weighted average exp for each line id,
  #weighted by the proportion the pixel is covered by the polygon
  mutate(
    #weight new is the weight divided by the sum of the non-missing weights
    #for that pixel
    weight_new = weight/sum_of_weights_no_miss,
    e_name = "lst", #this could be dynamic in the function. the name of the exposure
    #converting land-surface temperature from kelvin to celsius
    e = case_when(
      e_name== "lst" ~ e-273.15,
      TRUE ~ e
    ),
    e_miss = case_when(
      is.na(e)~1,
      TRUE ~0),
    #only calculate this if not missing
    e_times_weight_int = case_when( #int for intermediate calc.
      e_miss<1~e*weight_new,
      TRUE ~   NA_real_
    )
  ) %>% 
  group_by(line_id) %>% #summarizing over line segment
  summarise(
    e_weighted_sum = sum(e_times_weight_int, na.rm = TRUE),#now true is okay
    sum_of_weights = sum(weight_new, na.rm=TRUE), #will sum to 1 if no missing
    n_pixel_e_miss = sum(e_miss),#keep track of these too
    n_pixel = n() # number of observations corresponds to number of pixels per line segment
  )%>% 
  ungroup() %>% 
  mutate(
    e = e_weighted_sum/sum_of_weights,#weighted sum over sum of weights
    e_miss = case_when(      #indicator for missing, as above
      is.na(e)~1,
      TRUE ~0),
    study_id=1,#add study id. use test study id
    #proportion of the pixels with missing data
    n_pixel_prop_e_miss = n_pixel_e_miss/n_pixel
  ) %>% 
  #link in elapsed time for each line.
  left_join(
    lookup_duration_study_id_line_id_demo,
    by = c("study_id", "line_id")
  )    %>% 
  left_join(traj_extract_prop_non_miss_test, by = "line_id") %>% 
  #stop function here as a test
  #and now group by person and weight by time 
  #var name: exposure multiplied by elapsed time
  mutate(
    #Weight the time by the proportion non-missing for line id above
    #as if we're only including some of the exposure value from that line
    #segment, we should also only include some of its time
    t_elapsed_to_next_m_weight_non_miss=prop_non_miss*t_elapsed_to_next_m,
    #should return missings appropriately
    
    #Note for the demo, the elapsed time is always a minute but perhaps
    #it won't always be
    e_times_elapsed_t = e*t_elapsed_to_next_m_weight_non_miss,
    
    #this needs to be missing if exposure is missing for the weighted average
    t_elapsed_to_next_miss = case_when(
      is.na(e) ~ NA_real_,
      TRUE ~t_elapsed_to_next_m_weight_non_miss)  
    ) %>% 
  #put variables in a better order
  dplyr::select(starts_with("study_id"), starts_with("line_id"), 
                starts_with("e"),
                contains("t_elapsed") , everything())



#Done with the test. What's the distribution of the exposure for this study id?
summary(traj_extract_wrangle_test$e)
traj_extract_wrangle_test %>% 
  ggplot(aes(e))+
  geom_histogram()

## Write function to extract from all study ids----
traj_extract_e_demo=function(study_id_val){
  #filter the trajectory buffer to the corresponding study id
  traj_demo_buff_id = traj_demo_buff %>% #excluding high speed
    filter(study_id==study_id_val)
  
  #test process on study id 1
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
  
  traj_extract_e_weights_by_line_id_no_e_miss_obj  = traj_extract_e_obj  %>% 
    filter(is.na(e)==FALSE) %>% 
    group_by(line_id) %>% 
    summarise(sum_of_weights_no_miss=sum(weight,na.rm=TRUE)) %>% #sum of weights
    ungroup()
  
  
  #to weight the elapsed time, what proportion of the line segment
  #is missing?
  traj_extract_prop_non_miss_obj=traj_extract_e_obj %>% 
    group_by(line_id) %>% 
    summarise(sum_weights_original=sum(weight,na.rm=TRUE)) %>% #sum of weights
    ungroup() %>% 
    left_join(traj_extract_e_weights_by_line_id_no_e_miss_obj, by = "line_id") %>% 
    mutate(prop_non_miss=sum_of_weights_no_miss/sum_weights_original)
  
  #and now we can continue
  traj_extract_wrangle_test = traj_extract_e_test %>% 
    #we are within study id so this works
    left_join(traj_extract_e_weights_by_line_id_no_e_miss_test, by ="line_id") %>% 
    #sum over line_id and calculate a weighted average exp for each line id,
    #weighted by the proportion the pixel is covered by the polygon
    mutate(
      #weight new is the weight divided by the sum of the non-missing weights
      #for that pixel
      weight_new = weight/sum_of_weights_no_miss,
      e_name = "lst", #this could be dynamic in the function. the name of the exposure
      #converting land-surface temperature from kelvin to celsius
      e = case_when(
        e_name== "lst" ~ e-273.15,
        TRUE ~ e
      ),
      e_miss = case_when(
        is.na(e)~1,
        TRUE ~0),
      #only calculate this if not missing
      e_times_weight_int = case_when( #int for intermediate calc.
        e_miss<1~e*weight_new,
        TRUE ~   NA_real_
      )
    ) %>% 
    group_by(line_id) %>% #summarizing over line segment
    summarise(
      #mar 24, 2023: now this is okay even if part of the line segment
      e_weighted_sum = sum(e_times_weight_int, na.rm = TRUE),#now true is okay
      sum_of_weights = sum(weight_new, na.rm=TRUE), #will sum to 1 if no missing
      n_pixel_e_miss = sum(e_miss),#keep track of these too
      n_pixel = n() # number of observations corresponds to number of pixels per line segment
    )%>% 
    ungroup() %>% 
    mutate(
      e = e_weighted_sum/sum_of_weights,#weighted sum over sum of weights
      e_miss = case_when(      #indicator for missing, as above
        is.na(e)~1,
        TRUE ~0),
      study_id=study_id_val,#add study id based on function
      n_pixel_prop_e_miss = n_pixel_e_miss/n_pixel
    ) %>% 
    left_join(
      lookup_duration_study_id_line_id_demo,
      by = c("study_id", "line_id")
      )    %>% 
    left_join(traj_extract_prop_non_miss_test, by = "line_id") %>% 
    mutate(
      t_elapsed_to_next_m_weight_non_miss=prop_non_miss*t_elapsed_to_next_m,
      e_times_elapsed_t = e*t_elapsed_to_next_m_weight_non_miss,
      
      #this needs to be missing if exposure is missing for the weighted average
      t_elapsed_to_next_miss = case_when(
        is.na(e) ~ NA_real_,
        TRUE ~t_elapsed_to_next_m_weight_non_miss)  
    ) %>% 
    #put variables in a better order
    dplyr::select(starts_with("study_id"), starts_with("line_id"), 
                  starts_with("e"),
                  contains("t_elapsed") , everything())
}

traj_extract_wrangle_test_fun=traj_extract_e_demo(1)

#check it out for one study ID
traj_extract_wrangle_test_fun
summary(traj_extract_wrangle_test_fun$e)

### Run function over all study IDs-----
traj_extract_df_demo = study_id_list %>% 
  map_dfr(traj_extract_e_demo)

## Summarize over study IDs
