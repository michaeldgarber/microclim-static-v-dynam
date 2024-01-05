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
#Assess exposure in the buffers created in 
#source(here("scripts", "demo","create-buffers-around-traj.R"))


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
  traj_extract_wrangle_obj = traj_extract_e_obj %>% 
    #we are within study id so this works
    left_join(traj_extract_e_weights_by_line_id_no_e_miss_obj, by ="line_id") %>% 
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
    left_join(traj_extract_prop_non_miss_obj, by = "line_id") %>% 
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

#checks
traj_extract_df_demo
n_distinct(traj_extract_df_demo$study_id)
n_distinct(traj_extract_df_demo$line_id)
names(traj_extract_df_demo)
summary(traj_extract_df_demo$t_elapsed_to_next)
summary(traj_extract_df_demo$t_elapsed_to_next_m)

## Summarize by study ID---------
traj_summary_s_id_demo=traj_extract_df_demo %>% 
  group_by(study_id) %>% 
  summarise(
    #calculate max, min, sd in case needed.
    #These won't be weighted by elapsed t the same way, 
    #but still informative
    e_sd = sd(e, na.rm=TRUE),
    e_min = min(e, na.rm=TRUE),
    #max of line segments, which is weighted avg of constituent pixels
    e_max = max(e, na.rm=TRUE), 
    e_med = median(e, na.rm=TRUE),
    #prepare (intermediate calcs) for weighted mean, weighting by 
    #elapsed time.
    #Here, unlike above, I'm allowing missings to occur within a 
    #given person.
    #This will take the average (or sum) of all the non-missing values for 
    #that person
    #The alternative way would be to only calculate the sum if the 
    #entire person's data
    #was non-missing, but I think that's too strict.
    
    e_weighted_sum = sum(e_times_elapsed_t, na.rm = TRUE),
    #just to have this for reference, the simple version of
    t_elapsed_to_next_m = sum(t_elapsed_to_next_m, na.rm=TRUE),
    t_elapsed_to_next_m_weight_non_miss=sum(t_elapsed_to_next_m_weight_non_miss,
                                            na.rm=TRUE),
    #and the version that excludes if the exposure value is missing.
    #see above for definition of this.
    sum_of_weights = sum(t_elapsed_to_next_miss, na.rm=TRUE),
    #useful to keep track of this as well.
    #the number of lines where exposure is missing. recall, above, this is true
    #if ANY pixels on that line segment have missing information.
    n_line_miss = sum(e_miss),
    n_line = n(), #keep track of how many line segments
  ) %>% 
  ungroup() %>% 
  mutate(
    e_m = e_weighted_sum/sum_of_weights,
    
    #proportion of the pixels with missing data
    n_line_prop_e_miss = n_line_miss/n_line,
    
    #if all of the lines are missing for that person (or person-time category),
    #then this should be a binary e_miss variable, as with the home-based measure
    e_miss = case_when(
      n_line_prop_e_miss>0.95~1,
      TRUE ~0),
    e_not_miss = abs(e_miss-1)
  ) %>% 
  #lose this variable, because it may confuse: e_weighted_sum
  dplyr::select(-starts_with("e_weighted_sum")) %>% 
  #reorder for easier viewing
  dplyr::select(
    starts_with("study_id"), 
    starts_with("e_name"),
    starts_with("e_"),
    starts_with("t_elap"), 
    starts_with("sum_of"), #because it's non-missing version of t_elapsed_h 
    everything()) 

#traj_summary_s_id_demo %>% View()