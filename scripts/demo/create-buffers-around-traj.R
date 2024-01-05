# Create buffers around trajectories----
library(sf)
library(tidyverse)
#Could simply do the following, but in my experience it's kind of slow
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
