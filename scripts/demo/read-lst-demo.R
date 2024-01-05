#Read lst
#Read lst data
#using MOD11A2.061_QC_Day_doy2023153_aid0001
#downloaded from appeears for SD
library(here)
library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(raster)
library(tidyterra)

setwd(here("data-input-demo",
           "MOD11A2-061"))

#per the statistics csv in that folder, the dates correspond to these file names
lst_20230602=rast("MOD11A2.061_LST_Day_1km_doy2023145_aid0001.tif")
lst_20230610=rast("MOD11A2.061_LST_Day_1km_doy2023153_aid0001.tif")
lst_20230618=rast("MOD11A2.061_LST_Day_1km_doy2023161_aid0001.tif")

lst_20230602 %>% mapview()
lst_20230610 %>% mapview()
lst_20230618 %>% mapview() #this is the best image

