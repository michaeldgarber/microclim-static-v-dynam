This repository contains a demo for a way to extract and summarize raster microclimate data within GPS trajectories.The method uses the terra package to extract the data within GPS trajectories, which are assumed to be sf polygons. Then I do some data wrangling to summarize the extracted measures weighted by the time and areal overlap of a buffer around the trajectories.

All data in this demo are either simulated (the GPS trajectories) or are publicly available (land-surface temperature from satellite imagery).

The first script (`sim-trajectories.R)` simulates GPS trajectories for 3 study participants, with their simulated activity paths within San Diego County.

The second (`read-lst-demo.R`) reads in land-surface temperature data. Microclimate data used for the demo are Terra MODIS Land Surface Temperature & Emissivity (MOD11A2.061, 1000 m, 8-day) downloaded from the appEEARS platform. The data used for the demo are from June of 2023 for the San Diego area. These data are loaded in in the `read-lst-demo.R` script. The data are saved in this Dropbox folder:

<https://www.dropbox.com/scl/fo/x144sgc6w11gmvx7ednw7/h?rlkey=y2mymmw4mtzzj1x6v35mnox4g&dl=0>

And the third script (`measure-e-sim-traj.R`) extracts the the land-surface temperature information from within a buffer around the simulated GPS trajectories and summarizes the information.

The scripts can be run in the following order:

```{r}
libray(here)
source(here("scripts", "demo","sim-trajectories.R")) 
source(here("scripts", "demo","read-lst-demo.R")) 
source(here("scripts","demo","measure-e-sim-traj.R"))
```
