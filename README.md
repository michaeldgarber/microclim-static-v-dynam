This repository contains a demo for a way to extract raster data within GPS trajectories. The first script simulates GPS trajectories for 3 study participants, with their simulated activity paths within San Diego County.

The second reads in land-surface temperature data from Scripts can be run in the following order:

```{r}
libray(here)
source(here("scripts", "demo","sim-trajectories.R")) 
source(here("scripts", "demo","read-lst-demo.R")) 
source(here("scripts","demo","measure-e-sim-traj.R"))
```

Microclimate data used for the demo are Terra MODIS Land Surface Temperature & Emissivity (MOD11A2.061, 1000 m, 8-day) downloaded from the appEEARS platform. The data used for the demo are from June of 2023 for the San Diego area. Saved in this Dropbod folder:

<https://www.dropbox.com/scl/fo/x144sgc6w11gmvx7ednw7/h?rlkey=y2mymmw4mtzzj1x6v35mnox4g&dl=0>
