This repository contains a demo for a way to extract and summarize raster microclimate data within GPS trajectories. I use the terra package to extract the data within a buffer around simulated GPS trajectories. The buffers are sf objects. Then, at the level of the line segment, I summarize the extracted measure weighted by how much its corresponding buffer overlaps the exposure's raster grid cell. Then I weight each line segment by its duration. In this case, all line segments are assumed to be a minute, so the time-weighting is not consequential, but the method allows that the duration between GPS pings might not always be the same.

All data in this demo are either simulated (the GPS trajectories) or are publicly available (land-surface temperature from satellite imagery).

The first script (`sim-trajectories.R)` simulates GPS trajectories for 3 study participants, with their simulated activity paths within San Diego County.

The second script (`create-buffers-around-traj.R`) creates 20 m buffers around those simulated GPS trajectories using the sf package.

The third (`read-lst-demo.R`) reads in land-surface temperature data using the terra package. Microclimate data used for the demo are Terra MODIS Land Surface Temperature & Emissivity (MOD11A2.061, 1000 m, 8-day) downloaded from the appEEARS platform. The specific data used in the demo is a satellite image taken on June 18, 2023 for the San Diego area. These data are loaded into the R pipeline in the `read-lst-demo.R` script. The data are saved in this Dropbox folder:

<https://www.dropbox.com/scl/fo/x144sgc6w11gmvx7ednw7/h?rlkey=y2mymmw4mtzzj1x6v35mnox4g&dl=0>

The final script (`extract-summarize-exposure.R`) extracts the land-surface temperature information from within the buffers around the simulated GPS trajectories and summarizes the information by study ID.

The scripts can be run in the following order:

```{r}
library(here)
source(here("scripts", "demo","sim-trajectories.R"))
source(here("scripts", "demo","create-buffers-around-traj.R"))
source(here("scripts", "demo","read-lst-demo.R"))
source(here("scripts","demo","extract-summarize-exposure.R"))
```
