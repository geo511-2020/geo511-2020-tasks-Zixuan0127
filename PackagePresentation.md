---
title: "tidyhydat"
author: "Zixuan Chen"
output: 
  revealjs::revealjs_presentation:
      lib_dir: externals
      keep_md: true
      theme: black
      highlight: tango
      self_contained: false
      transition: zoom
---

## Lead Author: Sam Albers 

![](author.jpg)

* Data scientist
* University of Northern BC
* Last paper in Water Resources Research titled _Seasonal turbidity linked to physical dynamics in a deep lake following the catastrophic 2014 Mount Polley mine tailings spill_

## tidyhydat

### Import the hydraulic data into R

[tidyhydat]: https://cran.r-project.org/web/packages/tidyhydat/vignettes/tidyhydat_example_analysis.html

First Step: Download the data:

* `download_hydat()`

Extract the data:

* `hy_stn_data_range()`


## Example

We are going to plot the discharge for the station with the longest record.

```r
library(tidyhydat)
library(dplyr)
library(ggplot2)
library(lubridate)
longest_record_data <- hy_stn_data_range() %>%
  filter(DATA_TYPE == "Q", RECORD_LENGTH == max(RECORD_LENGTH)) %>%
  pull_station_number() %>%
  hy_daily_flows()
```
