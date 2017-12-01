library(tidyverse)
library(sf)
library(mapview)

# read in atl neighborhood and clean up
atl_neighborhood <- read_sf("https://opendata.arcgis.com/datasets/716f417a1990446389ef7fd2c381d09f_2.geojson") %>%
  select(neighborhood = NAME, npu = NPU)

atl_npu <- read_sf("https://opendata.arcgis.com/datasets/8c1d863e9b584b67ace764489e64a530_1.geojson") %>%
  select(npu = NPU)

# write to geojson, csv
st_write(atl_neighborhood, "./output/atl_neighbor.geojson", delete_dsn = TRUE)
st_write(atl_npu, "./output/atl_npu.geojson", delete_dsn = TRUE)