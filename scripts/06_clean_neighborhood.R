library(tidyverse)
library(sf)
library(mapview)

# read in atl neighborhood and clean up
atl_neighborhood <- read_sf("./data/atl_neighborhood.geojson") %>%
  select(neighborhood = NEIGHBORHO, pop2010 = POP2010)

# create atl city limits polygon
atl_limits <- st_union(atl_neighborhood)

# check out a map
mapview(atl_neighborhood)
mapview(atl_limits)

# write to geojson, csv
st_write(atl_neighborhood, "./output/atl_neighbor.geojson", delete_dsn = TRUE)
st_write(atl_neighborhood, "./output/atl_neighbor.csv", delete_dsn = TRUE)
st_write(atl_limits, "./output/atl_limits.geojson", delete_dsn = TRUE)
st_write(atl_limits, "./output/atl_limits.csv", delete_dsn = TRUE)