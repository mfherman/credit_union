library(tidyverse)
library(sf)
library(mapview)

atl_limits <- read_sf("./output/atl_limits.geojson")
atl_census_tract <- read_sf("./output/atl_tract.geojson")
atl_cu <- read_sf("./output/atl_cu.geojson")
atl_payday <- read_sf("./output/atl_payday.geojson")
atl_fdic <- read_sf("./output/fdic_clean.geojson")

atl_census_limit <- st_intersection(atl_limits, atl_census_tract)

atl_cu_limit <- atl_cu %>%
  filter(lengths(st_within(., atl_limits)) > 0) %>%
  mutate(cu_name = str_to_lower(cu_name))

atl_payday_limit <- read_sf("./output/atl_payday.geojson") %>%
  distinct(geometry, .keep_all = TRUE)
  filter(lengths(st_within(., atl_limits)) > 0)

atl_fdic_limit <- read_sf("./output/fdic_clean.geojson") %>%
  filter(lengths(st_within(., atl_limits)) > 0)

st_write(atl_cu_limit, "./output/final/atl_cu.geojson", delete_dsn = TRUE)
st_write(atl_payday_limit, "./output/atl_payday_limit.geojson", delete_dsn = TRUE)
st_write(atl_fdic_limit, "./output/atl_fdic_limit.geojson", delete_dsn = TRUE)
st_write(atl_census_limit, "./output/atl_census_limit.geojson", delete_dsn = TRUE)


download.file("https://github.com/efrank12/Payday_Lenders/raw/master/Payday_Lenders_geocoded.csv", "./data/atl_payday.csv")
