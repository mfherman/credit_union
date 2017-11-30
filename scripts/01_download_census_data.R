library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(mapview)

options(tigris_use_cache = TRUE)

# total population B01001_001
# % non hispanic white B03002_003
# % non hispanic black B03002_004
# % hispanic B03002_012
# median age B01002_001
# median hh income B19013_001y
# below 100% poverty level B06012_002
# poverty level denom B06012_001
# gini index B19083_001
# pub assistance/food stamp B19058_002
# total households B19058_001
# median gross rent as a pct of income B25071_001
# lived in owner-occuppied B07013_002
# live in renter-occuppied B07013_003
# total housesholds for owner/rent B07013_001

# select counties
atlanta_msa <- c("Fulton", "DeKalb", "Gwinnett", "Cobb", "Clayton",
                 "Coweta", "Douglas", "Fayette", "Henry")

# select variables
variables <- c("B01001_001", "B03002_003", "B03002_004", "B03002_012",
               "B01002_001", "B19013_001", "B06012_002", "B06012_001",
               "B19083_001", "B19058_002", "B19058_001", "B25071_001",
               "B07013_002", "B07013_003", "B07013_001")
               
# download acs 5 year estimates
atlanta_tract <- get_acs(
  state = "GA",
  county = atlanta_msa,
  geography = "tract",
  variables = variables,
  survey = "acs5",
  year = 2015,
  geometry = TRUE,
  output = "wide"
  )

# percentage and select variables, reproject to wgs84
atl_tract_stat <- atlanta_tract %>%
  mutate(
    white_pct = B03002_003E / B01001_001E * 100,
    black_pct = B03002_004E / B01001_001E * 100,
    hisp_pct = B03002_012E / B01001_001E * 100,
    pov_pct = B06012_002E / B06012_001E * 100,
    pub_as_pct = B19058_002E / B19058_001E * 100,
    own_pct = B07013_002E / B07013_001E * 100,
    rent_pct = B07013_003E / B07013_001E * 100
  ) %>%
  mutate_at(vars(ends_with("pct")), funs(signif(., 3))) %>%
  select(
    geoid = GEOID,
    name = NAME,
    pop_tot = B01001_001E,
    med_age = B01002_001E,
    med_hhinc = B19013_001E,
    gini = B19083_001E,
    med_rburd = B25071_001E,
    white_pct:rent_pct
  ) %>%
  st_transform(4326)

# check out a simple map
tm_shape(atl_tract_stat) +
  tm_fill(
    col = "pov_pct",
    n = 5,
    style = "jenks"
    )

# write to geojson
st_write(atl_tract_stat, "./output/atl_tract.geojson", delete_dsn = TRUE)

# write to shp
dir.create("./output/atl_tract")
st_write(atl_tract_stat, "./output/atl_tract/atl_tract.shp")

# zip up shp files
atl_shp <- dir("./output/atl_tract", full.names = TRUE)
zip(zipfile = "./output/atl_tract.zip", files = atl_shp)
unlink("./output/atl_tract", recursive = TRUE)