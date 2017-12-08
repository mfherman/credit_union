library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(mapview)

options(tigris_use_cache = TRUE)

# total population B01001_001
# % non hispanic white B03002_003
# % non hispanic black B03002_004
# % non hispanic asian B03002_0
# % hispanic B03002_012
# median age B01002_001
# median hh income B19013_001
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
atlanta_msa <- c(
  "013",
  "015",
  "035",
  "045",
  "057",
  "063",
  "067",
  "077",
  "085",
  "089",
  "097",
  "113",
  "117",
  "121",
  "135",
  "143",
  "149",
  "151",
  "159",
  "171",
  "199",
  "211",
  "217",
  "223",
  "227",
  "231",
  "247",
  "255",
  "297")

# select variables
tract_variables <- c("B01001_001", "B03002_003", "B03002_004", "B03002_006",
                     "B03002_012","B01002_001", "B19013_001", "B17021_002",
                     "B17021_001", "B19083_001", "B19058_002", "B19058_001",
                     "B25071_001", "B07013_002", "B07013_003", "B07013_001")

bg_variables <- c("B01001_001", "B03002_003", "B03002_004", "B03002_006",
                  "B03002_012","B01002_001", "B19013_001", "B19058_002",
                  "B25071_001", "B17021_002", "B17021_001")
               
# download acs 5 year estimates tract
atlanta_tract <- get_acs(
  state = "GA",
  county = atlanta_msa,
  geography = "tract",
  variables = tract_variables,
  survey = "acs5",
  year = 2015,
  geometry = TRUE,
  output = "wide"
  )

# download acs 5 year estimates block group
atlanta_bg <- get_acs(
  state = "GA",
  county = atlanta_msa,
  geography = "block group",
  variables = bg_variables,
  survey = "acs5",
  year = 2015,
  geometry = TRUE,
  output = "wide"
)

# percentage and select variables, reproject to wgs84
atl_tract_stat <- atlanta_tract %>%
  mutate(
    pop_other = B01001_001E - (B03002_003E + B03002_004E + B03002_012E + B03002_006E),
    white_pct = B03002_003E / B01001_001E * 100,
    black_pct = B03002_004E / B01001_001E * 100,
    hisp_pct = B03002_012E / B01001_001E * 100,
    asian_pct = B03002_006E / B01001_001E * 100,
    other_pct = 100 - (white_pct + black_pct + hisp_pct + asian_pct),
    pov_pct = B17021_002E / B17021_001E * 100,
    pub_as_pct = B19058_002E / B19058_001E * 100,
    own_pct = B07013_002E / B07013_001E * 100,
    rent_pct = B07013_003E / B07013_001E * 100
  ) %>%
  mutate_at(vars(ends_with("pct")), funs(round(., 1))) %>%
  select(
    geoid = GEOID,
    name = NAME,
    pop_tot = B01001_001E,
    med_age = B01002_001E,
    med_hhinc = B19013_001E,
    gini = B19083_001E,
    med_rburd = B25071_001E,
    pop_pov = B17021_002E,
    pop_pov_denom = B17021_001E,
    pop_white = B03002_003E,
    pop_black = B03002_004E,
    pop_hisp = B03002_012E,
    pop_asian = B03002_006E,
    pop_other,
    white_pct:rent_pct
  ) %>%
  st_transform(4326)

# percentage and select variables, reproject to wgs84 
atl_bg_stat <- atlanta_bg %>%
  mutate(
    pop_other = B01001_001E - (B03002_003E + B03002_004E + B03002_012E + B03002_006E),
    white_pct = B03002_003E / B01001_001E * 100,
    black_pct = B03002_004E / B01001_001E * 100,
    hisp_pct = B03002_012E / B01001_001E * 100,
    asian_pct = B03002_006E / B01001_001E * 100,
    other_pct = 100 - (white_pct + black_pct + hisp_pct + asian_pct),
    pov_pct = B17021_002E / B17021_001E * 100
  ) %>%
  mutate_at(vars(ends_with("pct")), funs(round(., 1))) %>%
  select(
    geoid = GEOID,
    name = NAME,
    pop_tot = B01001_001E,
    med_age = B01002_001E,
    med_hhinc = B19013_001E,
    med_rburd = B25071_001E,
    pop_pov = B17021_002E,
    pop_pov_denom = B17021_001E,
    pop_white = B03002_003E,
    pop_black = B03002_004E,
    pop_hisp = B03002_012E,
    pop_asian = B03002_006E,
    pop_other,
    white_pct:pov_pct
  ) %>%
  st_transform(4326)

# check out a simple map
tmap_mode("view")
tm_shape(atl_bg_stat) +
  tm_fill(
    col = "med_hhinc",
    n = 5,
    style = "jenks"
    )

# write to geojson
st_write(atl_tract_stat, "./output/atl_tract.geojson", delete_dsn = TRUE)
st_write(atl_bg_stat,  "./output/atl_bg.geojson", delete_dsn = TRUE)