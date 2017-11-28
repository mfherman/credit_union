library(tidyverse)
library(sf)
library(mapview)
library(janitor)
library(ggmap)

# read in payday lender data and geocode
atl_payday <- read_csv("https://github.com/efrank12/Payday_Lenders/raw/master/Payday_Lenders_geocoded.csv") %>%
  clean_names() %>%
  select(-(latitude:longitude)) %>%
  mutate_geocode(address) %>%

atl_payday_clean <- atl_payday %>%
  mutate(name = str_to_title(name))

# make it into a sf
atl_payday_sf <- atl_payday_clean %>%
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326,
    stringsAsFactors = FALSE,
    remove = FALSE,
    na.fail = TRUE
  )

# check out a map
mapview(atl_payday_sf)

# write geojson and csv
st_write(atl_payday_sf, "./output/atl_payday.geojson", delete_dsn = TRUE)
st_write(atl_payday_sf, "./output/atl_payday.csv", delete_dsn = TRUE)