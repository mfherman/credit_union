library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(ggmap)
library(mapview)

# read in credit union data
atl_cu <- read_excel("./data/atlanta_cu.xlsx", col_types = "text")

# clean up and geocode
atl_cu_clean <- atl_cu %>%
  clean_names() %>%
  mutate(addr_long = paste0(physical_address_line_1, ", ",
                            physical_address_city, ", ",
                            physical_address_state_code, " ",
                            physical_address_postal_code
                            )
         ) %>%
  mutate_geocode(addr_long)

# make it into a sf
atl_cu_sf <- atl_cu_clean %>%
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326,
    stringsAsFactors = TRUE,
    remove = FALSE,
    na.fail = TRUE
  )

# check out a map
mapview(atl_cu_sf)

# write geojson and csv
st_write(atl_cu_sf, "./output/atl_cu.geojson", delete_dsn = TRUE)
st_write(atl_cu_sf, "./output/atl_cu.csv", delete_dsn = TRUE)