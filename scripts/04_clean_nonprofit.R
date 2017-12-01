library(tidyverse)
library(sf)
library(mapview)
library(janitor)

# read in nonprofit data and 
all_nonprofit <- read_csv(
  "./data/all_nonprofits.csv",col_types = cols(.default = "c")
  ) %>% 
  clean_names()

ga_nonprofit_clean <- all_nonprofit %>%
  filter(state == "GA" & !is.na(lat)) %>%
  select(
    ein, name, street, city = city_1, state, zip, category, description,
    ntee_code = code, asset_amt, income_amt, revenue_amt, lat, long
    ) %>%
  mutate_at(vars(name, street, city), str_to_title) %>%
  mutate_at(vars(contains("amt")), as.integer) %>%
  mutate_at(vars(long, lat), as.double) %>%
  mutate(ein = if_else(nchar(ein) == 8, paste0("0", ein), ein))

# make it into a sf
ga_nonprofit_sf <- ga_nonprofit_clean %>%
  st_as_sf(
    coords = c("long", "lat"),
    agr = "constant",
    crs = 4326,
    stringsAsFactors = FALSE,
    remove = FALSE,
    na.fail = TRUE
  )

# read in atl limits polygon
atl_limits <- read_sf("./output/atl_limits.geojson")

# define selected nonprofit categories
nonprofit_select <- c("civil_rights", "community_capacity_bldg", "employment",
                      "food", "housing_shelter", "human_services",
                      "public_social_benefit", "philanthropy", "religious")

# filter nonprofits in atl msa and selected categories
atl_nonprofit_sf <- ga_nonprofit_sf %>%
  filter(category %in% nonprofit_select) %>%
  filter(lengths(st_within(., atl_limits)) > 0) %>%
  mutate(category = str_to_title(str_replace_all(category, "_", " "))) %>%
  select(-(lat:long))

# check out a map
mapview(atl_nonprofit_sf)

# write geojson and csv
st_write(atl_nonprofit_sf, "./output/atl_nonprofit.geojson", delete_dsn = TRUE)