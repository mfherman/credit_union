library(tidyverse)
library(sf)
library(mapview)
library(janitor)
library(ggmap)

# read in payday lender data
atl_payday <- read_sf("./data/financial/SubPrimeLending.shp") %>%
  clean_names() %>%
  st_transform(4326)

# read in atlanta msa polygon
atl_msa <- read_sf("./output/atl_tract.geojson") %>%
  st_union()

# filter nonprofits in atl msa, clean up, 
atl_payday_sf <- atl_payday %>%
  filter(lengths(st_within(., atl_msa)) > 0) %>%
  mutate_at(c("coname", "street", "city"), str_to_title) %>%
  mutate(naics5 = str_sub(naics, 1, 6),
         type = case_when(
           naics5 == "522291" ~ "Consumer Lending",
           naics5 == "522298" ~ "Pawn Shop",
           TRUE ~ "Other"
           )
         ) %>%
  filter(type != "Other") %>%
  select(locnum, name = coname, street:state, zip, type, naics5, naics7 = naics, salesvol)

# check out a map
mapview(atl_payday_sf, zcol = "type", legend = TRUE)

# write geojson and csv
st_write(atl_payday_sf, "./output/atl_financial.geojson", delete_dsn = TRUE)