library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(ggmap)
library(mapview)

# read in credit union data
atl_cu <- read_excel("./data/atlanta_cu_updated.xlsx", col_types = "text")

# clean up and geocode
atl_cu_clean <- atl_cu %>%
  clean_names() %>%
  mutate(addr_long = paste0(physical_address_line_1, ", ",
                            physical_address_city, ", ",
                            physical_address_state_code, " ",
                            physical_address_postal_code
                            )
         ) %>%
  mutate_at(vars(contains("avg")), as.integer) %>%
  mutate_geocode(addr_long)

# make it into a sf
atl_cu_sf <- atl_cu_clean %>%
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326,
    stringsAsFactors = FALSE,
    remove = FALSE,
    na.fail = TRUE
  )

# rename vars and clean up
atl_cu_sf <- atl_cu_sf %>%
  select(
    cu_number,
    name = cu_name,
    site_type = site_type_name,
    main_office,
    address = physical_address_line_1,
    city = physical_address_city,
    state = physical_address_state_code,
    county = physical_address_county_name,
    fom_category = x_1,
    fom_detail = x_2,
    fom_type = type_of_membership,
    avg_assets_branch:avg_loans_branch,
    low_income_designated:juntos_avanzamos,
    unsecured_credit_card_loans:other_real_estate_loans_lines_of_credit
  ) %>%
  mutate(name = str_to_title(name))

# check out a map
mapview(atl_cu_sf)

# write geojson and csv
st_write(atl_cu_sf, "./output/atl_cu.geojson", delete_dsn = TRUE)
