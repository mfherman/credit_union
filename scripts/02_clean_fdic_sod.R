library(tidyverse)
library(janitor)
library(sf)
library(ggmap)
library(mapview)

# download and read in fdic sod
download.file("https://www5.fdic.gov/sod/download/ALL_2017_10032017.ZIP",
              "./data/fdic_sod.zip")
unzip("./data/fdic_sod.zip", files = "ALL_2017.csv", exdir = "./data")
file.rename("./data/ALL_2017.csv", "./data/fdic_sod_2017.csv")
file.remove("./data/fdic_sod.zip")
fdic_sod <- read_csv("./data/fdic_sod_2017.csv", col_types = cols(.default = "c"))

# define atlanta counties
atlanta_msa <- c("Fulton", "DeKalb", "Gwinnett", "Cobb", "Clayton",
                 "Coweta", "Douglas", "Fayette", "Henry")

# clean up names, filter atl banks, select vars, geocode banks
# this will take a while to geocode
fdic_clean <- fdic_sod %>%
  clean_names() %>%
  filter(cntynamb %in% atlanta_msa & stalpbr == "GA") %>%
  select(uninumbr, namefull, namebr, addresbr, citybr, stalpbr, zipbr, cntynamb,
         sims_latitude, sims_longitude, sims_projection, asset, depdom, depsumbr,
         bkclass, bkmo, brnum, brsertyp, cert, charter, clcode, regagnt, specdesc
         ) %>%
  mutate_at(vars(sims_latitude, sims_longitude, asset:depsumbr), parse_number) %>%
  mutate(addr_long = paste0(addresbr, ", ", citybr, ", ", stalpbr, " ", zipbr)) %>%
  mutate_geocode(addr_long)

# make it into a sf
fdic_clean_sf <- fdic_clean %>%
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326,
    stringsAsFactors = TRUE,
    remove = FALSE,
    na.fail = TRUE
    )

# check out a map
mapview(fdic_clean_sf)

# write geojson and csv
st_write(fdic_clean_sf, "./output/fdic_clean.geojson", delete_dsn = TRUE)
st_write(fdic_clean_sf, "./output/fdic_clean.csv", delete_dsn = TRUE)