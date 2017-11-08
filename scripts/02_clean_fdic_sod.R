library(tidyverse)
library(janitor)
library(sf)

# read in fdic sod - from https://www5.fdic.gov/sod/download/ALL_2017_10032017.ZIP
fdic_sod <- read_csv("./data/fdic_sod_2017.csv", col_types = cols(.default = "c"))

# define atlanta counties
atlanta_msa <- c("Fulton", "DeKalb", "Gwinnett", "Cobb", "Clayton",
                 "Coweta", "Douglas", "Fayette", "Henry")

# clean up names, filter atl banks, select vars, convert to sf
fdic_clean <- fdic_sod %>%
  clean_names() %>%
  filter(cntynamb %in% atlanta_msa & stalpbr == "GA") %>%
  select(uninumbr, namefull, namebr, addresbr, citybr, stalpbr, zipbr, cntynamb,
         sims_latitude, sims_longitude, sims_projection, asset, depdom, depsumbr,
         bkclass, bkmo, brnum, brsertyp, cert, charter, clcode, regagnt, specdesc
         ) %>%
  mutate_at(vars(sims_latitude, sims_longitude, asset:depsumbr), parse_number) %>%
  st_as_sf(
    coords = c("sims_longitude", "sims_latitude"),
    agr = "constant",
    crs = 4326,
    stringsAsFactors = TRUE,
    remove = FALSE,
    na.fail = TRUE
    )

# beware, some of these lat/longs are not great
# probably should geocode address as well

# write geojson and csv
st_write(fdic_clean, "./output/fdic_clean.geojson")
st_write(fdic_clean, "./output/fdic_clean.csv")


  
  