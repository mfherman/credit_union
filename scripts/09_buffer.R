library(tidyverse)
library(sf)
library(mapview)
library(units)

atl_census_tract <- read_sf("./output/atl_tract.geojson") %>%
  st_transform(3519)

atl_bg <- read_sf("./output/atl_bg.geojson") %>%
  st_transform(3519)

atl_cu <- read_sf("./output/atl_msa_cu.geojson") %>%
  st_transform(3519)

atl_payday <- read_sf("./output/atl_financial.geojson") %>%
  st_transform(3519)

atl_fdic <- read_sf("./output/atl_fdic.geojson") %>%
  st_transform(3519)

# atl_msa polygon
atl_msa <- read_sf("./output/atl_tract.geojson") %>%
  st_union()  %>%
  st_transform(3519)

# create 1/4 mile buffers, clip to msa, calculate area
cu_buffer <- atl_cu %>%
  mutate(id = seq.int(nrow(.))) %>%
  st_buffer(dist = set_units(0.25, "mi")) %>%
  st_intersection(atl_msa) %>%
  mutate(area_buffer = st_area(.))

# intersect buffers with block groups, calculate proportion of bg in each buffer
cu_bg <- st_intersection(cu_buffer, atl_bg) %>%
  mutate(
    area_buffer_bg = st_area(.),
    area_bg_prop = as.numeric(area_buffer_bg / area_buffer)
    ) %>%
  mutate_at(vars(contains("pop")), funs(adj = (. * area_bg_prop))) %>%
  select(id, area_buffer_bg, area_bg_prop, geoid, med_age:med_rburd, white_pct:pop_other_adj)

hi <- cu_bg %>%
  group_by(id) %>%
  summarise(n = n(), sum = sum(area_bg_prop))

hi2 <- cu_bg %>%
  group_by(id) %>%
  summarize_at(vars(contains("pop")), funs(weighted.mean(., area))) %>%
  mutate_at(vars(pop_white:pop_other), funs(pct = (. / pop_tot) * 100)) %>%
  summarise_at(vars(pop_tot, pop_white_pct:pop_other_pct), funs(mean(., na.rm = TRUE)))
  

mapview(atl_bg, zcol = "med_hhinc", legend = TRUE) +
  cu_buffer +
  atl_cuut5