library(tidyverse)
library(sf)
library(mapview)
library(units)

atl_census_tract <- read_sf("./output/atl_tract.geojson") %>%
<<<<<<< HEAD
  st_transform(2240)

atl_bg <- read_sf("./output/atl_bg.geojson") %>%
  st_transform(2240) %>%
  mutate(area_bg = st_area(.))

atl_cu <- read_sf("./output/atl_msa_cu.geojson") %>%
  st_transform(2240)

atl_payday <- read_sf("./output/atl_financial.geojson") %>%
  st_transform(2240)

atl_fdic <- read_sf("./output/atl_fdic.geojson") %>%
  st_transform(2240)
=======
  st_transform(3519)

atl_bg <- read_sf("./output/atl_bg.geojson") %>%
  st_transform(3519)

atl_cu <- read_sf("./output/atl_msa_cu.geojson") %>%
  st_transform(3519)

atl_payday <- read_sf("./output/atl_financial.geojson") %>%
  st_transform(3519)

atl_fdic <- read_sf("./output/atl_fdic.geojson") %>%
  st_transform(3519)
>>>>>>> 4e4631b4dc2310e82281a2cd8998fc410feca13e

# atl_msa polygon
atl_msa <- read_sf("./output/atl_tract.geojson") %>%
  st_union()  %>%
<<<<<<< HEAD
  st_transform(2240)

# create 1/4 mile buffers, clip to msa, calculate buffer area
=======
  st_transform(3519)

# create 1/4 mile buffers, clip to msa, calculate area
>>>>>>> 4e4631b4dc2310e82281a2cd8998fc410feca13e
cu_buffer <- atl_cu %>%
  mutate(id = seq.int(nrow(.))) %>%
  st_buffer(dist = set_units(0.25, "mi")) %>%
  st_intersection(atl_msa) %>%
  mutate(area_buffer = st_area(.))

<<<<<<< HEAD
# intersect buffers with block groups
# calculate prop of bg in buffer and prop of buffer of each bg
# calculate adjusted population totals
cu_bg <- st_intersection(cu_buffer, atl_bg) %>%
  mutate(
    area_bg_buffer = st_area(.),
    area_bg_prop = as.numeric(area_bg_buffer / area_bg),
    area_buffer_prop = as.numeric(area_bg_buffer / area_buffer)
    ) %>%
  mutate_at(vars(contains("pop")), funs(adj = (. * area_bg_prop))) %>%
  select(id, area_bg_buffer, area_bg_prop, area_buffer_prop,
         geoid, med_age:med_rburd, white_pct:pop_other_adj)

pop_sum <- cu_bg %>%
  group_by(id) %>%
  summarize_at(vars(contains("pop")), funs(sum(., na.rm = TRUE))) %>%
  mutate_at(vars(pop_white_adj:pop_other_adj), funs(pct = (. / pop_tot_adj) * 100)) %>%
  st_set_geometry(NULL)

med_sum <- cu_bg %>%
  group_by(id) %>%
  summarize_at(vars(contains("med")), funs(weighted.mean(., pop_tot_adj, na.rm = TRUE))) %>%
  st_set_geometry(NULL)

hi4 <- left_join(pop_sum, med_sum, by = "id") %>%
  summarise_at(vars(pop_black_adj_pct, med_hhinc), funs(mean(., na.rm = TRUE)))





mapview(atl_bg, zcol = "med_hhinc", legend = TRUE) +
  cu_buffer +
  atl_cu
=======
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
>>>>>>> 4e4631b4dc2310e82281a2cd8998fc410feca13e
