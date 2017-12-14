library(tidyverse)
library(sf)
library(mapview)
library(units)

atl_census_tract <- read_sf("./output/atl_tract.geojson") %>%
  st_transform(2240)

atl_bg <- read_sf("./output/atl_bg.geojson") %>%
  st_transform(2240) %>%
  mutate(area_bg = st_area(.))

atl_cu <- read_sf("./output/atl_msa_cu.geojson") %>%
  st_transform(2240)

atl_city_cu <- read_sf("./output/atl_cu.geojson") %>%
  st_transform(2240)

atl_payday <- read_sf("./output/atl_financial.geojson") %>%
  st_transform(2240) %>%
  rename(pay_name = name)

atl_fdic <- read_sf("./output/atl_fdic.geojson") %>%
  st_transform(2240)

# atl_msa polygon
atl_msa <- read_sf("./output/atl_tract.geojson") %>%
  st_union()  %>%
  st_transform(2240)

bg_buffer_calc <- function(x) {
  # create 1/4 mile buffers, clip to msa, calculate buffer area
  cu_buffer <- x %>%
    mutate(id = seq.int(nrow(.))) %>%
    st_buffer(dist = set_units(0.25, "mi")) %>%
    st_intersection(atl_msa) %>%
    mutate(area_buffer = st_area(.))
  
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
    select(id, geoid, area_bg_buffer:area_buffer_prop,
           med_age:med_rburd, pop_tot_adj:pop_other_adj)
  
  pop_sum <- cu_bg %>%
    group_by(id) %>%
    summarize_at(vars(contains("pop")), funs(sum(., na.rm = TRUE))) %>%
    mutate_at(vars(pop_white_adj:pop_other_adj), funs(pct = (. / pop_tot_adj) * 100)) %>%
    mutate_at(vars(pop_pov_adj), funs(pop_pov_pct = (. / pop_pov_denom_adj) * 100)) %>%
    st_set_geometry(NULL)
  
  med_sum <- cu_bg %>%
    group_by(id) %>%
    summarize_at(vars(contains("med")), funs(weighted.mean(., pop_tot_adj, na.rm = TRUE))) %>%
    st_set_geometry(NULL)
  
  cu_bg_summary <- left_join(pop_sum, med_sum, by = "id")
}

sum_bg <- function(x) {
  summary <- summarise_at(x, vars(med_hhinc, pop_pov_pct, pop_black_adj_pct),
                          funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))
  return(summary)
}


hi <- map(list("Credit Union" = atl_cu, "FDIC Bank" = atl_fdic, "Predatory Lenders" = atl_payday),
    bg_buffer_calc)

hi2 <- map_dfr(hi, sum_bg, .id = "Type")


hi3 <- map_dfr(list("Credit Union" = atl_cu, "FDIC Bank" = atl_fdic, "Predatory Lender" = atl_payday),
          bg_buffer_calc, .id = "Type")


library(RColorBrewer)  
library(scales)

tract_plot <- hi3 %>%
  mutate(Type = fct_relevel(Type, "Predatory Lender", "FDIC Bank", "Credit Union")) %>%
  ggplot((aes(x = med_hhinc, y = ..density.. * 1e4, fill = Type, colour = Type))) +
  geom_density(alpha = 0.25, size = 1.75) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(labels = dollar, limits = c(0, 1.75e5)) +
  labs(
    x = "Median Household Income",
    y = "Density",
    title = "Distribution of Neighborhoods near\nFinancial Institutions by Income",
    fill = "Type of Financial\nInstitution",
    color = "Type of Financial\nInstitution"
  ) +
  theme(plot.title = element_text(margin = margin(t = 20, b = 40))) +
  theme(
    axis.title.x = element_text(size = 30, margin = margin(t = 25, b = 20)),
    axis.title.y = element_text(size = 30, margin = margin(l = 20, r = 25)),
    axis.text = element_text(size = 26),
    plot.title = element_text(size = 38, face = "bold", margin = margin(t = 20, b = 20)),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 26),
    legend.key.size = unit(36, "points"),
    legend.box.margin	= margin(r = 25),
    plot.background = element_rect(color = "black", size = 0.75)
    )

tract_plot
ggsave("./plots/tract_plot.png", device = "png", width = 17, height = 12, dpi = 600)


summary_stats <- map_dfr(
  list("Credit Union" = atl_cu, "FDIC Bank" = atl_fdic, "Payday" = atl_payday),
  bg_buffer_calc, .id = "Type"
  )


summarise_at(vars(pop_black_adj_pct, pop_white_adj_pct, med_hhinc), funs(mean(., na.rm = TRUE)))

## POOR BLACK vs. POOR WHITE??


library(maptools)
library(spatstat)

sp2 <- as_Spatial(atl_fdic$geometry)
sp_ppp <- as(sp2, "ppp")
sp_owin <- as.owin(sp_ppp)

plot(density(sp_ppp, sigma = 8000))


