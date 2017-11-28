library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)

atl_tract <- st_read("./output/atl_tract.geojson", stringsAsFactors = FALSE)
fdic <- st_read("./output/fdic_clean.geojson", stringsAsFactors = FALSE)
cu <- st_read("./output/atl_cu.geojson", stringsAsFactors = FALSE)

tmap_mode(mode = "view")

hh_inc_layer <- tm_shape(atl_tract) +
  tm_fill(
    col = "med_hhinc",
    palette = "GnBu",
    contrast = c(0.2, 0.8),
    n = 5,
    style = "jenks",
    title = "Median Household Income",
    textNA = "No Population",
    legend.format = list(
      fun = function(x) {
        paste0("$", prettyNum(x, big.mark	= ","))
      }
    ),
    popup.vars = c(
      "Total Population" = "pop_tot",
      "Median Household Income" = "med_hhinc",
      "Percent Renter Houshoulds " = "rent_pct"
    ),
    id = "name",
    popup.format = list(
      pop_tot = list(format = "f"),
      med_hhinc = list(
        fun = function(x) {
          paste0("$", prettyNum(x, big.mark	= ","))
          }
        ),
      rent_pct = list(
        fun = function(x) {
          if_else(is.nan(x), "n/a",
                  paste0(formatC(x, digits = 1 , format = "f"), "%")
          )
        }
      )
    )
  ) +
  tm_borders(col = "dark grey")



hh_inc_layer +  
tm_shape(fdic) +
  tm_dots(col = "green") +
tm_shape(cu) +
  tm_dots(col = "red")
  

