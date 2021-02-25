#' Produce Oakland map of discrete variable
#'
#' This function takes in category (gentrification, income, or ethnoracial) and
#' produces a census tract map of Oakland representing a discrete variable.
#'
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param discrete_cat Discrete category to plot: gent, income, ethnoracial
#' @param coord T if plotting coordinate values (lat, lon). Default is F.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Map of discrete variable: gentrification, income, or ethnoracial category.
#' @export

make_discrete_map <- function(shp_tracts,
                              discrete_cat = "gent",
                              coord = F,
                              save = F,
                              savename = "plot.png",
                              caption = paste0(frb_caption, ses_caption, period_caption)
) {
  library(tidyverse)
  library(sf)
  library(rgdal)
  library(ggmap)
  library(gridExtra)
  library(grid)
  library(scales)

  if (discrete_cat == "gent") {
    data = gentcat

    values = c("Intense"="#e31a1c",
               "Moderate"="#fb9a99",
               "Weak"="#fdbf6f",
               "People or Price"="#b2df8a",
               "Nongentrifiable"="#7a7a7a")

  } else if (discrete_cat == "income") {
    data = inccat

    values = c("Top Quintile" = "#102CA3",
               "Fourth Quintile" = "#1437cc",
               "Middle Quintile" = "#5A73DB",
               "Second Quintile" = "#A1AFEA",
               "Bottom Quintile" = "#D0D7F4")

    # Reverse order of levels for plotting
    data$cat = fct_rev(data$cat)

  } else if (discrete_cat == "ethnoracial") {
    data = racecat

    values = c("Predominantly Black" = "#481567FF",
               "Black-Other" = "#33638DDF",
               "White/White-Mixed" = "#FDE725FF",
               "Multiethnic/Other" = "#20A387FF")

  } else {
    return("Please select gent, income, or ethnoracial")
  }

  # county tract map, filter out three tracts
  oak_tracts <-
    shp_tracts %>%
    filter(GEOID10S %in% oak_ids$trtid10) %>%
    filter(!GEOID10S %in% c(6001981900, 6001982000, 6001983200))

  data = oak_tracts %>%
    right_join(data, by = c("GEOID10S" = "tractid10")) %>%
    st_transform(CRS("+proj=longlat +datum=WGS84"))

  # map data
  # Google Street Map for Oakland ----
  gmap_oak <- get_stamenmap(
    bbox = c(-122.3547, 37.6920, -122.1048, 37.890692),
    zoom = 12,
    maptype = "toner-lite",
    color = "bw")

  map <-
    ggmap(gmap_oak) +
    geom_sf(
      data = data,
      aes(fill = cat),
      size = 0,
      alpha = 0.7,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = data,
      size = 0.3,
      alpha = 0,
      inherit.aes = FALSE,
      color = "black"
    ) +
    scale_fill_manual(
      values = values
    ) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "right",
      plot.title = element_blank(),
      plot.caption = element_text(size = 7, hjust = .5, face = "italic"),
      plot.margin = margin(2,3,2,3, unit = "pt"),
      panel.border = element_rect(colour = "black", fill=NA)
    ) +
    labs(caption = caption)

  if (coord == T) {
    + geom_point(
      data = data,
      aes(x = lon, y = lat),
      color = "navy", size = 2
    )
  }

  if (save) {
    ggsave(savename, map, height = 5, width = 7)
    return(map)
  } else {
    return(map)
  }
}
