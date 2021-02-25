#' Produce Oakland map of continuous variable with gradient color scale
#'
#' This function takes in data and produces a census tract map of Oakland
#' representing the variable using a gradient color scale. Census tract
#' column must be named "tractid10".
#'
#' @param data Data with a column containing census tracts and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param palette Color palette: "sequential" (default) or "diverging"
#' @param jenksbreaks Uses Jenks breaks when T, otherwise uses continuous color scale
#' @param neg_bins For Jenks breaks, number of negative color bins. Default is 3.
#' @param pos_bins For Jenks breaks, number of positive color bins. Default is 3.
#' @param breaks Gradient scale breaks, either numeric vector or scales::extended_breaks(n = 6)
#' @param labels Gradient scale labels, either character vector or scales::percent or scales::comma
#' @param limits Manual limits for color scale, numeic vector: c(min, max)
#' @param coord T if plotting coordinate values (lat, lon). Default is F.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Map of variable of interest.
#' @export
## Single Map
make_map <- function(data,
                     var,
                     shp_tracts,
                     palette = "sequential",
                     jenksbreaks = T,
                     neg_bins = 3,
                     pos_bins = 3,
                     breaks = scales::extended_breaks(n = 7),
                     labels = scales::percent,
                     limits = NULL,
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
  library(BAMMtools)

  # Adjust color palette
  if (palette == "sequential") {
    lim = NULL
    type = "seq"
    MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")
    palette = "YlOrRd"
    direction = 1

    # set number of Jenks breaks
    if (jenksbreaks) {
      breaks = data %>%
        dplyr::pull({{var}}) %>%
        BAMMtools::getJenksBreaks(k = 6)
    }

  } else if (palette == "diverging") {

    # Get limits to center diverging palette around 0
    lim <- data %>%
      select({{var}}) %>%
      abs() %>%
      max(na.rm = T) * c(-1, 1)
    type = "div"
    MAP_COLORS <- rev(RColorBrewer::brewer.pal(n = 9, name = "RdBu"))
    palette = "RdBu"
    direction = -1

    # find Jenks breaks for negative and positive values separately, then combine
    if (jenksbreaks) {
      values = data %>%
        dplyr::pull({{var}})

      # add 0 value to range of values to split negative and positive values
      values = c(0, values)

      neg_values = values[which(values <= 0)]
      pos_values = values[which(values >= 0)]

      neg_breaks = neg_values %>%
        getJenksBreaks(k = neg_bins + 1)
      pos_breaks = pos_values %>%
        getJenksBreaks(k = pos_bins + 1)

      # Removes duplicate 0's in breaks
      breaks = unique(c(neg_breaks, pos_breaks))
    }

  } else {
    return("Please select sequential or diverging color palette.")
  }

  # Overrides lim value if user inputs limits
  if (!is.null(limits)) {
    lim = limits
  }


  # county tract map
  oak_tracts <-
    shp_tracts %>%
    filter(GEOID10S %in% oak_ids$trtid10)

  data$tractid10 = as.numeric(data$tractid10)

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
      aes(fill = {{var}}),
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
    guides(
      fill =
        guide_colorbar(
          barheight = 0.5,
          barwidth = 19,
          title = NULL,
          frame.colour = "black"
        )
    ) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.box.margin = margin(3,0,0,0, unit = "pt"),
      plot.title = element_blank(),
      plot.margin = margin(3,1,3,1, unit = "pt"),
      plot.caption = element_text(size = 6, hjust = .5),
      panel.border = element_rect(colour = "black", fill=NA)
    ) +
    labs(caption = caption)

  # discrete color bar
  if (jenksbreaks) {
    # set colors manually if different number of negative and positive bins
    if (neg_bins != pos_bins) {
      # Custom palette
      # one negative, 3 positive bins:
      pal <- c("#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")

      scale_fill_fermenter_custom <- function(pal,
                                              breaks,
                                              labels) {
        binned_scale("fill",
                     "fermenter",
                     ggplot2:::binned_pal(scales::manual_pal(unname(pal))),
                     breaks = breaks,
                     labels = labels
                     )
      }

      map = map + scale_fill_fermenter_custom(pal,
                                              breaks = breaks,
                                              labels = labels)
    } else {
      map = map + scale_fill_fermenter(breaks = breaks,
                                       type = type,
                                       palette = palette,
                                       direction = direction,
                                       labels = labels)
    }

    # gradient color scale
  } else {
    map = map + scale_fill_gradientn(breaks = breaks,
                                     labels = labels,
                                     colors = alpha(MAP_COLORS, .8),
                                     limits = lim)
  }

  # plot coordinate points
  if (coord) {
    map = map + geom_point(data = data,
                           aes(x = lon, y = lat),
                           color = "navy", size = 2
    )
  }

  if (save) {
    ggsave(savename, map, height = 5, width = 5)
    return(map)
  } else {
    return(map)
  }
}
