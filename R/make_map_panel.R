#' Produce Oakland map panel of continuous variable with gradient color scale
#'
#' This function takes in data and produces a panel of census tract maps of
#' Oakland representing the variable using a gradient color scale, across
#' four distinct periods. Should have "tractid10" column for census tracts
#' and "periods" column for distinct time periods.
#'
#' @param data Data with a column containing census tracts, distinct periods, and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param palette Color palette: "sequential" (default) or "diverging"
#' @param jenksbreaks Uses Jenks Breaks when T, otherwise uses continuous color scale
#' @param neg_bins For Jenks breaks, number of negative color bins. Default is 3.
#' @param pos_bins For Jenks breaks, number of positive color bins. Default is 3.
#' @param breaks Gradient scale breaks, either numeric vector or scales::extended_breaks(n = 6)
#' @param labels Gradient scale labels, either character vector or scales::percent or scales::comma
#' @param limits Gradient scale limits, c(min, max)
#' @param coord T if plotting coordinate values (lat, lon). Default is F.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Map panel of variable of interest across four periods.
#' @export

make_map_panel <- function(
  data,
  var,
  shp_tracts,
  palette = "sequential",
  jenksbreaks = T,
  neg_bins = 3,
  pos_bins = 3,
  breaks = scales::extended_breaks(n = 6),
  labels = scales::percent,
  limits = NULL,
  coord = F,
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
) {

  library(sf)
  library(rgdal)
  library(foreach)
  library(ggmap)
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(BAMMtools)

  # Adjust color palette
  if (palette == "sequential") {

    # Get max and min values for common gradient scale
    max = data %>%
      select({{var}})%>%
      max(na.rm = T)

    min = data %>%
      select({{var}}) %>%
      min(na.rm = T)

    range = c(min, max)

    MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")
    type = "seq"
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
    range <- data %>%
      select({{var}}) %>%
      abs() %>%
      max(na.rm = T) * c(-1, 1)

    # Diverging palette
    MAP_COLORS <- rev(RColorBrewer::brewer.pal(n = 9, name = "RdBu"))
    type = "div"
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
    range = limits
  }

  # county tract map
  oak_tracts <-
    shp_tracts %>%
    filter(GEOID10S %in% oak_ids$trtid10)

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

  maps_all = list()
  period_panels = levels(data$periods)

  # Get common legend
  legend_map <-
    ggmap(gmap_oak) +
    geom_sf(
      data = data,
      aes(fill = {{var}}),
      size = 0,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    guides(
      fill =
        guide_colorbar(
          barheight = 0.8,
          barwidth = 27,
          title = NULL,
          frame.colour = "black"
        )
    ) +
    theme(
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(3,0,0,0, unit = "pt"),
      panel.border = element_rect(colour = "black", fill=NA)
    )

  # discrete color bar
  if (jenksbreaks) {

    # set colors manually if different number of negative and positive bins
    if (neg_bins != pos_bins) {
      # Custom palettes ***MUST SET CUSTOM COLORS

      # 1 negative, 3 positive bins:
      if (neg_bins == 1 & pos_bins == 3) {
        pal <- c("#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")
      # 2 negative, 3 positive bins:
      } else if (neg_bins == 2 & pos_bins == 3) {
        pal <- c("#2166ac", "#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")
      }

      scale_fill_fermenter_custom <- function(pal,
                                              breaks,
                                              labels) {
        binned_scale("fill",
                     "fermenter",
                     ggplot2:::binned_pal(scales::manual_pal(unname(pal))),
                     breaks = breaks,
                     labels = labels)
      }
      legend_map = legend_map +
        scale_fill_fermenter_custom(pal,
                                    breaks = breaks,
                                    labels = labels)
    } else {
      legend_map = legend_map +
        scale_fill_fermenter(breaks = breaks,
                             type = type,
                             palette = palette,
                             direction = direction,
                             labels = labels)
    }

    # gradient color scale
  } else {
    legend_map = legend_map +
      scale_fill_gradientn(breaks = breaks,
                           labels = labels,
                           colors = alpha(MAP_COLORS, .8),
                           limits = range)
  }

  # Save legend object
  tmp <- ggplot_gtable(ggplot_build(legend_map))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]

  # Plot maps
  foreach(i = 1:length(period_panels)) %do% {
    data_period = data %>%
      dplyr::filter(periods == period_panels[i])

    map <-
      ggmap(gmap_oak) +
      geom_sf(
        data = data_period,
        aes(fill = {{var}}),
        size = 0,
        alpha = 0.7,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = data_period,
        size = 0.3,
        alpha = 0,
        inherit.aes = FALSE,
        color = "black"
      ) +
      guides(
        fill =
          guide_colorbar(
            barheight = 0.5,
            barwidth = 15,
            title = NULL,
            frame.colour = "black"
          )
      ) +
      theme_void() +
      theme(
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 12, hjust = .5, vjust = 3),
        plot.margin = margin(3,-.5,3,-.5, unit = "pt"),
        plot.caption = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill=NA)
      ) +
      labs(title = period_panels[i])

    # discrete color bar
    if (jenksbreaks) {

      # set colors manually if different number of negative and positive bins
      if (neg_bins != pos_bins) {
        # Custom palette
        # 1 negative, 3 positive bins:
        pal <- c("#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")

        scale_fill_fermenter_custom <- function(pal,
                                                breaks,
                                                labels) {
          binned_scale("fill",
                       "fermenter",
                       ggplot2:::binned_pal(scales::manual_pal(unname(pal))),
                       breaks = breaks,
                       labels = labels)
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
                                       limits = range,
                                       na.value = "grey60")
    }

    # plot coordinate points
    if (coord == T) {
      + geom_point(
        data = data,
        aes(x = lon, y = lat),
        color = "navy", size = 2
      )
    }

    # add map to list of grobs
    maps_all = c(maps_all, list(map))
  }

  # arrange period maps into panels
  map_number <- length(period_panels)
  width = 0
  height = 0

  if(map_number == 4) {
    layout <- rbind(c(1, 2), c(3, 4), c(5, 5))
    map_panel =
      grid.arrange(maps_all[[1]], maps_all[[2]], maps_all[[3]], maps_all[[4]],
                   legend,
                   nrow = 1 + ceiling(map_number/2), ncol = 2,
                   layout_matrix = layout,
                   heights = c(rep(5, ceiling(map_number/2)), 1.2),
                   bottom=textGrob(caption, gp=gpar(fontsize=9,font=3)))
    width = 7
    height = 7.8
  }

  if(map_number == 2) {
    layout <- rbind(c(1, 2), c(3, 3))
    map_panel =
      grid.arrange(maps_all[[1]], maps_all[[2]],
                   legend,
                   nrow = 2, ncol = 2,
                   layout_matrix = layout,
                   heights = c(5, 1.2),
                   bottom=textGrob(caption, gp=gpar(fontsize=9,font=3)))
    width = 7
    height = 4.1
  }


  if (save) {
    ggsave(savename, map_panel, height = height, width = width)
    return(map_panel)
  } else {
    return(map_panel)
  }
}
