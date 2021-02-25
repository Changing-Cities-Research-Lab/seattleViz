#' Produce panel of stacked bar charts
#'
#' This function takes in data and produces a panel of stacked bar charts,
#' based on different groupings. Intended to be used with oakViz::aggregate_categories()
#'
#' @param dat Data with a column containing variables of interest and grouping variables.
#' @param fill Name of column containing fill variable: "ses" (default), "move", "dest".
#' @param group Category for x-axis grouping: "period" (default), "ses"
#' @param y_title Title to display along y-axis.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Panel of stacked bar charts of different groupings
#' @export

stacked_bar <- function(
  dat,
  fill = "ses", # "move", "dest"
  group = "period", # ses
  y_title = NULL,
  save = F,
  savename = "plot.png",
  caption = paste0(frb_acs_caption, ses_caption, period_caption)
) {
  library(rgdal)
  library(foreach)
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(reshape2)

  theme =
    theme_bw() +
    theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 9),
      legend.position = "none",
      legend.direction = "horizontal",
      # Caption
      plot.caption = element_text(size = 10, hjust = .5, face = "italic"),
      # X-axis
      axis.ticks.x = element_blank(),
      # Y-axis
      axis.ticks.y=element_blank(),
      axis.title.y=element_text(size = 10),
      axis.text.y=element_text(size = 10),
      # Background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      # Margins
      plot.margin=unit(c(0,0.5,0,0.5), "cm"))

  data_full = dat

  data_full$facet <- plyr::revalue(data_full$facet, c("All" = "Ethnoracial"))

  # Plot bar charts
  plots_all = list()

  foreach(i = 1:3) %do% {
    facetorder = c("Ethnoracial", "Income", "Gentrification")
    dat = data_full %>%
      filter(facet %in% facetorder[i])

    # X-axis grouping: period or SES
    if (group == "period") {
      dat = dat %>%
        filter(year %in% c("Boom", "Bust", "Recovery", "Post-Recovery"))

      dat$year <- factor(dat$year,
                         levels = c("Boom", "Bust", "Recovery", "Post-Recovery"))
      dat$x_group = dat$year
      x_labels = c("Boom", "Bust", "Recovery", "Post-Recovery")

    } else if (group == "ses") {
      dat$ses <- factor(dat$ses,
                        levels = c("Low", "Moderate","Middle", "High"))
      dat$x_group = dat$ses
      x_labels = c("Low", "Moderate","Middle", "High")
    }
    # Double check NA's

    # Fill grouping: ses, move, or dest
    if (fill == "ses") {
      # Compute share of population by SES
      dat = dat %>%
        group_by(cat, year) %>%
        mutate(denom = sum(pop)) %>%
        mutate(value = pop/denom)

      dat$ses <- factor(dat$ses,
                        levels = c("Low", "Moderate","Middle", "High"))

      dat$fill = dat$ses

      values = c(
        "Low" = "#fcbba1",
        "Moderate" = "#fc9272",
        "Middle" = "#fb6a4a",
        "High" = "#b63b36")

      fill_labels = c("Low", "Moderate","Middle", "High")

    } else if (fill == "move") {
      # Compute share of total moves by move type
      dat = dat %>%
        select(x_group,
               cat,
               facet,
               outmigration_outba,
               outmigration_outoak,
               withinoakmigration) %>%
        group_by(x_group,
                 cat,
                 facet) %>%
        summarise_all(sum) %>%
        mutate(denom = outmigration_outba +
                 (outmigration_outoak - outmigration_outba) +
                 withinoakmigration) %>%
        mutate(moved_outba_pct = outmigration_outba/denom) %>%
        mutate(diff_city_ba_pct = (outmigration_outoak - outmigration_outba)/denom) %>%
        mutate(moved_within_oak_pct = withinoakmigration/denom) %>%
        select(x_group,
               cat,
               facet,
               moved_outba_pct,
               diff_city_ba_pct,
               moved_within_oak_pct)

      # Melt data for plotting
      dat = dat %>%
        melt(id = c("cat", "x_group", "facet"))

      # Rename/order/select colors/label move categories
      dat$variable <- plyr::revalue(dat$variable,
                                    relabel_move_cat)

      dat$variable <- factor(dat$variable,
                             levels = move_order)
      dat$fill = dat$variable

      values = move_cat_colors

      fill_labels = move_order

    } else if (fill == "dest") {
      # Compute share of moves by destination
      dat = dat %>%
        select(x_group,
               cat,
               facet,
               outmigration_outba,
               withinoakmigration,
               outmigration_alameda,
               outmigration_contracosta,
               outmigration_northbay,
               outmigration_sanfran,
               outmigration_southbay) %>%
        group_by(x_group,
                 cat,
                 facet) %>%
        summarise_all(sum) %>%
        mutate(denom =
                 outmigration_outba +
                 withinoakmigration +
                 outmigration_alameda +
                 outmigration_contracosta +
                 outmigration_northbay +
                 outmigration_sanfran +
                 outmigration_southbay) %>%
        mutate(outmigration_outba_pct = outmigration_outba/denom) %>%
        mutate(withinoakmigration_pct = withinoakmigration/denom) %>%
        mutate(outmigration_alameda_pct = outmigration_alameda/denom) %>%
        mutate(outmigration_contracosta_pct = outmigration_contracosta/denom) %>%
        mutate(outmigration_northbay_pct = outmigration_northbay/denom) %>%
        mutate(outmigration_sanfran_pct = outmigration_sanfran/denom) %>%
        mutate(outmigration_southbay_pct = outmigration_southbay/denom) %>%
        select(x_group,
               cat,
               facet,
               outmigration_outba_pct,
               withinoakmigration_pct,
               outmigration_alameda_pct,
               outmigration_contracosta_pct,
               outmigration_northbay_pct,
               outmigration_sanfran_pct,
               outmigration_southbay_pct)

      # Melt data for plotting
      dat = dat %>%
        melt(id = c("cat", "x_group", "facet"))

      # Rename/order/select colors/label move categories
      dat$variable <- plyr::revalue(dat$variable,
                                    relabel_dest_cat)

      dat$variable <- factor(dat$variable,
                             levels = dest_order)

      dat$fill = dat$variable

      values = dest_colors

      fill_labels = dest_order

    } else {
      return("Please select 'ses', 'move', or 'dest'")
    }

    if (i == 3) {
      plot <-
        ggplot(dat, aes(y = value,
                        x = x_group,
                        fill = fill)) +
        geom_bar(stat="identity", position = "stack") +
        facet_grid(cols = vars(cat),
                   rows = vars(facet)) +
        scale_fill_manual(values = values,
                          labels = fill_labels) +
        scale_x_discrete(
          labels = x_labels) +
        scale_y_continuous(expand = c(0, 0.01), labels = scales::percent) +
        labs(x = NULL, y = y_title) +
        theme +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 1, reverse = T))
    } else {
      plot <-
        ggplot(dat, aes(y = value,
                        x = x_group,
                        fill = fill)) +
        geom_bar(stat="identity", position = "stack") +
        facet_grid(cols = vars(cat),
                   rows = vars(facet)) +
        scale_fill_manual(values = values,
                          labels = fill_labels) +
        scale_x_discrete(
          labels = x_labels) +
        scale_y_continuous(expand = c(0, 0.01), labels = scales::percent) +
        labs(x = NULL, y = y_title) +
        theme +
        theme(axis.text.x = element_blank())
    }
    # add map to list of grobs
    plots_all = c(plots_all, list(plot))
  }

  # arrange period maps into 4 panels
  layout <- rbind(c(1),c(2),c(3))
  panel =
    grid.arrange(plots_all[[1]], plots_all[[2]], plots_all[[3]],
                 nrow = 3, ncol = 1,
                 layout_matrix = layout,
                 heights = c(4, 4, 5.9),
                 bottom=textGrob(caption, gp=gpar(fontsize=7,font=3)))

  if (save) {
    ggsave(savename, panel, height = 9, width = 8)
  }
  return(panel)
}

