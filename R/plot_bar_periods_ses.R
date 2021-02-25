#' Produce grouped bar chart panel across four periods and SES category.
#'
#' This function takes in data and produces a bar chart panel grouped by either
#' gentrification, ethnoracial, income, ses, or period category across four
#' housing periods and SES categories.
#'
#' @param dat Data with a column containing census tracts and variable of interest. Aggregated data with column "period"
#' @param var Name of column containing variable to plot.
#' @param limits Y-axis limits
#' @param y_title Title to display along y-axis.
#' @param group Category for x-axis grouping: "gent" (default), "ethnoracial", "income", "ses", or "period"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Grouped bar chart panel across four periods and SES categories.
#' @export
# Plot bar chart by group across periods
plot_bar_periods_ses_grid <- function(
  dat,
  var,
  limits,
  y_title = NULL,
  group = "gent",
  save = F,
  savename = "plot.png",
  caption = paste0(frb_acs_caption, ses_caption, period_caption)
) {

  library('tidyverse')

  # Combine with either gentcat, racecat, inccat, ses, or period
  if (group == "gent") {
    dat = dat %>% #left_join(gentcat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(gent_cat))) %>%
      filter(!is.na(cat))

    colors = gent_cat_colors
    labels = gent_cat

  } else if (group == "ethnoracial") {
    dat = dat %>% #left_join(racecat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(race_short))) %>%
      filter(!is.na(cat))

    colors = race_short_colors
    labels = race_short

  } else if (group == "income") {
    dat = dat %>% #left_join(inccat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(inc_cat))) %>%
      filter(!is.na(cat))

    colors = inc_cat_colors
    labels = inc_cat

  } else if (group == "ses") {
    dat = dat %>%
      mutate(cat = factor(ses, levels = ses_cat)) %>%
      filter(!is.na(cat))

    colors = ses_cat_colors
    labels = ses_cat

  } else if (group == "period") {
    dat = dat %>%
      mutate(cat = factor(period, levels = period_cat)) %>%
      filter(!is.na(cat))

    colors = period_cat_colors
    labels = period_cat
  } else {
    return("Please select valid group: 'gent', 'ethnoracial', 'income', 'ses', 'period")
  }

  # Filter/order SES
  dat = dat %>%
    filter(ses %in% c("Low", "Moderate", "Middle", "High"))

  dat$ses <- factor(dat$ses,
                    levels = c("Low",
                               "Moderate",
                               "Middle",
                               "High"))
  # Plotting
  plots_all = list()

  foreach(i = 1:length(ses_short)) %do% {
    dat_ses = dat %>%
      dplyr::filter(ses == ses_short[i])

    if (group == "period") {
      plot <-
        ggplot(dat_ses, aes(x = cat, y = {{ var }}, fill = cat)) +
        geom_bar(stat = "identity", position = "stack", width = 0.5) +
        facet_grid(~ period, scales = "free", space = "free") +
        scale_fill_manual(values = colors) +
        scale_y_continuous(limits = limits,
                           expand = c(0, 0),
                           labels = scales::percent) +
        geom_hline(yintercept=0, linetype="dashed") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_blank(),
              axis.title.y = element_text(size = 9),
              axis.title.x = element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 12, hjust = .5),
              plot.caption = element_text(size = 7, hjust = .5, face = "italic"),
              plot.margin = margin(0.2,0.6,0.2,0.6, unit = "cm")) +
        labs(title = ses_short[i], y = y_title)

    } else {
      plot <-
        ggplot(dat_ses, aes(x = cat, y = {{ var }}, fill = cat)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_grid(~ period) +
        scale_fill_manual(values = colors,
                          labels = labels) +
        scale_y_continuous(limits = limits,
                           expand = c(0, 0),
                           labels = scales::percent) +
        geom_hline(yintercept=0, linetype="dashed") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.y = element_text(size = 9),
              axis.title.x = element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 12, hjust = .5),
              plot.caption = element_text(size = 7, hjust = .5, face = "italic"),
              plot.margin = margin(0.2,0.6,0.2,0.6, unit = "cm")) +
        labs(title = paste0(ses_short[i], " SES"), y = y_title) +
        theme(panel.grid.minor.y = element_line(color = "grey80", size = 0.3),
              panel.grid.major.y = element_line(color = "grey80", size = 0.3))
    }
    # add plot to list of grobs
    plots_all = c(plots_all, list(plot))
  }

  # arrange period maps into 4 panels
  layout <- rbind(c(1, 2), c(3, 4))
  plot_panel =
    grid.arrange(plots_all[[1]], plots_all[[2]], plots_all[[3]], plots_all[[4]],
                 nrow = 2, ncol = 2,
                 layout_matrix = layout,
                 heights = c(6, 6),
                 bottom=textGrob(caption, gp=gpar(fontsize=8,font=3)))

  if (save) {
    ggsave(savename, plot_panel, height = 8, width = 10.5)
  }
  return(plot_panel)
}
