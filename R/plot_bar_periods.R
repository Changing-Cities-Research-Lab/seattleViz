#' Produce grouped bar chart across periods.
#'
#' This function takes in data and produces a bar chart grouped by either
#' gentrification, ethnoracial, income, SES, or period category across four housing periods.
#'
#' @param dat Data with a column containing census tracts and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param limits Y-axis limits
#' @param y_title Title to display along y-axis.
#' @param group Category for x-axis grouping: "gent" (default), "ethnoracial", "race", "income", "ses", or "period"
#' @param scale_type Y-axis scale type: "numeric" or "percent"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Grouped bar chart across periods.
#' @export
# Plot bar chart by group across periods
plot_bar_periods <- function(
  dat,
  var,
  limits,
  y_title = NULL,
  group = "gent", # gent, ethnoracial, income, ses, period
  scale_type = "percent",
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
) {
  ## Read Data
  library('tidyverse')

  if (scale_type == "percent") {
    label_type = scales::percent
  } else if (scale_type == "numeric") {
    label_type = scales::comma
  } else {
    return("Please select percent or numeric")
  }

  # Combine with either gentcat, racecat, inccat, ses, or period
  if (group == "gent") {
    dat = dat %>%
      mutate(cat = factor(cat, levels = c(gent_cat))) %>%
      filter(!is.na(cat))

    colors = gent_cat_colors
    labels = gent_cat

  } else if (group == "ethnoracial") {
    dat = dat %>%
      mutate(cat = factor(cat, levels = c(race_short))) %>%
      filter(!is.na(cat))

    colors = race_short_colors
    labels = race_short

  } else if (group == "race") {
    dat = dat %>%
      mutate(cat = factor(cat, levels = c(race_cat))) %>%
      filter(!is.na(cat))

    colors = race_colors
    labels = race_cat

  } else if (group == "income") {
    dat = dat %>%
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

  # Plotting
  if (group == "period") {
    plot <-
      ggplot(dat, aes(x = cat, y = {{ var }}, fill = cat)) +
      geom_bar(stat = "identity", position = "stack", width = 0.5) +
      facet_grid(~ period, scales = "free", space = "free") +
      scale_fill_manual(values = colors) +
      scale_y_continuous(limits = limits,
                         expand = c(0, 0),
                         labels = label_type) +
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
            plot.title = element_blank(),
            plot.caption = element_text(size = 7, hjust = .5, face = "italic")) +
      labs(caption = caption, y = y_title)

  } else {
    plot <-
      ggplot(dat, aes(x = cat, y = {{ var }}, fill = cat)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid(~ period) +
      scale_fill_manual(values = colors,
                        labels = labels) +
      scale_y_continuous(limits = limits,
                         expand = c(0, 0),
                         labels = label_type) +
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
            plot.title = element_blank(),
            plot.caption = element_text(size = 7, hjust = .5, face = "italic"),
            plot.margin = unit(c(.5,.5,.5,2), "cm")) +
      labs(caption = caption, y = y_title) +
      theme(panel.grid.minor.y = element_line(color = "grey80", size = 0.3),
            panel.grid.major.y = element_line(color = "grey80", size = 0.3))
  }

  height = 5
  width = 7
  if (group == "period") {
    height = 4
    width = 5
  }

  if (save) {
    ggsave(savename, plot, height = height, width = width)
  }
  return(plot)
}
