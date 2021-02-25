#' Produce grouped bar chart by race across 2 periods.
#'
#' This function takes in data and produces a bar chart grouped by race
#'  across two periods (2006-2010 and 2012-2016).
#'
#' @param dat Data with a column containing "race", "periods", and variable of interest
#' @param var Name of column containing variable to plot.
#' @param limits Y-axis limits
#' @param y_title Title to display along y-axis.
#' @param scale_type Y-axis scale type: "numeric" or "percent"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Grouped bar chart by race across 2 periods.
#' @export
# Plot bar chart by race across periods
plot_bar_race <- function(
  dat,
  var,
  limits,
  y_title = NULL,
  scale_type = "percent",
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
) {
  ## Read Data
  library('tidyverse')

  dat = dat %>%
    mutate(cat = factor(race, levels = c(race_cat))) %>%
    filter(!is.na(race))

  colors = race_colors

  if (scale_type == "percent") {
    label_type = scales::percent
  } else if (scale_type == "numeric") {
    label_type = scales::comma
  } else {
    return("Please select percent or numeric")
  }

  plot <-
    ggplot(dat, aes(x = periods, y = {{var}}, fill = cat)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    facet_grid(~ cat, scales = "free", space = "free") +
    scale_fill_manual(values = colors) +
    scale_y_continuous(limits = limits,
                       expand = c(0, 0),
                       labels = label_type) +
    scale_x_discrete(labels=c("2006 - 2010" = "2006-10", "2012 - 2016" = "2012-16")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
          axis.title.y = element_text(size = 9),
          axis.title.x = element_blank(),
          legend.position = "none",
          plot.title = element_blank(),
          plot.caption = element_text(size = 7, hjust = .5, face = "italic")) +
    theme(panel.grid.minor.y = element_line(color = "grey80", size = 0.3),
          panel.grid.major.y = element_line(color = "grey80", size = 0.3)) +
    labs(caption = caption, y = y_title)

  if (save) {
    ggsave(savename, plot, height = 5, width = 7)
  }
  return(plot)
}
