#' Produce line graph of variable by periods
#'
#' This function takes in data and produces a line graph of variables by periods,
#' grouped by neighborhood category or race.
#'
#' @param dat Data with a columns containing variable of interest, character periods variable ("periods"), and grouping variable ("cat").
#' @param var Name of variable to plot.
#' @param y_title Title to display along y-axis.
#' @param group Category for color grouping: "race" (default), "ethnoracial", "income", "gent"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Line graph of variable by periods, grouped by race, ethnoracial, income, or gentrification category.
#' @export

line_graph <- function(
  dat,
  var,
  y_title = NULL,
  group = "race",
  save = F,
  savename = "plot.png",
  caption = paste0(acs_caption, "\nUnemployment Estimates were sourced from Catalist's DEEP-MAPS Project.")
) {

  if (group == "race") {
    colors = race_colors
  } else if (group == "ethnoracial") {
    colors = race_short_colors
  } else if (group == "income") {
    colors = inc_cat_colors
  } else if (group == "gentrification") {
    colors = gent_cat_colors
  } else {
    return("Please select 'race', 'ethnoracial', 'income', or 'gentrification'")
  }

  plot = ggplot(dat, aes(x = periods, y = {{var}}, group = cat)) +
    geom_line(aes(color = cat), size = 0.8) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_discrete(expand = c(0.03, 0.03)) +
    scale_color_manual(values = colors) +
    theme_bw() + theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      # Caption
      plot.caption = element_text(size = 9, hjust = .5,face = "italic"),
      # X-axis
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 9),
      # Y-axis
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(size = 9),
      # Background
      panel.grid.minor.y = element_line(color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank()) +
    guides(color = guide_legend(nrow = 1)) +
    labs(y = y_title, caption = caption)

  if (save) {
    ggsave(savename, plot, height = 5, width = 6.8)
  }
  return(plot)
}
