#' Produce panel of stacked bar charts of SES by a 'periods' column
#'
#' This function takes in data and produces a panel of stacked bar charts,
#' based on different groupings. Intended to be used with oakViz::aggregate_categories()
#'
#' @param dat Data with a column containing variables of interest and grouping variables.
#' @param var Name of column containing variable to plot
#' @param y_title Title to display along y-axis.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Panel of stacked bar charts of different groupings
#' @export

stacked_bar_ses <- function(
  dat,
  var,
  y_title = NULL,
  save = F, 
  savename = "plot.png",
  title = NULL,
  caption = "test"
) {
  
  values = ses_cat_colors
  
  fill_labels = c("Low", "Moderate","Middle", "High")
  
  plot <-
    ggplot(dat, aes(y = {{var}},
                    x = periods,
                    fill = ses,
                    label = scales::percent({{var}}, accuracy = 0.1))) +
    ggtitle(title) +
    geom_bar(stat="identity", position = "stack") +
    geom_text(size = 2.2, position = position_stack(vjust = 0.5)) +
    #facet_grid(cols = vars(cat),
    #           rows = vars(facet)) +
    scale_fill_manual(values = values,
                      labels = fill_labels) +
    scale_y_continuous(expand = c(0, 0.01), labels = scales::percent) +
    theme_bw() +
    theme(
      # Legend Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      # Caption
      plot.caption = element_text(size = 7, hjust = 0),
      # X-axis
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      # Y-axis
      axis.ticks.y=element_blank(),
      axis.title.y=element_text(size = 10),
      axis.text.y=element_text(size = 10),
      # Background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank()
      # Margins
      #plot.margin=unit(c(0,0.5,0,0.5), "cm")
    ) +
    labs(y = y_title, caption = caption)
  
  if (save) {
    ggsave(savename, plot, height = 5, width = 6.8)
  }
  return(plot)
  
}
