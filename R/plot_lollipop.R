#' Produce lollipop plot by King County HRA.
#'
#' This function takes in data and produces a horizontal lollipop plot by
#' King County HRA The order of these categories can be adjusted by changing 
#' the factor levels of the facet variable. Input data needs columns for 
#' variable of interest (titled "var") and HRA.
#'
#' @param data Data with column for variable of interest with "facet" and "facet_col"
#' @param var Column name of variable of interest.
#' @param limits Y-axis limits.
#' @param title Plot title
#' @param x_title Title to display along x-axis
#' @param scale_type Y-axis scale type: "numeric" or "percent"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Lollipop plot of variable by HRA and SES.
#' @export

# Lollipop Plot
plot_lollipop <- function(
  data,
  var,
  limits,
  title = NULL,
  x_title = NULL,
  scale_type = "numeric",
  save = F,
  savename = "plot.png",
  caption = paste0(frb_acs_caption_splitline, ses_caption)
) {
  library('tidyverse')
  
  #labels = c("Overall", gent_cat, race_short, inc_cat, ses_lollipop_cat)
  #colors = c("white", gent_cat_colors, race_short_colors, inc_cat_colors, ses_lollipop_colors)
  #names(colors) = labels
  
  if (scale_type == "percent") {
    label_type = scales::percent
  } else if (scale_type == "numeric") {
    label_type = scales::comma
  } else if (scale_type == "dollar") {
    label_type = scales::dollar
  } else if (scale_type == "dollark") {
    label_type = function(x) {scales::dollar(x, scale = 0.001, suffix = "k")}
  } else {
    return("Please select percent or numeric")
  }
  
  # Have line segment start at 0
  ystart = 0
  if (limits[1] > 0) {
    ystart = limits[1]
  }
  
  plot <-
    ggplot(data, aes(x = cat, y = {{var}}, fill = facet_col)) +
    ggtitle(title) +
    geom_segment(aes(x=cat, xend=cat,
                     y=ystart, yend={{var}}), size=0.25,
                 show.legend = FALSE) +
    geom_point(aes(color = factor(cat)), size = 3.25, shape = 21,
               colour = "black", show.legend = TRUE) +
    geom_hline(yintercept=0, linetype="dashed") +
    scale_y_continuous(limits = limits,
                       expand = c(0, 0),
                       labels = label_type) +
    #scale_color_manual(values = colors,
    #                   labels = labels) +
    scale_fill_manual(values = ses_cat_colors) +
    facet_grid(rows = vars(facet),
               cols = vars(facet_col),
               scale = "free",
               space = "free") +
    theme_bw() +
    theme(
      # Panel
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.spacing.x = unit(1, "lines"),
      # Axis
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      axis.title.x = element_text(size = 9),
      # Legend
      legend.position = "none",
      # Caption
      plot.caption = element_text(size = 7, hjust = 0)) +
    labs(y = x_title, caption = caption) +
    coord_flip()
  
  final_plot = plot
  height = 7
  width = 9
  
  if (save) {
    ggsave(savename, final_plot, height = height, width = width)
  }
  
  return(final_plot)
}
