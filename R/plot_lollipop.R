#' Produce lollipop plot by ethnoracial, income, and gentrification category.
#'
#' This function takes in data and produces a horizontal lollipop plot by
#' ethnoracial, income, and gentrification category. The order of these categories
#' can be adjusted by changing the factor levels of the facet variable. Input data
#' needs columns for variable of interest (titled "var") and tract number (titled
#' "tractid10"). Intended to be used after oakViz::aggregate_categories().
#'
#' @param data Data with column for variable of interest.
#' @param var Column name of variable of interest.
#' @param limits Y-axis limits.
#' @param panel If T, creates horizontal panel of 2 plots
#' @param var2 Column name of second variable of interest, if panel = T
#' @param panel_titles Vector of 2 strings containing titles for plots in panel
#' @param x_title Title to display along x-axis
#' @param scale_type Y-axis scale type: "numeric" or "percent"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Lollipop plot of variable by ethnoracial, income, and gentrification category, with optional columns by SES.
#' @export
# Lollipop Plot
plot_lollipop <- function(
  data,
  var,
  limits,
  panel = F,
  var2 = NULL,
  panel_titles = NULL,
  x_title = NULL,
  scale_type = "numeric",
  save = F,
  savename = "plot.png",
  caption = paste0(frb_acs_caption_splitline, ses_caption)
) {
  library('tidyverse')

  labels = c("Overall", gent_cat, race_short, inc_cat, ses_lollipop_cat)
  colors = c("white", gent_cat_colors, race_short_colors, inc_cat_colors, ses_lollipop_colors)
  names(colors) = labels

  if (scale_type == "percent") {
    label_type = scales::percent
  } else if (scale_type == "numeric") {
    label_type = scales::comma
  } else {
    return("Please select percent or numeric")
  }

  # Have line segment start at 0
  ystart = 0
  if (limits[1] > 0) {
    ystart = limits[1]
  }

  plot <-
    ggplot(data, aes(x = cat, y = {{var}}, fill = cat)) +
    geom_segment(aes(x=cat, xend=cat,
                     y=ystart, yend={{var}}), size=0.25,
                 show.legend = FALSE) +
    geom_point(aes(color = factor(cat)), size = 3.25, shape = 21,
               colour = "black", show.legend = TRUE) +
    geom_hline(yintercept=0, linetype="dashed") +
    scale_y_continuous(limits = limits,
                       expand = c(0, 0),
                       labels = label_type) +
    scale_color_manual(values = colors,
                       labels = labels) +
    scale_fill_manual(values = colors) +
    facet_grid(rows = vars(facet),
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
      plot.caption = element_text(size = 4.5, hjust = .5, face = "italic")) +
    labs(y = x_title, caption = caption) +
    coord_flip()

  # Create horizontal panel of two plots if panel = T
  if (panel) {
    # Add title to first plot
    plot = plot +
      labs(title = panel_titles[1], caption = NULL) +
      theme(plot.title = element_text(hjust = .5))

    # Create second plot
    plot2 <-
      ggplot(data, aes(x = cat, y = {{var2}}, fill = cat)) +
      geom_segment(aes(x=cat, xend=cat,
                       y=ystart, yend={{var2}}), size=0.25,
                   show.legend = FALSE) +
      geom_point(aes(color = factor(cat)), size = 3.25, shape = 21,
                 colour = "black", show.legend = TRUE) +
      geom_hline(yintercept=0, linetype="dashed") +
      scale_y_continuous(limits = limits,
                         expand = c(0, 0),
                         labels = label_type) +
      scale_color_manual(values = colors,
                         labels = labels) +
      scale_fill_manual(values = colors) +
      facet_grid(rows = vars(facet),
                 scale = "free",
                 space = "free") +
      theme_bw() +
      # Remove neighborhood sub-labels on left y-axis
      theme(
        # Title
        plot.title = element_text(hjust = .5),
        # Panel
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        # Axis
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_text(size = 9),
        # Legend
        legend.position = "none",
        # Caption
        plot.caption = element_text(size = 4.5, hjust = .5, face = "italic")) +
      labs(y = x_title, title = panel_titles[2]) +
      coord_flip()

    # Create panel
    layout <- rbind(c(1, 2))
    final_plot =
      grid.arrange(plot, plot2,
                   nrow = 1, ncol = 2,
                   layout_matrix = layout,
                   widths = c(4, 3),
                   bottom=textGrob(caption, gp=gpar(fontsize=9,font=3)))
    width = 7.5
    height = 4.5
  } else {
    final_plot = plot
    height = 4.5
    width = 4.5
  }

  if (save) {
    ggsave(savename, final_plot, height = height, width = width)
    return(final_plot)
  } else {
    return(final_plot)
  }
}


