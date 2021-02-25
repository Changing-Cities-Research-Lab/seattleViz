#' Produce stacked bar chart panel of landlord location by neighborhood category.
#'
#' This function takes in data and produces a stacked bar chart panel of landlord
#' location grouped by ethnoracial, income, gentrification, 
#'
#' @param dat oak-data-cleaning/County_Assessor/landlords_clean.csv
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Stacked bar chart panel of landlord location by neighborhood category.
#' @export
# Plot bar chart by neighborhood category
landlord_location_stacked_bar <- function(
  dat,
  save = F,
  savename = "plot.png",
  caption = paste0(frb_acs_caption, ses_caption, period_caption)
) {
  
  library(oakViz)
  library(reshape2)
  
  agg = dat %>% 
    mutate(tractid10 = situs_trtid10) %>% 
    aggregate_categories("sum") %>% 
    select(cat, 
           facet, 
           num_total_owners,
           num_owners_within_tract,
           num_owners_within_oak,
           num_owners_within_ba,
           num_owners_within_ca,
           num_owners_outside_ca) %>% 
    # Compute mutually exclusive categories
    mutate(num_owners_within_oak_out_tract = num_owners_within_oak - num_owners_within_tract, 
           num_owners_within_ba_out_oak = num_owners_within_ba - num_owners_within_oak,
           num_owners_within_ca_out_ba = num_owners_within_ca - num_owners_within_ba) %>% 
    # Compute percentages out of total number of owners
    mutate("Within Tract" = num_owners_within_tract/num_total_owners,
           "Outside Tract, within Oakland" = num_owners_within_oak_out_tract/num_total_owners,
           "Outside Oakland, within Bay Area" = num_owners_within_ba_out_oak/num_total_owners,
           "Outside Bay Area, within CA" = num_owners_within_ca_out_ba/num_total_owners,
           "Outside CA" = num_owners_outside_ca/num_total_owners) %>% 
    select(cat,
           facet,
           "Within Tract",
           "Outside Tract, within Oakland",
           "Outside Oakland, within Bay Area",
           "Outside Bay Area, within CA",
           "Outside CA")
  
  # agg$facet[agg$facet == "All"] <- "Ethnoracial"
  
  theme =
    theme_bw() +
    theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 8.5),
      legend.position = "right",
      # Caption
      plot.caption = element_text(size = 10, hjust = .5, face = "italic"),
      # X-axis
      axis.text.x=element_text(size = 8.5, hjust = 1, angle = 45),
      axis.title.x=element_blank(),
      # Y-axis
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size = 10),
      # Background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      # Margins
      plot.margin=unit(c(0,0,0,0), "cm"))
  
  # Test: facet_wrap
  plot_data = agg %>% 
    # Melt data for plotting
    melt(id = c("cat", "facet"))
  
  plot_wrap <-
    ggplot(plot_data, aes(y = value,
                          x = cat,
                          fill = variable)) +
    geom_bar(stat="identity", position = "stack") +
    facet_grid(col = vars(facet), scales = "free", space = "free") +
    scale_y_continuous(expand = c(0, 0.01), labels = scales::percent) +
    scale_fill_manual(values = c("#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494")) +
    theme 
  plot_wrap
  
  if (save) {
    ggsave(savename, plot_wrap, height = 5, width = 8)
  }
  return(plot_wrap)
}

