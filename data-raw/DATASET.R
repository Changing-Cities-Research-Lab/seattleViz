
# COLORS/LABELS
# (used in plot_lollipop(), plot_bar_periods(), and plot_bar_periods_ses(), aggregate_categories())
gent_cat_colors <-
  c("snow3","#d94801", "#fa7b00", "#fdcc8a", "#a6d894")
gent_cat <- c("Nongentrifiable", "Intense", "Moderate", "Weak", "People or Price")
names(gent_cat_colors) <- gent_cat

# refers to ethnoracial category
race_short_colors <-
  c("#481567FF", "#33638DDF", "#FDE725FF", "#20A387FF")
race_short <- c("Predominantly Black", "Black-Other", "White/White-Mixed", "Multiethnic/Other")
names(race_short_colors) <- race_short

# refers to race category
race_colors <-
  c("#17202A", "#DE3163", "#FFBF00", "#DFFF00", "#9FE2BF", "#6495ED")
race_cat <- c("Overall", "Asian", "Black", "Hispanic", "White", "Other")
names(race_colors) <- race_cat

inc_cat_colors <-
  c("#c7cff2","#8897db","#697fe0","#4c66d9","#1437cc")
inc_cat <- c("Bottom Quintile", "Second Quintile", "Middle Quintile", "Fourth Quintile", "Top Quintile")
names(inc_cat_colors) <- inc_cat

ses_cat_colors <-
  c("#9b9b9b", "#fcbba1", "#fc9272", "#faab8c","#fb6a4a", "#b63b36")
ses_cat = c("All", "Low", "Moderate", "LMM" ,"Middle", "High")
ses_short = c("Low", "Moderate", "Middle", "High")
names(ses_cat_colors) <- ses_cat

# " Moderate" includes a space to differentiate from "Moderate" gentrification
# Used when ordering cat levels for lollipop plots
ses_lollipop_colors <-
  c("#9b9b9b", "#fcbba1", "#fc9272", "#faab8c","#fb6a4a", "#b63b36")
ses_lollipop_cat = c("All", "Low", " Moderate", "LMM" ,"Middle", "High")
ses_lollipop_short = c("Low", " Moderate", "Middle", "High")
names(ses_lollipop_colors) <- ses_lollipop_cat

period_cat_colors <-
  c("#46aac8", "#46aac8", "#46aac8", "#46aac8")
period_cat = c("Boom", "Bust", "Recovery", "Post-Recovery")
names(period_cat_colors) <- period_cat

move_cat_colors = c("Moved out of Bay Area" = "#8baf3e",
           "Different City within Bay Area" = "#fdbd3b",
           "Moved within Oakland" = "#2e5e8b")

dest_colors = c("Outside Bay Area" = "#d53e4f",
                "South Bay" = "#fc8d59",
                "San Francisco" = "#fee08b",
                "North Bay" = "#ffffbf",
                "Contra Costa" = "#e6f598",
                "Alameda" = "#99d594",
                "Within Oakland" = "#3288bd")

# LABELS/ORDERING
# used in plot_bar_ses() and stacked_bar()
relabel_gent_cat <- c("nongentrifiable" = "Nongentrifiable",
                      "gentrifying" = "Gentrifying",
                      "intense"  = "Intense",
                      "moderate" = "Moderate",
                      "earlygent" = "Early Gentrification",
                      "weak" = "Weak",
                      "peoplepricegent" = "People or Price")

gent_cat_plot_order <- c("Nongentrifiable", "Gentrifying",
                         "Intense", "Moderate",
                         "Early Gentrification", "Weak", "People or Price")

relabel_race_cat <- c("PredWhite" = "Predominantly White",
                      "PredBlack" = "Predominantly Black",
                      "PredOther"  = "Predominantly Other",
                      "WhiteOther" = "White-Other",
                      "BlackWhite" = "Black-White",
                      "BlackOther" = "Black-Other",
                      "Multiethnic" = "Multiethnic",
                      "Overall" = "Overall",
                      "WhiteMixed" = "White/White-Mixed",
                      "MixedOther" = "Multiethnic/Other")

relabel_move_cat <- c("moved_outba_pct"="Moved out of Bay Area",
                      "diff_city_ba_pct" = "Different City within Bay Area",
                      "moved_within_oak_pct" = "Moved within Oakland")

relabel_dest_cat <- c("outmigration_outba_pct" = "Outside Bay Area",
                      "withinoakmigration_pct" = "Within Oakland",
                      "outmigration_alameda_pct" = "Alameda",
                      "outmigration_contracosta_pct" = "Contra Costa",
                      "outmigration_northbay_pct" = "North Bay",
                      "outmigration_sanfran_pct" = "San Francisco",
                      "outmigration_southbay_pct" = "South Bay")

race_cat_plot_order <- c("Predominantly White", "Predominantly Black",
                         "Predominantly Other","White-Other","Black-White","Black-Other","Multiethnic",
                         "White/White-Mixed", "Multiethnic/Other")

inc_cat_plot_order <- c("Bottom Quintile", "Second Quintile", "Middle Quintile",
                        "Fourth Quintile", "Top Quintile")

move_order <- c("Moved out of Bay Area",
                "Different City within Bay Area",
                "Moved within Oakland")

dest_order <- c("Outside Bay Area",
                "South Bay",
                "San Francisco",
                "North Bay",
                "Contra Costa",
                "Alameda",
                "Within Oakland")

# READ IN DATA
# Oakland tractids
oak_ids <- readr::read_csv("../../oak-data-repo/oakland_geographies/trtid10_oak.csv")

# Bay Area tractids
bay_ids <- readr::read_csv("../../oak-data-repo/oakland_geographies/trtid10_bayarea.csv")

# gentrification data
gentcat <- read_csv("../../oak-data-repo/gentrification_categories/gentcat_006a_50_oak.csv") %>%
  select(tractid10 = trtid10, cat = gentcat_006a_50)
gentcat$cat <- plyr::revalue(gentcat$cat, relabel_gent_cat)
gentcat$cat <- factor(gentcat$cat, levels = gent_cat_plot_order)
gentcat$facet = "Gentrification"

# race data
racecat <- read_csv("../../oak-data-repo/ethnoracial_composition/racetypology_oak_tracts_00.csv") %>%
  select(tractid10 = trtid10, cat = race.shortcategory00)
racecat$cat <- plyr::revalue(racecat$cat, relabel_race_cat)
racecat$cat <- factor(racecat$cat, levels = race_cat_plot_order)
racecat$facet = "Ethnoracial"

# income data
inccat <- read_csv("../../oak-data-repo/income_categories/hinc09_categories.csv")
inccat$cat <- factor(inccat$cat, levels = inc_cat_plot_order)
inccat$facet = "Income"

# city shapefiles
cities <- read_csv("../../oak-data-repo/oakland_geographies/census_2010b_tracts_places_ca.csv")

# Oakland tracts data
oak_tracts <- oak_ids %>%
  select(tractid10 = trtid10)

# CAPTIONS
ses_caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+."
period_caption = "\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017."
frb_caption = "\nSource: Federal Reserve Bank of New York Consumer Credit Panel/Equifax Data."
frb_acs_caption = "\nSource: Federal Reserve Bank of New York Consumer Credit Panel/Equifax Data and 2000 US Census, 2005-2009 ACS, and 2012-2016 ACS."
frb_acs_caption_splitline = "\nSource: Federal Reserve Bank of New York Consumer Credit Panel/Equifax Data\nand 2000 US Census, 2005-2009 ACS, and 2012-2016 ACS."
acs_caption = "\nSource: 2000 US Census, 2005-2009 ACS, and 2012-2016 ACS."

usethis::use_data(gent_cat_colors,
                  gent_cat,
                  race_short_colors,
                  race_short,
                  race_colors,
                  race_cat,
                  inc_cat_colors,
                  inc_cat,
                  ses_cat_colors,
                  ses_cat,
                  ses_short,
                  ses_lollipop_colors,
                  ses_lollipop_cat,
                  ses_lollipop_short,
                  period_cat_colors,
                  period_cat,
                  move_cat_colors,
                  dest_colors,
                  relabel_gent_cat,
                  gent_cat_plot_order,
                  relabel_race_cat,
                  relabel_move_cat,
                  relabel_dest_cat,
                  race_cat_plot_order,
                  inc_cat_plot_order,
                  move_order,
                  dest_order,
                  oak_ids,
                  bay_ids,
                  gentcat,
                  racecat,
                  inccat,
                  cities,
                  oak_tracts,
                  ses_caption,
                  period_caption,
                  frb_caption,
                  frb_acs_caption,
                  frb_acs_caption_splitline,
                  acs_caption,
                  overwrite = TRUE, internal = TRUE)
