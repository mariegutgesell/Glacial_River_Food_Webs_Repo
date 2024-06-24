##Code to create site species matrix of benthic invertebrate biomass data

library(tidyverse)

##Read in benthic invert data
df <- read.csv("data/SEAK_Inverts_IndMeasure_AbunTakeMeanBL.csv")


##Calculate total biomass of each family for each date
df_summary <- df %>%
  select(StreamID, Date, Origin.x, Order.x, Family.x, Genus, Biomass_Ind) %>%
  group_by(StreamID, Date, Origin.x, Order.x, Family.x) %>% ##family is lowest level ID for all taxa
  summarise_at(vars(Biomass_Ind), list(total_biomass = sum))

df_summary$Date <- as.Date(df_summary$Date, "%m/%d/%y")


##Plot time series of family biomass for each stream 
ggplot(df_summary, aes(x = Date, y = total_biomass, color = Family.x)) +
  geom_line() +
  theme_classic() +
  facet_wrap(~StreamID)


# Ensure dataframe is in the correct format
df_summary <- df_summary %>%
  arrange(Family.x, Date, StreamID)

# Create the array
sp_matrix <- xtabs(total_biomass ~ Family.x + Date + StreamID, data = df_summary)


##I think blanks are already 0..indicating no abundance at particular time
##Still need to go through and see if true 0's or a missed sampling event
