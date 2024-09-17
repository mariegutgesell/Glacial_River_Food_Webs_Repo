##Code to create site species matrix of benthic invertebrate biomass data

library(tidyverse)

##Read in benthic invert data
df <- read.csv("data/SEAK_Inverts_IndMeasure_AbunTakeMeanBL.csv") 
  
df$Date <- as.Date(df$Date, "%m/%d/%y")
df <- df %>% mutate(Month_Year = format(Date, "%Y-%m"))

df_na <- df %>%
  filter(is.na(Biomass_Ind)) 
##Calculate total biomass of each family for each date
df_summary <- df %>%
  select(StreamID, Month_Year, Date, Origin.x, Order.x, Family.x, Genus, Biomass_Ind) %>%
  filter(!is.na(Biomass_Ind)) %>% ##why are there NAs? ask Matt
  group_by(StreamID, Month_Year, Date, Origin.x, Order.x, Family.x) %>% ##family is lowest level ID for all taxa
  summarise_at(vars(Biomass_Ind), list(total_biomass = sum)) %>%
  mutate(site_type = case_when(
    startsWith(StreamID, "Herbert") ~ "Glacial",
    startsWith(StreamID, "Steep") ~ "Snow",
    startsWith(StreamID, "Peter") ~ "Rain", 
    startsWith(StreamID, "Montana") ~ "Mixed",
  )) %>%
  ungroup()


# Ensure dataframe is in the correct format -- create array for variance partitioning function
df_summary <- df_summary %>%
  arrange(Family.x, Date, StreamID)
# Create the array
sp_matrix_all <- xtabs(total_biomass ~ Family.x + Date + StreamID, data = df_summary)


##Create array w/ mixed site removed
df_summary_2 <- df_summary %>%
  filter(site_type != "Mixed")
df_summary_2 <- df_summary_2 %>%
  arrange(Family.x, Date, StreamID)
sp_matrix_2 <- xtabs(total_biomass ~ Family.x + Month_Year + StreamID, data = df_summary_2)



##Create a list of arrays with all unique possible site combinations (not including mixed site)
sites <- unique(df_summary_2$StreamID)
site_combinations <- expand.grid(site1 = sites, site2 = sites, site3 = sites)
site_combinations_sorted <- t(apply(site_combinations, 1, sort))
unique_combinations <- unique(as.data.frame(site_combinations_sorted))
colnames(unique_combinations) <- c("site1", "site2", "site3")
# Initialize an empty list to store the xtabs arrays
xtabs_list <- list()

#Loop through each combination and create an xtabs array
for (i in 1:nrow(unique_combinations)) {
  combo <- unique_combinations[i, ]
  selected_sites <- c(combo$site1, combo$site2, combo$site3)

  site_counts <- table(selected_sites)
  
  replicated_data <- data.frame()
  
  #For each site in the combination, replicate its data based on how many times it occurs
  for (site in names(site_counts)) {
    count <- site_counts[[site]]
    site_data <- df_summary_2[df_summary_2$StreamID == site, ]
    
    # Replicate the data according to how many times the site appears in the combination
    for (replicate_number in 1:count) {

      replicated_site_data <- site_data
      replicated_site_data$Replicate_Type <- replicate_number

      replicated_data <- rbind(replicated_data, replicated_site_data) 
    }

  }
  replicated_data <- replicated_data %>%
    mutate(streamID_2 = paste(StreamID, Replicate_Type, sep = "_"))
  #Create an xtabs array for the replicated dataframe
  xtabs_array <- xtabs(total_biomass ~ Family.x + Month_Year + streamID_2, data = replicated_data)
  
  xtabs_list[[paste(combo$site1, combo$site2, combo$site3, sep = "_")]] <- xtabs_array
}



  
  
