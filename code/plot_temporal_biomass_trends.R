##Plotting seasonal insect community biomass

source("code/sp_matrix.R")
##Plot time series of family biomass for each stream 





##calculate total biomass
df_total <- df_summary %>%
  group_by(site_type, StreamID, Month_Year, Date) %>%
  summarise_at(vars(total_biomass), list(total_biomass = sum))


ggplot() +
  geom_line(data = df_summary, aes(x = Date, y = total_biomass, color = Family.x)) +
  geom_line(data = df_total, aes(x = Date, y = total_biomass)) + 
  geom_line() +
  scale_y_log10()+
  theme_classic() +
  facet_wrap(~StreamID)

ggplot(df_total, aes(x = Date, y = total_biomass, color = site_type)) +
  geom_line()+
  scale_y_log10() +
  theme_classic()

