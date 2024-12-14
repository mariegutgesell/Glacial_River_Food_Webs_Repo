##plotting mean biomass combinations overtime

library(tidyverse)
library(ggplot2)
library(ggpubr)

inverts <- readRDS("data/intermediate_data/invert_sp_matrix_stacked_array.rds")
peri <- readRDS("data/intermediate_data/peri_sp_matrix_stacked_array.rds")
det <- readRDS("data/intermediate_data/det_sp_matrix_stacked_array.rds")
fish <- readRDS("data/intermediate_data/fish_sp_matrix_stacked_array.rds")

str(inverts)

invert_list<- lapply(names(inverts), function(name) {
  array_data <- inverts[[name]]
  as.data.frame(as.table(array_data)) %>%
    mutate(Combo = name)  # Use the name of the array as an identifier
})

inverts_df <- bind_rows(invert_list)

inverts_df <- inverts_df %>%
  mutate(combo_type = case_when(
    startsWith(Combo, "Glacier-fed_Glacier-fed_Glacier-fed") ~ "Homogenous",
    startsWith(Combo, "Glacier-fed_Glacier-fed_Rain-fed") ~ "2:1",
    startsWith(Combo, "Glacier-fed_Glacier-fed_Snow-fed") ~ "2:1",
    startsWith(Combo, "Glacier-fed_Rain-fed_Rain-fed") ~ "2:1",
    startsWith(Combo, "Glacier-fed_Rain-fed_Snow-fed") ~ "Heterogenous",
    startsWith(Combo, "Glacier-fed_Snow-fed_Snow-fed") ~ "2:1",
    startsWith(Combo, "Rain-fed_Rain-fed_Rain-fed") ~ "Homogenous",
    startsWith(Combo, "Rain-fed_Rain-fed_Snow-fed") ~ "2:1",
    startsWith(Combo, "Rain-fed_Snow-fed_Snow-fed") ~ "2:1",
    startsWith(Combo, "Snow-fed_Snow-fed_Snow-fed") ~ "Homogenous",
  )) %>%
  mutate(taxa = "inverts") 
  
inverts_df_total_biomass <- inverts_df %>%  
  group_by(taxa, Combo, combo_type, Month_Year) %>%
  summarise_at(vars(Freq), list(total_biomass = sum)) %>%
  rename(Combination = "Combo")




peri_list <- lapply(names(peri), function(name) {
  array_data <- peri[[name]]
  as.data.frame(as.table(array_data)) %>%
    mutate(Combination = name)  # Use the name of the array as an identifier
})

peri_df <- bind_rows(peri_list)

peri_df <- peri_df %>%
  mutate(combo_type = case_when(
    startsWith(Combination, "Glacier-fed_Glacier-fed_Glacier-fed") ~ "Homogenous",
    startsWith(Combination, "Glacier-fed_Glacier-fed_Rain-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Glacier-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Rain-fed_Rain-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Rain-fed_Snow-fed") ~ "Heterogenous",
    startsWith(Combination, "Glacier-fed_Snow-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Rain-fed_Rain-fed_Rain-fed") ~ "Homogenous",
    startsWith(Combination, "Rain-fed_Rain-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Rain-fed_Snow-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Snow-fed_Snow-fed_Snow-fed") ~ "Homogenous",
  )) %>%
  mutate(taxa = "Periphyton")


peri_df_total_biomass <- peri_df %>%  
  group_by(taxa, Combination, combo_type, Month_Year) %>%
  summarise_at(vars(Freq), list(total_biomass = sum))

det_list <- lapply(names(det), function(name) {
  array_data <- det[[name]]
  as.data.frame(as.table(array_data)) %>%
    mutate(Combination = name)  # Use the name of the array as an identifier
})

det_df <- bind_rows(det_list)

det_df <- det_df %>%
  mutate(combo_type = case_when(
    startsWith(Combination, "Glacier-fed_Glacier-fed_Glacier-fed") ~ "Homogenous",
    startsWith(Combination, "Glacier-fed_Glacier-fed_Rain-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Glacier-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Rain-fed_Rain-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Rain-fed_Snow-fed") ~ "Heterogenous",
    startsWith(Combination, "Glacier-fed_Snow-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Rain-fed_Rain-fed_Rain-fed") ~ "Homogenous",
    startsWith(Combination, "Rain-fed_Rain-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Rain-fed_Snow-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Snow-fed_Snow-fed_Snow-fed") ~ "Homogenous",
  )) %>%
  mutate(taxa = "Detritus")

det_df_total_biomass <- det_df %>%  
  group_by(taxa, Combination, combo_type, Month_Year) %>%
  summarise_at(vars(Freq), list(total_biomass = sum))

fish_list <- lapply(names(fish), function(name) {
  array_data <- fish[[name]]
  as.data.frame(as.table(array_data)) %>%
    mutate(Combination = name)  # Use the name of the array as an identifier
})

fish_df <- bind_rows(fish_list)

fish_df <- fish_df %>%
  mutate(combo_type = case_when(
    startsWith(Combination, "Glacier-fed_Glacier-fed_Glacier-fed") ~ "Homogenous",
    startsWith(Combination, "Glacier-fed_Glacier-fed_Rain-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Glacier-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Rain-fed_Rain-fed") ~ "2:1",
    startsWith(Combination, "Glacier-fed_Rain-fed_Snow-fed") ~ "Heterogenous",
    startsWith(Combination, "Glacier-fed_Snow-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Rain-fed_Rain-fed_Rain-fed") ~ "Homogenous",
    startsWith(Combination, "Rain-fed_Rain-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Rain-fed_Snow-fed_Snow-fed") ~ "2:1",
    startsWith(Combination, "Snow-fed_Snow-fed_Snow-fed") ~ "Homogenous",
  )) %>%
  mutate(taxa = "fish")

fish_df_total_biomass <- fish_df %>%  
  group_by(taxa, Combination, combo_type, Month_Year) %>%
  summarise_at(vars(Freq), list(total_biomass = sum))

library(lubridate)


total_biomass_df <- rbind(inverts_df_total_biomass, peri_df_total_biomass, det_df_total_biomass, fish_df_total_biomass) 
total_biomass_df$month <- months(as.Date(paste("01", total_biomass_df$Month_Year, sep = "-"), format = "%d-%Y-%m"), abbr = TRUE)



total_biomass_df_avg <- total_biomass_df %>%
  group_by(taxa, combo_type, Month_Year) %>%
  summarise_at(vars(total_biomass), list(avg_total_biomass = mean))


ggplot(total_biomass_df_avg, aes(x = Month_Year, y = avg_total_biomass, group = combo_type, color = combo_type)) +
  geom_point()+
  geom_line() +
  theme_classic() +
  facet_wrap(~taxa)


total_biomass_df$combo_type <- ordered(total_biomass_df$combo_type,
                                       levels = c("Homogenous", "2:1", "Heterogenous"))  
##Making plot of biomass over time w/ increasing heterogeneity -- all combos -------------
##Fish 
fish_homo_plot <- total_biomass_df %>%
  filter(taxa == "fish" & combo_type == "Homogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "purple")+
  geom_line(color = "purple") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
    ylim(0,5200)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

fish_homo_plot

fish_21_plot <- total_biomass_df %>%
  filter(taxa == "fish" & combo_type == "2:1") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkblue")+
  geom_line(color = "darkblue") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,5200)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

fish_21_plot

fish_hetero_plot <- total_biomass_df %>%
  filter(taxa == "fish" & combo_type == "Heterogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,5200)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")


fish_hetero_plot

fish_plot <- ggarrange(fish_homo_plot, fish_21_plot, fish_hetero_plot,
                   
                       ncol = 3, nrow = 1)
fish_plot
##Invertebrates 
invert_homo_plot <- total_biomass_df %>%
  filter(taxa == "inverts" & combo_type == "Homogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "purple")+
  geom_line(color = "purple") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,5200)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

invert_homo_plot

invert_21_plot <- total_biomass_df %>%
  filter(taxa == "inverts" & combo_type == "2:1") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkblue")+
  geom_line(color = "darkblue") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,5200)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

invert_21_plot

invert_hetero_plot <- total_biomass_df %>%
  filter(taxa == "inverts" & combo_type == "Heterogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,5200)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

invert_hetero_plot

inverts_plot <- ggarrange(invert_homo_plot, invert_21_plot, invert_hetero_plot,
                       
                       ncol = 3, nrow = 1)
inverts_plot
  

##Periphyton
peri_homo_plot <- total_biomass_df %>%
  filter(taxa == "Periphyton" & combo_type == "Homogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "purple")+
  geom_line(color = "purple") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3500)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

peri_homo_plot

peri_21_plot <- total_biomass_df %>%
  filter(taxa == "Periphyton" & combo_type == "2:1") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkblue")+
  geom_line(color = "darkblue") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3500)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

peri_21_plot

peri_hetero_plot <- total_biomass_df %>%
  filter(taxa == "Periphyton" & combo_type == "Heterogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3500)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

peri_hetero_plot

peri_plot <- ggarrange(peri_homo_plot, peri_21_plot, peri_hetero_plot,
                    
                       ncol = 3, nrow = 1)
peri_plot


##Detritus
det_homo_plot <- total_biomass_df %>%
  filter(taxa == "Detritus" & combo_type == "Homogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "purple")+
  geom_line(color = "purple") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,7650)+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
det_homo_plot

det_21_plot <- total_biomass_df %>%
  filter(taxa == "Detritus" & combo_type == "2:1") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkblue")+
  geom_line(color = "darkblue") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,7650)+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
det_21_plot

det_hetero_plot <- total_biomass_df %>%
  filter(taxa == "Detritus" & combo_type == "Heterogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,7650)+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
det_hetero_plot

det_plot <- ggarrange(det_homo_plot, det_21_plot, det_hetero_plot,
                       labels = c("i) Homogenous", "ii) 2:1", "iii) Heterogenous"),
                       ncol = 3, nrow = 1, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
det_plot



all_plot<- ggarrange(peri_plot, inverts_plot, fish_plot,
                     labels = c("a) Periphyton", "b) Detritus", "c) Invertebrates", "d) Fish"),
                     ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
all_plot


##Plots with only rain-rain-snow etc.--------------------
##Making plot of biomass over time w/ increasing heterogeneity 
total_biomass_df_2 <- total_biomass_df %>%
   filter(Combination %in% c("Glacier-fed_Rain-fed_Snow-fed", "Rain-fed_Rain-fed_Rain-fed", "Rain-fed_Snow-fed_Snow-fed")) 


  
##Fish 
fish_homo_plot <- total_biomass_df_2 %>%
  filter(taxa == "fish" & combo_type == "Homogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "purple")+
  geom_line(color = "purple") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3000)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")
fish_homo_plot

fish_21_plot <- total_biomass_df_2 %>%
  filter(taxa == "fish" & combo_type == "2:1") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkblue")+
  geom_line(color = "darkblue") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3000)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

fish_21_plot

fish_hetero_plot <- total_biomass_df_2 %>%
  filter(taxa == "fish" & combo_type == "Heterogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3000)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

 fish_hetero_plot

fish_plot <- ggarrange(fish_homo_plot, fish_21_plot, fish_hetero_plot,
                       ncol = 3, nrow = 1)
fish_plot
##Invertebrates 
invert_homo_plot <- total_biomass_df_2 %>%
  filter(taxa == "inverts" & combo_type == "Homogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "purple")+
  geom_line(color = "purple") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,6000)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

invert_homo_plot

invert_21_plot <- total_biomass_df_2 %>%
  filter(taxa == "inverts" & combo_type == "2:1") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkblue")+
  geom_line(color = "darkblue") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,6000)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

invert_21_plot

invert_hetero_plot <- total_biomass_df_2 %>%
  filter(taxa == "inverts" & combo_type == "Heterogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,6000)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

invert_hetero_plot

inverts_plot <- ggarrange(invert_homo_plot, invert_21_plot, invert_hetero_plot,
                          ncol = 3, nrow = 1)
inverts_plot


##Periphyton
peri_homo_plot <- total_biomass_df_2 %>%
  filter(taxa == "Periphyton" & combo_type == "Homogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "purple")+
  geom_line(color = "purple") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3500)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

peri_homo_plot

peri_21_plot <- total_biomass_df_2 %>%
  filter(taxa == "Periphyton" & combo_type == "2:1") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkblue")+
  geom_line(color = "darkblue") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3500)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

peri_21_plot

peri_hetero_plot <- total_biomass_df_2 %>%
  filter(taxa == "Periphyton" & combo_type == "Heterogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3500)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none")

peri_hetero_plot

peri_plot <- ggarrange(peri_homo_plot, peri_21_plot, peri_hetero_plot,
                       ncol = 3, nrow = 1)
peri_plot


##Detritus
det_homo_plot <- total_biomass_df_2 %>%
  filter(taxa == "Detritus" & combo_type == "Homogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "purple")+
  geom_line(color = "purple") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,7650)+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
det_homo_plot

det_21_plot <- total_biomass_df_2 %>%
  filter(taxa == "Detritus" & combo_type == "2:1") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkblue")+
  geom_line(color = "darkblue") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,7650)+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
det_21_plot

det_hetero_plot <- total_biomass_df_2 %>%
  filter(taxa == "Detritus" & combo_type == "Heterogenous") %>%
  ggplot(aes(x = Month_Year, y = total_biomass, group = Combination)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen") +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,7650)+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
det_hetero_plot

det_plot <- ggarrange(det_homo_plot, det_21_plot, det_hetero_plot,
                      labels = c("i) Homogenous", "ii) 2:1", "iii) Heterogenous"),
                      ncol = 3, nrow = 1, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
det_plot



all_plot<- ggarrange(peri_plot, inverts_plot, fish_plot,
                     labels = c("a) Periphyton",  "b) Invertebrates", "c) Fish"),
                     ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
all_plot




  


## testing out adding community level time series to plots  ----------------------
library(readxl)
fish_df <- read_excel("data/SEAK_Fish_Biomass_Supplement.xlsx")

##Add month_date to standardize to sampling month 
fish_df$Date <- as.Date(fish_df$Date, "%Y-%m-%d")
fish_df <- fish_df %>% mutate(Month_Year = format(Date, "%Y-%m")) %>%
  filter(Stream != "Transitional") %>%
  group_by(Stream, Month_Year) %>%
  summarise_at(vars(Biomass), list(total_biomass = sum)) %>%
  rename(site_type = "Stream") %>%
  filter(Month_Year < "2019-01")

fish_df$month <- months(as.Date(paste("01", fish_df$Month_Year, sep = "-"), format = "%d-%Y-%m"), abbr = TRUE)


##Homogenous 
fish_homo_lc <- fish_df %>%
  filter(site_type == "Rain-fed") 

fish_homo_lc <- bind_rows(
  fish_homo_lc %>% mutate(rep = 1),
  fish_homo_lc %>% mutate(rep = 2),
  fish_homo_lc %>% mutate(rep = 3)
)

fish_homo_mc <- total_biomass_df_2 %>%
  filter(taxa == "fish" & combo_type == "Homogenous") 

fish_homo_plot <- ggplot()+
  #geom_point(data = fish_homo_lc, aes(x = Month_Year, y = total_biomass, group = rep, color = site_type), color = "purple", alpha = 0.6)+
  geom_line(data = fish_homo_lc, aes(x = Month_Year, y = total_biomass, group = rep, color = site_type),color = "purple", alpha = 0.6, position = "jitter") +
#  geom_point(data = fish_homo_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "purple", size =2)+
  geom_line(data = fish_homo_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "purple", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,2700)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = fish_homo_mc$month)

fish_homo_plot

##2:1
fish_21_rain <- fish_df %>%
  filter(site_type == "Rain-fed")

fish_21_snow <- fish_df %>%
  filter(site_type == "Snow-fed") 

fish_21_snow <- bind_rows(
  fish_21_snow %>% mutate(rep = 1),
  fish_21_snow %>% mutate(rep = 2)
)

fish_21_mc <- total_biomass_df_2 %>%
  filter(taxa == "fish" & combo_type == "2:1") 

fish_21_plot <- ggplot()+
 # geom_point(data = fish_21_lc, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type), color = "darkblue", alpha = 0.6)+
  geom_line(data = fish_21_rain, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkblue", alpha = 0.6) +
  geom_line(data = fish_21_snow, aes(x = Month_Year, y = total_biomass, group = rep, color = site_type),color = "darkblue", alpha = 0.6, position = "jitter", linetype = "dashed") +
 # geom_point(data = fish_21_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "darkblue", size =2)+
  geom_line(data = fish_21_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "darkblue", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,2700)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = fish_21_mc$month)

fish_21_plot

##Heterogenous
fish_het_rain<- fish_df %>%
  filter(site_type == "Rain-fed")
fish_het_snow<- fish_df %>%
  filter(site_type == "Snow-fed")
fish_het_glacier<- fish_df %>%
  filter(site_type == "Glacier-fed")

fish_het_mc <- total_biomass_df_2 %>%
  filter(taxa == "fish" & combo_type == "Heterogenous") 

fish_het_plot <- ggplot()+
 # geom_point(data = fish_het_lc, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type), color = "darkgreen", alpha = 0.6)+
  geom_line(data = fish_het_rain, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6) +
  geom_line(data = fish_het_snow, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6, linetype = "dashed") +
  geom_line(data = fish_het_glacier, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6, linetype = "dotted") +
#  geom_point(data = fish_het_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "darkgreen", size =2)+
  geom_line(data = fish_het_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "darkgreen", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,2700)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = fish_het_mc$month)

fish_het_plot




fish_plot <- ggarrange(fish_homo_plot, fish_21_plot, fish_het_plot,
                          ncol = 3, nrow = 1)
fish_plot


##inverts
df <- read.csv("data/SEAK_Inverts_IndMeasure_AbunTakeMean_Repaired.csv") 

df$Date <- as.Date(df$Date, "%m/%d/%y")
df <- df %>% mutate(Month_Year = format(Date, "%Y-%m"))

df_na <- df %>%
  filter(is.na(Biomass_Ind)) 

##Calculate total biomass of each family for each date
invert_df_total <- df %>%
  select(StreamID, Month_Year, Date, Origin.x, Order.x, Family.x, Genus, Biomass_Ind) %>%
  filter(Origin.x == "Aquatic") %>%
  group_by(StreamID, Month_Year, Date, Origin.x, Order.x, Family.x) %>% ##family is lowest level ID for all taxa
  summarise_at(vars(Biomass_Ind), list(sp_total_biomass = sum)) %>%
  mutate(site_type = case_when(
    startsWith(StreamID, "Herbert") ~ "Glacier-fed",
    startsWith(StreamID, "Steep") ~ "Snow-fed",
    startsWith(StreamID, "Peter") ~ "Rain-fed", 
    startsWith(StreamID, "Montana") ~ "Mixed",
  )) %>%
  ungroup()%>%
  group_by(site_type, StreamID, Month_Year, Date) %>%
  summarise_at(vars(sp_total_biomass), list(total_biomass = sum)) %>%
  filter(site_type != "Mixed")%>%
  filter(Month_Year < "2019-01")

##Homogenous 
invert_homo_lc <- invert_df_total %>%
  filter(site_type == "Rain-fed")

invert_homo_lc <- bind_rows(
  invert_homo_lc %>% mutate(rep = 1),
  invert_homo_lc %>% mutate(rep = 2),
  invert_homo_lc %>% mutate(rep = 3)
)

invert_homo_mc <- total_biomass_df_2 %>%
  filter(taxa == "inverts" & combo_type == "Homogenous") 

invert_homo_plot <- ggplot()+
#  geom_point(data = invert_homo_lc, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type), color = "purple", alpha = 0.6)+
  geom_line(data = invert_homo_lc, aes(x = Month_Year, y = total_biomass, group = rep, color = site_type),color = "purple", alpha = 0.6, position = "jitter") +
#  geom_point(data = invert_homo_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "purple", size =2)+
  geom_line(data = invert_homo_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "purple", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,6000)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = invert_homo_mc$month)

invert_homo_plot

##2:1
invert_21_rain <- invert_df_total %>%
  filter(site_type == "Rain-fed")

invert_21_snow <- invert_df_total %>%
  filter(site_type == "Snow-fed")
invert_21_snow <- bind_rows(
  invert_21_snow %>% mutate(rep = 1),
  invert_21_snow %>% mutate(rep = 2)
)

invert_21_mc <- total_biomass_df_2 %>%
  filter(taxa == "inverts" & combo_type == "2:1") 

invert_21_plot <- ggplot()+
#  geom_point(data = invert_21_lc, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type), color = "darkblue", alpha = 0.6)+
  geom_line(data = invert_21_rain, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkblue", alpha = 0.6) +
  geom_line(data = invert_21_snow, aes(x = Month_Year, y = total_biomass, group = rep, color = site_type),color = "darkblue", alpha = 0.6, position = "jitter", linetype = "dashed") +
 # geom_point(data = invert_21_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "darkblue", size =2)+
  geom_line(data = invert_21_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "darkblue", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,6000)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = invert_21_mc$month)

invert_21_plot

##Heterogenous
invert_het_rain <- invert_df_total %>%
  filter(site_type == "Rain-fed")
invert_het_snow <- invert_df_total %>%
  filter(site_type == "Snow-fed")
invert_het_glacier <- invert_df_total %>%
  filter(site_type == "Glacier-fed")


invert_het_mc <- total_biomass_df_2 %>%
  filter(taxa == "inverts" & combo_type == "Heterogenous") 

invert_het_plot <- ggplot()+
#  geom_point(data = invert_het_lc, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type), color = "darkgreen", alpha = 0.6)+
  geom_line(data = invert_het_rain, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6) +
  geom_line(data = invert_het_snow, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6, linetype = "dashed") +
  geom_line(data = invert_het_glacier, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6, linetype = "dotted") +
#  geom_point(data = invert_het_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "darkgreen", size =2)+
  geom_line(data = invert_het_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "darkgreen", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,6000)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = invert_het_mc$month)

invert_het_plot




inverts_plot <- ggarrange(invert_homo_plot, invert_21_plot, invert_het_plot,
                          ncol = 3, nrow = 1)
inverts_plot

##Periphyton

peri_df <- read.csv("data/SEAK_Periphyton_Summary_Data.csv")
##Add month_date to standardize to sampling month 
peri_df$Date <- as.Date(peri_df$Date, "%Y-%m-%d")
peri_df_total <- peri_df %>% mutate(Month_Year = format(Date, "%Y-%m")) %>%
  filter(Site != "Transitional") %>%
  select(Site:Month_Year) %>%
  mutate(Taxa = "Periphyton") %>%
  rename(site_type = "Site", total_biomass = "meanAFDM") %>%
  filter(Month_Year < "2019-01")


##Homogenous 
peri_homo_lc <- peri_df_total %>%
  filter(site_type == "Rain-fed")
peri_homo_lc <- bind_rows(
  peri_homo_lc %>% mutate(rep = 1),
  peri_homo_lc %>% mutate(rep = 2),
  peri_homo_lc %>% mutate(rep = 3)
)


peri_homo_mc <- total_biomass_df_2 %>%
  filter(taxa == "Periphyton" & combo_type == "Homogenous") 

peri_homo_plot <- ggplot()+
  #geom_point(data = peri_homo_lc, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type), color = "purple", alpha = 0.6)+
  geom_line(data = peri_homo_lc, aes(x = Month_Year, y = total_biomass, group = rep, color = site_type),color = "purple", alpha = 0.6, position = "jitter") +
 # geom_point(data = peri_homo_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "purple", size =2)+
  geom_line(data = peri_homo_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "purple", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3200)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = peri_homo_mc$month)

peri_homo_plot

##2:1
peri_21_rain <- peri_df_total %>%
  filter(site_type == "Rain-fed")
peri_21_snow <- peri_df_total %>%
  filter(site_type == "Snow-fed")

peri_21_snow <- bind_rows(
  peri_21_snow %>% mutate(rep = 1),
  peri_21_snow %>% mutate(rep = 2)
)

peri_21_mc <- total_biomass_df_2 %>%
  filter(taxa == "Periphyton" & combo_type == "2:1") 

peri_21_plot <- ggplot()+
 # geom_point(data = peri_21_lc, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type), color = "darkblue", alpha = 0.6)+
  geom_line(data = peri_21_rain, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkblue", alpha = 0.6) +
  geom_line(data = peri_21_snow, aes(x = Month_Year, y = total_biomass, group = rep, color = site_type),color = "darkblue", alpha = 0.6, position = "jitter", linetype = "dashed") +
  #geom_point(data = peri_21_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "darkblue", size =2)+
  geom_line(data = peri_21_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "darkblue", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3200)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = peri_21_mc$month)

peri_21_plot

##Heterogenous
peri_het_rain <- peri_df_total  %>%
  filter(site_type == "Rain-fed")
peri_het_snow <- peri_df_total  %>%
  filter(site_type == "Snow-fed")
peri_het_glacier <- peri_df_total  %>%
  filter(site_type == "Glacier-fed")

peri_het_mc <- total_biomass_df_2 %>%
  filter(taxa == "Periphyton" & combo_type == "Heterogenous") 

peri_het_plot <- ggplot()+
#  geom_point(data = peri_het_lc, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type), color = "darkgreen", alpha = 0.6)+
  geom_line(data = peri_het_rain, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6) +
  geom_line(data = peri_het_snow, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6, linetype = "dashed") +
  geom_line(data = peri_het_glacier, aes(x = Month_Year, y = total_biomass, group = site_type, color = site_type),color = "darkgreen", alpha = 0.6, linetype = "dotted") +
#  geom_point(data = peri_het_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type), color = "darkgreen", size =2)+
  geom_line(data = peri_het_mc, aes(x = Month_Year, y = total_biomass, group = combo_type, color = combo_type),color = "darkgreen", linewidth=1) +
  theme_classic() +
  xlab("Date") +
  ylab("Total Biomass (g)") +
  ylim(0,3200)+
  theme(axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), legend.position  = "none") +
  scale_x_discrete(labels = peri_het_mc$month)

peri_het_plot




peri_plot <- ggarrange(peri_homo_plot, peri_21_plot, peri_het_plot,
                          ncol = 3, nrow = 1)
peri_plot


all_plot<- ggarrange(peri_plot, inverts_plot, fish_plot,

                     ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
all_plot
