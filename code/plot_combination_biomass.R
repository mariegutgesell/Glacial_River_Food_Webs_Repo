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
    startsWith("Herbert River_Herbert River_Herbert River", Combo) ~ "Homogenous",
    startsWith("Herbert River_Herbert River_Peterson Creek", Combo) ~ "2:1",
    startsWith("Herbert River_Herbert River_Steep Creek", Combo) ~ "2:1",
    startsWith("Herbert River_Peterson.Creek_Peterson Creek", Combo) ~ "2:1",
    startsWith("Herbert River_Steep Creek_Steep Creek", Combo) ~ "2:1",
    startsWith("Peterson Creek_Steep Creek_Steep Creek", Combo) ~ "2:1",
    startsWith("Peterson Creek_Peterson Creek_Steep Creek", Combo) ~ "2:1",
    startsWith("Steep Creek_Steep Creek_Steep Creek", Combo) ~ "Homogenous",
    startsWith("Peterson Creek_Peterson Creek_Peterson Creek", Combo) ~ "Homogenous",
    startsWith("Herbert River_Peterson Creek_Steep Creek", Combo) ~ "Heterogenous",
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


total_biomass_df <- rbind(inverts_df_total_biomass, peri_df_total_biomass, det_df_total_biomass, fish_df_total_biomass)



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
##Making plot of biomass over time w/ increasing heterogeneity 
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
fish_hetero_plot

fish_plot <- ggarrange(fish_homo_plot, fish_21_plot, fish_hetero_plot,
                       labels = c("i) Homogenous", "ii) 2:1", "iii) Heterogenous"),
                       ncol = 3, nrow = 1, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
invert_hetero_plot

inverts_plot <- ggarrange(invert_homo_plot, invert_21_plot, invert_hetero_plot,
                       labels = c("i) Homogenous", "ii) 2:1", "iii) Heterogenous"),
                       ncol = 3, nrow = 1, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
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
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
peri_hetero_plot

peri_plot <- ggarrange(peri_homo_plot, peri_21_plot, peri_hetero_plot,
                       labels = c("i) Homogenous", "ii) 2:1", "iii) Heterogenous"),
                       ncol = 3, nrow = 1, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
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



  