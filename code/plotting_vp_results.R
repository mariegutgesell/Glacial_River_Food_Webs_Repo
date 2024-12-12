#### Plotting Variance Paritioning Results across all taxa

library(tidyverse)
library(ggplot2)
library(ggpubr)

##Read in variance partitoning results

inverts <- read.csv("data/variance_partitioning_results/invert_var_partition_results_bootstrapped.csv") %>%
  rename(Combination = "Combo") %>%
  select(taxa, Iteration, Combination, combo_type, CV_C_L, CV_C_R, CV_S_L, CV_S_R, phi_C_L2R, phi_S_L2R, phi_S2C_L, phi_S2C_R, cv_diff)

pd <- read.csv("data/variance_partitioning_results/peri_det_var_partition_results.csv") %>%
  mutate(Iteration = 1) %>%
  select(taxa, Iteration, Combination, combo_type, CV_C_L, CV_C_R, CV_S_L, CV_S_R, phi_C_L2R, phi_S_L2R, phi_S2C_L, phi_S2C_R, cv_diff)

fish <- read.csv("data/variance_partitioning_results/fish_var_partition_results.csv") %>%
  mutate(Iteration = 1) %>%
  select(taxa, Iteration, Combination, combo_type, CV_C_L, CV_C_R, CV_S_L, CV_S_R, phi_C_L2R, phi_S_L2R, phi_S2C_L, phi_S2C_R, cv_diff)

df <- rbind(inverts, pd, fish)

df$combo_type <- ordered(df$combo_type,
                         levels = c("Homogenous", "2:1", "Heterogenous"))
df$taxa <- ordered(df$taxa,
                   levels = c("Periphyton", "Detritus", "inverts", "fish"))

##
meta_cv <- ggplot(df, aes(x = taxa, y = CV_C_R, group = taxa, fill = taxa)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Metacommunity Variability (CV-C,R)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~combo_type)
meta_cv

meta_cv_2 <- ggplot(df, aes(x = combo_type, y = CV_C_R, group = combo_type, fill = taxa)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen", "chocolate4", "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity Variability (CV-C,R)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
meta_cv_2

local_cv <- ggplot(df, aes(x = taxa, y = CV_C_L, group = taxa, fill = taxa)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Local Community Variability (CV-C,R)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~combo_type)
local_cv

local_cv_2 <- ggplot(df, aes(x = combo_type, y = CV_C_L, group = combo_type, fill = taxa)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen", "chocolate4", "deepskyblue", "darksalmon"))+
  ylab("Local Community Variability (CV-C,R)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
local_cv_2


cv_diff<- ggplot(df, aes(x = combo_type, y = cv_diff, group = combo_type, fill = taxa)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen", "chocolate4", "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
cv_diff


comm_async<- ggplot(df, aes(x = combo_type, y = phi_C_L2R, group = combo_type, fill = taxa)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen", "chocolate4", "deepskyblue", "darksalmon"))+
  ylab("Community Level Spatial Synchrony")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
comm_async


meta_local_cv <- ggarrange(meta_cv_2, local_cv_2, legend = "none", 
                           labels = c("a)", "b)", "c)"),
                           ncol = 1, nrow = 2, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv

cv_dif_async <- ggarrange(cv_diff, comm_async, legend = "none", 
                           labels = c("a)", "b)"),
                           ncol = 1, nrow = 2, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
cv_dif_async
