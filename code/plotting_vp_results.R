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

##Plotting Results without detritus -----------------------
df_2 <- df %>%
  filter(taxa != "Detritus")
df_2$combo_type <- ordered(df_2$combo_type,
                         levels = c("Homogenous", "2:1", "Heterogenous"))
df_2$taxa <- ordered(df_2$taxa,
                   levels = c("Periphyton", "inverts", "fish"))

##

meta_cv_2 <- ggplot(df_2, aes(x = combo_type, y = CV_C_R, group = combo_type, fill = taxa)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity Variability (CV-C,R)")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
meta_cv_2


local_cv_2 <- ggplot(df_2, aes(x = combo_type, y = CV_C_L, group = combo_type, fill = taxa)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local Community Variability (CV-C,R)")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
local_cv_2


cv_diff<- ggplot(df_2, aes(x = combo_type, y = cv_diff, group = combo_type, fill = taxa)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
cv_diff


comm_async<- ggplot(df_2, aes(x = combo_type, y = phi_C_L2R, group = combo_type, fill = taxa)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level Spatial Synchrony")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
comm_async


meta_local_cv_2 <- ggarrange(meta_cv_2, local_cv_2, comm_async, legend = "none", 
                           labels = c("a)", "b)", "c)"),
                           ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv_2

cv_dif_async_2 <- ggarrange(cv_diff, comm_async, legend = "none", 
                          labels = c("a)", "b)"),
                          ncol = 1, nrow = 2, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
cv_dif_async_2


##Plotting results for predicted climate homogenization gradient (rain, rain-rain-snow (or rain-snow-snow), and then rain-snow-ice) ------------

df_3 <- df_2 %>%
  filter(Combination %in% c("Glacier-fed_Rain-fed_Snow-fed", "Rain-fed_Rain-fed_Rain-fed", "Rain-fed_Snow-fed_Snow-fed")) %>%
  select(taxa, Combination, combo_type, CV_C_R, CV_C_L, cv_diff, phi_C_L2R) %>%
  group_by(taxa, Combination, combo_type) %>%
  summarise_at(vars(CV_C_R, CV_C_L, cv_diff, phi_C_L2R), list(mean = mean, sd = sd))
  

df_3$combo_type <- ordered(df_3$combo_type,
                         levels = c("Homogenous", "2:1", "Heterogenous"))
df_3$taxa <- ordered(df_3$taxa,
                   levels = c("Periphyton", "inverts", "fish"))

##
meta_cv <- ggplot(df_3, aes(x = taxa, y = CV_C_R_mean, group = taxa, fill = taxa)) +
  geom_col() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity \nVariability (CV-C,R)")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~combo_type)
meta_cv

meta_cv_2 <- ggplot(df_3, aes(x = combo_type, y = CV_C_R_mean, group = combo_type, fill = taxa)) +
  geom_col() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity \nVariability (CV-C,R)")+
  theme(axis.title.y = element_text(size = 14),axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~taxa)
meta_cv_2


##
local_cv <- ggplot(df_3, aes(x = taxa, y = CV_C_L_mean, group = taxa, fill = taxa)) +
  geom_col() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Local Community \nVariability (CV-C,R)")+
  theme(axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~combo_type)
local_cv

local_cv_2 <- ggplot(df_3, aes(x = combo_type, y = CV_C_L_mean, group = combo_type, fill = taxa)) +
  geom_col() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Local Community \nVariability (CV-C,R)")+
  theme(axis.title.y = element_text(size = 14),axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~taxa)
local_cv_2



cv_diff<- ggplot(df_3, aes(x = taxa, y = cv_diff_mean, group = taxa, fill = taxa)) +
  geom_col() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme(axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~combo_type, nrow = 1)
cv_diff

cv_diff_2<- ggplot(df_3, aes(x = combo_type, y = cv_diff_mean, group = combo_type, fill = taxa)) +
  geom_col() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme(axis.title.y = element_text(size = 14),axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~taxa, nrow = 1)
cv_diff_2

comm_async<- ggplot(df_3, aes(x = taxa, y = phi_C_L2R_mean, group = taxa, fill = taxa)) +
  geom_col() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level \nSpatial Synchrony")+
  theme(axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~combo_type, nrow = 1)
comm_async

comm_async_2<- ggplot(df_3, aes(x = combo_type, y = phi_C_L2R_mean, group = combo_type, fill = taxa)) +
  geom_col() +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level \nSpatial Synchrony")+
  theme(axis.title.y = element_text(size = 14), axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~taxa, nrow = 1)
comm_async_2


meta_local_cv <- ggarrange(meta_cv, local_cv, comm_async, legend = "none", 
                             labels = c("a)", "b)", "c)"),
                             ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv


meta_local_cv_2 <- ggarrange(meta_cv_2, local_cv_2, comm_async_2, legend = "none", 
                           labels = c("a)", "b)", "c)"),
                           ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv_2

final_fig <- ggarrange(meta_local_cv_2, cv_diff_2, legend = "none", ncol = 2, nrow = 1)
final_fig
##Plotting column chart of mean w/ standard deviation -- for all combinations ---------------------
standard_error <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
}

df_4 <- df_2 %>%
  # filter(Combination %in% c("Glacier-fed_Rain-fed_Snow-fed", "Rain-fed_Rain-fed_Rain-fed", "Rain-fed_Snow-fed_Snow-fed")) %>%
  select(taxa, Combination, combo_type, CV_C_R, CV_C_L, cv_diff, phi_C_L2R) %>%
  group_by(taxa, combo_type) %>%
  summarise_at(vars(CV_C_R, CV_C_L, cv_diff, phi_C_L2R), 
               list(mean = ~mean(.x, na.rm = TRUE), 
                    sd = ~sd(.x, na.rm = TRUE), 
                    se = ~standard_error(.x)))



df_4$combo_type <- ordered(df_4$combo_type,
                           levels = c("Homogenous", "2:1", "Heterogenous"))
df_4$taxa <- ordered(df_4$taxa,
                     levels = c("Periphyton", "inverts", "fish"))

##
meta_cv <- ggplot(df_4, aes(x = taxa, y = CV_C_R_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_R_mean - CV_C_R_se, ymax = CV_C_R_mean + CV_C_R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity \nVariability (CV-C,R)")+
  theme(axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~combo_type)
meta_cv

meta_cv_2 <- ggplot(df_4, aes(x = combo_type, y = CV_C_R_mean, group = combo_type, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_R_mean - CV_C_R_se, ymax = CV_C_R_mean + CV_C_R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity \nVariability (CV-C,R)")+
  theme(axis.title.y = element_text(size = 14),axis.title.x = element_blank(), legend.position = "right",text = element_text(family = "Times New Roman"),  strip.text = element_blank()) +
  facet_wrap(~taxa)
meta_cv_2


##
local_cv <- ggplot(df_4, aes(x = taxa, y = CV_C_L_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_L_mean - CV_C_L_se, ymax = CV_C_L_mean + CV_C_L_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Local Community \nVariability (CV-C,R)")+
  theme( axis.title.y = element_text(size = 14),axis.title.x = element_blank(), legend.position = "right",text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~combo_type)
local_cv

local_cv_2 <- ggplot(df_4, aes(x = combo_type, y = CV_C_L_mean, group = combo_type, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_L_mean - CV_C_L_se, ymax = CV_C_L_mean + CV_C_L_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Local Community \nVariability (CV-C,R)")+
  theme(axis.title.y = element_text(size = 14), axis.title.x = element_blank(), legend.position = "right",text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~taxa)
local_cv_2


cv_diff<- ggplot(df_4, aes(x = taxa, y = cv_diff_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = cv_diff_mean - cv_diff_se, ymax = cv_diff_mean + cv_diff_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme(axis.title.y = element_text(size = 14), axis.title.x = element_blank(), legend.position = "right",text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~combo_type, nrow = 1)
cv_diff

cv_diff_2<- ggplot(df_4, aes(x = combo_type, y = cv_diff_mean, group = combo_type, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = cv_diff_mean - cv_diff_se, ymax = cv_diff_mean + cv_diff_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme(axis.title.y = element_text(size = 14), axis.title.x = element_blank(), legend.position = "right",text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~taxa, nrow = 1)
cv_diff_2



comm_async<- ggplot(df_4, aes(x = taxa, y = phi_C_L2R_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = phi_C_L2R_mean - phi_C_L2R_se, ymax = phi_C_L2R_mean + phi_C_L2R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level \nSpatial Synchrony")+
  theme(axis.title.y = element_text(size = 14),axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~combo_type, nrow = 1)
comm_async

comm_async_2<- ggplot(df_4, aes(x = combo_type, y = phi_C_L2R_mean, group = combo_type, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = phi_C_L2R_mean - phi_C_L2R_se, ymax = phi_C_L2R_mean + phi_C_L2R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level \nSpatial Synchrony")+
  theme(axis.title.y = element_text(size = 14),axis.title.x = element_blank(), legend.position = "right", text = element_text(family = "Times New Roman"), strip.text = element_blank()) +
  facet_wrap(~taxa, nrow = 1)
comm_async_2


meta_local_cv <- ggarrange(meta_cv, local_cv, comm_async, legend = "none", 
                             labels = c("a)", "b)", "c)"),
                             ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv

meta_local_cv_2 <- ggarrange(meta_cv_2, local_cv_2, comm_async_2, legend = "none", 
                           labels = c("a)", "b)", "c)"),
                           ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv_2

final_fig <- ggarrange(meta_local_cv_2, cv_diff_2, legend = "none", ncol = 2, nrow = 1)
final_fig

###ANOVAs

meta_aov <- aov(CV_C_R ~ combo_type*taxa, df_2)
summary(meta_aov)
TukeyHSD(meta_aov)


##Unpacking 2:1 ----------------------------
df_5 <- df_2 %>%
  filter(combo_type == "2:1") %>%
  select(taxa, Combination, combo_type, CV_C_R, CV_C_L, cv_diff, phi_C_L2R) %>%
  group_by(taxa, Combination) %>%
  summarise_at(vars(CV_C_R, CV_C_L, cv_diff, phi_C_L2R), 
               list(mean = ~mean(.x, na.rm = TRUE), 
                    sd = ~sd(.x, na.rm = TRUE), 
                    se = ~standard_error(.x)))



df_4$combo_type <- ordered(df_4$combo_type,
                           levels = c("Homogenous", "2:1", "Heterogenous"))
df_4$taxa <- ordered(df_4$taxa,
                     levels = c("Periphyton", "inverts", "fish"))

##
meta_cv <- ggplot(df_5, aes(x = taxa, y = CV_C_R_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_R_mean - CV_C_R_se, ymax = CV_C_R_mean + CV_C_R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity Variability (CV-C,R)")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~Combination)
meta_cv

meta_cv_2 <- ggplot(df_5, aes(x = Combination, y = CV_C_R_mean, group = Combination, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_R_mean - CV_C_R_se, ymax = CV_C_R_mean + CV_C_R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity Variability (CV-C,R)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa)
meta_cv_2


##
local_cv <- ggplot(df_5, aes(x = taxa, y = CV_C_L_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_L_mean - CV_C_L_se, ymax = CV_C_L_mean + CV_C_L_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Local Community Variability (CV-C,R)")+
  theme( axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~Combination)
local_cv

local_cv_2 <- ggplot(df_5, aes(x = Combination, y = CV_C_L_mean, group = Combination, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_L_mean - CV_C_L_se, ymax = CV_C_L_mean + CV_C_L_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Local Community Variability (CV-C,R)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa)
local_cv_2


cv_diff<- ggplot(df_5, aes(x = taxa, y = cv_diff_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = cv_diff_mean - cv_diff_se, ymax = cv_diff_mean + cv_diff_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme( axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~Combination)
cv_diff

cv_diff_2<- ggplot(df_5, aes(x = Combination, y = cv_diff_mean, group = Combination, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = cv_diff_mean - cv_diff_se, ymax = cv_diff_mean + cv_diff_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme( axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
cv_diff_2



comm_async<- ggplot(df_5, aes(x = taxa, y = phi_C_L2R_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = phi_C_L2R_mean - phi_C_L2R_se, ymax = phi_C_L2R_mean + phi_C_L2R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level Spatial Synchrony")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~Combination)
comm_async

comm_async_2<- ggplot(df_5, aes(x = Combination, y = phi_C_L2R_mean, group = Combination, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = phi_C_L2R_mean - phi_C_L2R_se, ymax = phi_C_L2R_mean + phi_C_L2R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level Spatial Synchrony")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
comm_async_2


meta_local_cv <- ggarrange(meta_cv, local_cv, comm_async, legend = "none", 
                           labels = c("a)", "b)", "c)"),
                           ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv

meta_local_cv_2 <- ggarrange(meta_cv_2, local_cv_2, comm_async_2, legend = "none", 
                             labels = c("a)", "b)", "c)"),
                             ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv_2

##Unpacking Homogenous ----------------------------
df_6 <- df_2 %>%
  filter(combo_type == "Homogenous") %>%
  select(taxa, Combination, combo_type, CV_C_R, CV_C_L, cv_diff, phi_C_L2R) %>%
  group_by(taxa, Combination) %>%
  summarise_at(vars(CV_C_R, CV_C_L, cv_diff, phi_C_L2R), 
               list(mean = ~mean(.x, na.rm = TRUE), 
                    sd = ~sd(.x, na.rm = TRUE), 
                    se = ~standard_error(.x)))



df_6$Combination <- ordered(df_6$Combination,
                           levels = c("Glacier-fed_Glacier-fed_Glacier-fed", "Snow-fed_Snow-fed_Snow-fed", "Rain-fed_Rain-fed_Rain-fed"))

##
meta_cv <- ggplot(df_6, aes(x = taxa, y = CV_C_R_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_R_mean - CV_C_R_se, ymax = CV_C_R_mean + CV_C_R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity Variability (CV-C,R)")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~Combination)
meta_cv

meta_cv_2 <- ggplot(df_6, aes(x = Combination, y = CV_C_R_mean, group = Combination, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_R_mean - CV_C_R_se, ymax = CV_C_R_mean + CV_C_R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Metacommunity Variability (CV-C,R)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa)
meta_cv_2


##
local_cv <- ggplot(df_6, aes(x = taxa, y = CV_C_L_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_L_mean - CV_C_L_se, ymax = CV_C_L_mean + CV_C_L_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Local Community Variability (CV-C,R)")+
  theme( axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~Combination)
local_cv

local_cv_2 <- ggplot(df_6, aes(x = Combination, y = CV_C_L_mean, group = Combination, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = CV_C_L_mean - CV_C_L_se, ymax = CV_C_L_mean + CV_C_L_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  theme_classic() +
  ylab("Local Community Variability (CV-C,R)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa)
local_cv_2


cv_diff<- ggplot(df_6, aes(x = taxa, y = cv_diff_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = cv_diff_mean - cv_diff_se, ymax = cv_diff_mean + cv_diff_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme( axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~Combination)
cv_diff

cv_diff_2<- ggplot(df_6, aes(x = Combination, y = cv_diff_mean, group = Combination, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = cv_diff_mean - cv_diff_se, ymax = cv_diff_mean + cv_diff_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Local to Metacommunity Stabilization \n(Metacommunity CV-Local Community CV)")+
  theme( axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
cv_diff_2



comm_async<- ggplot(df_6, aes(x = taxa, y = phi_C_L2R_mean, group = taxa, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = phi_C_L2R_mean - phi_C_L2R_se, ymax = phi_C_L2R_mean + phi_C_L2R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level Spatial Synchrony")+
  theme(axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~Combination)
comm_async

comm_async_2<- ggplot(df_6, aes(x = Combination, y = phi_C_L2R_mean, group = Combination, fill = taxa)) +
  geom_col() +
  geom_errorbar(aes(ymin = phi_C_L2R_mean - phi_C_L2R_se, ymax = phi_C_L2R_mean + phi_C_L2R_se, group = taxa), position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  scale_fill_manual(values = c("darkgreen",  "deepskyblue", "darksalmon"))+
  ylab("Community Level Spatial Synchrony")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(), legend.position = "right") +
  facet_wrap(~taxa, nrow = 1)
comm_async_2


meta_local_cv <- ggarrange(meta_cv, local_cv, comm_async, legend = "none", 
                           labels = c("a)", "b)", "c)"),
                           ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv

meta_local_cv_2 <- ggarrange(meta_cv_2, local_cv_2, comm_async_2, legend = "none", 
                             labels = c("a)", "b)", "c)"),
                             ncol = 1, nrow = 3, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
meta_local_cv_2
