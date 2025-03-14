##Plotting Stream Hydrology/Physiochemical Data

library(tidyverse)
library(ggplot2)
library(lubridate)

##Read in and clean/format data
##temperature data
temp_df <- read.csv("data/SEAK_2018_TempData.csv") %>%
  select(Stream:Temp) %>%
  mutate(site_type = case_when(
    startsWith(Stream, "Herbert") ~ "Glacier-fed",
    startsWith(Stream, "Steep") ~ "Snow-fed",
    startsWith(Stream, "Peter") ~ "Rain-fed", 
    startsWith(Stream, "Montana") ~ "Mixed",
  )) %>%
  filter(site_type != "Mixed") 

temp_df$date <- as.Date(temp_df$DateTime, "%Y-%m-%d %H:%M:%S")

temp_df_avg <- temp_df %>%
  select(site_type, date, Temp) %>%
  group_by(site_type, date) %>%
  summarise_at(vars(Temp), list(temp_mean = mean, temp_sd = sd)) %>%
  mutate(Month_Year = format(date, "%Y-%m")) %>%
  filter(Month_Year < "2019-01")

##discharge data
dis_df <- read.csv("data/SEAK_Discharge_Summary_Data[58].csv") %>%
  select(Date, Stream:Discharge_cmday) %>%
  filter(Stream != "Transitional")
  
 dis_df$Date <- as.Date(dis_df$Date, "%Y-%m-%d")  
  
dis_df <- dis_df %>%
  mutate(Month_Year = format(Date, "%Y-%m")) %>%
  filter(Month_Year < "2019-01") 
  

##physical data 
p_df <- read.csv("data/SEAK_FoodWebs_BackgroundConditions_PointData.csv") %>%
mutate(site_type = case_when(
  startsWith(Stream, "Herbert") ~ "Glacier-fed",
  startsWith(Stream, "Steep") ~ "Snow-fed",
  startsWith(Stream, "Peter") ~ "Rain-fed", 
  startsWith(Stream, "Montana") ~ "Mixed",
)) %>%
  filter(site_type != "Mixed") %>%
  mutate(date = format(ymd_hms(Date, tz = "UTC"),"%Y-%m-%d")) %>%
  filter(date < "2019-01-01")

##Plotting 
##what are the most important to show? look at matts stuff 
temp_df_avg$site_type <- ordered(temp_df_avg$site_type,
                             levels = c("Glacier-fed", "Snow-fed", "Rain-fed"))
temp_plot <- ggplot(temp_df_avg, aes(x = date, y = log(temp_mean), group = site_type, color = site_type)) +
  geom_line() +
  scale_colour_manual(values = c("gray", "blue", "brown")) +
  theme_classic()
temp_plot


dis_df$Stream <- ordered(dis_df$Stream,
                                 levels = c("Glacier-fed", "Snow-fed", "Rain-fed"))
dis_plot <- ggplot(dis_df, aes(x = Date, y = log(Discharge_cmday), group = Stream, color = Stream)) +
  geom_line() +
  scale_colour_manual(values = c("gray", "blue", "brown")) +
  theme_classic()
dis_plot

##why NAs for some dates for rain-fed? 
