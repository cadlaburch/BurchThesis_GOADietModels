#Author: Catalina Burch
#Date Modified: 5/19/23
#Description: This file prepares the raw data for summary figures and models.

#Load Libraries
library(here) #for finding working directory
library(readr) #for loading CSV
library(tidyverse)

#Load food habits data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
data <- raw_stomach_contents2021
groupings <- read_csv(here("data/groupings.csv")) #this csv contains prey grouping structures

#join data with prey groupings
data <- left_join(data,groupings,by="Prey_Name")

#create unique haul identifier
data$Haul_Join <- paste(data$VESSEL, data$CRUISE, data$HAUL, sep = "")

#Exclude data before 1990
data <- data %>% 
  filter(Year >= 1990)

#create unique stomach ID
data <- data %>% 
  mutate(uniqueID = paste("ID", Haul_Join, PRED_NODC, PRED_SPECN, sep = ""))

#change year to factor
data$Year <- factor(data$Year)

#Remove deep hauls and data entry error
#data <- data %>% 
#  filter(GEAR_DEPTH <= 300 & GEAR_DEPTH > 0)

#remove hauls with missing data
data <- data %>% 
  drop_na(GEAR_DEPTH, GEAR_TEMP)

#remove empty stomachs
data <- data %>% 
  filter(Prey_Name != "Empty")

#Save filtered data for all predators
write.csv(data, here("data/all_pred_data.csv"), row.names = F)

#Create predator specific length bins
WP <- data %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 280)),#Length Bin for model
         Len_bin_sample = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 280)),#length bins based on sampling methodology
         Len_bin_SB = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 280)))#Length Bin for stacked bar
levels(WP$Len_bin) = c("1", "2", "3", "4")
levels(WP$Len_bin_sample) = c("<25", "25-39", "40-54", ">54")
levels(WP$Len_bin_SB) = c("<20", "21-30", "31-40", "41-50", "51-60", "61-70", ">70")

PH <- data %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 280)),#Length Bin for model
         Len_bin_sample = cut(PRED_LEN, breaks = c(0, 39, 69, 280)),#length bins based on sampling methodology
         Len_bin_SB = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, #Length Bin for stacked bar
                                               120, 280))) 
levels(PH$Len_bin) = c("1", "2", "3", "4")
levels(PH$Len_bin_sample) = c("<39", "39-69", ">69")
levels(PH$Len_bin_SB) = c("<20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120", ">120")

#Note: I couldn't find the sampling bins for PC so I made it the same as WP
PC <- data %>% 
  filter(Pred_common == "Pacific cod") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 280)),#Length Bin for model
         Len_bin_sample = cut(PRED_LEN, breaks = c(0, 29, 44, 59, 280)), #length bins based on sampling methodology
         Len_bin_SB = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 280))) #Length Bin for stacked bar
levels(PC$Len_bin) = c("1", "2", "3", "4")
levels(PC$Len_bin_sample) = c("<29", "29-44", "45-59", ">59")
levels(PC$Len_bin_SB) = c("<20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80")

AF <- data %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 280)),  #Length Bin for model
         Len_bin_sample = cut(PRED_LEN, breaks = c(0, 29, 49, 280)),
         Len_bin_SB = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 280))) #Length Bin for stacked bar
levels(AF$Len_bin) = c("1", "2", "3", "4")
levels(AF$Len_bin_sample) = c("<29", "29-49", ">49")
levels(AF$Len_bin_SB) = c("<20", "21-30", "31-40", "41-50", "51-60", "61-70", ">70")


#creating dataframe for just focal predators with distinct length bins
data <- rbind(WP, PH, PC, AF)

length(unique(data$uniqueID))
length(unique(data$Haul_Join))

#save data
write.csv(data, here("data/data.csv"), row.names = F) #85676

