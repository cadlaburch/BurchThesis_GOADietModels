#Author: Catalina Burch
#Date Modified: 5/19/23
#Description: This file prepares the raw data for summary figures and models.

#Load Libraries
library(here) #for finding working directory
library(readr) #for loading CSV
library(tidyverse)

#DATA ASSEMBLY
#Load food habits data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
data <- raw_stomach_contents2021

#create unique haul identifier
data$Haul_Join <- paste(data$VESSEL, data$CRUISE, data$HAUL, sep = "")

#Exclude data before 1990
data <- data %>% 
  filter(Year >= 1990)

#Select only predators of interest
data <- data %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder"))

#remove empty stomachs and create unique stomach ID
data <- data %>% 
  filter(Prey_Name != "Empty") %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN), sep = "")

#change year to factor
data$Year <- factor(data$Year)
data$Haul_Join <- factor(data$Haul_Join)

#Remove deep hauls and data entry error
data <- data %>% 
  filter(GEAR_DEPTH <= 300 & GEAR_DEPTH > 0)

#create day of year (Julien)
# #Note: not currently using this in my model
# data <- data %>%
#   mutate(date = paste(Month, Day, sep = "-"))
# 
# data$date <- as.Date(data$date, "%m-%d")
# data$julien <- format(data$date, "%j")
# data$julien <- as.numeric(data$julien)
# 
# class(data$julien)

# Set coordinate boundaries for plotting:
lonmin = -172
lonmax = -130
latmin = 52
latmax = 62

#Creating a function that transforms the data into wide format based on haul and predator size class.
haul_wide_fun <- function(data) {
  data %>% 
    mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>% #create binary presence absence for each prey item
    distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR,
             SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, PRED_LEN, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #remove redundancies, i.e. the same prey species listed twice for the same stomach with different life history stages
    group_by(Pred_common, Len_bin, Haul_Join) %>% 
    distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR,
             SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #This is to remove redundancies again. I only want one line for each predator in the haul length bin
    pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% #create wide dataframe with a column for each prey type
    rename(Walleyepollock = 'Walleye pollock', TannerCrab = 'Tanner Crab',
           Crangonidae = 'Crangonidae (shrimp)', Pandalidae = `Pandalidae (shrimp)`) %>% #rename (this was an issue for running the walleyepollock prey model below)
    mutate(forage = sum(Osmerid, Clupeoidei),
           forage = ifelse(forage > 0, 1, 0)) %>% 
    na.omit() #remove missing environmental data
}


#_________________
#Create separate dataframes for each predator species with different length bins based on sampling methods

WP <- data %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>%
  haul_wide_fun() 
levels(WP$Len_bin) = c("1", "2", "3", "4")

PH <- data %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun()  
levels(PH$Len_bin) = c("1", "2", "3", "4")

#Note: I couldn't find the sampling bins for PC so I made it the same as WP
PC <- data %>% 
  filter(Pred_common == "Pacific cod") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>% 
  haul_wide_fun() 
levels(PC$Len_bin) = c("1", "2", "3", "4")

AF <- data %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun()  
levels(AF$Len_bin) = c("1", "2", "3", "4")


#creating dataframe with all predators
all_pred <- rbind(WP, PH, PC, AF)