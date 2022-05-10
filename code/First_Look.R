#This is my first look at the food habits data
#edit
#load some libraries
library(readr)
library(tidyverse)
library(sf)
library(here)

#load in the raw data
raw_prey_length <- read_csv("data/GOA_Raw_PreyLength.csv")
raw_stomach_contents <- read_csv("data/GOA_Raw_StomachContents.csv")

#what type of dataframe is this?
class(raw_prey_length) # "spec_tbl_df"
class(raw_stomach_contents) # "spec_tbl_df"

#don't know if I need the race data but I found it...
RaceData2015_2019 <- read_csv("data/RaceDatagoa2015_2019.csv")

#which years are included in the data?
unique(raw_prey_length$Year)
  #Answer: 1981 1987 1990 1993 1996 1999 2001 2003 2005 2007 2009 2011 2013 2015 2017 2019
unique(raw_stomach_contents$Year)
  #Answer: 1981 1987 1990 1993 1996 1999 2001 2003 2005 2007 2009 2011 2013 2015 2017 2019 
#They had the same years of collection

#what are the geographic boundaries?
range(raw_prey_length$Rlat)
  #Answer: 52.46, 60.30
range(raw_prey_length$Rlong)
  #Answer: -169.98, -132.68

#From what I learned in the data biography it seems like I will mainly be using the stomach contents data

#I want to convert it into a spatial dataframe
stomach <- st_as_sf(raw_stomach_contents, coords = c("Rlong", "Rlat"), crs = 3467)
class(stomach)
head(stomach)

#Warning: This takes a minute to load
ggplot(data = stomach) +
  geom_sf()

surfacetemp <- ggplot(data = stomach, aes(x = Year, y = Surface_temp)) +
  geom_point() +
  theme_classic()

ggsave("surfacetemp.jpg", plot = surfacetemp, device = "jpg", path = "output")

#------------------- Halibut! ----------------
#Let's play around with looking at a specific predator, I'm using the nodc code for P Halibut
PacificHalibut <- stomach %>% 
  filter(Pred_nodc == 8857041901)

phabstom <- ggplot(data = PacificHalibut, aes(y = Pred_stomwt, x = Pred_len)) +
  geom_point() +
  theme_classic() +
  labs(title = "P. Halibut")

ggsave("phab_stom_len.jpg", plot = phabstom, device = "jpg", path = "output")

phabhist <- ggplot(data = PacificHalibut, aes(x = Pred_len)) +
  geom_histogram() +
  theme_classic() +
  labs(title = "P. Halibut")

ggsave("phab_len_hist.jpg", plot = phabhist, device = "jpg", path = "output")

#Checking that the Prey_Name column is the 92 groupings given on the data page

length(unique(stomach$Prey_Name)) #I got 90, sooo there are two missing groups?
length(unique(PacificHalibut$Prey_Name)) #82 for halibut not very picky eaters


#-------------------QC Checks ------------------
#check that Pred_Full matches the Pred_stomwt
stomach %>% 
  filter(Pred_full == 1) %>% 
  ggplot(aes(x = Pred_stomwt, y = Pred_len)) +
  geom_point()
#looks clean lets try the other way

stomach %>% 
  filter(Pred_stomwt == 0) %>% 
  ggplot(aes(x = Pred_full, y = Pred_len)) +
  geom_point()
#CHECK THIS, seems like an error? if the stomach weight is 0 the pred full values should all be 1
stomach %>% 
  filter(Pred_stomwt == 0) %>% 
  ggplot(aes(x = Prey_twt, y = Pred_len)) +
  geom_point()
#HMMM this also seems wrong for the same reasons?
stomach %>% 
  filter(Pred_stomwt == 0) %>% 
  ggplot(aes(x = Pred_dig, y = Pred_len)) +
  geom_point()
#this also seems wrong?
