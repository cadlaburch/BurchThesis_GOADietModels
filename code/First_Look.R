#This is my first look at the food habits data
#edit
#load some libraries
library(readr)
library(tidyverse)
library(sf)
library(here)

#load in the raw data
raw_prey_length <- read_csv(here("data/GOA_Raw_PreyLength.csv"))
raw_stomach_contents <- read_csv(here("data/GOA_Raw_StomachContents.csv"))

#what type of dataframe is this?
class(raw_prey_length) # "spec_tbl_df"
class(raw_stomach_contents) # "spec_tbl_df"

#don't know if I need the race data but I found it...
RaceData2015_2019 <- read_csv(here("data/RaceDatagoa2015_2019.csv"))

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

QC1 <- stomach %>% 
  filter(Pred_stomwt == 0) %>% 
  ggplot(aes(x = Pred_full, y = Pred_len)) +
  geom_point() +
  labs(title = "filtered for Pred_stomwt = 0")

ggsave(plot = QC1, path = "output", "QC1.jpg", device = "jpg")

#CHECK THIS, seems like an error? if the stomach weight is 0 the pred full values should all be 1
QC2 <- stomach %>% 
  filter(Pred_stomwt == 0) %>% 
  ggplot(aes(x = Prey_twt, y = Pred_len)) +
  geom_point() +
  labs(title = "filtered for Pred_stomwt = 0")

ggsave(plot = QC2, path = "output", "QC2.jpg", device = "jpg")

#HMMM this also seems wrong for the same reasons?
QC3 <- stomach %>% 
  filter(Pred_stomwt == 0) %>% 
  ggplot(aes(x = Pred_dig, y = Pred_len)) +
  geom_point()+
  labs(title = "filtered for Pred_stomwt = 0")

ggsave(plot = QC3, path = "output", "QC3.jpg", device = "jpg")
#this also seems wrong?

#this one looks ok
stomach %>% 
  ggplot(aes(x = Pred_dig, y = Prey_twt)) +
  geom_point()

#----------------- Yang replication attempt
Exp<-raw_stomach_contents %>% 
  filter(Year == 1999) %>% 
  group_by(Pred_name) %>% 
  summarise(if(Pred_stomwt == 0){mutate(E = n())} else {mutate(F = n())})


  mutate(filter())

#------------------ Yang table summary stats -----------------

#2003
#Calculate number of empty stomachs by species
E <- raw_stomach_contents %>% 
  filter(Year == 2003, Pred_stomwt == 0) %>% 
  group_by(Pred_name) %>% 
  summarise(E = n())

#Calculate number of not-empty stomachs by species
F <- raw_stomach_contents %>% 
  filter(Year == 2003, Pred_stomwt != 0) %>% 
  group_by(Pred_name) %>% 
  summarise(F = n())

#I think I'm supposed to only do the mean for fish that had food in their stomachs
#Calculate mean and sd of length by species
T <- raw_stomach_contents %>% 
  filter(Year == 2003) %>% 
  group_by(Pred_name) %>% 
  summarise(T = n(), 
            "Fork length (cm)* Mean" = mean(Pred_len),
            "+- SD" = sd(Pred_len))

#join dataframes into table
EF <- full_join(E, F, by = "Pred_name")

EFT <- full_join(EF, T, by = "Pred_name")

#I think there is a more efficient way to do this

#looking into swapping scientific names for common names
#come back to this
library(taxize)
uids.found <- get_uid(T$Pred_name)
uids.found <- as.uid(uids[!is.na(uids)])
# keep only species names  corresponding to your ids
species.found <- T[!is.na(uids.found)]

common.names <- sci2comm(uids.found, db = 'ncbi')
names(common.names) <- species.found
uids

taxize::use_entrez()

#come back to this

#---------------------Yang Summary tables ---------------------

#2003 Pollock Theragra chalcogramma

Poll <- raw_stomach_contents %>%
  filter(Year == 2003, Pred_name == "Gadus chalcogrammus")

#Percent Weight
#calculate weight of each prey group
Weight <- Poll %>% 
  group_by(Prey_Name) %>% 
  summarise(TotalWt = sum(Prey_twt))

#calculate total prey weight
sum(Weight$TotalWt) #A: 4756.197

#calculate percent weight in new column
Weight <- Weight %>%
  mutate(PW = (TotalWt/4756.197)*100)
  

#percent frequency
Number <- Poll %>% 
  filter(Pred_stomwt != 0) %>% 
  group_by(Prey_Name) %>% 
  summarise(N = n())

sum(Number$N) #1254

Number <- Number %>% 
  mutate(PF = (N/1254)*100)

PollSum <- full_join(Number, Weight, by = "Prey_Name") %>% 
  select(Prey_Name, PF, PW)
  