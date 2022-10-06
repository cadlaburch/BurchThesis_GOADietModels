#Load libraries
library(readr) 
library(tidyverse)
library(here)

#load data
raw_stomach_contents <- read_csv(here("data/GOA_Raw_StomachContents.csv"))

###########
#Prey Grouping
#In order to create figures I must bin the Prey into groups
#Buckley et al. 2016 looked at pollock and grouped prey into 10 common prey groups
#Fishes, Larvaceans, Chaetognaths, Shrimps, Euphausiids, Gammarids, Mysids, Copepods, Other

#Barnes et al. 2018 looked at A flounder and Halibut and grouped prey into 33 groups

#Yang et al. 2006 grouped prey differently for each commercial species
##Pollock: Misc. prey, Calanoid, Amphipod, Euphausiid, Shrimp, Misc. fish, Osmeridae, Pollock
##Cod: Misc. prey, Polychaete, Euphausiid, Shrimp, Tanner crab, other crab, Misc. fish, Flatfish, pollock
#A Flounder: Misc. Prey, Euphausiid, Shrimp, Misc. fish, Ammodytidae, Osmeridae, Pollock
#Halibut: Misc. Prey, Other crab, Shrimp, Hermit crab, Tanner crab, Misc. fish, Pollock
#Sablefish: Misc. prey, Jellyfish, Cephalopod, Shrimp, Crab, misc. fish, pollock, fishery offal
###The other commercial species didn't have stacked bar charts, only tables

#Livingston et al 2017 showes an example of Pollock stomachs with four groups
##copepods, euphausiids, pollock, and other

#The raw data has 90 groupings

#The GOAL of this document is to create a csv file called "groupings" to hold different aggregation scheme
#Part of the work will be done outside of the code because it will be easier to check.

#First I will create a csv that has all of the unique Prey_Names as one column of data
groupings <- raw_stomach_contents %>% 
  summarize(raw_preynames = unique(raw_stomach_contents$Prey_Name))

write.csv(groupings, here("output/groupings.csv"), row.names = F)

#I will manually input the prey grouping aggregation schemes

#############
#Spatial Grouping
#Yang grouped the study area into West, Central and East GOA
#Barnes et al 2020 grouped into both West, Central, East GOA and the 5 INPFC subregions
#Barnes et al 2018 grouped into the 5 INPFC statistical regions and the four IPHC regulatory areas
# Buckley et al 2016 looks at the EBS and groups by sampling strata



#############
#Species size classes
#The following code shows how to bin size classes. The issue is that the bins need to be different because
#predators can be wildly different sizes from species to species
sc_data <- left_join(raw_stomach_contents,PreyCategories,by="Prey_Name")

#Binning predator lengths
range(sc_data$Pred_len)

sc_data <- sc_data %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 20, 30, 40, 50, 60, 275)))

