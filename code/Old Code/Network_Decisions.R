#Load libraries
library(readr) 
library(tidyverse)
library(here)

#Load Data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
groupings <- read_csv(here("output/groupings.csv"))

#------------------------
#WHAT IS MY MODEL PURPOSE????????

#'Assess variation in GOA food web structure and groundfish diets in relation to temperature using network analysis
#'and statistical modeling.
#'Analyze shifts in food web structure and energy flow in response to altered temperature regimes.

#DEFINE THE INDEPENDENT VARIABLES AND RESPONSE VARIABLES
#Independent: Temperature, Year, Region, Depth?
#Response: Network Structure (choose a metric from network analysis)

#WRITE MODEL EQUATION
#'We will identify key prey nodes and model variation in the contribution (PW, Occurance) of those key prey to select 
#'predator diets as a function of sampling date, geographic location, depth, and temperature at trawl depth

#-----------------------
#QUESTION 1:
#Which Predators should I include?

#This chunk calculates the number of predator stomachs of each species sampled for each year
Predator_sample_size <- raw_stomach_contents2021 %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_")) %>%
  select(Year, uniqueID, Pred_Species, Pred_common) %>% 
  distinct(Year, uniqueID, Pred_Species, Pred_common) %>% 
  group_by(Year, Pred_common) %>% 
  mutate(n_stomachs = length(unique(uniqueID))) %>% 
  distinct(Year, Pred_Species, Pred_common, n_stomachs)

Pred_sample_size_wide <- Predator_sample_size %>% 
  distinct(Year, Pred_common, n_stomachs) %>% 
  pivot_wider(names_from = c(Pred_common), values_from = n_stomachs)

write.csv(Predator_sample_size, here("output/summary_tables/Predator_sample_size.csv"), row.names = F)
write.csv(Pred_sample_size_wide, here("output/summary_tables/Predator_sample_size_wide_matrix.csv"), row.names = F)

#Next create a figure that shows which years each predator were sampled
ggplot(Predator_sample_size, aes(x = Year, y = n_stomachs)) +
  facet_wrap(~Pred_common) +
  geom_line()+
  theme_classic()

#DECISION:
#I am only going to build networks using only Walleye Pollock, Arrowtooth Flounder, Pacific Cod, Pacific Halibut
#species that I'm considering including. Pacific Ocean perch (2 years with only 8 stomachs)
#Sablefish were only sampled up to 2011

#-----------------------------
#Question 2: 
#Which Years to include?

select_pred <- Pred_sample_size_wide %>% 
  select(Year, `Walleye pollock`, `Pacific cod`, `Pacific halibut`, `Arrowtooth flounder`, 
         `Pacific ocean perch`, Sablefish)

#DECISION
#1981 and 1987 are consistently missing data across focal predators. I'm going to exclude those years from analysis
#POP are sampled through the heatwave period but sablefish are not. I'll table them for now and focus on the main 4 preds.

network_sc_filtered <- raw_stomach_contents2021 %>% 
  filter(Year > 1987 & Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder"))

#----------------------------
#Question 3:
#How should I break up the data geographically?

#'Aleutian islands data: on the N side of the chain, all hauls at bottom depths of 550m or less get assigned to AI
#'On the south side of the chain, all hauls (regardless of depth) westward of 170 W longitude get assigned to AI. INPFC (541, 542, 543)
#'Bering Sea Data: All hauls on the shelf INPFC 508, 509, 512, 513, 516, 517, 518, 521, 524, 530, 550.
#'GOA data: All hauls eastward of 170 W longitude, S of AI or AK peninusla. INPFC(610, 620, 630, 640, 650, 690)

range(network_sc_filtered$RLONG) # -170.00 -132.88 this looks correct
unique(network_sc_filtered$INPFC_AREA) #"519" "541" "610" "620" "630" "640" "649" "650" 659 NA This doesn't match the data description.

#'I found this map https://media.fisheries.noaa.gov/dam-migration/iphc-nmfs-areas.jpg
#'which I am using to designate which areas are GOA
#' 650, 640, 630, 620, 610, 541, 649
#' 649 is a small area near seward, may have sample size issues?
#' 519 looks like it is north of Dutch harbor, shouldn't that be AI?
#' I'm not sure where 659 is... I'm going to check the lat long for that

test <- network_sc_filtered %>% 
  group_by(INPFC_AREA) %>% 
  mutate(r = max(RLAT)) %>% 
  distinct(INPFC_AREA, r)

range(test$RLAT) #55.59 57.09
range(test$RLONG) #-135.53 -133.45

#It looks like 659 is a tiny area around ketchikan

#'Different types of geographies:
#'IPHC: 5 groupings by the Halibut Comission
#'INPFC: International North Pacific Fisheries Commission 
#'StationID: RACE station ID number (is this the strata?) I went down a rabit hole trying to figure this out
#'I found this website: https://apps-afsc.fisheries.noaa.gov/RACE/groundfish/survey_data/default.htm 
#'It has a button for strata but I couldn't figure out how to see it, which is a pain
#'Rlat, Rlong: gives precise location of haul

network_sc_filtered <- network_sc_filtered %>% 
  filter(INPFC_AREA != 519) #I might want to also exclude 649 if it is too sample size limited

#Chery's paper calls these areas something different
#610 = Shumagin
#620 = Chirikof
#630 = Kodiak
#640+649 = Yakutat
#650 + 659 = Southeast

#I want to check the sample sizes for if we broke it up by INPFC Area
INPFC_samle_Size <- network_sc_filtered %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_")) %>%
  select(Year, uniqueID, Pred_common, INPFC_AREA) %>% 
  distinct(Year, uniqueID, Pred_common, INPFC_AREA) %>% 
  group_by(Year, Pred_common, INPFC_AREA) %>% 
  mutate(n_stomachs = length(unique(uniqueID))) %>% 
  distinct(Year, Pred_common, n_stomachs)

INPFC_samle_Size_wide <- INPFC_samle_Size %>% 
  pivot_wider(names_from = c(Pred_common), values_from = n_stomachs)

#DECISION:
#This table shows me which areas have low sample sizes for particular species, and which years
#they did not sample a specific area
write.csv(INPFC_samle_Size_wide, here("output/summary_tables/INPFC_sample_size.csv"), row.names = F)

#For now I will exclude 541, 640, 650, 659
#I’ll build models for 610, 620, 630 for the 15 years of data.

network_sc_filtered <- network_sc_filtered %>% 
  filter(INPFC_AREA < 640 & INPFC_AREA > 541)

unique(test$INPFC_AREA)
#Question, is 15 too small a number to run a GLM?

#-----------------------------
#Question 4:
#How to calculate temperature for each year and geography?

#Problems: the temperature is going to be confounded by the location (lat/long), the depth, and the time of year

#'Gear_Temp: gear water temperature (Is this the same as temperature at depth?)
#'Surface Temp: surface water temp
#'In my prospectus I said we would use mean temperature at trawl depth (bottom temp)


#-------------------------------
#Question 5: 
#Do I need to subset by length class to account for ontogenetic shifts in diet?
Length_sample_sizes <- network_sc_filtered %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_")) %>%
  select(Year, uniqueID, Pred_common, INPFC_AREA, PRED_LEN) %>% 
  distinct(Year, uniqueID, Pred_common, INPFC_AREA, PRED_LEN) %>% 
  group_by(Year, Pred_common, INPFC_AREA) %>% 
  mutate(n_stomachs = length(unique(uniqueID)),
         mean_len = mean(PRED_LEN),
         sd_len = sd(PRED_LEN),
         min_len = min(PRED_LEN),
         max_len = max(PRED_LEN)) %>% 
  distinct(Year, Pred_common, n_stomachs, mean_len, sd_len, min_len, max_len) %>% 
  group_by(Pred_common) %>% 
  mutate(meta_mean = mean(mean_len),
         meta_sd = sd(mean_len))

#DECISION
#'for now I'm going to ignore this question, but I think that I need to think about it more. For example if one model
#'is mostly large sizes in 2011, but then the next year is mostly small sizes we may see a difference in the networks
#'that is due to ontogeny that we might accidentally attribute to temperature
#'Similarly if one year we have a large sample size and the next a small



#---------------------------
#Question 6:
#What metrics should I pull from the networks to use for GAMS?
#In-Degree, Out-Degree, Degree Centrality
#Clustering Coefficient
#Network diameter
#Average path length
#Weighted Edges: Node strength, closeness, betweeness, clustering coeff redefined by using a triplet value WGCNA R Package?


#----------------------------
#Question 7:
#Should I be modeling predators and prey of the same species as one node?
unique(network_sc_filtered$Pred_Species)
#The Pred_Species matches the Prey_name_clean in the groupings doc
#The Pred_Common matches the Prey_Name in the original document

#Decision: Yes I should be modeling predators and prey as the same node for the big 4 species.

#-----------------------------
#Question 8:
#How should I weight the edges in the network.
#Percent frequency of occurrence, Percent Weight of prey


#-----------------------------
#Question 9:
#Should I include empty stomachs?



#-----------------------------
#Output Files
#These files are the outputs from my decisions about what to model. I will use these files to generate the networks
#in a different R script.

network_sc_filtered <- network_sc_filtered %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN), sep = "")

write.csv(network_sc_filtered, here("output/source_data/network_sc_filtered.csv"), row.names = F)
