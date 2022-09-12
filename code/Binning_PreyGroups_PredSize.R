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

#I would like to create a grouping based on Buckley's 10 common prey groups

print(unique(raw_stomach_contents$Prey_Name))
test <- raw_stomach_contents

test %>% 
  mutate(Buckley_Prey = case_when(Prey_Name == c("Misc Shrimp", "Crangonidae (shrimp)") ~ "Shrimp"))

#############
#Spatial Grouping
#Yang grouped the study area into West, Central and East GOA
#Barnes et al 2020 grouped into both West, Central, East GOA and the 5 INPFC subregions
#Barnes et al 2018 grouped into the 5 INPFC statistical regions and the four IPHC regulatory areas
# Buckley et al 2016 looks at the EBS and groups by sampling strata



#############
#Species size classes
