#Load libraries
library(readr) 
library(tidyverse)
library(here)

#load data
raw_stomach_contents <- read_csv(here("data/GOA_Raw_StomachContents.csv"))

#Binning Prey Taxa
unique(raw_stomach_contents$Prey_Name)

Prey_Name <- c("Empty", "Pandalidae (shrimp)", "Crangonidae (shrimp)", "Euphausiacea",
              "Gammaridea", "Paguridae", "Misc Majidae", "Misc Org",
              "Misc Teleost", "Hyperiidea", "Brittle Star", "Tanner Crab",
              "Copepoda", "Misc Shrimp", "Mysidacea", "Osmerid",
              "Teuthida", "Misc Gadidae", "Polychaeta", "Cottid",
              "Walleye pollock", "Cnidaria", "Hippolytidae (shrimp)", "Bivalvia",
              "Misc Flatfish", "Clupeoidei", "Stichaeidae", "Zoarcoidae",
              "Sebastes", "Arrowtooth flounder", "Misc Anomura", "Chaetognatha",
              "Isopoda", "Ctenophora", "Misc Crustacea", "Myctophidae",
              "Ammodytidae", "Gastropod", "Pteropoda", "Octopoda",
              "Offal", "Cancridea", "Rajadae", "Misc Crab",
              "Larvacea", "Misc Amphipoda", "Misc Cephalopoda", "Salmonidae",
              "Misc Worm", "Misc Invert", "Chionoecetes spp.", "Cumacea",
              "Sea Urchin", "Misc Decapoda", "Capreillidea", "Cyclopteridae", 
              "Agonidae", "Misc Lithodidae", "Sea Cucumber", "Misc Non-teleost fish",
              "Sand Dollar", "Sebastelobus", "Bathylagid", "Misc Echinoderm",
              "Unid Eggs", "Misc Mollusca", "Northern rock sole", "Misc Brachyura",
              "Unid Rockfish", "Tunicate", "Misc Bird", "Flathead sole",
              "Macrouridae", "Yellowfin sole", "Atka Mackeral", "Pacific Cod",
              "Fish Eggs", "Southern rock sole", "Misc", "Red King Crab",
              "Lepidopsetta sp", "Misc Hexagrammidae", "Pholidae", "Sablefish",
              "Opilio Crab", "Pacific halibut", "Greenland turbot", "Pacific sandfish",
              "Kamchatka flounder", "Alaska plaice")

BuckleyGroup <- c("Empty", "Shrimps", "Shrimps", "Euphausiids", 
                  "Gammarids", "Other Prey", "Other Prey", "Other Prey",
                  "Fishes", "Hyperiids", "Other Prey", "Other Prey",
                  "Copepods", "Shrimps", "Mysids", "Fishes",
                  "Other Prey", "Fishes", "Other Prey", "Fishes",
                  "Fishes", "Other Prey", "Shrimps", "Other Prey",
                  "Fishes", "Fishes", "Fishes", "Fishes",
                  "Fishes", "Fishes", "Other Prey", "Chaetognaths",
                  "Other Prey", "Other Prey", "Other Prey", "Fishes",
                  "Fishes", "Other Prey", "Other Prey", "Other Prey",
                  "Other Prey", "Other Prey", "Fishes", "Other Prey",
                  "Larvaceans", "Other Prey", "Other Prey", "Fishes",
                  "Other Prey", "Other Prey", "Other Prey", "Shrimps",
                  "Other Prey", "Other Prey", "Shrimps", "Fishes", 
                  "Fishes", "Other Prey", "Other Prey", "Fishes",
                  "Other Prey", "Fishes", "Fishes", "Other Prey",
                  "Other Prey", "Other Prey", "Fishes", "Other Prey",
                  "Fishes", "Other Prey", "Other Prey", "Fishes",
                  "Fishes", "Fishes", "Fishes", "Fishes",
                  "Fishes", "Fishes", "Other Prey", "Other Prey",
                  "Fishes", "Fishes", "Fishes", "Fishes",
                  "Other Prey", "Fishes", "Fishes", "Fishes",
                  "Fishes", "Fishes")
#Questions:  Are comeacean/Capreillidea shrimp? Seems weird that crabs are lumped into other prey
#Should a tunicate be labeled a larvacean? (I labeled as other)
#Should fish eggs be labeled fishes? I labeled as other

PreyCategories <- data.frame(Prey_Name, BuckleyGroup)

# Next, we join the full data table with the new PreyCategories data frame
# to create a new column with PreyGroup for each observation

sc_data <- left_join(raw_stomach_contents,PreyCategories,by="Prey_Name")

#Binning predator lengths
range(sc_data$Pred_len)

sc_data <- sc_data %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 20, 30, 40, 50, 60, 275)))

