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

BuckleyGroup <- c("Other Prey", "Shrimps", "Shrimps", "Euphausiids", 
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
#I labeled "empty" as other prey

PreyCategories <- data.frame(Prey_Name, BuckleyGroup)

# Next, we join the full data table with the new PreyCategories data frame
# to create a new column with PreyGroup for each observation

sc_data <- left_join(raw_stomach_contents,PreyCategories,by="Prey_Name")

#Binning predator lengths
range(sc_data$Pred_len)

sc_data <- sc_data %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 20, 30, 40, 50, 60, 275)))

#Stacked Bar Plots

#Walleye Pollock %W for all years
Poll_PW_AY <- sc_data %>% 
  filter(Pred_nodc == 8791030701) %>%
  group_by(Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

colorlist<-c('#e6194b', '#f58231', '#ffe119', '#a9a9a9', '#3cb44b', 
             '#46f0f0', '#4363d8', '#911eb4', '#f032e6', '#800000')

WalleyePollockAllYears <- ggplot(Poll_PW_AY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorlist) +
  labs(title = "Walleye Pollock All Years", y = "percent weight", 
       x = "predator length")

ggsave("WalleyePollockAllYears.jpg", plot = WalleyePollockAllYears, 
       path = here("output"), device = "jpg")

#Walleye Pollock %W by year
Poll_PW_BY <- sc_data %>% 
  filter(Pred_nodc == 8791030701) %>%
  group_by(Year, Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

WalleyePollockByYear <- ggplot(Poll_PW_BY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Walleye Pollock By Year", y = "percent weight", 
       x = "predator length") 

ggsave("WalleyePollockByYear.jpg", plot = WalleyePollockByYear, 
       path = here("output"), device = "jpg")

#Walleye Pollock %W by year for each size class
WalleyeYearClass <- ggplot(Poll_PW_BY, aes(x = Year, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Len_bin) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Walleye Pollock By Year", y = "percent weight", 
       x = "predator length") 

ggsave("WalleyeYearClass.jpg", plot = WalleyeYearClass, 
       path = here("output"), device = "jpg")

#######
#Pacific Cod %W for all years
Cod_PW_AY <- sc_data %>% 
  filter(Pred_nodc == 8791030401) %>%
  group_by(Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

CodAllYears <- ggplot(Cod_PW_AY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorlist) +
  labs(title = "P. Cod All Years", y = "percent weight", 
       x = "predator length")

ggsave("CodAllYears.jpg", plot = CodAllYears, 
       path = here("output"), device = "jpg")

#Cod %W by year
Cod_PW_BY <- sc_data %>% 
  filter(Pred_nodc == 8791030401) %>%
  group_by(Year, Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

CodByYear <- ggplot(Cod_PW_BY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "P. Cod By Year", y = "percent weight", 
       x = "predator length") 

ggsave("CodByYear.jpg", plot = CodByYear, 
       path = here("output"), device = "jpg")


#######
#Sablefish %W for all years
Sable_PW_AY <- sc_data %>% 
  filter(Pred_nodc == 8827020101) %>%
  group_by(Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

SableAllYears <- ggplot(Sable_PW_AY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorlist) +
  labs(title = "Sablefish All Years", y = "percent weight", 
       x = "predator length")

ggsave("SableAllYears.jpg", plot = SableAllYears, 
       path = here("output"), device = "jpg")

#Sablefish %W by year
Sable_PW_BY <- sc_data %>% 
  filter(Pred_nodc == 8827020101) %>%
  group_by(Year, Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

SableByYear <- ggplot(Sable_PW_BY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Sablefish By Year", y = "percent weight", 
       x = "predator length") 

ggsave("SableByYear.jpg", plot = SableByYear, 
       path = here("output"), device = "jpg")

#######
#Arrowtooth Flounder %W for all years
Arrow_PW_AY <- sc_data %>% 
  filter(Pred_nodc == 8857040102) %>%
  group_by(Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

ArrowAllYears <- ggplot(Arrow_PW_AY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorlist) +
  labs(title = "A. Flounder All Years", y = "percent weight", 
       x = "predator length")

ggsave("ArrowAllYears.jpg", plot = ArrowAllYears, 
       path = here("output"), device = "jpg")

#Arrow %W by year
Arrow_PW_BY <- sc_data %>% 
  filter(Pred_nodc == 8857040102) %>%
  group_by(Year, Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

ArrowByYear <- ggplot(Arrow_PW_BY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "A. Flounder By Year", y = "percent weight", 
       x = "predator length") 

ggsave("ArrowByYear.jpg", plot = ArrowByYear, 
       path = here("output"), device = "jpg")

#######
#Halibut %W for all years
Hal_PW_AY <- sc_data %>% 
  filter(Pred_nodc == 8857041901) %>%
  group_by(Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

HalAllYears <- ggplot(Hal_PW_AY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorlist) +
  labs(title = "Halibut All Years", y = "percent weight", 
       x = "predator length")

ggsave("HalAllYears.jpg", plot = HalAllYears, 
       path = here("output"), device = "jpg")

#Halibut %W by year
Hal_PW_BY <- sc_data %>% 
  filter(Pred_nodc == 8857041901) %>%
  group_by(Year, Len_bin, BuckleyGroup) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

HalByYear <- ggplot(Hal_PW_BY, aes(x = Len_bin, y = PW, fill = BuckleyGroup)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Halibut By Year", y = "percent weight", 
       x = "predator length") 

ggsave("HalByYear.jpg", plot = HalByYear, 
       path = here("output"), device = "jpg")

########################
#Next I want to create some different prey groupings that are more specific to each species

Yang_Cod <- c("Other Prey", "Shrimp", "Shrimp", "Euphausiid",
               "Other Prey", "Other Crab", "Other Crab", "Other Prey",
               "Other Fish", "Other Prey", "Other Prey", "Tanner Crab",
               "Other Prey", "Shrimp", "Shrimp", "Other Fish",
               "Other Prey", "Other Fish", "Polychaeta", "Other Fish",
               "Walleye pollock", "Other Prey", "Shrimp", "Other Prey",
               "Flatfish", "Other Fish", "Other Fish", "Other Fish",
               "Other Fish", "Flatfish", "Other Crab", "Other Prey",
               "Other Prey", "Other Prey", "Other Crab", "Other Fish",
               "Other Fish", "Other Prey", "Other Prey", "Other Prey",
               "Other Prey", "Other Crab", "Other Fish", "Other Crab",
               "Other Prey", "Other Prey", "Other Prey", "Other Fish",
               "Other Prey", "Other Prey", "Other Crab", "Other Prey",
               "Other Prey", "Other Crab", "Shrimp", "Other Fish", 
               "Other Fish", "Other Crab", "Other Prey", "Other Fish",
               "Other Prey", "Other Fish", "Other Fish", "Other Prey",
               "Other Prey", "Other Prey", "Other Fish", "Other Crab",
               "Other Fish", "Other Prey", "Other Prey", "Flatfish",
               "Other Fish", "Flatfish", "Other Fish", "Other Fish",
               "Other Fish", "Flatfish", "Misc", "Other Crab",
               "Flatfish", "Other Fish", "Other Fish", "Other Fish",
               "Other Crab", "Flatfish", "Flatfish", "Other Fish",
               "Flatfish", "Flatfish")
#issue I'm labeling decapods as crabs but the group is larger than that?
#did I get all the flatfish right?

PreyCategories <- data.frame(Prey_Name, BuckleyGroup, Yang_Cod)

# Next, we join the full data table with the new PreyCategories data frame
# to create a new column with PreyGroup for each observation

sc_data <- left_join(raw_stomach_contents,PreyCategories,by="Prey_Name")
cod <-raw_stomach_contents %>% 
  filter(Pred_nodc == 8791030401)

quantile(cod$Pred_len)
#0%  25%  50%  75% 100% 
#7   45   55   62  105 

sc_data <- sc_data %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 10, 20, 30, 40, 50, 60, 105)))

colorlist<-c('#e6194b', '#f58231', '#ffe119', '#a9a9a9', '#3cb44b',
             '#46f0f0', '#4363d8', '#911eb4', '#f032e6', '#800000', '#e6beff')

#Pacific Cod %W for all years
Cod_PW_AY <- sc_data %>% 
  filter(Pred_nodc == 8791030401) %>%
  group_by(Len_bin, Yang_Cod) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

CodAllYears <- ggplot(Cod_PW_AY, aes(x = Len_bin, y = PW, fill = Yang_Cod)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorlist) +
  labs(title = "P. Cod All Years (Yang)", y = "percent weight", 
       x = "predator length")

ggsave("CodAllYearsYang.jpg", plot = CodAllYears, 
       path = here("output"), device = "jpg")

#Cod %W by year
Cod_PW_BY <- sc_data %>% 
  filter(Pred_nodc == 8791030401) %>%
  group_by(Year, Len_bin, Yang_Cod) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

CodByYear <- ggplot(Cod_PW_BY, aes(x = Len_bin, y = PW, fill = Yang_Cod)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "P. Cod By Year (Yang)", y = "percent weight", 
       x = "predator length") 

ggsave("CodByYearYang.jpg", plot = CodByYear, 
       path = here("output"), device = "jpg")

#Cod %W by year for each size class
CodYearClass <- ggplot(Poll_PW_BY, aes(x = Year, y = PW, fill = Yang_Cod)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Len_bin) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Cod By Year (Yang)", y = "percent weight", 
       x = "predator length") 

ggsave("CodYearClassYang.jpg", plot = CodYearClass, 
       path = here("output"), device = "jpg")
