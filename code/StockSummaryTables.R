#Load libraries
library(readr) 
library(tidyverse)
library(here)
library(patchwork)

#load data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
groupings <- read_csv(here("output/groupings.csv"))

#What months were sampled for each year
months <- raw_stomach_contents2021 %>%
  group_by(Year) %>% 
  summarize(min = min(Month), max = max(Month))

#Joining stomach contents with prey groupings
#methods: groupings was done manually. clean names are all taxonomic classifications
sc_groupings <- left_join(raw_stomach_contents2021,groupings,by="Prey_Name")

#creating sample size table
#Create length bins
stomach_contents_2021 <- sc_groupings %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_"),
         Len_bin_20 = cut(PRED_LEN, breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200,
                                             220, 240, 260, 280)),
         Len_bin_10 = cut(PRED_LEN, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 
                                               120, 130, 140, 150, 160, 170, 180, 190, 200, 210,
                                               220, 230, 240, 250, 260, 270, 280)),
         Len_bin_AF = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 280)),
         Len_bin_HB = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 
                                               120, 280)),
         Len_bin_CD = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 280)))

#____________
sample_size2021 <- stomach_contents_2021 %>%
  filter(PRED_FULL != 1) %>% 
  select(uniqueID, Pred_common, Year) %>% 
  group_by(Pred_common, Year) %>% 
  summarize(n = length(unique(uniqueID)))

filter(sample_size2021, Pred_common == "Big skate")

Barnes <- filter(sample_size2021, Pred_common == "Arrowtooth flounder" | Pred_common == "Pacific halibut")

#why does this not match the tables that Cheryl Barnes made?

#-------------------------
#General Diet tables
#Arrowtooth Flounder
#Methods: I'm removing the empty stomachs
A_Flounder <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Arrowtooth flounder")

P_Halibut <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Pacific halibut")

P_Cod <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Pacific cod")

W_Pollock <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Walleye pollock")

Sablefish <- stomach_contents_2021 %>%
  filter(Pred_common == "Sablefish", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Sablefish")

raw_diet_table <- rbind(A_Flounder, P_Halibut, P_Cod, W_Pollock, Sablefish)

write.csv(raw_diet_table, here("output/raw_diet_table.csv"), row.names = F)

#--------------------------------
#Creating summary figures (using general stock prey grouping) which includes predator length
#recalculating PW including length bins
#Arrowtooth
# Arrowtooth_len <- stomach_contents_2021 %>% 
#   filter(Pred_common == "Arrowtooth flounder", stock_groupings != "empty") %>%
#   group_by(Len_bin_AF, stock_groupings) %>% 
#   summarise(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID)))  %>% 
#   mutate(PW = (TotalWt/(sum(TotalWt))*100))

Arrowtooth_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Arrowtooth flounder", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_AF) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_AF, stock_arrowtooth) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Halibut
# Halibut_len <- stomach_contents_2021 %>% 
#   filter(Pred_common == "Pacific halibut", stock_groupings != "empty") %>%
#   group_by(Len_bin_10, stock_groupings) %>% 
#   summarise(TotalWt = sum(PREY_TWT))  %>% 
#   mutate(PW = (TotalWt/(sum(TotalWt))*100))

Halibut_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific halibut", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_HB) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_HB, stock_halibut) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Pollock
# Pollock_len <- stomach_contents_2021 %>% 
#   filter(Pred_common == "Walleye pollock", stock_groupings != "empty") %>%
#   group_by(Len_bin_20, stock_groupings) %>% 
#   summarise(TotalWt = sum(PREY_TWT))  %>% 
#   mutate(PW = (TotalWt/(sum(TotalWt))*100))

Pollock_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Walleye pollock", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_AF) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_AF, stock_pollock) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#cod
# Cod_len <- stomach_contents_2021 %>% 
#   filter(Pred_common == "Pacific cod", stock_groupings != "empty") %>%
#   group_by(Len_bin_20, stock_groupings) %>% 
#   summarise(TotalWt = sum(PREY_TWT))  %>% 
#   mutate(PW = (TotalWt/(sum(TotalWt))*100))

Cod_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific cod", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_CD) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_CD, stock_cod) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Plotting
#colorlist<-c('#e6194b', '#ffe119', '#bcf60c', '#3cb44b', '#46f0f0', #'#f58231',
             #'#4363d8', '#911eb4', '#f032e6', '#800000', '#9A6324', 
             #'#ffd8b1', '#808000', '#fffac8', '#000075', '#008080',   
             #'#aaffc3', '#e6beff', '#a9a9a9', '#fabebe')

colorlist<-c('#b30000', '#e34a33', '#fc8d59', '#fdcc8a', '#fef0d9', 'black', 
             '#f1eef6', '#d0d1e6', '#a6bddb', '#74a9cf', '#3690c0', '#0570b0', '#034e7b')

arrowcolor <- c('#b30000', '#fdcc8a', "black",
                '#f1eef6', '#d0d1e6', '#a6bddb', '#0570b0', '#034e7b')

halibutcolor <- c('#b30000', '#fef0d9', 'black',
                  '#f1eef6', '#d0d1e6', '#a6bddb','#74a9cf', '#3690c0', '#0570b0', '#034e7b')

pollockcolor <- c('#b30000', '#e34a33', '#fdcc8a', '#fef0d9', 'black',
                  '#a6bddb', '#0570b0', '#034e7b')

codcolor <- c('#b30000', '#fc8d59', '#fdcc8a', '#fef0d9', 'black',
              '#a6bddb', '#0570b0', '#034e7b')


#Arrowtooth
Arrowtooth_len$stock_arrowtooth <- factor(Arrowtooth_len$stock_arrowtooth, 
                                          levels = c("arthropoda", "euphausiids", "other","cod", "flatfish",
                                                     "walleye pollock", "forage fish", "other fish"))
#Outdated
# Arrow_Stock <- ggplot(Arrowtooth_len, aes(x = Len_bin_10, y = PW, fill = stock_groupings)) +
#   geom_bar(stat = "identity") + 
#   scale_fill_manual(values = colorlist) +
#   labs(title = "Arrowtooth Diet", y = "percent weight", 
#        x = "predator length") +
#   theme_minimal()

#Current Version
Arrow_Stock <- ggplot(Arrowtooth_len, aes(x = Len_bin_AF, y = PW, fill = stock_arrowtooth)) +
  geom_bar(stat = "identity", show.legend = T) + 
  geom_text(aes(x = Len_bin_AF, y = 102, label = n), color = '#b30000') +
  scale_fill_manual(values = arrowcolor) +
  labs(title = "Arrowtooth Flounder", y = "", 
       x = "") +
  scale_x_discrete(labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", ">70"),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_classic()

ggsave("Arrow_stock_legend.jpg", plot = Arrow_Stock, 
       path = here("output/Figure 1"), device = "jpg")

#Halibut OUTDATED
# Halibut_Stock <- ggplot(Halibut_len, aes(x = Len_bin_HB, y = PW, fill = stock_groupings)) +
#   geom_bar(stat = "identity") + 
#   scale_fill_manual(values = colorlist) +
#   labs(title = "Halibut Diet", y = "percent weight", 
#        x = "predator length") +
#   theme_minimal()

#current
Halibut_len$stock_halibut <- factor(Halibut_len$stock_halibut, 
                                          levels = c("arthropoda", "zooplankton", "other","cod", "flatfish",
                                                     "walleye pollock", "salmon", "rockfish", "forage fish", "other fish"))

Halibut_Stock <- ggplot(Halibut_len, aes(x = Len_bin_HB, y = PW, fill = stock_halibut)) +
  geom_bar(stat = "identity", show.legend = T) + 
  geom_text(aes(x = Len_bin_HB, y = 102, label = n), color = '#b30000') +
  scale_fill_manual(values = halibutcolor) +
  labs(title = "Pacific Halibut", y = "", 
       x = "") +
  scale_x_discrete(labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90",
                              "90-100", "100-110", "110-120", ">120"),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_classic()

ggsave("Halibut_stock_legend.jpg", plot = Halibut_Stock, 
       path = here("output/Figure 1"), device = "jpg")

#Pollock
# Pollock_Stock <- ggplot(Pollock_len, aes(x = Len_bin_20, y = PW, fill = stock_groupings)) +
#   geom_bar(stat = "identity") + 
#   scale_fill_manual(values = colorlist) +
#   labs(title = "Pollock Diet", y = "percent weight", 
#        x = "predator length") +
#   theme_minimal()

#sablefish didn't appear in my initial groupings does this mean sablefish have
#never been found in pollock diets?

Pollock_len$stock_pollock <- factor(Pollock_len$stock_pollock, 
                                          levels = c("arthropoda", "copepod", "euphausiids", "zooplankton", "other",
                                                     "walleye pollock", "forage fish", "other fish"))

Pollock_Stock <- ggplot(Pollock_len, aes(x = Len_bin_AF, y = PW, fill = stock_pollock)) +
  geom_bar(stat = "identity", show.legend = T) + 
  geom_text(aes(x = Len_bin_AF, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = pollockcolor) +
  labs(title = "Walleye Pollock", y = "percent weight (cm)", 
       x = "predator length (cm)") +
  scale_x_discrete(labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", ">70"),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_classic()

ggsave("Pollock_stock_legend.jpg", plot = Pollock_Stock, 
       path = here("output/Figure 1"), device = "jpg")

#outdated
# Cod_Stock <- ggplot(Cod_len, aes(x = Len_bin_20, y = PW, fill = stock_groupings)) +
#   geom_bar(stat = "identity") + 
#   scale_fill_manual(values = colorlist) +
#   labs(title = "Cod Diet", y = "percent weight", 
#        x = "predator length") +
#   theme_minimal()

Cod_len$stock_cod <- factor(Cod_len$stock_cod, 
                                    levels = c("arthropoda", "benthic invert", "euphausiids", "zooplankton", "other",
                                               "walleye pollock", "forage fish", "other fish"))

Cod_Stock <- ggplot(Cod_len, aes(x = Len_bin_CD, y = PW, fill = stock_cod)) +
  geom_bar(stat = "identity", show.legend = T) + 
  geom_text(aes(x = Len_bin_CD, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = codcolor) +
  labs(title = "Pacific Cod", y = "", 
       x = "") +scale_x_discrete(labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", ">80"),
                                                expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_classic()

ggsave("Cod_stock_legend.jpg", plot = Cod_Stock, 
       path = here("output/Figure 1"), device = "jpg")

#Combine
Fig1 <- (Arrow_Stock + Halibut_Stock) / (Pollock_Stock + Cod_Stock)

ggsave("Fig1L.pdf", plot = Fig1, 
       path = here("output/Figure 1"), device = "pdf", height = 10, width = 15)

#save
ggsave("Arrow_stock_10.jpg", plot = Arrow_Stock, 
       path = here("output"), device = "jpg")

ggsave("Halibut_stock_20.jpg", plot = Halibut_Stock, 
       path = here("output"), device = "jpg")

ggsave("Pollock_stock_20.jpg", plot = Pollock_Stock, 
       path = here("output"), device = "jpg")

ggsave("Cod_stock_20.jpg", plot = Cod_Stock, 
       path = here("output"), device = "jpg")

#Create a percent weight over time by

#------------
#Figures showing prey abundance over time in different major predator stomachs
#Note this is %by weight for all sizes of each predator. This may be an issue because large or small predators
#may not be consuming much of the prey item.

PW_by_year <- stomach_contents_2021 %>% 
  filter(Pred_common %in% c("Pacific cod", "Walleye pollock", "Pacific halibut", "Arrowtooth flounder",
                            "Sablefish")) %>% 
  group_by(Pred_common, Prey_Name_Clean, Year) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID)))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))
    
herring_line <- PW_by_year %>% 
  filter(Prey_Name_Clean == "Clupeoidei") %>% 
  ggplot(aes(x = Year, y = PW, color = Pred_common))+
  geom_line()+
  geom_text(aes(x = Year, y = PW+2, label = n))+
  theme_minimal()+
  labs(title = "Herring Percent Weight")

herring_line_non <- PW_by_year %>% 
  filter(Prey_Name_Clean == "Clupeoidei") %>% 
  ggplot(aes(x = Year, y = PW, color = Pred_common))+
  geom_line()+
  theme_minimal()+
  labs(title = "Herring Percent Weight")

herring_bar <- PW_by_year %>% 
  filter(Prey_Name_Clean == "Clupeoidei") %>% 
  ggplot(aes(x = Year, y = PW, fill = Pred_common))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  labs(title = "Herring Percent Weight")

#Why is there a year that herring is super high in pollock diets, but 2 fish skew the results?
show <- stomach_contents_2021 %>% 
  filter(Prey_Name_Clean == "Clupeoidei" | Pred_common == "Walleye pollock")

ggsave("herring_line.jpg", plot = herring_line, 
       path = here("output"), device = "jpg")

ggsave("herring_line_non.jpg", plot = herring_line_non, 
       path = here("output"), device = "jpg")

ggsave("herring_bar.jpg", plot = herring_bar, 
       path = here("output"), device = "jpg")

euphasiid_line <- PW_by_year %>% 
  filter(Prey_Name_Clean == "Euphausiacea") %>% 
  ggplot(aes(x = Year, y = PW, color = Pred_common))+
  geom_line()+
  geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "Euphausiids")

ggsave("euphausiid_line.jpg", plot = euphasiid_line, 
       path = here("output"), device = "jpg")

osmerid_line <- PW_by_year %>% 
  filter(Prey_Name_Clean == "Osmeridae") %>% 
  ggplot(aes(x = Year, y = PW, color = Pred_common))+
  geom_line()+
  geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "Osmerid")

ggsave("osmerid_line.jpg", plot = osmerid_line, 
       path = here("output"), device = "jpg")

pollock_line <- PW_by_year %>% 
  filter(Prey_Name_Clean == "Gadus chalcogrammus") %>% 
  ggplot(aes(x = Year, y = PW, color = Pred_common))+
  geom_line()+
  geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "W Pollock")

ggsave("pollock_line.jpg", plot = pollock_line, 
       path = here("output"), device = "jpg")

#Now I'm going to create similar figures but for prey functional groups. 
PW_by_year <- stomach_contents_2021 %>% 
  filter(Pred_common %in% c("Pacific cod", "Walleye pollock", "Pacific halibut", "Arrowtooth flounder",
                            "Sablefish")) %>% 
  group_by(Pred_common, stock_groupings, Year) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID)))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

forage_line <- PW_by_year %>% 
  filter(stock_groupings == "forage fish") %>% 
  ggplot(aes(x = Year, y = PW, color = Pred_common))+
  geom_line()+
  geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "forage fish")

ggsave("forage_line.jpg", plot = forage_line, 
       path = here("output"), device = "jpg")

arthropod_line <- PW_by_year %>% 
  filter(stock_groupings == "arthropoda") %>% 
  ggplot(aes(x = Year, y = PW, color = Pred_common))+
  geom_line()+
  geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "arthropod")

ggsave("arthropod_line.jpg", plot = arthropod_line, 
       path = here("output"), device = "jpg")

raw_stomach_contents2021 %>%
  filter(unique(HAULJOIN)) %>% 
  ggplot(aes(x = START_DATE)) +
  geom_histogram(stat = "count")

#---------------------------------------
#Attempt at Sparklines
#First I will look at the relative occurrence of prey by species over time

#Halibut consuming pollock
Halibut_poll <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Gadus chalcogrammus", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs)

Halibut_pol_plot <- ggplot(Halibut_poll, aes(x = Year, y = walleye_RO)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(limits = c(0,1)) +
  geom_text(aes( x = Year, y = walleye_RO+0.05, label = walleyestomachs)) +
  theme_void()

#Arrowtooth consuming pollock
Arrow_poll <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", ) %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Gadus chalcogrammus", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs)

Arrow_pol_plot <- ggplot(Arrow_poll, aes(x = Year, y = walleye_RO)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(limits = c(0,1)) +
  geom_text(aes( x = Year, y = walleye_RO+0.05, label = walleyestomachs)) +
  theme_void()


#Pollock eating pollock
Poll_poll <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Gadus chalcogrammus", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs)

Poll_pol_plot <- ggplot(Poll_poll, aes(x = Year, y = walleye_RO)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(limits = c(0,1)) +
  geom_text(aes( x = Year, y = walleye_RO+0.05, label = walleyestomachs))+
  theme_void()

#Cod eating pollock
Cod_poll <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Gadus chalcogrammus", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs)

Cod_pol_plot <- ggplot(Cod_poll, aes(x = Year, y = walleye_RO)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(limits = c(0,1)) +
  geom_text(aes( x = Year, y = walleye_RO+0.05, label = walleyestomachs)) +
  theme_classic() +
  labs(y = "relative occurance") +
  theme(axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks=element_blank())

#piece together
Arrow_pol_plot / Halibut_pol_plot / Poll_pol_plot / Cod_pol_plot




  