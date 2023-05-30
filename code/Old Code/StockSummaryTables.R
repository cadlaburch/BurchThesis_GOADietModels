#Load libraries
library(readr) 
library(tidyverse)
library(here)
library(patchwork)
library(extrafont)

#load data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
groupings <- read_csv(here("output/groupings.csv"))

#What months were sampled for each year
months <- raw_stomach_contents2021 %>%
  group_by(Year) %>% 
  summarize(min = min(Month), max = max(Month))

#What years were data collected
year <-unique(raw_stomach_contents2021$Year)
range(raw_stomach_contents2021$Year)
view(year)

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
         Len_bin_CD = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 280)),
         Len_bin_PC_broad = cut(PRED_LEN, breaks = c(0, 30, 60, 280)),
         Len_bin_WP_broad = cut(PRED_LEN, breaks = c(0, 30, 40, 50, 280)),
         Len_bin_AF_broad = cut(PRED_LEN, breaks = c(0, 20, 40, 280)),
         Len_bin_PH_broad = cut(PRED_LEN, breaks = c(0, 30, 60, 280)),
         Len_bin_gen = cut(PRED_LEN, breaks = c(0, 20, 40, 60, 280)))

#____________
#Creating summary figures (using general stock prey grouping) which includes predator length
#Arrowtooth

Arrowtooth_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Arrowtooth flounder", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_AF) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_AF, stock_groupings) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Halibut
Halibut_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific halibut", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_HB) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_HB, stock_groupings) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Pollock
Pollock_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Walleye pollock", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_AF) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_AF, stock_groupings) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#cod
Cod_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific cod", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_CD) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_CD, stock_groupings) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Plotting
#colorlist<-c('#e6194b', '#ffe119', '#bcf60c', '#3cb44b', '#46f0f0', #'#f58231',
             #'#4363d8', '#911eb4', '#f032e6', '#800000', '#9A6324', 
             #'#ffd8b1', '#808000', '#fffac8', '#000075', '#008080',   
             #'#aaffc3', '#e6beff', '#a9a9a9', '#fabebe')

colorlist<-c('#8A0000', "#C80000", '#e34a33', '#fc8d59', '#fdcc8a', '#fef0d9', 'white', 
             '#f1eef6', '#d0d1e6', '#a6bddb', '#74a9cf', '#3690c0', '#0570b0', '#034e7b')

# arrowcolor <- c('#b30000', '#fdcc8a', "white",
#                 '#f1eef6', '#d0d1e6', '#a6bddb', '#0570b0', '#034e7b')
# 
# halibutcolor <- c('#b30000', '#fef0d9', 'white',
#                   '#f1eef6', '#d0d1e6', '#a6bddb','#74a9cf', '#3690c0', '#0570b0', '#034e7b')
# 
# pollockcolor <- c('#b30000', '#e34a33', '#fdcc8a', '#fef0d9', 'white',
#                   '#a6bddb', '#0570b0', '#034e7b')
# 
# codcolor <- c('#b30000', '#fc8d59', '#fdcc8a', '#fef0d9', 'white',
#               '#a6bddb', '#0570b0', '#034e7b')

#legend figure
legend <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific cod", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_CD) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin_CD, stock_groupings) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = unique(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

legend$stock_groupings <- factor(legend$stock_groupings, 
                                          levels = c("Arthropoda", "commercial crab", "copepod", "benthic invertebrate", "euphausiids",
                                                     "zooplankton", "other","cod", "flatfish",
                                                     "walleye pollock","salmon","rockfish", "forage fish", "other fish"))

legend_plot <- ggplot(legend, aes(x = Len_bin_CD, y = PW, fill = stock_groupings)) +
  geom_bar(stat = "identity", show.legend = T) +
  scale_fill_manual(values = colorlist)

ggsave("legend.pdf", plot = legend_plot, 
       path = here("output/Figure 1"), device = "pdf",
       height = 15, width = 10)


#Arrowtooth
# Arrowtooth_len$stock_arrowtooth <- factor(Arrowtooth_len$stock_arrowtooth, 
#                                           levels = c("arthropoda", "euphausiids", "other","cod", "flatfish",
#                                                      "walleye pollock", "forage fish", "other fish"))

Arrowtooth_len$stock_groupings <- factor(Arrowtooth_len$stock_groupings, 
                                 levels = c("Arthropoda", "commercial crab", "copepod", "benthic invertebrate", "euphausiids",
                                            "zooplankton", "other","cod", "flatfish",
                                            "walleye pollock","salmon","rockfish", "forage fish", "other fish"))

Arrow_Stock <- ggplot(Arrowtooth_len, aes(x = Len_bin_AF, y = PW, fill = stock_groupings)) +
  geom_bar(stat = "identity", show.legend = F) + #geom_text(aes(x = Len_bin_AF, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = colorlist) +
  labs(title = "Arrowtooth Flounder", y = "", 
       x = "") +
  scale_x_discrete(labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", ">70"),
                   expand = c(0, 0),
                   guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0),
        axis.text=element_text(size=15),
        plot.title = element_text(size = 20))

ggsave("Arrow_stock_legend.jpg", plot = Arrow_Stock, 
       path = here("output/Figure 1"), device = "jpg")

#this website shows you which fonts are available http://www.cookbook-r.com/Graphs/Fonts/

#Halibut 
# Halibut_len$stock_halibut <- factor(Halibut_len$stock_halibut, 
#                                           levels = c("arthropoda", "zooplankton", "other","cod", "flatfish",
#                                                      "walleye pollock", "salmon", "rockfish", "forage fish", "other fish"))

Halibut_len$stock_groupings <- factor(Halibut_len$stock_groupings, 
                                         levels = c("Arthropoda", "commercial crab", "copepod", "benthic invertebrate", "euphausiids",
                                                    "zooplankton", "other","cod", "flatfish",
                                                    "walleye pollock","salmon","rockfish", "forage fish", "other fish"))

Halibut_Stock <- ggplot(Halibut_len, aes(x = Len_bin_HB, y = PW, fill = stock_groupings)) +
  geom_bar(stat = "identity", show.legend = F) + #geom_text(aes(x = Len_bin_HB, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = colorlist) +
  labs(title = "Pacific Halibut", y = "", 
       x = "") +
  scale_x_discrete(labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90",
                              "90-100", "100-110", "110-120", ">120"),
                   expand = c(0, 0),
                   guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0),
        axis.text=element_text(size=15),
        plot.title = element_text(size = 20))

ggsave("Halibut_stock_legend.jpg", plot = Halibut_Stock, 
       path = here("output/Figure 1"), device = "jpg")

#Pollock
# Pollock_len$stock_pollock <- factor(Pollock_len$stock_pollock, 
#                                           levels = c("arthropoda", "copepod", "euphausiids", "zooplankton", "other",
#                                                      "walleye pollock", "forage fish", "other fish"))

Pollock_len$stock_groupings <- factor(Pollock_len$stock_groupings, 
                                      levels = c("Arthropoda", "commercial crab", "copepod", "benthic invertebrate", "euphausiids",
                                                 "zooplankton", "other","cod", "flatfish",
                                                 "walleye pollock","salmon","rockfish", "forage fish", "other fish"))

Pollock_Stock <- ggplot(Pollock_len, aes(x = Len_bin_AF, y = PW, fill = stock_groupings)) +
  geom_bar(stat = "identity", show.legend = F) + #geom_text(aes(x = Len_bin_AF, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = colorlist) +
  labs(title = "Walleye Pollock", y = "Percent weight", 
       x = "Predator length (cm)") +
  scale_x_discrete(labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", ">70"),
                   expand = c(0, 0),
                   guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_classic()+
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0),
        axis.text=element_text(size=15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 20))

ggsave("Pollock_stock_legend.jpg", plot = Pollock_Stock, 
       path = here("output/Figure 1"), device = "jpg")


#Cod
# Cod_len$stock_cod <- factor(Cod_len$stock_cod, 
#                                     levels = c("arthropoda", "benthic invert", "euphausiids", "zooplankton", "other",
#                                                "walleye pollock", "forage fish", "other fish"))

Cod_len$stock_groupings <- factor(Cod_len$stock_groupings, 
                                      levels = c("Arthropoda", "commercial crab", "copepod", "benthic invertebrate", "euphausiids",
                                                 "zooplankton", "other","cod", "flatfish",
                                                 "walleye pollock","salmon","rockfish", "forage fish", "other fish"))

Cod_Stock <- ggplot(Cod_len, aes(x = Len_bin_CD, y = PW, fill = stock_groupings)) +
  geom_bar(stat = "identity", show.legend = F) + #geom_text(aes(x = Len_bin_CD, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = colorlist) +
  labs(title = "Pacific Cod", y = "", x = "") +
  scale_x_discrete(labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", ">80"),
                   expand = c(0, 0),
                   guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0),
        axis.text=element_text(size=15),
        plot.title = element_text(size = 20))

ggsave("Cod_stock_legend.jpg", plot = Cod_Stock, 
       path = here("output/Figure 1"), device = "jpg")

#Combine
Fig1 <- (Arrow_Stock + Halibut_Stock) / (Pollock_Stock + Cod_Stock)

ggsave("Fig1.pdf", plot = Fig1, 
       path = here("output/Figure 1"), device = "pdf", height = 8, width = 12.5)




#------------
#Figures showing prey abundance over time in different major predator stomachs
#Note this is %by weight for all sizes of each predator. This may be an issue because large or small predators
#may not be consuming much of the prey item.

# PW_by_year <- stomach_contents_2021 %>% 
#   filter(Pred_common %in% c("Pacific cod", "Walleye pollock", "Pacific halibut", "Arrowtooth flounder",
#                             "Sablefish")) %>% 
#   group_by(Pred_common, Prey_Name_Clean, Year) %>% 
#   summarise(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID)))  %>% 
#   mutate(PW = (TotalWt/(sum(TotalWt))*100))
#     
# herring_line <- PW_by_year %>% 
#   filter(Prey_Name_Clean == "Clupeoidei") %>% 
#   ggplot(aes(x = Year, y = PW, color = Pred_common))+
#   geom_line()+
#   geom_text(aes(x = Year, y = PW+2, label = n))+
#   theme_minimal()+
#   labs(title = "Herring Percent Weight")
# 
# herring_line_non <- PW_by_year %>% 
#   filter(Prey_Name_Clean == "Clupeoidei") %>% 
#   ggplot(aes(x = Year, y = PW, color = Pred_common))+
#   geom_line()+
#   theme_minimal()+
#   labs(title = "Herring Percent Weight")
# 
# herring_bar <- PW_by_year %>% 
#   filter(Prey_Name_Clean == "Clupeoidei") %>% 
#   ggplot(aes(x = Year, y = PW, fill = Pred_common))+
#   geom_bar(stat = "identity")+
#   theme_minimal()+
#   labs(title = "Herring Percent Weight")
# 
# #Why is there a year that herring is super high in pollock diets, but 2 fish skew the results?
# show <- stomach_contents_2021 %>% 
#   filter(Prey_Name_Clean == "Clupeoidei" | Pred_common == "Walleye pollock")
# 
# ggsave("herring_line.jpg", plot = herring_line, 
#        path = here("output"), device = "jpg")
# 
# ggsave("herring_line_non.jpg", plot = herring_line_non, 
#        path = here("output"), device = "jpg")
# 
# ggsave("herring_bar.jpg", plot = herring_bar, 
#        path = here("output"), device = "jpg")
# 
# euphasiid_line <- PW_by_year %>% 
#   filter(Prey_Name_Clean == "Euphausiacea") %>% 
#   ggplot(aes(x = Year, y = PW, color = Pred_common))+
#   geom_line()+
#   geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
#   theme_minimal()+
#   labs(title = "Euphausiids")
# 
# ggsave("euphausiid_line.jpg", plot = euphasiid_line, 
#        path = here("output"), device = "jpg")
# 
# osmerid_line <- PW_by_year %>% 
#   filter(Prey_Name_Clean == "Osmeridae") %>% 
#   ggplot(aes(x = Year, y = PW, color = Pred_common))+
#   geom_line()+
#   geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
#   theme_minimal()+
#   labs(title = "Osmerid")
# 
# ggsave("osmerid_line.jpg", plot = osmerid_line, 
#        path = here("output"), device = "jpg")
# 
# pollock_line <- PW_by_year %>% 
#   filter(Prey_Name_Clean == "Gadus chalcogrammus") %>% 
#   ggplot(aes(x = Year, y = PW, color = Pred_common))+
#   geom_line()+
#   geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
#   theme_minimal()+
#   labs(title = "W Pollock")
# 
# ggsave("pollock_line.jpg", plot = pollock_line, 
#        path = here("output"), device = "jpg")
# 
# #Now I'm going to create similar figures but for prey functional groups. 
# PW_by_year <- stomach_contents_2021 %>% 
#   filter(Pred_common %in% c("Pacific cod", "Walleye pollock", "Pacific halibut", "Arrowtooth flounder",
#                             "Sablefish")) %>% 
#   group_by(Pred_common, stock_groupings, Year) %>% 
#   summarise(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID)))  %>% 
#   mutate(PW = (TotalWt/(sum(TotalWt))*100))
# 
# forage_line <- PW_by_year %>% 
#   filter(stock_groupings == "forage fish") %>% 
#   ggplot(aes(x = Year, y = PW, color = Pred_common))+
#   geom_line()+
#   geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
#   theme_minimal()+
#   labs(title = "forage fish")
# 
# ggsave("forage_line.jpg", plot = forage_line, 
#        path = here("output"), device = "jpg")
# 
# arthropod_line <- PW_by_year %>% 
#   filter(stock_groupings == "arthropoda") %>% 
#   ggplot(aes(x = Year, y = PW, color = Pred_common))+
#   geom_line()+
#   geom_text(aes(x = Year, y = PW+2, label = n), color = "grey", alpha = 0.5)+
#   theme_minimal()+
#   labs(title = "arthropod")
# 
# ggsave("arthropod_line.jpg", plot = arthropod_line, 
#        path = here("output"), device = "jpg")
# 
# raw_stomach_contents2021 %>%
#   filter(unique(HAULJOIN)) %>% 
#   ggplot(aes(x = START_DATE)) +
#   geom_histogram(stat = "count")
# 












#---------------------------------------
#Attempt at Sparklines
#First I will look at the relative occurrence of prey by species over time

colorlist2<-c('#a6bddb', '#3690c0', '#034e7b')

#--------------------
#Consumption of Euphausiacea (krill)

#Halibut eating krill
Halibut_krill <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Euphausiacea", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100)))

range(Halibut_krill$walleyestomachs)

#PLOT
Halibut_krill_plot <- ggplot(Halibut_krill, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,75),
                     breaks = c(0, 25, 50, 75)) +
  scale_color_manual(values = colorlist2) +
  labs(y = "", x = "") +
  theme_classic()+
  xlim(1990, 2022) +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

#Arrowtooth consuming krill
Arrow_krill <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", ) %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Euphausiacea", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 530)))

range(Arrow_krill$walleyestomachs)

#PLOT
Arrow_krill_plot <- ggplot(Arrow_krill, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,75),
                     breaks = c(0, 25, 50, 75)) +
  scale_color_manual(values = colorlist2) +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic()+
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))


#Pollock eating krill
Poll_krill <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Euphausiacea", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 650)))

range(Poll_krill$walleyestomachs)

#PLOT
Poll_krill_plot <- ggplot(Poll_krill, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,75),
                     breaks = c(0, 25, 50, 75)) +
  scale_color_manual(values = colorlist2) +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

#Cod eating krill
Cod_krill <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Euphausiacea", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 190)))

range(Cod_krill$walleyestomachs)

#PLOT
Cod_krill_plot <- ggplot(Cod_krill, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(aes(size = samplesize), show.legend = F, alpha = 0.5) +
  scale_y_continuous(limits = c(0,75),
                     breaks = c(0, 25, 50, 75)) +
  theme_classic() +
  scale_color_manual(values = colorlist2) +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

#piece together
krillconsumption <- Arrow_krill_plot / Halibut_krill_plot / Poll_krill_plot / Cod_krill_plot

ggsave("krillconsumption.pdf", plot = krillconsumption, 
       path = here("output/Figure 2"), device = "pdf",
       height = 9, width = 5, dpi = 300)

#---------

#Halibut consuming pollock
Halibut_poll <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Gadus chalcogrammus", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 120)))

range(Halibut_poll$walleyestomachs)

#PLOT
Halibut_pol_plot <- ggplot(Halibut_poll, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_color_manual(values = c('#3690c0', '#034e7b')) +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

#Arrowtooth consuming pollock
Arrow_poll <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", ) %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Gadus chalcogrammus", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 530)))

range(Arrow_poll$walleyestomachs)

#PLOT
Arrow_pol_plot <- ggplot(Arrow_poll, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_color_manual(values = colorlist2) +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

ggsave("legends.pdf", plot = Arrow_pol_plot, 
       path = here("output/Figure 2"), device = "pdf", dpi = 300,
       height = 5, width = 5)

#Pollock eating pollock
Poll_poll <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Gadus chalcogrammus", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 110)))

range(Poll_poll$walleyestomachs)

#PLOT
Poll_pol_plot <- ggplot(Poll_poll, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_color_manual(values = colorlist2) +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

#Cod eating pollock
Cod_poll <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Gadus chalcogrammus", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 110)))

range(Cod_poll$walleyestomachs)

#PLOT
Cod_pol_plot <- ggplot(Cod_poll, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(aes(size = samplesize), alpha = 0.5, show.legend = F) +
  scale_y_continuous(limits = c(0,25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  theme_classic() +
  scale_color_manual(values = colorlist2) +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

#geom_text(aes( x = Year, y = walleye_RO+0.05, label = walleyestomachs)) +

#piece together
pollockconsumption <- Arrow_pol_plot / Halibut_pol_plot / Poll_pol_plot / Cod_pol_plot

ggsave("pollockconsumption.pdf", plot = pollockconsumption, 
       path = here("output/Figure 2"), device = "pdf",
       height = 9, width = 5, dpi = 300)

#--------------------
#Consumption of Ammodytidae (sand lance)

#Halibut
Halibut_sl <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Ammodytidae", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 120)))

#PLOT
Halibut_sl_plot <- ggplot(Halibut_sl, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_color_manual(values = colorlist2) +
  labs() +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

#Arrowtooth consuming pollock
Arrow_sl <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", ) %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Ammodytidae", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 530)))

#PLOT
Arrow_sl_plot <- ggplot(Arrow_sl, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_color_manual(values = colorlist2) +
  labs() +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))


#Pollock eating pollock
Poll_sl <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Ammodytidae", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 110)))

#PLOT
Poll_sl_plot <- ggplot(Poll_sl, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize), alpha = 0.5) +
  scale_y_continuous(limits = c(0,25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_color_manual(values = colorlist2) +
  labs() +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

#Cod eating pollock
Cod_sl <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Ammodytidae", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 30, 100, 110)))

#PLOT
Cod_sl_plot <- ggplot(Cod_sl, aes(x = Year, y = walleye_RO*100, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(aes(size = samplesize), show.legend = F, alpha = 0.5) +
  scale_y_continuous(limits = c(0,25),
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  theme_classic() +
  scale_color_manual(values = colorlist2) +
  labs(y = "", x = "") +
  xlim(1990, 2022) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.ticks=element_line(color = "black"),
        axis.text = element_text(color = "black", size=15),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))


#piece together
slconsumption <- Arrow_sl_plot / Halibut_sl_plot / Poll_sl_plot / Cod_sl_plot

ggsave("sandlanceconsumption.pdf", plot = slconsumption, 
       path = here("output/Figure 2"), device = "pdf",
       height = 9, width = 5)






#below code is the as above, for other species of prey not included in poster












#--------------------
#Consumption of Osmerids (smelts)

#Halibut
Halibut_osm <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Osmeridae", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 120)))

Halibut_osm_plot <- ggplot(Halibut_osm, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.25)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Pacific Halibut") +
  theme_void()

#Arrowtooth consuming pollock
Arrow_osm <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", ) %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Osmeridae", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 530)))

Arrow_osm_plot <- ggplot(Arrow_osm, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.25)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Arrowtooth Flounder") +
  theme_void()


#Pollock eating pollock
Poll_osm <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Osmeridae", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 40)))

Poll_osm_plot <- ggplot(Poll_osm, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.25)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Walleye Pollock") +
  theme_void()

#Cod eating pollock
Cod_osm <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Osmeridae", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 80)))

Cod_osm_plot <- ggplot(Cod_osm, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(aes(size = samplesize), show.legend = F) +
  scale_y_continuous(limits = c(0,0.25)) +
  theme_classic() +
  scale_color_manual(values = colorlist2) +
  labs(y = "relative occurance", title = "Pacific Cod") +
  theme(axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks=element_blank())

#geom_text(aes( x = Year, y = walleye_RO+0.05, label = walleyestomachs)) +

#piece together
osmeridconsumption <- Arrow_osm_plot / Halibut_osm_plot / Poll_osm_plot / Cod_osm_plot

ggsave("osmeridconsumption.pdf", plot = osmeridconsumption, 
       path = here("output/Figure 2"), device = "pdf",
       height = 11, width = 6)

#--------------------
#Consumption of Clupeoidei (herring)

#Halibut
Halibut_herr <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Clupeoidei", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 120)))

Halibut_herr_plot <- ggplot(Halibut_herr, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.25)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Pacific Halibut") +
  theme_void()

#Arrowtooth consuming pollock
Arrow_herr <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", ) %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Clupeoidei", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 530)))

Arrow_herr_plot <- ggplot(Arrow_herr, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.25)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Arrowtooth Flounder") +
  theme_void()


#Pollock eating pollock
Poll_herr <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Clupeoidei", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 40)))

Poll_herr_plot <- ggplot(Poll_herr, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.25)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Walleye Pollock") +
  theme_void()

#Cod eating pollock
Cod_herr <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod") %>% 
  mutate(walleye_p = ifelse(Prey_Name_Clean == "Clupeoidei", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 80)))

Cod_herr_plot <- ggplot(Cod_herr, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(aes(size = samplesize), show.legend = F) +
  scale_y_continuous(limits = c(0,0.25)) +
  theme_classic() +
  scale_color_manual(values = colorlist2) +
  labs(y = "relative occurence (0-0.25)", title = "Pacific Cod") +
  theme(axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks=element_blank())

#geom_text(aes( x = Year, y = walleye_RO+0.05, label = walleyestomachs)) +

#piece together
herringconsumption <- Arrow_herr_plot / Halibut_herr_plot / Poll_herr_plot / Cod_herr_plot

ggsave("herringconsumption.pdf", plot = herringconsumption, 
       path = here("output/Figure 2"), device = "pdf",
       height = 11, width = 6)


#--------------------
#Consumption of Arthropoda

#Halibut
Halibut_art <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(walleye_p = ifelse(stock_groupings == "arthropoda", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 350)))

range(Halibut_art$walleyestomachs)

Halibut_art_plot <- ggplot(Halibut_art, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.75)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Pacific Halibut") +
  theme_void()

#Arrowtooth consuming pollock
Arrow_art <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", ) %>% 
  mutate(walleye_p = ifelse(stock_groupings == "arthropoda", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 530)))

range(Arrow_art$walleyestomachs)

Arrow_art_plot <- ggplot(Arrow_art, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.75)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Arrowtooth Flounder") +
  theme_void()


#Pollock eating pollock
Poll_art <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(walleye_p = ifelse(stock_groupings == "arthropoda", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 650)))

range(Poll_art$walleyestomachs)

Poll_art_plot <- ggplot(Poll_art, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(show.legend = F, aes(size = samplesize)) +
  scale_y_continuous(limits = c(0,0.75)) +
  scale_color_manual(values = colorlist2) +
  labs(title = "Walleye Pollock") +
  theme_void()

#Cod eating pollock
Cod_art <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod") %>% 
  mutate(walleye_p = ifelse(stock_groupings == "arthropoda", 1, 2)) %>% 
  group_by(Year) %>% 
  mutate(totalstomachs = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  filter(walleye_p == 1) %>% 
  group_by(Year, Len_bin_PC_broad) %>% 
  mutate(walleyestomachs = length(unique(uniqueID))) %>% 
  select(Year, walleyestomachs, totalstomachs, PRED_LEN, uniqueID, Len_bin_PC_broad) %>% 
  mutate(walleye_RO = walleyestomachs/totalstomachs,
         samplesize = cut(walleyestomachs, breaks = c(0, 10, 30, 560)))

range(Cod_art$walleyestomachs)

Cod_art_plot <- ggplot(Cod_art, aes(x = Year, y = walleye_RO, color = Len_bin_PC_broad)) +
  geom_line(show.legend = F) + 
  geom_point(aes(size = samplesize), show.legend = F) +
  scale_y_continuous(limits = c(0,0.75)) +
  theme_classic() +
  scale_color_manual(values = colorlist2) +
  labs(y = "relative occurence (0-0.75)", title = "Pacific Cod") +
  theme(axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks=element_blank())

#geom_text(aes( x = Year, y = walleye_RO+0.05, label = walleyestomachs)) +

#piece together
artconsumption <- Arrow_art_plot / Halibut_art_plot / Poll_art_plot / Cod_art_plot

ggsave("arthropodconsumption.pdf", plot = artconsumption, 
       path = here("output/Figure 2"), device = "pdf",
       height = 11, width = 6)
  