#Author: Catalina Burch
#Date Modified: 5/21/23
#Description: This file contains summary figures included in Appendix1

#Load Libraries
library(here) #for finding working directory
library(readr) #for loading CSV
library(tidyverse)

#Load Data
focal <- read_csv(here("data/data.csv"))
allpred <- read_csv(here("data/all_pred_data.csv"))

length(unique(allpred$uniqueID))
class(allpred$uniqueID)
#-------------- Appendix 1 -------------
#Calculate Percent weight by species and year
APX1 <- focal %>% 
  group_by(Pred_common, Len_bin_sample, Year) %>% 
  mutate(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Pred_common, Len_bin_sample, stock_groupings, Year) %>% 
  mutate(PreyWt = sum(PREY_TWT), PW = (PreyWt/TotalWt)*100) %>% 
  distinct(Pred_common, Len_bin_sample, stock_groupings, Year, TotalWt,PreyWt, PW, n)

#ordering the length bins
APX1$Len_bin_sample <- factor(APX1$Len_bin_sample, 
                            levels = c("<25", "<29", "<39",
                                       "25-39", "29-44", "29-49", "39-69", "40-54", "45-59",
                                       ">39", ">49", ">54", ">59", ">69"))
unique(APX1$Len_bin_sample)

#Colors for plot
colorlist<-c('#8A0000', "#C80000", '#e34a33', '#fc8d59', '#fdcc8a', '#fef0d9', 'white', 
                      '#f1eef6', '#d0d1e6', '#a6bddb', '#74a9cf', '#3690c0', '#0570b0', '#034e7b')
                      
#Ordering the prey items
APX1$stock_groupings <- factor(APX1$stock_groupings, 
                                 levels = c("Arthropoda", "commercial crab", "copepod", "benthic invertebrate", "euphausiids",
                                            "zooplankton", "other","cod", "flatfish",
                                            "walleye pollock","salmon","rockfish", "forage fish", "other fish"))

#Appendix Plot 1
APXPlot1 <- APX1 %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  ggplot(aes(x = Year, y = PW, fill = stock_groupings)) + 
  geom_bar(stat = "identity", show.legend = F) +  
  geom_text(aes(x = Year, y = 103, label = ifelse(n<=30, "*", "")), color = '#b30000') +
  facet_wrap(~Len_bin_sample) +
  scale_fill_manual(values = colorlist, name = "Prey Groupings") +
  labs(x = "Year", y = "Percent weight ", title = "Arrowtooth Flounder Diet") +
  scale_y_continuous(expand = c(0,4)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

#Export figure
ggsave(plot = APXPlot1, device = png, path = here("output/Appendix"), filename = "APXPlot1.png", dpi = 300,
       height = 3, width = 8)

#Appendix Plot 2
APXPlot2 <- APX1 %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  ggplot(aes(x = Year, y = PW, fill = stock_groupings)) + 
  geom_bar(stat = "identity", show.legend = F) +  
  geom_text(aes(x = Year, y = 103, label = ifelse(n<=30, "*", "")), color = '#b30000') +
  facet_wrap(~Len_bin_sample) +
  scale_fill_manual(values = colorlist, name = "Prey Groupings") +
  labs(x = "Year", y = "Percent weight ", title = "Pacific halibut Diet") +
  scale_y_continuous(expand = c(0,4)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

#Export figure
ggsave(plot = APXPlot2, device = png, path = here("output/Appendix"), filename = "APXPlot2.png", dpi = 300,
       height = 3, width = 8)

#Appendix Plot 3
APXPlot3 <- APX1 %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  ggplot(aes(x = Year, y = PW, fill = stock_groupings)) + 
  geom_bar(stat = "identity", show.legend = T) +  
  geom_text(aes(x = Year, y = 103, label = ifelse(n<=30, "*", "")), color = '#b30000') +
  facet_wrap(~Len_bin_sample) +
  scale_fill_manual(values = colorlist, name = "Prey Groupings") +
  labs(x = "Year", y = "Percent weight ", title = "Walleye pollock Diet") +
  scale_y_continuous(expand = c(0,4)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

#Export figure
ggsave(plot = APXPlot3, device = png, path = here("output/Appendix"), filename = "APXPlot3.png", dpi = 300,
       height = 5, width = 9)

#Appendix Plot 4
APXPlot4 <- APX1 %>% 
  filter(Pred_common == "Pacific cod") %>% 
  ggplot(aes(x = Year, y = PW, fill = stock_groupings)) + 
  geom_bar(stat = "identity", show.legend = T) +  
  geom_text(aes(x = Year, y = 103, label = ifelse(n<=30, "*", "")), color = '#b30000') +
  facet_wrap(~Len_bin_sample) +
  scale_fill_manual(values = colorlist, name = "Prey Groupings") +
  labs(x = "Year", y = "Percent weight ", title = "Pacific cod Diet") +
  scale_y_continuous(expand = c(0,4)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

#Export figure
ggsave(plot = APXPlot4, device = png, path = here("output/Appendix"), filename = "APXPlot4.png", dpi = 300,
       height = 5, width = 9)



#----------- Appendix 2 -----------------
#plots showing occurrences of key prey items over time
#Note: I did not group hauls into one stomach

APX2 <- focal %>% 
  group_by(Pred_common, Len_bin_sample, Year) %>% 
  mutate(totn = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Pred_common, Len_bin_sample, Prey_Name, Year) %>% 
  mutate(preyn = length(unique(uniqueID)),
         PO = (preyn/totn)*100) %>% 
  distinct(Pred_common, Len_bin_sample, Prey_Name, totn, Year, PO)

APX2$Len_bin_sample <- factor(APX2$Len_bin_sample, 
                              levels = c("<25", "<29", "<39",
                                         "25-39", "29-44", "29-49", "39-69", 
                                         "40-54", "45-59",
                                         ">49", ">54", ">59", ">69"))

levels(APX2$Len_bin_sample) = c("1", "1", "1",
                                  "2", "2", "2", "2", 
                                  "3", "3",
                                  "3", "4", "4", "3")



colorlist2<-c('#a6bddb', '#3690c0', '#0570b0', '#034e7b')

#Plot 1: Euphausiids
APX2Plot1 <- APX2 %>% 
  filter(Prey_Name == "Euphausiacea") %>% 
  ggplot(aes(x = Year, y = PO, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Euphausiid Occurrence in Diets", y = "Percent Occurrence", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

#Export figure
ggsave(plot = APX2Plot1, device = png, path = here("output/Appendix"), filename = "APX2Plot1.png", dpi = 300,
       height = 8, width = 14)

#Plot 2: Pollock
APX2Plot2 <- APX2 %>% 
  filter(Prey_Name == "Walleye pollock") %>% 
  ggplot(aes(x = Year, y = PO, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Walleye Pollock Occurrence in Diets", y = "Percent Occurrence", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX2Plot2, device = png, path = here("output/Appendix"), filename = "APX2Plot2.png", dpi = 300,
       height = 8, width = 14)

#Plot 3: Osmerids
APX2Plot3 <- APX2 %>% 
  filter(Prey_Name == "Osmerid") %>% 
  ggplot(aes(x = Year, y = PO, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Osmeridae Occurrence in Diets", y = "Percent Occurrence", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX2Plot3, device = png, path = here("output/Appendix"), filename = "APX2Plot3.png", dpi = 300,
       height = 8, width = 14)

#Plot 4: Ammodytidae
APX2Plot4 <- APX2 %>% 
  filter(Prey_Name == "Ammodytidae") %>% 
  ggplot(aes(x = Year, y = PO, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Ammodytidae Occurrence in Diets", y = "Percent Occurrence", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX2Plot4, device = png, path = here("output/Appendix"), filename = "APX2Plot4.png", dpi = 300,
       height = 8, width = 14)

#Plot 5: Clupeidae
APX2Plot5 <- APX2 %>% 
  filter(Prey_Name == "Clupeoidei") %>% 
  ggplot(aes(x = Year, y = PO, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Clupeidae Occurrence in Diets", y = "Percent Occurrence", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX2Plot5, device = png, path = here("output/Appendix"), filename = "APX2Plot5.png", dpi = 300,
       height = 8, width = 14)

#Plot 6: Paguridae
APX2Plot6 <- APX2 %>% 
  filter(Prey_Name == "Paguridae") %>% 
  ggplot(aes(x = Year, y = PO, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Paguridae Occurrence in Diets", y = "Percent Occurrence", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX2Plot6, device = png, path = here("output/Appendix"), filename = "APX2Plot6.png", dpi = 300,
       height = 8, width = 14)

#Plot 7: Pandalidae
APX2Plot7 <- APX2 %>% 
  filter(Prey_Name == "Pandalidae (shrimp)") %>% 
  ggplot(aes(x = Year, y = PO, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Pandalidae Occurrence in Diets", y = "Percent Occurrence", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX2Plot7, device = png, path = here("output/Appendix"), filename = "APX2Plot7.png", dpi = 300,
       height = 8, width = 14)

#Plot 8: Tanner Crab
APX2Plot8 <- APX2 %>% 
  filter(Prey_Name == "Tanner Crab") %>% 
  ggplot(aes(x = Year, y = PO, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Tanner Crab Occurrence in Diets", y = "Percent Occurrence", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX2Plot8, device = png, path = here("output/Appendix"), filename = "APX2Plot8.png", dpi = 300,
       height = 8, width = 14)


#----------Appendix 3 ------------------
#Using the metric of PSIRI


APX3 <- focal %>% 
  drop_na(PREY_CNT, PREY_TWT) %>% 
  group_by(Pred_common, Len_bin_sample, Year) %>% 
  mutate(TotalWt = sum(PREY_TWT), totn = length(unique(uniqueID)), count = sum(PREY_CNT)) %>% 
  ungroup() %>% 
  group_by(Pred_common, Len_bin_sample, Prey_Name, Year) %>% 
  mutate(PreyWt = sum(PREY_TWT), preyn = length(unique(uniqueID)), preycount = sum(PREY_CNT),
         PW = (PreyWt/TotalWt)*100, PO = (preyn/totn)*100, PN = (preycount/count)*100,
         PSIRI = (PO*(PN+PW))/2) %>% 
  distinct(Pred_common, Len_bin_sample, stock_groupings, Year, totn, PSIRI)

APX3$Len_bin_sample <- factor(APX3$Len_bin_sample, 
                                levels = c("<25", "<29", "<39",
                                           "25-39", "29-44", "29-49", "39-69", 
                                           "40-54", "45-59",
                                           ">49", ">54", ">59", ">69"))

levels(APX3$Len_bin_sample) = c("1", "1", "1",
                                  "2", "2", "2", "2", 
                                  "3", "3",
                                  "3", "4", "4", "3")

#Plot 1: Euphausiids
APX3Plot1 <- APX3 %>% 
  filter(Prey_Name == "Euphausiacea") %>% 
  ggplot(aes(x = Year, y = PSIRI, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Euphausiid PSIRI in Diets", y = "PSIRI", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

#Export figure
ggsave(plot = APX3Plot1, device = png, path = here("output/Appendix"), filename = "APX3Plot1.png", dpi = 300,
       height = 8, width = 14)

#Plot 2: Pollock
APX3Plot2 <- APX3 %>% 
  filter(Prey_Name == "Walleye pollock") %>% 
  ggplot(aes(x = Year, y = PSIRI, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Walleye Pollock PSIRI in Diets", y = "PSIRI", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX3Plot2, device = png, path = here("output/Appendix"), filename = "APX3Plot2.png", dpi = 300,
       height = 8, width = 14)

#Plot 3: Osmerids
APX3Plot3 <- APX3 %>% 
  filter(Prey_Name == "Osmerid") %>% 
  ggplot(aes(x = Year, y = PSIRI, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Osmeridae PSIRI in Diets", y = "PSIRI", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX3Plot3, device = png, path = here("output/Appendix"), filename = "APX3Plot3.png", dpi = 300,
       height = 8, width = 14)

#Plot 4: Ammodytidae
APX3Plot4 <- APX3 %>% 
  filter(Prey_Name == "Ammodytidae") %>% 
  ggplot(aes(x = Year, y = PSIRI, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Ammodytidae PSIRI in Diets", y = "PSIRI", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX3Plot4, device = png, path = here("output/Appendix"), filename = "APX3Plot4.png", dpi = 300,
       height = 8, width = 14)

#Plot 5: Clupeidae
APX3Plot5 <- APX3 %>% 
  filter(Prey_Name == "Clupeoidei") %>% 
  ggplot(aes(x = Year, y = PSIRI, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Clupeidae PSIRI in Diets", y = "PSIRI", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX3Plot5, device = png, path = here("output/Appendix"), filename = "APX3Plot5.png", dpi = 300,
       height = 8, width = 14)
class(POP$uniqueID)
#Plot 6: Paguridae
APX3Plot6 <- APX3 %>% 
  filter(Prey_Name == "Paguridae") %>% 
  ggplot(aes(x = Year, y = PSIRI, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Paguridae PSIRI in Diets", y = "PSIRI", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX3Plot6, device = png, path = here("output/Appendix"), filename = "APX3Plot6.png", dpi = 300,
       height = 8, width = 14)

#Plot 7: Pandalidae
APX3Plot7 <- APX3 %>% 
  filter(Prey_Name == "Pandalidae (shrimp)") %>% 
  ggplot(aes(x = Year, y = PSIRI, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Pandalidae PSIRI in Diets", y = "PSIRI", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX3Plot7, device = png, path = here("output/Appendix"), filename = "APX3Plot7.png", dpi = 300,
       height = 8, width = 14)

#Plot 8: Tanner Crab
APX3Plot8 <- APX3 %>% 
  filter(Prey_Name == "Tanner Crab") %>% 
  ggplot(aes(x = Year, y = PSIRI, color = Len_bin_sample)) +
  geom_line() +
  geom_point(aes(size = totn), alpha = 0.5) +
  facet_wrap(~Pred_common)+
  scale_color_manual(values = colorlist2, name = "Predator length bin") +
  theme_minimal() +
  labs(title = "Tanner Crab PSIRI in Diets", y = "PSIRI", size = "Sample size") +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30),
        title = element_text(size = 12, face = "bold"))

ggsave(plot = APX3Plot8, device = png, path = here("output/Appendix"), filename = "APX3Plot8.png", dpi = 300,
       height = 8, width = 14)



#------------ Appendix 4 ------------
#This section creates general diet summaries for the other predators sampled by the bottom trawl

#SABLEFISH
#calculate percent weight and sample size based on sablefish specific prey groups
sable <- allpred %>% 
  filter(Pred_common == "Sablefish") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 40, 50, 60, 70, 280))) %>% 
  group_by(Len_bin) %>% 
  mutate(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin, sable_group) %>% 
  mutate(PreyWt = sum(PREY_TWT), PW = (PreyWt/TotalWt)*100) %>% 
  distinct(Pred_common, Len_bin, sable_group, PW, n)
levels(sable$Len_bin) <- c("<40", "40-50", "51-60", "61-70", ">70")

#colors for plot
colorlistS <-c('#8A0000', '#fc8d59', '#fdcc8a', '#fef0d9', 
                        '#c51b8a', '#fa9fb5', '#fde0dd',
                      'white', 
                      '#f1eef6','#a6bddb', '#0570b0', '#034e7b', 'black')

#Ordering the prey items
sable$sable_group <- factor(sable$sable_group, 
                               levels = c("Arthropoda", "benthic invertebrate", "euphausiids", "zooplankton", 
                                          "squid", "jellyfish","octopus",
                                          "other",
                                          "cod", "walleye pollock", "forage fish", "other fish", "offal"))

#Plot diet figure
APX4Plot1 <- sable %>% 
  ggplot( aes(x = Len_bin, y = PW, fill = sable_group)) +
  geom_bar(stat = "identity", show.legend = T) +  
  geom_text(aes(x = Len_bin, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = colorlistS, name = "Prey Groupings") +
  labs(x = "Predator length (cm)", y = "Percent weight", title = "Sablefish Diet") +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"))

#save figure
ggsave(plot = APX4Plot1, device = png, path = here("output/Appendix"), filename = "APX4Plot1.png", dpi = 300,
       height = 4, width = 5)

unique(allpred$Pred_common)


#-------
#Atka mackerel
#calculate percent weight and sample size based on sablefish specific prey groups
atka <- allpred %>% 
  filter(Pred_common == "Atka mackerel") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 40, 280))) %>% 
  group_by(Len_bin) %>% 
  mutate(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin, atka_group) %>% 
  mutate(PreyWt = sum(PREY_TWT), PW = (PreyWt/TotalWt)*100) %>% 
  distinct(Pred_common, Len_bin, atka_group, PW, n)
levels(atka$Len_bin) <- c("<40", ">40")

#colors for plot
colorlistA <-c('#e34a33', '#fdcc8a', '#fef0d9',
                        'white')
                        
#Ordering the prey items
atka$atka_group <- factor(atka$atka_group, 
                            levels = c("copepod", "euphausiids", "zooplankton", 
                                       "other"))

#Plot diet figure
APX4Plot2 <- atka %>% 
  ggplot( aes(x = Len_bin, y = PW, fill = atka_group)) +
  geom_bar(stat = "identity", show.legend = T) +  
  geom_text(aes(x = Len_bin, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = colorlistA, name = "Prey Groupings") +
  labs(x = "Predator length (cm)", y = "Percent weight", title = "Atka Mackerel Diet") +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"))

#save figure
ggsave(plot = APX4Plot2, device = png, path = here("output/Appendix"), filename = "APX4Plot2.png", dpi = 300,
       height = 4, width = 5)



#-------
#Pacific Ocean Perch
#calculate percent weight and sample size based on sablefish specific prey groups
POP <- allpred %>% 
  filter(Pred_common == "Pacific ocean perch") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 280))) %>% 
  group_by(Len_bin) %>% 
  mutate(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin, POP_grouping) %>% 
  mutate(PreyWt = sum(PREY_TWT), PW = (PreyWt/TotalWt)*100) %>% 
  distinct(Pred_common, Len_bin, POP_grouping, PW, n)
levels(POP$Len_bin) <- c("<20","20-30", "31-40", ">40")

#Colors for plot
colorlistP<-c('#8A0000', '#e34a33', '#fc8d59', '#fdcc8a', '#fef0d9', 'white', 
                       '#a6bddb',  '#034e7b')
                      
#Ordering the prey items
POP$POP_grouping <- factor(POP$POP_grouping, 
                               levels = c("Arthropoda", "copepod", "benthic invertebrate", "euphausiids","zooplankton",
                                          "other",
                                          "walleye pollock","salmon","rockfish", "forage fish", "other fish"))

#Plot diet figure
POP %>% 
  ggplot( aes(x = Len_bin, y = PW, fill = POP_grouping)) +
  geom_bar(stat = "identity", show.legend = T) +  
  geom_text(aes(x = Len_bin, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = colorlistP, name = "Prey Groupings") +
  labs(x = "Predator length (cm)", y = "Percent weight", title = "Pacific Ocean Perch Diet") +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"))



#What other predators have been surveyed
test <- allpred %>% 
  group_by(Pred_common) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  distinct(Pred_common, n)

#--------
#Flathead Sole
#calculate percent weight and sample size based on sablefish specific prey groups
Flathead <- allpred %>% 
  filter(Pred_common == "Flathead sole") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 280))) %>% 
  group_by(Len_bin) %>% 
  mutate(TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Len_bin, flathead_groupings) %>% 
  mutate(PreyWt = sum(PREY_TWT), PW = (PreyWt/TotalWt)*100) %>% 
  distinct(Pred_common, Len_bin, flathead_groupings, PW, n)
levels(POP$Len_bin) <- c("<20","20-30", "31-40", ">40")

#Colors for plot
colorlistF<-c('#8A0000', "#C80000", '#fc8d59', '#fdcc8a', '#fef0d9', 
                       "#7a0177", "#c51b8a", "#f768a1", "#fa9fb5", "#fcc5c0", 
                       'white', 
                       '#034e7b')
                       
#Ordering the prey items
Flathead$flathead_groupings <- factor(Flathead$flathead_groupings, 
                           levels = c("Arthropoda","commercial crab", "benthic invertebrate", "euphausiids","zooplankton",
                                      "Bivalvia", "brittle star", "Crangonidae", "Paguridae", "Pandalidae",
                                      "other",
                                      "other fish"))

#Plot diet figure
Flathead %>% 
  ggplot( aes(x = Len_bin, y = PW, fill = flathead_groupings)) +
  geom_bar(stat = "identity", show.legend = T) +  
  geom_text(aes(x = Len_bin, y = 103, label = n), color = '#b30000') +
  scale_fill_manual(values = colorlistF, name = "Prey Groupings") +
  labs(x = "Predator length (cm)", y = "Percent weight", title = "Pacific Ocean Perch Diet") +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_minimal() 










