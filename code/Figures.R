#DATA ASSEMBLY
#Load food habits data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
data <- raw_stomach_contents2021

#create unique haul identifier
data$Haul_Join <- paste(data$VESSEL, data$CRUISE, data$HAUL, sep = "")

#Exclude data before 1990
data <- data %>% 
  filter(Year >= 1990)

#Select only predators of interest
data <- data %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder"))

#remove empty stomachs and create unique stomach ID
data <- data %>% 
  filter(Prey_Name != "Empty") %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN), sep = "")

#change year to factor
data$Year <- factor(data$Year)
data$Haul_Join <- factor(data$Haul_Join)

#Remove deep hauls and data entry error
data <- data %>% 
  filter(GEAR_DEPTH <= 300 & GEAR_DEPTH > 0)

groupings <- read_csv(here("output/groupings.csv"))

sc_groupings <- left_join(raw_stomach_contents2021,groupings,by="Prey_Name") %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_"),
         Len_bin = cut(PRED_LEN, breaks = c(0, 20, 40, 60, 80, 280)))

sc_groupings$gam_grouping <-  sc_groupings$gam_grouping %>% replace_na("other")

diet_bylen <- sc_groupings %>% 
  group_by(Pred_common, Len_bin, gam_grouping) %>% 
  summarise(TotalWt = sum(PREY_TWT))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

colorlist<-c('#8A0000', "#C80000", '#e34a33', '#fc8d59', '#fdcc8a', '#fef0d9')


plot <- ggplot(diet_bylen, aes(x = Len_bin, y = PW, fill = gam_grouping)) +
  geom_bar(stat = "identity", show.legend = F) +
  facet_wrap(~Pred_common)
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