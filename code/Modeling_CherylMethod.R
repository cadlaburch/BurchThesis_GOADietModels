library(here) #for finding working directory
library(readr) #for loading CSV
library(tidyverse)
library(mgcv) #for running gams
library(MuMIn) #for the dredge summary table
library(patchwork) #for combining plots
library(gridExtra) #for laying out plots
options(na.action = "na.fail") 


#DATA ASSEMBLY
#Load food habits data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
data <- raw_stomach_contents2021

#create unique haul identifier
#?????????? I'm not sure why she created another haul identifier when there was already a HaulJoin Column?
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

#Set length bins: for now I've commented this out because I need there to be predator specific length bins
#data <- data %>% 
#  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 29, 39, 49, 59, 69, 1000)))
#levels(data$Len_bin) = c("<30", "30-39", "40-49", "50-59", "60-69", ">70")

#change year to factor
data$Year <- factor(data$Year)

#Remove outlier deep hauls
plot(raw_stomach_contents2021$GEAR_DEPTH, raw_stomach_contents2021$HAULJOIN)
depth <- data %>% 
  distinct(Haul_Join, GEAR_DEPTH)
hist(depth$GEAR_DEPTH, col = "blue")

# Remove the extreme outlying station in considerably deep water:
#Commented out because: I checked and the depth at this station was 641 which doesn't seem like an outlier
#data <- data %>% 
  #filter(Haul_Join == "148201101201")

#I think I need to do a bit of data manipulation to get my data to look more like cheryls which is in a wide format
#I believe that her data is formatted so each line is a predator and the prey items are listed as columns within the pred stomach
#I need to create a unique identifier for each predator
wide_data <- data %>% 
  mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
  distinct(uniqueID, Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
           SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, PRED_LEN, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies because sometimes there are two seperate rows for the same prey item in the same stomach (the prey species may have been a different life stage)
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
  rename(Walleyepollock = 'Walleye pollock') 

#check that the each stomach is it's own unique line
length(unique(wide_data$uniqueID))

#I'm creating a secondary wide dataframe that treats each haul as an individual stomach for each length class of predator
haul_wide <- data %>% 
  mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
  distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
           SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies because sometimes there are two seperate rows for the same prey item in the same stomach (the prey species may have been a different life stage)
  group_by(Pred_common, Len_bin, Haul_Join) %>% 
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
  rename(Walleyepollock = 'Walleye pollock') 

#Double checking that the number of hauls is correct 
test <- haul_wide %>% 
  group_by(Pred_common, Len_bin) %>% 
  mutate(hauls = length(unique(Haul_Join))) %>% 
  distinct(Pred_common, Len_bin, hauls)

sum(test$hauls) #this matches the number of rows in the haul_wide dataframe

#create day of year (Julien)
wide_data <- wide_data %>% 
  mutate(date = paste(Month, Day, sep = "-"))

wide_data$date <- as.Date(wide_data$date, "%m-%d")
wide_data$julien <- format(wide_data$date, "%j")

#_________________
#THIS CHUNK IS TO TEST OUT HAUL GROUPING 
#the reason I had to run each one seperately was so that you could tailor the predator length bins

WP <- data %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>% 
  mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
  group_by(Pred_common, Len_bin, Haul_Join) %>% 
  mutate(Group_Pred_Len = mean(PRED_LEN)) %>% 
  distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
           SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Group_Pred_Len, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies because sometimes there are two seperate rows for the same prey item in the same stomach (the prey species may have been a different life stage)
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
  rename(Walleyepollock = 'Walleye pollock') %>% 
  na.omit()
levels(WP$Len_bin) = c("<25", "25-39", "40-54", ">54")

PH <- data %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
  group_by(Pred_common, Len_bin, Haul_Join) %>% 
  mutate(Group_Pred_Len = mean(PRED_LEN)) %>% 
  distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
           SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Group_Pred_Len, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies because sometimes there are two seperate rows for the same prey item in the same stomach (the prey species may have been a different life stage)
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
  rename(Walleyepollock = 'Walleye pollock') %>% 
  na.omit()
levels(PH$Len_bin) = c("<31", "31-50", "51-70", ">70")

#Note: I couldn't find the sampling bins for PC so I made it the same as WP
PC <- data %>% 
  filter(Pred_common == "Pacific cod") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>% 
  mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
  group_by(Pred_common, Len_bin, Haul_Join) %>% 
  mutate(Group_Pred_Len = mean(PRED_LEN)) %>% 
  distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
           SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Group_Pred_Len, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies because sometimes there are two seperate rows for the same prey item in the same stomach (the prey species may have been a different life stage)
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
  rename(Walleyepollock = 'Walleye pollock') %>% 
  na.omit()
levels(PC$Len_bin) = c("<25", "25-39", "40-54", ">54")

AF <- data %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
  group_by(Pred_common, Len_bin, Haul_Join) %>% 
  mutate(Group_Pred_Len = mean(PRED_LEN)) %>% 
  distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
           SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Group_Pred_Len, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies because sometimes there are two seperate rows for the same prey item in the same stomach (the prey species may have been a different life stage)
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
  rename(Walleyepollock = 'Walleye pollock') %>% 
  na.omit()
levels(AF$Len_bin) = c("<31", "31-50", "51-70", ">70")

#-------------------------------
#######################################
#-------------------------------------
#I use na.omit() to remove any rows that are missing environmental data (gear temp/depth)

WP <- haul_wide %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  na.omit()

PH <- haul_wide %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  na.omit()

PC <- haul_wide %>% 
  filter(Pred_common == "Pacific cod") %>% 
  na.omit()

AF <- haul_wide %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  na.omit()


#PREY:  EUPHAUSIACEA
#PRED: Walleye Pollock

#Full Model
Euph_WP_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Group_Pred_Len,
    data = WP,
    family = binomial(link = logit),
    method = "GCV.Cp")

summary(Euph_WP_M)

#Comparing Delta AIC of alternative Models
Euph_WP_fit <- dredge(Euph_WP_M, beta = F, evaluate = T, rank = "AIC", trace = F)


#Plotting partial effects
Euph_WP_Plot1 <- visreg(Euph_WP_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Euphausiacea P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Euph_WP_Plot2 <- visreg(Euph_WP_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Euph_WP_Plot3 <- visreg(Euph_WP_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Euph_WP_Plot4 <- visreg(Euph_WP_M, "Group_Pred_Len",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Group_Pred_Len", ylab = "") +
  theme_classic() 


Euph_WP_MainP <- (Euph_WP_Plot1 + Euph_WP_Plot2) / (Euph_WP_Plot3 + Euph_WP_Plot4) + 
  plot_annotation(title = "Predator: Walleye Pollock") + 
  ylab("label")

grid.arrange(patchworkGrob(Euph_WP_MainP), left = "Partial Effect on Euphausiacea Prescence (1) Absence (0)")
arrangeGrob(patchworkGrob(Euph_WP_MainP), left = "Partial Effect on Euphausiacea Prescence (1) Absence (0)")

ggsave("pollock_eat_euph.jpg", plot = last_plot(), device = "jpg", path = here("output/Models"))

data(worldHiresMapEnv) # source world data for plot
vis.gam(Euph_WP_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", main="Euphausiacea Prescence in Pollock Stom", too.far=0.025, n.grid=250, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#PRED: Pacific Halibut
Euph_PH_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
                 data = PH,
                 family = binomial(link = logit),
                 method = "GCV.Cp")
summary(Euph_PH_M)

Euph_PH_fit <- dredge(Euph_PH_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(Euph_PH_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Euphausiacea Prescence in Halibut Stom")
visreg(Euph_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Euphausiacea Prescence in Halibut Stom")
visreg(Euph_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Euphausiacea Prescence in Halibut Stom")
visreg(Euph_PH_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Euphausiacea Prescence in Halibut Stom")
visreg(Euph_PH_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Euphausiacea Prescence in Halibut Stom")
visreg(Euph_PH_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Euphausiacea Prescence in Pollock Stom")

  #PRED: Pacific cod
Euph_PC_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
                 data = PC,
                 family = binomial(link = logit),
                 method = "GCV.Cp")
summary(Euph_PC_M)

Euph_PC_fit <- dredge(Euph_PC_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(Euph_PC_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Euphausiacea Prescence in Cod Stom")
visreg(Euph_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Euphausiacea Prescence in Cod Stom")
visreg(Euph_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Euphausiacea Prescence in Cod Stom")
visreg(Euph_PC_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Euphausiacea Prescence in Cod Stom")
visreg(Euph_PC_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Euphausiacea Prescence in Cod Stom")
visreg(Euph_PC_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Euphausiacea Prescence in Cod Stom")

  #PRED: Arrowtooth flounder
Euph_AF_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
                 data = AF,
                 family = binomial(link = logit),
                 method = "GCV.Cp")
summary(Euph_AF_M)

Euph_AF_fit <- dredge(Euph_PC_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(Euph_AF_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Euphausiacea Prescence in AFlounder Stom")
visreg(Euph_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Euphausiacea Prescence in AFlounder Stom")
visreg(Euph_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Euphausiacea Prescence in AFlounder Stom")
visreg(Euph_AF_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Euphausiacea Prescence in AFlounder Stom")
visreg(Euph_AF_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Euphausiacea Prescence in AFlounder Stom")
visreg(Euph_AF_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Euphausiacea Prescence in AFlounder Stom")




#------------------------------
################################################
#-------------------------------
#PREY: Sand Lance
Model_SL <- function (Ammodytidae, data) {
  Model1 <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
                data = data,
                family = binomial(link = logit),
                method = "GCV.Cp")
  return(Model1)
}

#PRED: Walleye Pollock
SL_WP_M <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
               data = WP,
               family = binomial(link = logit),
               method = "GCV.Cp")
summary(SL_WP_M)

SL_WP_fit <- dredge(SL_WP_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(SL_WP_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Sand Lance Prescence in Pollock Stom")
visreg(SL_WP_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Sand Lance Prescence in Pollock Stom")
visreg(SL_WP_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Sand Lance Prescence in Pollock Stom")
visreg(SL_WP_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Sand Lance Prescence in Pollock Stom")
visreg(SL_WP_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Sand Lance Prescence in Pollock Stom")
visreg(SL_WP_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Sand Lance Prescence in Pollock Stom")


#PRED: Pacific Halibut
SL_PH_M <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
               data = PH,
               family = binomial(link = logit),
               method = "GCV.Cp")
summary(SL_PH_M)

SL_PH_fit <- dredge(SL_PH_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(SL_PH_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Sand Lance Prescence in Halibut Stom")
visreg(SL_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Sand Lance Prescence in Halibut Stom")
visreg(SL_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Sand Lance Prescence in Halibut Stom")
visreg(SL_PH_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Sand Lance Prescence in Halibut Stom")
visreg(SL_PH_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Sand Lance Prescence in Halibut Stom")
visreg(SL_PH_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Sand Lance Prescence in Halibut Stom")

#PRED: Pacific cod
SL_PC_M <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
               data = PC,
               family = binomial(link = logit),
               method = "GCV.Cp")
summary(SL_PC_M)

SL_PC_fit <- dredge(SL_PC_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(SL_PC_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Sand Lance Prescence in Cod Stom")
visreg(SL_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Sand Lance Prescence in Cod Stom")
visreg(SL_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Sand Lance Prescence in Cod Stom")
visreg(SL_PC_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Sand Lance Prescence in Cod Stom")
visreg(SL_PC_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Sand Lance Prescence in Cod Stom")
visreg(SL_PC_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Sand Lance Prescence in Cod Stom")

#PRED: Arrowtooth flounder
SL_AF_M <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
               data = AF,
               family = binomial(link = logit),
               method = "GCV.Cp")
summary(SL_AF_M)

SL_AF_fit <- dredge(SL_AF_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(SL_AF_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Sand Lance Prescence in AFlounder Stom")
visreg(SL_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Sand Lance Prescence in AFlounder Stom")
visreg(SL_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Sand Lance Prescence in AFlounder Stom")
visreg(SL_AF_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Sand Lance Prescence in AFlounder Stom")
visreg(SL_AF_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Sand Lance Prescence in AFlounder Stom")
visreg(SL_AF_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Sand Lance Prescence in AFlounder Stom")






#-------------------------------
#################################
#-------------------------------
#PREY: Walleye Pollock
Model_WP <- function (Walleyepollock, data) {
  Model1 <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
                data = data,
                family = binomial(link = logit),
                method = "GCV.Cp")
  return(Model1)
}

#PRED: Walleye Pollock
WP_WP_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
               data = WP,
               family = binomial(link = logit),
               method = "GCV.Cp")
summary(WP_WP_M)

WP_WP_fit <- dredge(WP_WP_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(WP_WP_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Pollock Prescence in Pollock Stom")
visreg(WP_WP_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Pollock Prescence in Pollock Stom")
visreg(WP_WP_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Pollock Prescence in Pollock Stom")
visreg(WP_WP_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Pollock Prescence in Pollock Stom")
visreg(WP_WP_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Pollock Prescence in Pollock Stom")
visreg(WP_WP_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Pollock Prescence in Pollock Stom")


#PRED: Pacific Halibut
WP_PH_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
               data = PH,
               family = binomial(link = logit),
               method = "GCV.Cp")
summary(WP_PH_M)

WP_PH_fit <- dredge(WP_PH_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(WP_PH_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Pollock Prescence in Halibut Stom")
visreg(WP_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Pollock Prescence in Halibut Stom")
visreg(WP_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Pollock Prescence in Halibut Stom")
visreg(WP_PH_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Pollock Prescence in Halibut Stom")
visreg(WP_PH_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Pollock Prescence in Halibut Stom")
visreg(WP_PH_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Pollock Prescence in Halibut Stom")


#PRED: Pacific cod
WP_PC_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
               data = PC,
               family = binomial(link = logit),
               method = "GCV.Cp")
summary(WP_PC_M)

WP_PC_fit <- dredge(WP_PC_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(WP_PC_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Pollock Prescence in Cod Stom")
visreg(WP_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Pollock Prescence in Cod Stom")
visreg(WP_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Pollock Prescence in Cod Stom")
visreg(WP_PC_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Pollock Prescence in Cod Stom")
visreg(WP_PC_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Pollock Prescence in Cod Stom")
visreg(WP_PC_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Pollock Prescence in Cod Stom")


#PRED: Arrowtooth flounder
WP_AF_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
               data = AF,
               family = binomial(link = logit),
               method = "GCV.Cp")
summary(WP_AF_M)

WP_AF_fit <- dredge(WP_AF_M, beta = F, evaluate = T, rank = "AIC", trace = F)

visreg(WP_AF_M, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Pollock Prescence in AFlounder Stom")
visreg(WP_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", 
       ylab = "Pollock Prescence in AFlounder Stom")
visreg(WP_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", 
       ylab = "Pollock Prescence in AFlounder Stom")
visreg(WP_AF_M, "PRED_LEN",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Pred_length", 
       ylab = "Pollock Prescence in AFlounder Stom")
visreg(WP_AF_M, "RLONG", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLONG", 
       ylab = "Pollock Prescence in AFlounder Stom")
visreg(WP_AF_M, "RLAT", type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "RLAT", 
       ylab = "Pollock Prescence in AFlounder Stom")


