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
#?????????? I'm not sure why Cheryl created another haul identifier when there was already a HaulJoin Column?
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

#I also tried running hauljoin as a numeric explanitory variale but it didn't work. 
#data$Haul_Join <- as.numeric(data$Haul_Join, length = 1)
#class(data$Haul_Join)

#Check for outlier deep hauls
depth <- data %>% 
  distinct(Haul_Join, GEAR_DEPTH)
plot(depth$GEAR_DEPTH, depth$Haul_Join)
length(unique(depth$GEAR_DEPTH))
hist(depth$GEAR_DEPTH, breaks = 495)
boxplot(depth$GEAR_DEPTH)
summary(depth$GEAR_DEPTH)

#Remove the extreme outlying station in considerably deep water:
#Commented out because: I checked and the depth at this station was 641 which doesn't seem like an outlier
#data <- data %>% 
  #filter(Haul_Join != "148201101201")

# #Reformat the data so each row is an individual stomach and each prey item is a seperate binary column 
# #Note: I'm not using this anymore because I wanted to incorporate haul into my model
# wide_data <- data %>% 
#   mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
#   distinct(uniqueID, Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
#            SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, PRED_LEN, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies because sometimes there are two seperate rows for the same prey item in the same stomach (the prey species may have been a different life stage)
#   pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
#   rename(Walleyepollock = 'Walleye pollock') 
# 
# #check that the each stomach is it's own unique line
# length(unique(wide_data$uniqueID)) #this number matches the number of rows in wide_data

# #Reformat the data so each row is a predator species for each size class in each haul. The prey items of these same size class predators are consolidated and listed as binary rows for each prey item.
# #Note: I'm not using this because I wanted to create different size bins for each predator species. 
# haul_wide <- data %>% 
#   mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
#   distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
#            SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies because sometimes there are two seperate rows for the same prey item in the same stomach (the prey species may have been a different life stage)
#   group_by(Pred_common, Len_bin, Haul_Join) %>% 
#   pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
#   rename(Walleyepollock = 'Walleye pollock') 
# 
# #Double checking that the number of hauls is correct 
# test <- haul_wide %>% 
#   group_by(Pred_common, Len_bin) %>% 
#   mutate(hauls = length(unique(Haul_Join))) %>% 
#   distinct(Pred_common, Len_bin, hauls)
# 
# sum(test$hauls) #this matches the number of rows in the haul_wide dataframe

#Creating a function that transforms the data into wide format based on haul and predator size class.
haul_wide_fun <- function(data) {
  data %>% 
    mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>% #create binary presence absence for each prey item
    distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
             SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, PRED_LEN, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #remove redundancies, i.e. the same prey species listed twice for the same stomach with different life history stages
    group_by(Pred_common, Len_bin, Haul_Join) %>% 
    mutate(Group_Pred_Len = mean(PRED_LEN)) %>% #calculate mean length for each predator in each haul length group
    distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
             SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Group_Pred_Len, Pred_common, Prey_Name, pres_absent) %>% #This is to remove redundancies again. I only want one line for each predator in the haul length bin
    pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% #create wide dataframe with a column for each prey type
    rename(Walleyepollock = 'Walleye pollock') %>% #rename (this was an issue for running the walleyepollock prey model below)
    na.omit() #remove missing environmental data
}

# #create day of year (Julien)
# #Note: not currently using this in my model
# wide_data <- wide_data %>%
#   mutate(date = paste(Month, Day, sep = "-"))
# 
# wide_data$date <- as.Date(wide_data$date, "%m-%d")
# wide_data$julien <- format(wide_data$date, "%j")

#_________________
#Create separate dataframes for each predator species with different length bins based on sampling methods

WP <- data %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>%
  haul_wide_fun() %>% 
  mutate(len_num = as.numeric(Len_bin))
levels(WP$Len_bin) = c("<25", "25-39", "40-54", ">54")

PH <- data %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun() %>% 
  mutate(len_num = as.numeric(Len_bin))
levels(PH$Len_bin) = c("<31", "31-50", "51-70", ">70")

#Note: I couldn't find the sampling bins for PC so I made it the same as WP
PC <- data %>% 
  filter(Pred_common == "Pacific cod") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>% 
  haul_wide_fun() %>% 
  mutate(len_num = as.numeric(Len_bin))
levels(PC$Len_bin) = c("<25", "25-39", "40-54", ">54")

AF <- data %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun() %>% 
  mutate(len_num = as.numeric(Len_bin))
levels(AF$Len_bin) = c("<31", "31-50", "51-70", ">70")

#Sample sizes (COME BACK TO THIS)
pred_list <- list(WP, PH, PC, AF)
prey_list <- c("Euphausiacea", "Walleyepollock", "Ammodytidae", "Clupeoidei", "Osmerid")

output <- matrix(NA, nrow = 16 , ncol = 7)
output[1:16, 1] <- "Euphausiacea"

output[1, 7] <- sum(WP$Euphausiacea)


for(i in 1:length(pred_list)) {
  for(j in 1:length(prey_list)) {
    output[] 
  }
}

PC$Osmerid

sum(WP$Euphausiacea)
x <- WP %>% filter(Len_bin == ">54")
sum(x$Euphausiacea)
#------------------
#Normality and Correlation Analysis????


#-------------------------------
#######################################
#-------------------------------------
#I use na.omit() to remove any rows that are missing environmental data (gear temp/depth)
# 
# WP <- haul_wide %>% 
#   filter(Pred_common == "Walleye pollock") %>% 
#   na.omit()
# 
# PH <- haul_wide %>% 
#   filter(Pred_common == "Pacific halibut") %>% 
#   na.omit()
# 
# PC <- haul_wide %>% 
#   filter(Pred_common == "Pacific cod") %>% 
#   na.omit()
# 
# AF <- haul_wide %>% 
#   filter(Pred_common == "Arrowtooth flounder") %>% 
#   na.omit()

#-------------------------------
#PREY:  EUPHAUSIACEA
#PRED: Walleye Pollock

#Full Model
Euph_WP_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP) + Group_Pred_Len,
    data = WP,
    family = binomial(link = logit), #logistic scale
    method = "GCV.Cp")

summary(Euph_WP_M)

plogis(1.372175)

#Comparing Delta AIC of alternative Models
Euph_WP_fit <- dredge(Euph_WP_M, beta = F, evaluate = T, rank = "AIC", trace = F)
class(Euph_WP_fit)

Euph_WP_fit <- as.data.frame(Euph_WP_fit)

write.csv(Euph_WP_fit, here("output/Models/Pollock_eat_Euph_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Euph_WP_M)
concurvity(Euph_WP_M, full = T)
concurvity(Euph_WP_M, full = F)

#Plotting partial effects
Euph_WP_Plot1 <- visreg(Euph_WP_M, "Year",type = "conditional", scale = "response", #scale creates plot based on probability not log odds
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

ggsave("pollock_eat_euph.jpg", plot = Euph_WP_MainP, device = "jpg", path = here("output/Models"))

data(worldHiresMapEnv) # source world data for plot
vis.gam(Euph_WP_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Euphausiacea Prescence in Pollock Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#------------------------------
#PRED: Pacific Halibut

#Full Model
Euph_PH_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Group_Pred_Len,
                 data = PH,
                 family = binomial(link = logit),
                 method = "GCV.Cp")

summary(Euph_PH_M)

#Comparing Delta AIC of alternative Models
Euph_PH_fit <- dredge(Euph_PH_M, beta = F, evaluate = T, rank = "AIC", trace = F)
class(Euph_PH_fit)

Euph_PH_fit <- as.data.frame(Euph_PH_fit)

write.csv(Euph_PH_fit, here("output/Models/Halibut_eat_Euph_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Euph_PH_M)

#Plotting partial effects
Euph_PH_Plot1 <- visreg(Euph_PH_M, "Year",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Euphausiacea P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Euph_PH_Plot2 <- visreg(Euph_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Euph_PH_Plot3 <- visreg(Euph_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Euph_PH_Plot4 <- visreg(Euph_PH_M, "Group_Pred_Len",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Group_Pred_Len", ylab = "") +
  theme_classic() 


Euph_PH_MainP <- (Euph_PH_Plot1 + Euph_PH_Plot2) / (Euph_PH_Plot3 + Euph_PH_Plot4) + 
  plot_annotation(title = "Predator: Pacific Halibut") + 
  ylab("label")

ggsave("halibut_eat_euph.jpg", plot = Euph_PH_MainP, device = "jpg", path = here("output/Models"))

data(worldHiresMapEnv) # source world data for plot
vis.gam(Euph_PH_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Euphausiacea Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#----------------
  #PRED: Pacific cod

#Full Model
Euph_PC_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Group_Pred_Len,
                 data = PC,
                 family = binomial(link = logit),
                 method = "GCV.Cp")

summary(Euph_PC_M)

#Comparing Delta AIC of alternative Models
Euph_PC_fit <- dredge(Euph_PC_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Euph_PC_fit <- as.data.frame(Euph_PC_fit)

write.csv(Euph_PC_fit, here("output/Models/Cod_eat_Euph_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Euph_PC_M)

#Plotting partial effects
Euph_PC_Plot1 <- visreg(Euph_PC_M, "Year",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Euphausiacea P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Euph_PC_Plot2 <- visreg(Euph_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Euph_PC_Plot3 <- visreg(Euph_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Euph_PC_Plot4 <- visreg(Euph_PC_M, "Group_Pred_Len",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Group_Pred_Len", ylab = "") +
  theme_classic() 


Euph_PC_MainP <- (Euph_PC_Plot1 + Euph_PC_Plot2) / (Euph_PC_Plot3 + Euph_PC_Plot4) + 
  plot_annotation(title = "Predator: Pacific Cod") 

ggsave("Cod_eat_euph.jpg", plot = Euph_PC_MainP, device = "jpg", path = here("output/Models"))

vis.gam(Euph_PC_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Euphausiacea Prescence in Cod Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#----------------
#PRED: Arrowtooth flounder

#Full Model
Euph_AF_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Group_Pred_Len,
                 data = AF,
                 family = binomial(link = logit),
                 method = "GCV.Cp")

summary(Euph_AF_M)

#Comparing Delta AIC of alternative Models
Euph_AF_fit <- dredge(Euph_AF_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Euph_AF_fit <- as.data.frame(Euph_AF_fit)

write.csv(Euph_AF_fit, here("output/Models/AFlounder_eat_Euph_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Euph_AF_M)

#Plotting partial effects
Euph_AF_Plot1 <- visreg(Euph_AF_M, "Year",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Euphausiacea P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Euph_AF_Plot2 <- visreg(Euph_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Euph_AF_Plot3 <- visreg(Euph_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Euph_AF_Plot4 <- visreg(Euph_AF_M, "Group_Pred_Len",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Group_Pred_Len", ylab = "") +
  theme_classic() 


Euph_AF_MainP <- (Euph_AF_Plot1 + Euph_AF_Plot2) / (Euph_AF_Plot3 + Euph_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_euph.jpg", plot = Euph_AF_MainP, device = "jpg", path = here("output/Models"))

data(worldHiresMapEnv) # source world data for plot
vis.gam(Euph_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Euphausiacea Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")



#------------------------------
################################################
#-------------------------------
#----------------
#PREY: Osmeridae
#PRED: Arrowtooth flounder

#Full Model
Euph_AF_M <- gam(Osmerid ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Group_Pred_Len,
                 data = AF,
                 family = binomial(link = logit),
                 method = "GCV.Cp")

summary(Euph_AF_M)

#Comparing Delta AIC of alternative Models
Euph_AF_fit <- dredge(Euph_AF_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Euph_AF_fit <- as.data.frame(Euph_AF_fit)

write.csv(Euph_AF_fit, here("output/Models/AFlounder_eat_Euph_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Euph_AF_M)

#Plotting partial effects
Euph_AF_Plot1 <- visreg(Euph_AF_M, "Year",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Euphausiacea P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Euph_AF_Plot2 <- visreg(Euph_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Euph_AF_Plot3 <- visreg(Euph_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Euph_AF_Plot4 <- visreg(Euph_AF_M, "Group_Pred_Len",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Group_Pred_Len", ylab = "") +
  theme_classic() 


Euph_AF_MainP <- (Euph_AF_Plot1 + Euph_AF_Plot2) / (Euph_AF_Plot3 + Euph_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_euph.jpg", plot = Euph_AF_MainP, device = "jpg", path = here("output/Models"))

data(worldHiresMapEnv) # source world data for plot
vis.gam(Euph_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Euphausiacea Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")













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


