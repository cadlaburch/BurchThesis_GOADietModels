library(here) #for finding working directory
library(readr) #for loading CSV
library(writexl) #for exporting 
library(tidyverse)
library(mgcv) #for running gams
library(MuMIn) #for the dredge summary table
library(patchwork) #for combining plots
library(gridExtra) #for laying out plots
library(visreg) #for visualizing partial effects
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

#Check for outlier deep hauls
depth <- data %>% 
  distinct(Haul_Join, GEAR_DEPTH)
plot(depth$GEAR_DEPTH, depth$Haul_Join)
length(unique(depth$GEAR_DEPTH))
hist(depth$GEAR_DEPTH, breaks = 495)
boxplot(depth$GEAR_DEPTH)
summary(depth$GEAR_DEPTH)

#Remove deep hauls
data <- data %>% 
  filter(GEAR_DEPTH <=300)

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
    distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
             SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #This is to remove redundancies again. I only want one line for each predator in the haul length bin
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
  haul_wide_fun() 
levels(WP$Len_bin) = c("<25", "25-39", "40-54", ">54")

PH <- data %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun() 
levels(PH$Len_bin) = c("<31", "31-50", "51-70", ">70")

#Note: I couldn't find the sampling bins for PC so I made it the same as WP
PC <- data %>% 
  filter(Pred_common == "Pacific cod") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>% 
  haul_wide_fun()
levels(PC$Len_bin) = c("<25", "25-39", "40-54", ">54")

AF <- data %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun() 
levels(AF$Len_bin) = c("<31", "31-50", "51-70", ">70")


#-----
# #Sample sizes

samplesize <- matrix(NA, nrow = 4, ncol = 7)
samplesize[1,1] <- "Arrowtooth flounder"
samplesize[2,1] <- "Pacific halibut"
samplesize[3,1] <- "Pacific cod"
samplesize[4,1] <- "Walleye pollock"
colnames(samplesize) <- c("Predator name", "Total stomachs sampled", "Euphausiacea", "Walleyepollock", "Ammodytidae", "Clupeoidei", "Osmeridae")


pred_list <- list(AF, PH, PC, WP)
prey_list <- c("Euphausiacea", "Walleyepollock", "Ammodytidae", "Clupeoidei", "Osmeridae")

for(i in 1:length(pred_list)) {
  for(j in 1:length(prey_list)) {
    samplesize[i, 2] <- nrow(pred_list[[i]]) #total stomachs
    samplesize[i, 3] <- sum(pred_list[[i]]$Euphausiacea)
    samplesize[i, 4]<- sum(pred_list[[i]]$Walleyepollock)
    samplesize[i, 5]<- sum(pred_list[[i]]$Ammodytidae)
    samplesize[i, 6]<- sum(pred_list[[i]]$Clupeoidei)
    samplesize[i, 7]<- sum(pred_list[[i]]$Osmerid)
  }
}

samplesize <- as.data.frame(samplesize)

write.csv(samplesize, here("output/summary_tables/samplesizemodels.csv"), row.names = F)

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
Euph_WP_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
    data = WP,
    family = binomial(link = logit), #logistic scale
    method = "GCV.Cp")

summary(Euph_WP_M)

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
sum(residuals(Euph_WP_M, type = "pearson")^2) / df.residual(Euph_WP_M)

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

Euph_WP_Plot4 <- visreg(Euph_WP_M, "Len_bin",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
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


#----------------
  #PRED: Pacific cod

#Full Model
Euph_PC_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
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

Euph_PC_Plot4 <- visreg(Euph_PC_M, "Len_bin",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
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
Euph_AF_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
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

Euph_AF_Plot4 <- visreg(Euph_AF_M, "Len_bin",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Euph_AF_MainP <- (Euph_AF_Plot1 + Euph_AF_Plot2) / (Euph_AF_Plot3 + Euph_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_euph.jpg", plot = Euph_AF_MainP, device = "jpg", path = here("output/Models"))

vis.gam(Euph_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Euphausiacea Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")



#------------------------------
################################################
#-------------------------------
#----------------
#PREY: Walleyepollock
#PRED: Arrowtooth flounder

#Full Model
WP_AF_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                 data = AF,
                 family = binomial(link = logit),
                 method = "GCV.Cp")

summary(WP_AF_M)

#Comparing Delta AIC of alternative Models
WP_AF_fit <- dredge(WP_AF_M, beta = F, evaluate = T, rank = "AIC", trace = F)

WP_AF_fit <- as.data.frame(WP_AF_fit)

write.csv(WP_AF_fit, here("output/Models/AFlounder_eat_Euph_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(WP_AF_M)

#Plotting partial effects
WP_AF_Plot1 <- visreg(WP_AF_M, "Year",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pollock P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

WP_AF_Plot2 <- visreg(WP_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

WP_AF_Plot3 <- visreg(WP_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

WP_AF_Plot4 <- visreg(WP_AF_M, "Len_bin",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


WP_AF_MainP <- (WP_AF_Plot1 + WP_AF_Plot2) / (WP_AF_Plot3 + WP_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_WP.jpg", plot = WP_AF_MainP, device = "jpg", path = here("output/Models"))


vis.gam(WP_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pollock Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#-------------
#PRED: Pacific cod
#Full Model
WP_PC_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                 data = PC,
                 family = binomial(link = logit),
                 method = "GCV.Cp")

summary(WP_PC_M)

#Comparing Delta AIC of alternative Models
WP_PC_fit <- dredge(WP_PC_M, beta = F, evaluate = T, rank = "AIC", trace = F)

WP_PC_fit <- as.data.frame(WP_PC_fit)

write.csv(WP_PC_fit, here("output/Models/Cod_eat_WP_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(WP_PC_M)

#Plotting partial effects
WP_PC_Plot1 <- visreg(WP_PC_M, "Year",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pollock P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

WP_PC_Plot2 <- visreg(WP_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

WP_PC_Plot3 <- visreg(WP_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

WP_PC_Plot4 <- visreg(WP_PC_M, "Len_bin",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


WP_PC_MainP <- (WP_PC_Plot1 + WP_PC_Plot2) / (WP_PC_Plot3 + WP_PC_Plot4) + 
  plot_annotation(title = "Predator: Pacific Cod") 

ggsave("Cod_eat_WP.jpg", plot = WP_PC_MainP, device = "jpg", path = here("output/Models"))

vis.gam(WP_PC_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pollock Prescence in Cod Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#-----------------
#PRED: Pacific halibut
#Full Model
WP_PH_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = PH,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(WP_PH_M)

#Comparing Delta AIC of alternative Models
WP_PH_fit <- dredge(WP_PH_M, beta = F, evaluate = T, rank = "AIC", trace = F)

WP_PH_fit <- as.data.frame(WP_PH_fit)

write.csv(WP_PH_fit, here("output/Models/Halibut_eat_WP_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(WP_PH_M)

#Plotting partial effects
WP_PH_Plot1 <- visreg(WP_PH_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pollock P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

WP_PH_Plot2 <- visreg(WP_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

WP_PH_Plot3 <- visreg(WP_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

WP_PH_Plot4 <- visreg(WP_PH_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


WP_PH_MainP <- (WP_PH_Plot1 + WP_PH_Plot2) / (WP_PH_Plot3 + WP_PH_Plot4) + 
  plot_annotation(title = "Predator: Pacific Halibut") 

ggsave("Halibut_eat_euph.jpg", plot = WP_PH_MainP, device = "jpg", path = here("output/Models"))

vis.gam(WP_PH_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pollock Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


###################################
#-----------------------------------
###################################

#PREY: Sand Lance
#Pred: Arrowtooth Flounder

#Full Model
SL_AF_M <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = AF,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(SL_AF_M)

#Comparing Delta AIC of alternative Models
SL_AF_fit <- dredge(SL_AF_M, beta = F, evaluate = T, rank = "AIC", trace = F)

SL_AF_fit <- as.data.frame(SL_AF_fit)

write.csv(SL_AF_fit, here("output/Models/AFlounder_eat_SandL_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(SL_AF_M)

#Plotting partial effects
SL_AF_Plot1 <- visreg(SL_AF_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Sand Lance P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

SL_AF_Plot2 <- visreg(SL_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

SL_AF_Plot3 <- visreg(SL_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

SL_AF_Plot4 <- visreg(SL_AF_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


SL_AF_MainP <- (SL_AF_Plot1 + SL_AF_Plot2) / (SL_AF_Plot3 + SL_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_SL.jpg", plot = SL_AF_MainP, device = "jpg", path = here("output/Models"))


vis.gam(SL_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Sand Lance Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#-------------
#PRED: Pacific cod
#Full Model
SL_PC_M <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = PC,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(SL_PC_M)

#Comparing Delta AIC of alternative Models
SL_PC_fit <- dredge(SL_PC_M, beta = F, evaluate = T, rank = "AIC", trace = F)

SL_PC_fit <- as.data.frame(SL_PC_fit)

write.csv(SL_PC_fit, here("output/Models/Cod_eat_SL_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(SL_PC_M)

#Plotting partial effects
SL_PC_Plot1 <- visreg(SL_PC_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Sand Lance P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

SL_PC_Plot2 <- visreg(SL_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

SL_PC_Plot3 <- visreg(SL_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

SL_PC_Plot4 <- visreg(SL_PC_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


SL_PC_MainP <- (SL_PC_Plot1 + SL_PC_Plot2) / (SL_PC_Plot3 + SL_PC_Plot4) + 
  plot_annotation(title = "Predator: Pacific Cod") 

ggsave("Cod_eat_SL.jpg", plot = SL_PC_MainP, device = "jpg", path = here("output/Models"))

vis.gam(SL_PC_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Sand Lance Prescence in Cod Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#-----------------
#PRED: Pacific halibut
#Full Model
SL_PH_M <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = PH,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(SL_PH_M)

#Comparing Delta AIC of alternative Models
SL_PH_fit <- dredge(SL_PH_M, beta = F, evaluate = T, rank = "AIC", trace = F)

SL_PH_fit <- as.data.frame(SL_PH_fit)

write.csv(SL_PH_fit, here("output/Models/Halibut_eat_SL_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(SL_PH_M)

#Plotting partial effects
SL_PH_Plot1 <- visreg(SL_PH_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Sand Lance P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

SL_PH_Plot2 <- visreg(SL_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

SL_PH_Plot3 <- visreg(SL_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

SL_PH_Plot4 <- visreg(SL_PH_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


SL_PH_MainP <- (SL_PH_Plot1 + SL_PH_Plot2) / (SL_PH_Plot3 + SL_PH_Plot4) + 
  plot_annotation(title = "Predator: Pacific Halibut") 

ggsave("Halibut_eat_SL.jpg", plot = SL_PH_MainP, device = "jpg", path = here("output/Models"))

vis.gam(SL_PH_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Sand Lance Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")







################################
#------------------------------
################################

#PREY: Herring
#Pred: Arrowtooth Flounder

#Full Model
Her_AF_M <- gam(Clupeoidei ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = AF,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(Her_AF_M)


#Comparing Delta AIC of alternative Models
Her_AF_fit <- dredge(Her_AF_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Her_AF_fit <- as.data.frame(Her_AF_fit)

write.csv(Her_AF_fit, here("output/Models/AFlounder_eat_Her_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Her_AF_M)

#Plotting partial effects
Her_AF_Plot1 <- visreg(Her_AF_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Herring P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Her_AF_Plot2 <- visreg(Her_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Her_AF_Plot3 <- visreg(Her_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Her_AF_Plot4 <- visreg(Her_AF_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Her_AF_MainP <- (Her_AF_Plot1 + Her_AF_Plot2) / (Her_AF_Plot3 + Her_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_SL.jpg", plot = SL_AF_MainP, device = "jpg", path = here("output/Models"))


vis.gam(Her_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Herring Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")



#-----------------
#PRED: Pacific halibut
#Full Model
Her_PH_M <- gam(Clupeoidei ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = PH,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(Her_PH_M)

#Comparing Delta AIC of alternative Models
Her_PH_fit <- dredge(Her_PH_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Her_PH_fit <- as.data.frame(Her_PH_fit)

write.csv(Her_PH_fit, here("output/Models/Halibut_eat_Her_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Her_PH_M)

#Plotting partial effects
Her_PH_Plot1 <- visreg(Her_PH_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Herring P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Her_PH_Plot2 <- visreg(Her_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Her_PH_Plot3 <- visreg(Her_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Her_PH_Plot4 <- visreg(Her_PH_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Her_PH_MainP <- (Her_PH_Plot1 + Her_PH_Plot2) / (Her_PH_Plot3 + Her_PH_Plot4) + 
  plot_annotation(title = "Predator: Pacific Halibut") 

ggsave("Halibut_eat_Her.jpg", plot = Her_PH_MainP, device = "jpg", path = here("output/Models"))

vis.gam(Her_PH_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Herring Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


##################################
#--------------------------------
####################################

#PREY: Capelin
#Pred: Arrowtooth Flounder

#Full Model
Cap_AF_M <- gam(Osmerid ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = AF,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(Cap_AF_M)


#Comparing Delta AIC of alternative Models
Cap_AF_fit <- dredge(Cap_AF_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Cap_AF_fit <- as.data.frame(Cap_AF_fit)

write.csv(Cap_AF_fit, here("output/Models/AFlounder_eat_Cap_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Cap_AF_M)

#Plotting partial effects
Cap_AF_Plot1 <- visreg(Cap_AF_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Capelin P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Cap_AF_Plot2 <- visreg(Her_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Cap_AF_Plot3 <- visreg(Her_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Cap_AF_Plot4 <- visreg(Her_AF_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Cap_AF_MainP <- (Cap_AF_Plot1 + Cap_AF_Plot2) / (Cap_AF_Plot3 + Cap_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_Cap.jpg", plot = Cap_AF_MainP, device = "jpg", path = here("output/Models"))


vis.gam(Cap_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Capelin Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#-----------------
#PRED: Pacific halibut
#Full Model
Cap_PH_M <- gam(Osmerid ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = PH,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(Cap_PH_M)

#Comparing Delta AIC of alternative Models
Cap_PH_fit <- dredge(Cap_PH_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Cap_PH_fit <- as.data.frame(Cap_PH_fit)

write.csv(Cap_PH_fit, here("output/Models/Halibut_eat_Cap_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Cap_PH_M)

#Plotting partial effects
Cap_PH_Plot1 <- visreg(Cap_PH_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Capelin P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Cap_PH_Plot2 <- visreg(Cap_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Cap_PH_Plot3 <- visreg(Cap_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Cap_PH_Plot4 <- visreg(Cap_PH_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Cap_PH_MainP <- (Cap_PH_Plot1 + Cap_PH_Plot2) / (Cap_PH_Plot3 + Cap_PH_Plot4) + 
  plot_annotation(title = "Predator: Pacific Halibut") 

ggsave("Halibut_eat_Cap.jpg", plot = Cap_PH_MainP, device = "jpg", path = here("output/Models"))

vis.gam(Cap_PH_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Capelin Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#-------------
#PRED: Pacific cod
#Full Model
Cap_PC_M <- gam(Osmerid ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = PC,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(Cap_PC_M)

#Comparing Delta AIC of alternative Models
Cap_PC_fit <- dredge(Cap_PC_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Cap_PC_fit <- as.data.frame(Cap_PC_fit)

write.csv(Cap_PC_fit, here("output/Models/Cod_eat_Cap_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Cap_PC_M)

#Plotting partial effects
Cap_PC_Plot1 <- visreg(Cap_PC_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Capelin P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Cap_PC_Plot2 <- visreg(Cap_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Cap_PC_Plot3 <- visreg(Cap_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Cap_PC_Plot4 <- visreg(Cap_PC_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Cap_PC_MainP <- (Cap_PC_Plot1 + Cap_PC_Plot2) / (Cap_PC_Plot3 + Cap_PC_Plot4) + 
  plot_annotation(title = "Predator: Pacific Cod") 

ggsave("Cod_eat_Cap.jpg", plot = SL_PC_MainP, device = "jpg", path = here("output/Models"))

vis.gam(Cap_PC_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Capelin Prescence in Cod Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#-------------
#PRED: Walleye Pollock
#Full Model
Cap_WP_M <- gam(Osmerid ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = WP,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(Cap_WP_M)

#Comparing Delta AIC of alternative Models
Cap_WP_fit <- dredge(Cap_WP_M, beta = F, evaluate = T, rank = "AIC", trace = F)

Cap_WP_fit <- as.data.frame(Cap_WP_fit)

write.csv(Cap_WP_fit, here("output/Models/Cod_eat_WP_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Cap_WP_M)

#Plotting partial effects
Cap_WP_Plot1 <- visreg(Cap_WP_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Capelin P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Cap_WP_Plot2 <- visreg(Cap_WP_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Cap_WP_Plot3 <- visreg(Cap_WP_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Cap_WP_Plot4 <- visreg(Cap_WP_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Cap_WP_MainP <- (Cap_WP_Plot1 + Cap_WP_Plot2) / (Cap_WP_Plot3 + Cap_WP_Plot4) + 
  plot_annotation(title = "Predator: Pacific Cod") 

ggsave("WP_eat_Cap.jpg", plot = SL_PC_MainP, device = "jpg", path = here("output/Models"))

vis.gam(Cap_WP_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Capelin Prescence in WP Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


############################
#---------------------------
#############################


#PREY: Crabs









