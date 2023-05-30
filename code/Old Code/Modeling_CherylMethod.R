library(here) #for finding working directory
library(readr) #for loading CSV
library(writexl) #for exporting 
library(tidyverse)
library(mgcv) #for running gams
library(MuMIn) #for the dredge summary table
library(patchwork) #for combining plots
library(gridExtra) #for laying out plots
library(visreg) #for visualizing partial effects
library(mapdata) #for partial effects map
options(na.action = "na.fail") 


#DATA ASSEMBLY
#Load food habits data
data <- read_csv(here("data/data.csv"))

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

#Check for outlier deep hauls
depth <- data %>% 
#   distinct(Haul_Join, GEAR_DEPTH)
# plot(depth$GEAR_DEPTH, depth$Haul_Join)
# length(unique(depth$GEAR_DEPTH))
hist(data$GEAR_DEPTH, breaks = 495)
# boxplot(depth$GEAR_DEPTH)
# summary(depth$GEAR_DEPTH)

#Remove deep hauls and data entry error
data <- data %>% 
  filter(GEAR_DEPTH <= 300 & GEAR_DEPTH > 0)

# ggplot(data = data, aes(x = GEAR_TEMP, y = GEAR_DEPTH, color = Year))+
#   geom_point()
# 
# ggplot(data = data, aes(x = GEAR_TEMP)) +
#   geom_histogram()
# 
# write.csv(data, here("output/Models/ModelFilgData.csv"), row.names = F)

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
    rename(Walleyepollock = 'Walleye pollock', TannerCrab = 'Tanner Crab',
           Crangonidae = 'Crangonidae (shrimp)', Pandalidae = `Pandalidae (shrimp)`) %>% #rename (this was an issue for running the walleyepollock prey model below)
    mutate(forage = sum(Osmerid, Clupeoidei),
           forage = ifelse(forage > 0, 1, 0)) %>% 
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
levels(WP$Len_bin) = c("1", "2", "3", "4")

PH <- data %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun()  
levels(PH$Len_bin) = c("1", "2", "3", "4")

#Note: I couldn't find the sampling bins for PC so I made it the same as WP
PC <- data %>% 
  filter(Pred_common == "Pacific cod") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>% 
  haul_wide_fun() 
levels(PC$Len_bin) = c("1", "2", "3", "4")

AF <- data %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun()  
levels(AF$Len_bin) = c("1", "2", "3", "4")


#creating dataframe with all predators
all_pred <- rbind(WP, PH, PC, AF)
#-----
# #Sample sizes

samplesize <- matrix(NA, nrow = 4, ncol = 7)
samplesize[1,1] <- "Arrowtooth flounder"
samplesize[2,1] <- "Pacific halibut"
samplesize[3,1] <- "Pacific cod"
samplesize[4,1] <- "Walleye pollock"
colnames(samplesize) <- c("Predator name", "Total stomachs sampled", "Euphausiacea", "Walleye pollock", "Forage Fish", "Pandalidae", "Tanner Crab")


pred_list <- list(AF, PH, PC, WP)
prey_list <- c("Euphausiacea", "Walleyepollock", "Forage", "Pandalidae", "TannerCrab")

for(i in 1:length(pred_list)) {
  for(j in 1:length(prey_list)) {
    samplesize[i, 2] <- nrow(pred_list[[i]]) #total stomachs
    samplesize[i, 3] <- sum(pred_list[[i]]$Euphausiacea)
    samplesize[i, 4]<- sum(pred_list[[i]]$Walleyepollock)
    samplesize[i, 5]<- sum(pred_list[[i]]$forage)
    samplesize[i, 6]<- sum(pred_list[[i]]$Pandalidae)
    samplesize[i, 7]<- sum(pred_list[[i]]$TannerCrab)
  }
}

samplesize <- as.data.frame(samplesize)

write.csv(samplesize, here("output/summary_tables/samplesizemodels.csv"), row.names = F)

#-------------------------------
#######################################
#-------------------------------------
###################################################
#                                                 #  
#            PREY:  EUPHAUSIACEA
#                                                  #
#                                                 #
################################################### 
#Model with predators included as a predictor
#Full Model

Euph <- all_pred %>% 
  filter(Pred_common != "Pacific halibut") 

Euph_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
                 data = Euph,
                 family = binomial(link = logit), #logistic scale
                 method = "GCV.Cp")

summary(Euph_M)

anova(Euph_M)

#Comparing Delta AIC of alternative Models
Euph_fit <- dredge(Euph_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                      extra = c("adjR^2", "deviance"))

Euph_AIC <- as.data.frame(Euph_fit) %>% 
  mutate(Response = "Euphausiid ", Predator = "Walleye pollock")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Euph_WP_M)
concurvity(Euph_WP_M, full = T)
concurvity(Euph_WP_M, full = F)
sum(residuals(Euph_M, type = "pearson")^2) / df.residual(Euph_M)

#Plotting partial effects
Euph_Plot1 <- visreg(Euph_M, "Year",type = "conditional", scale = "response", #scale creates plot based on probability not log odds
                        gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Euphausiacea P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Euph_Plot2 <- visreg(Euph_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Euph_Plot3 <- visreg(Euph_M, "GEAR_TEMP",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Euph_WP_Plot4 <- visreg(Euph_M, "Len_bin",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 

Euph_Plot5 <- visreg(Euph_M, "Pred_common",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
  theme_classic() 


Euph_WP_MainP <- (Euph_WP_Plot1 + Euph_WP_Plot2) / (Euph_WP_Plot3 + Euph_WP_Plot4) + 
  plot_annotation(title = "Predator: Walleye Pollock") + 
  ylab("label")

ggsave("pollock_eat_euph.jpg", plot = Euph_WP_MainP, device = "jpg", path = here("output/Models"))


Euph_WP_Map <- vis.gam(Euph_WP_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
                       contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
                       main="Walleye pollock", too.far=0.025, n.grid=250, 
                       xlim=c(lonmin, lonmax), ylim=c(latmin, latmax)) 
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add = T, col="lightgrey")



#PREY:  EUPHAUSIACEA
#PRED: Walleye Pollock

#Full Model
Euph_WP_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
    data = WP,
    family = binomial(link = logit), #logistic scale
    method = "GCV.Cp")

summary(Euph_WP_M)

anova(Euph_WP_M)

#Comparing Delta AIC of alternative Models
Euph_WP_fit <- dredge(Euph_WP_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                      extra = c("adjR^2", "deviance"))

Euph_WP_AIC <- as.data.frame(Euph_WP_fit) %>% 
  mutate(Response = "Euphausiid ", Predator = "Walleye pollock")

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

Euph_WP_Plot <- visreg(Euph_WP_M, "Pred_common",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
  theme_classic() 

Euph_WP_Plot4 <- visreg(Euph_WP_M, "Len_bin",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Euph_WP_MainP <- (Euph_WP_Plot1 + Euph_WP_Plot2) / (Euph_WP_Plot3 + Euph_WP_Plot4) + 
  plot_annotation(title = "Predator: Walleye Pollock") + 
  ylab("label")

ggsave("pollock_eat_euph.jpg", plot = Euph_WP_MainP, device = "jpg", path = here("output/Models"))

### Plot P/A results, Pacific Halibut ###
# Set coordinate boundaries for plotting:
lonmin = -172
lonmax = -130
latmin = 52
latmax = 62

box <- c(xmin = -172,
                ymin = 52,
                xmax = -130,
                ymax = 62) %>%
  st_bbox(crs = st_crs(4326))

class(world)

data(worldHiresMapEnv) # source world data for plot
world <- read_sf(system.file("shapes/world.gpkg", package="spData"))
coast <- read_sf(here("data", "10m_physical",
                      "ne_10m_coastline.shp"))

ggplot(data = coast) +
  geom_sf(color = "black", fill = "lightgrey") +
  theme_void() + 
  coord_sf(xlim = c(box$xmin, box$xmax),
           ylim = c(box$ymin, box$ymax),
           expand = TRUE)


Euph_WP_Map <- vis.gam(Euph_WP_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Walleye pollock", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax)) 

Euph_WP_Map +
  ggplot(data = coast) +
  geom_sf(color = "black", fill = "lightgrey") +
  theme_void() + 
  coord_sf(xlim = c(box$xmin, box$xmax),
           ylim = c(box$ymin, box$ymax),
           expand = TRUE) 
  
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add = T, col="lightgrey")


ggsave(plot = last_plot(), filename = "test.pdf", path = here("output/test.pdf"), device = pdf)

ggsave("Cod_eat_euph.jpg", plot = Euph_PC_MainP, device = "jpg", path = here("output/Models"))

#----------------
  #PRED: Pacific cod

#Full Model
Euph_PC_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                 data = PC,
                 family = binomial(link = logit),
                 method = "GCV.Cp")

summary(Euph_PC_M)

anova(Euph_PC_M)

#Comparing Delta AIC of alternative Models
Euph_PC_fit <- dredge(Euph_PC_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                      extra = c("adjR^2", "deviance"))

Euph_PC_AIC <- as.data.frame(Euph_PC_fit) %>% 
  mutate(Response = "Euphausiid  ", Predator = "Pacific cod")


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

anova(Euph_WP_M)

#Comparing Delta AIC of alternative Models
Euph_AF_fit <- dredge(Euph_AF_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                      extra = c("adjR^2", "deviance"))

Euph_AF_AIC <- as.data.frame(Euph_AF_fit) %>% 
  mutate(Response = "Euphausiid  ", Predator = "Arrowtooth flounder")


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
###################################################
#                                                 #  
#            PREY:  Walleye pollock
#                                                  #
#                                                 #
################################################### 

#ALL PRED MODEL
WPrey <- all_pred %>% 
  filter(Pred_common != "Walleye pollock") 

#Full Model
WP_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
                 data = WPrey,
                 family = binomial(link = logit), #logistic scale
                 method = "GCV.Cp")

summary(WP_M)

anova(WP_M)

#Comparing Delta AIC of alternative Models
WP_fit <- dredge(WP_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    extra = c("adjR^2", "deviance"))

WP_AIC <- as.data.frame(WP_fit) %>% 
  mutate(Response = "Walleye pollock", Predator = "Arrowtooth flounder")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(WP_M)

#Plotting partial effects
WP_Plot1 <- visreg(WP_M, "Year",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pollock P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

WP_Plot2 <- visreg(WP_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

WP_Plot3 <- visreg(WP_M, "GEAR_TEMP",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

WP_Plot4 <- visreg(WP_M, "Len_bin",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 

WP_Plot5 <- visreg(WP_M, "Pred_common",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
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

anova(WP_PC_M)

#Comparing Delta AIC of alternative Models
WP_PC_fit <- dredge(WP_PC_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    extra = c("adjR^2", "deviance"))

WP_PC_AIC <- as.data.frame(WP_PC_fit) %>% 
  mutate(Response = "Walleye pollock", Predator = "Pacific cod")

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

anova(WP_PH_M)

#Comparing Delta AIC of alternative Models
WP_PH_fit <- dredge(WP_PH_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    extra = c("adjR^2", "deviance"))

WP_PH_AIC <- as.data.frame(WP_PH_fit) %>% 
  mutate(Response = "Walleye pollock", Predator = "Pacific halibut")

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
###################################################
#                                                 #  
#            PREY:  Forage Fish
#                                                  #
#                                                 #
################################################### 
#All preds

For <- all_pred %>% 
  filter(Pred_common == "Arrowtooth flounder" | Pred_common == "Pacific halibut")

For_M <- gam(forage ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
                data = For,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(For_M)

anova(For_M)

#Comparing Delta AIC of alternative Models
For_fit <- dredge(For_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                     extra = c("adjR^2", "deviance"))

For_AIC <- as.data.frame(For_fit) %>% 
  mutate(Response = "Forage Fish", Predator = "Arrowtooth flounder")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(For_M)

#Plotting partial effects
For_Plot1 <- visreg(For_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Forage Fish P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

For_Plot2 <- visreg(For_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

For_Plot3 <- visreg(For_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

For_Plot4 <- visreg(For_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 

For_Plot5 <- visreg(For_M, "Pred_common",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
  theme_classic() 


For_MainP <- (For_AF_Plot1 + For_AF_Plot2) / (For_AF_Plot3 + For_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_For.jpg", plot = For_AF_MainP, device = "jpg", path = here("output/Models"))


vis.gam(For_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Forage Fish Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")



#-------------------
#PREDATOR: Arrowtooth flounder
#PREY: Forage Fish

#Full Model
For_AF_M <- gam(forage ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = AF,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(For_AF_M)

anova(For_AF_M)

#Comparing Delta AIC of alternative Models
For_AF_fit <- dredge(For_AF_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                     extra = c("adjR^2", "deviance"))

For_AF_AIC <- as.data.frame(For_AF_fit) %>% 
  mutate(Response = "Forage Fish", Predator = "Arrowtooth flounder")

write.csv(SL_AF_fit, here("output/Models/AFlounder_eat_SandL_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(For_AF_M)

#Plotting partial effects
For_AF_Plot1 <- visreg(For_AF_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Forage Fish P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

For_AF_Plot2 <- visreg(For_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

For_AF_Plot3 <- visreg(For_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

For_AF_Plot4 <- visreg(For_AF_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


For_AF_MainP <- (For_AF_Plot1 + For_AF_Plot2) / (For_AF_Plot3 + For_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_For.jpg", plot = For_AF_MainP, device = "jpg", path = here("output/Models"))


vis.gam(For_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Forage Fish Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")



#-----------------
#PRED: Pacific halibut
#Full Model
For_PH_M <- gam(forage ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = PH,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(For_PH_M)

anova(For_PH_M)

#Comparing Delta AIC of alternative Models
For_PH_fit <- dredge(For_PH_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    extra = c("adjR^2", "deviance"))

For_PH_AIC <- as.data.frame(For_PH_fit) %>% 
  mutate(Response = "Forage Fish", Predator = "Pacific halibut")

write.csv(For_PH_fit, here("output/Models/Halibut_eat_For_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(For_PH_M)

#Plotting partial effects
For_PH_Plot1 <- visreg(For_PH_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Forage Fish P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

For_PH_Plot2 <- visreg(For_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

For_PH_Plot3 <- visreg(For_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

For_PH_Plot4 <- visreg(For_PH_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


For_PH_MainP <- (For_PH_Plot1 + For_PH_Plot2) / (For_PH_Plot3 + For_PH_Plot4) + 
  plot_annotation(title = "Predator: Pacific Halibut") 

ggsave("Halibut_eat_SL.jpg", plot = SL_PH_MainP, device = "jpg", path = here("output/Models"))

vis.gam(For_PH_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Forage Fish Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


###################################
#-----------------------------------
###################################
###################################################
#                                                 #  
#            PREY:  Pandalidae
#                                                  #
#                                                 #
################################################### 

#All Predators
Pan <- all_pred %>% 
  filter(Pred_common != "Pacific halibut")


Pan_M <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = Pan,
                family = binomial(link = logit),
                method = "GCV.Cp")

Pan <- test %>% 
  filter(Pred_common != "Pacific halibut")

Pan_M <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
                data = Pan,
                family = binomial(link = logit), #logistic scale
                method = "GCV.Cp")


summary(Pan_M)

anova(Pan_M)

#Comparing Delta AIC of alternative Models
Pan_fit <- dredge(Pan_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                     extra = c("adjR^2", "deviance"))

Pan_AIC <- as.data.frame(Pan_fit) %>% 
  mutate(Response = "Pandalidae", Predator = "Arrowtooth flounder")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Pan_M)

#Plotting partial effects
Pan_Plot1 <- visreg(Pan_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pandalidae P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Pan_Plot2 <- visreg(Pan_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Pan_Plot3 <- visreg(Pan_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Pan_Plot4 <- visreg(Pan_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 

Pan_Plot4 <- visreg(Pan_M, "Pred_common",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
  theme_classic() 


Pan_AF_MainP <- (Pan_AF_Plot1 + Pan_AF_Plot2) / (Pan_AF_Plot3 + Pan_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_Pan.jpg", plot = Pan_AF_MainP, device = "jpg", path = here("output/Models"))


vis.gam(Pan_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pandalidae Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#-----------------
#PREY: Pandalidae
#Pred: Arrowtooth Flounder

#Full Model
Pan_AF_M <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = AF,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(Pan_AF_M)

anova(Pan_AF_M)

#Comparing Delta AIC of alternative Models
Pan_AF_fit <- dredge(Pan_AF_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                     extra = c("adjR^2", "deviance"))

Pan_AF_AIC <- as.data.frame(Pan_AF_fit) %>% 
  mutate(Response = "Pandalidae", Predator = "Arrowtooth flounder")

write.csv(Pan_AF_fit, here("output/Models/AFlounder_eat_Pan_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Pan_AF_M)

#Plotting partial effects
Pan_AF_Plot1 <- visreg(Pan_AF_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pandalidae P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Pan_AF_Plot2 <- visreg(Pan_AF_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Pan_AF_Plot3 <- visreg(Pan_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Pan_AF_Plot4 <- visreg(Pan_AF_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Pan_AF_MainP <- (Pan_AF_Plot1 + Pan_AF_Plot2) / (Pan_AF_Plot3 + Pan_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_Pan.jpg", plot = Pan_AF_MainP, device = "jpg", path = here("output/Models"))


vis.gam(Pan_AF_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pandalidae Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#----------------------------------
#PREY: Pandalidae
#Pred: Pacific Cod

#Full Model
Pan_PC_M <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = PC,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(Pan_PC_M)

anova(Pan_PC_M)

#Comparing Delta AIC of alternative Models
Pan_PC_fit <- dredge(Pan_PC_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                     extra = c("adjR^2", "deviance"))

Pan_PC_AIC <- as.data.frame(Pan_PC_fit) %>% 
  mutate(Response = "Pandalidae", Predator = "Pacific Cod")

write.csv(Pan_PC_fit, here("output/Models/Cod_eat_Pan_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Pan_PC_M)

#Plotting partial effects
Pan_PC_Plot1 <- visreg(Pan_PC_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pandalidae P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Pan_PC_Plot2 <- visreg(Pan_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Pan_PC_Plot3 <- visreg(Pan_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Pan_PC_Plot4 <- visreg(Pan_PC_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Pan_PC_MainP <- (Pan_PC_Plot1 + Pan_PC_Plot2) / (Pan_PC_Plot3 + Pan_PC_Plot4) + 
  plot_annotation(title = "Predator: P Cod") 

ggsave("PCod_eat_Pan.jpg", plot = Pan_PC_MainP, device = "jpg", path = here("output/Models"))


vis.gam(Pan_PC_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pandalidae Prescence in PCod Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#----------------------------------
#PREY: Pandalidae
#Pred: Walleye pollock

#Full Model
Pan_WP_M <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = WP,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(Pan_WP_M)
sum(WP$Pandalidae)
anova(Pan_WP_M)

#Comparing Delta AIC of alternative Models
Pan_WP_fit <- dredge(Pan_WP_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                     extra = c("adjR^2", "deviance"))

Pan_WP_AIC <- as.data.frame(Pan_WP_fit) %>% 
  mutate(Response = "Pandalidae", Predator = "Walleye Pollock")

write.csv(Pan_WP_fit, here("output/Models/WP_eat_Pan_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Pan_WP_M)

#Plotting partial effects
Pan_WP_Plot1 <- visreg(Pan_WP_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pandalidae P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

Pan_WP_Plot2 <- visreg(Pan_WP_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

Pan_WP_Plot3 <- visreg(Pan_WP_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

Pan_WP_Plot4 <- visreg(Pan_WP_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


Pan_WP_MainP <- (Pan_WP_Plot1 + Pan_WP_Plot2) / (Pan_WP_Plot3 + Pan_WP_Plot4) + 
  plot_annotation(title = "Predator: Walleye Pollock") 

ggsave("WP_eat_Pan.jpg", plot = Pan_WP_MainP, device = "jpg", path = here("output/Models"))


vis.gam(Pan_WP_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pandalidae Prescence in WPollock Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


###################################
#-----------------------------------
###################################
###################################################
#                                                 #  
#            PREY:  Tanner Crab
#                                                  #
#                                                 #
################################################### 
#ALL PREDS
TC <- all_pred %>% 
  filter(Pred_common == "Pacific cod" | Pred_common == "Pacific halibut")

#Full Model
TC_M <- gam(TannerCrab ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
               data = TC,
               family = binomial(link = logit), #logistic scale
               method = "GCV.Cp")

summary(TC_M)

anova(TC_M)

#Comparing Delta AIC of alternative Models
TC_fit <- dredge(TC_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    extra = c("adjR^2", "deviance"))

TC_AIC <- as.data.frame(TC_fit) %>% 
  mutate(Response = "Tanner Crab", Predator = "Pacific Halibut")

write.csv(TC_fit, here("output/Models/Halibut_eat_TC_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(TC_M)

#Plotting partial effects
TC_Plot1 <- visreg(TC_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="red"), xlab = "Year", ylab = "Partial Effect on Tanner Crab P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45)) +
  TC_PC_Plot1

TC_Plot2 <- visreg(TC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

TC_Plot3 <- visreg(TC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

TC_Plot4 <- visreg(TC_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


TC_MainP <- (TC_Plot1 + TC_Plot2) / (TC_Plot3 + TC_Plot4) + 
  plot_annotation(title = "Predator: P Cod") 


vis.gam(TC_PH_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Tanner Crab Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")


#----------------------------------
#PREY: Tanner Crab
#Pred: Pacific Halibut

#Full Model
TC_PH_M <- gam(TannerCrab ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
               data = PH,
               family = binomial(link = logit),
               method = "GCV.Cp")

summary(TC_PH_M)

anova(TC_PH_M)

#Comparing Delta AIC of alternative Models
TC_PH_fit <- dredge(TC_PH_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    extra = c("adjR^2", "deviance"))

TC_PH_AIC <- as.data.frame(TC_PH_fit) %>% 
  mutate(Response = "Tanner Crab", Predator = "Pacific Halibut")

write.csv(TC_PH_fit, here("output/Models/Halibut_eat_TC_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(TC_PH_M)

#Plotting partial effects
TC_PH_Plot1 <- visreg(TC_PH_M, "Year",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="red"), xlab = "Year", ylab = "Partial Effect on Tanner Crab P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45)) +
  TC_PC_Plot1

TC_PH_Plot2 <- visreg(TC_PH_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

TC_PH_Plot3 <- visreg(TC_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

TC_PH_Plot4 <- visreg(TC_PH_M, "Len_bin",type = "conditional", scale = "response",
                      gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


TC_PH_MainP <- (TC_PH_Plot1 + TC_PH_Plot2) / (TC_PH_Plot3 + TC_PH_Plot4) + 
  plot_annotation(title = "Predator: P Cod") 

ggsave("Halibut_eat_TC.jpg", plot = TC_PH_MainP, device = "jpg", path = here("output/Models"))


vis.gam(TC_PH_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Tanner Crab Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")



#----------------------------------
#PREY: Tanner Crab
#Pred: Pacific Cod

#Full Model
TC_PC_M <- gam(TannerCrab ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + Len_bin,
                data = PC,
                family = binomial(link = logit),
                method = "GCV.Cp")

summary(TC_PC_M)

anova(TC_PC_M)

#Comparing Delta AIC of alternative Models
TC_PC_fit <- dredge(TC_PC_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                     extra = c("adjR^2", "deviance"))

TC_PC_AIC <- as.data.frame(TC_PC_fit) %>% 
  mutate(Response = "Tanner Crab", Predator = "Pacific Cod")

write.csv(TC_PC_fit, here("output/Models/Cod_eat_TC_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(TC_PC_M)

#Plotting partial effects
TC_PC_Plot1 <- visreg(TC_PC_M, "Year",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Tanner Crab P/A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

TC_PC_Plot2 <- visreg(TC_PC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic() 

TC_PC_Plot3 <- visreg(TC_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Gear_temp", ylab = "") +
  theme_classic() 

TC_PC_Plot4 <- visreg(TC_PC_M, "Len_bin",type = "conditional", scale = "response",
                       gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


TC_PC_MainP <- (TC_PC_Plot1 + TC_PC_Plot2) / (TC_PC_Plot3 + TC_PC_Plot4) + 
  plot_annotation(title = "Predator: P Cod") 

ggsave("PCod_eat_TC.jpg", plot = TC_PC_MainP, device = "jpg", path = here("output/Models"))


vis.gam(TC_PC_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Tanner Crab Prescence in PCod Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
maps::map('worldHires', fill=T, xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add=T, col="lightgrey")




#Combining AIC Tables
TC_AIC <- rbind(TC_PH_AIC, TC_PC_AIC) %>% 
  mutate(deviance = deviance/100)

TC_AIC <- TC_AIC[,c(15, 14, 4, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)]


############################
#---------------------------
#############################

#Combining AIC Tables

Euph_AIC <- rbind(Euph_AF_AIC, Euph_PC_AIC, Euph_WP_AIC) %>% 
  mutate(deviance = deviance/100)

Euph_AIC <- Euph_AIC[,c(15, 14, 4, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)]

WP_AIC <- rbind(WP_AF_AIC, WP_PH_AIC, WP_PC_AIC) %>% 
  mutate(deviance = deviance/100)

WP_AIC <- WP_AIC[,c(15, 14, 4, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)]

For_AIC <- rbind(For_AF_AIC, For_PH_AIC) %>% 
  mutate(deviance = deviance/100)

For_AIC <- For_AIC[,c(15, 14, 4, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)]

Pan_AIC <- rbind(Pan_AF_AIC, Pan_PC_AIC, Pan_WP_AIC) %>% 
  mutate(deviance = deviance/100)

Pan_AIC <- Pan_AIC[,c(15, 14, 4, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)]

TC_AIC <- rbind(TC_PH_AIC, TC_PC_AIC) %>% 
  mutate(deviance = deviance/100)

TC_AIC <- TC_AIC[,c(15, 14, 4, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)]


#Combining all of the AIC tables into one

global_AIC <- rbind(Euph_AIC, WP_AIC, For_AIC, Pan_AIC, TC_AIC) 

#Calculating Parameter weights
AIC <- global_AIC %>% 
  group_by(Predator, Response, `s(GEAR_TEMP, k = 4)`) %>% 
  mutate(Gear_Temp = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predator, Response, Len_bin) %>% 
  mutate(Length_Bin = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predator, Response, `s(GEAR_DEPTH)`) %>% 
  mutate(Gear_Depth = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predator, Response, `s(RLONG, RLAT)`) %>% 
  mutate(Lat_Long = sum(weight)) %>% 
  ungroup() %>% 
  mutate(Year = ifelse(Year != 0, '+', 0)) %>% 
  group_by(Predator, Response, Year) %>% 
  mutate(Year_ = sum(weight)) %>% 
  ungroup() %>% 
  filter(delta <=2) 



parameter_weights <- AIC %>% 
  drop_na() %>% 
  distinct(Predator, Response, Gear_Temp, Length_Bin, Gear_Depth, Lat_Long, Year_)

write.csv(AIC, here("output/Models/Global_AIC.csv"), row.names = F)
write.csv(parameter_weights, here("output/Models/parameter_weights.csv"), row.names = F)


#Calculating parameter weights
AIC_list <- list(Euph_AF_AIC, Euph_AF_AIC)


