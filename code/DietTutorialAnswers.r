################################
##  Diet Composition Metrics  ##
##    Anne Beaudreau          ##
##    5 September 2018        ##
##   Updated 11 April 2022    ##
################################

# Load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(vegan)

# Set working directory: CHANGE THIS TO THE CORRECT FILE PATH ON YOUR COMPUTER
setwd("C:/Users/annebeau/Documents/Thesis_Burch/Diet tutorial/")

# Read files
data <- read.csv("DietDataClean.csv")
head(data); dim(data)
colnames(data)

loc <- read.csv("Location.csv")

data.raw <- read_csv(here("data", "DietTutorialRaw.csv"))
data <- read_csv(here("data", "DietTutorialClean.csv"))
loc <- read_csv(here("data", "LocationTutorial.csv"))

# Join location data to diet dataset
data <- left_join(data,loc)

head(data); dim(data)

# Binning:
# Say you wanted to simplify the number of prey groups for visualization.
# One way to do this is to reclassify the prey into broader guilds or functional groups.
# This chunk of code defines a vector of prey taxa (PreyTaxa), a vector of broader
# prey groups (PreyGroup), and puts them together into a new data frame (PreyCategories).

PreyTaxa <- c("Ammodytidae","Empty","Order Pleuronectiformes","Cottidae",
          "Order Decapoda","Gadidae","Clupeidae","Suborder Zoarcoidei",
          "Embiotocidae","Order Scorpaeniformes","Teleostei NEI","Scorpaenidae",
          "Hexagrammidae","Myctophidae","Class Cephalopoda","Bathylagidae",
          "Osmeridae","Salmonidae","Engraulidae","Squalidae")
PreyGroup <- c("Forage Fish","Empty","Groundfish","Groundfish",
           "Invertebrate","Groundfish","Forage Fish","Groundfish",
           "Forage Fish","Groundfish","Teleostei NEI","Groundfish",
           "Groundfish","Forage Fish","Invertebrate","Forage Fish",
           "Forage Fish","Forage Fish","Forage Fish","Groundfish")
PreyCategories <- data.frame(PreyTaxa,PreyGroup)

# Next, we join the full data table with the new PreyCategories data frame
# to create a new column with PreyGroup for each observation

data <- left_join(data,PreyCategories,by="PreyTaxa")

# We may also want to define lingcod size bins for the purpose of summarizing diets.
# Here is some code that first defines the bins (cuts), then creates a new column
# in the data frame that assigns those bins to each observation based on lingcod length

cuts <- c(20,30,40,50,60,70,80,90,110)

data %>%
  mutate(Len_bin = cut(c(LingLength_cm),c(cuts), 
    include.lowest = TRUE, right = FALSE)) -> data

# Number of fish sampled
length(unique(data$LingID))
n_distinct(data$LingID)

# Data types
str(data)
class(data$Season)

# List unique values
unique(data$Season)
unique(data$PreyTaxa)

# Replace erroneous values
data %>%
  mutate(PreyTaxa = replace(PreyTaxa, PreyTaxa == "Teleostei unid", "Teleostei NEI")) -> dataV2
unique(dataV2$PreyTaxa)

data %>%
  mutate(PreyTaxa = recode(PreyTaxa,"Teleostei unid" = "Teleostei NEI")) -> dataV3
unique(dataV3$PreyTaxa)

# Plot lingcod length vs. lingcod weight
ggplot(data, aes(x=LingLength_cm, y=LingWt_g)) +
  geom_point(size = 3, alpha = 0.5) +
  xlab("Length (cm)") + 
  ylab("Weight (g)") +
  theme_bw(base_size = 14) -> plot
ggplotly(plot)

# Calculate percent by weight
Weight <- as.data.frame(summarise(group_by(data, PreyTaxa), TotalWt=sum(PreyWt_g)))
Weight$PropWt <- Weight$TotalWt/sum(Weight$TotalWt)
Weight1 <- Weight[!(Weight$PreyTaxa=="Empty"),]

# Calculate percent by number
Num <- as.data.frame(summarise(group_by(data, PreyTaxa), TotalN=sum(PreyNum)))
Num$PropN <- Num$TotalN/sum(Num$TotalN)
Num <- Num[!(Num$PreyTaxa=="Empty"),]

# Calculate total number of predators sampled
TotalPredN <- length(unique(data$LingID))

# Calculate frequency of occurrence
Freq <- summarise(group_by(data, PreyTaxa), NStomachs=sum(unique(LingID)*0+1))
Freq$FreqOccur <- Freq$NStomachs/TotalPredN
Freq <- Freq[!(Freq$PreyTaxa=="Empty"),]

# Merge weight, number, and frequency of occurrence into one data frame
DietSummary <- merge(Weight,Num) %>%
  merge(Freq)

# Plot proportion by weight vs. frequency of occurrence
ggplot(DietSummary, aes(x=FreqOccur, y=PropWt)) +
  geom_point(size = 3, alpha = 0.5) +
  xlab("Frequency of occurrence") + 
  ylab("Proportion by weight") +
  theme_bw(base_size = 14)

# Calculate diet by season
dat <- final.data %>%
  group_by(Season, PreyTaxa) %>%
  summarize(sum(PreyWt_g))
colnames(dat)<-c("Season","PreyTaxa","PreyWt")
seasonal <- dat %>% spread(PreyTaxa,PreyWt)

# Calculate cumulative prey curves (rarefaction curves) and set up the data for plotting
dat.sub <- data[,c(1,4:5)]
dat.sub <- distinct(dat.sub)
colnames(dat.sub) <- c("LingID","Season","PreyTaxa")
dat.sub$NPred <- 1
dat.long <- dat.sub %>% spread(PreyTaxa,NPred); dat.long[is.na(dat.long)]<-0
dat.winter<-dat.long[dat.long$Season=="Winter",-c(1:2,9)]
dat.spring<-dat.long[dat.long$Season=="Spring",-c(1:2,9)]
dat.summer<-dat.long[dat.long$Season=="Summer",-c(1:2,9)]
dat.fall<-dat.long[dat.long$Season=="Fall",-c(1:2,9)]

cumu.winter <- specaccum(dat.winter)
cumu.winter <- data.frame(Stomachs=cumu.winter$sites, Richness=cumu.winter$richness, SD=cumu.winter$sd) %>%
  mutate(Season = "Winter") #Convert object of class specaccum to data frame for plotting in ggplot and add column with season
cumu.spring <- specaccum(dat.spring)
cumu.spring <- data.frame(Stomachs=cumu.spring$sites, Richness=cumu.spring$richness, SD=cumu.spring$sd) %>%
  mutate(Season = "Spring")
cumu.summer <- specaccum(dat.summer)
cumu.summer <- data.frame(Stomachs=cumu.summer$sites, Richness=cumu.summer$richness, SD=cumu.summer$sd) %>%
  mutate(Season = "Summer")
cumu.fall <- specaccum(dat.fall)
cumu.fall <- data.frame(Stomachs=cumu.fall$sites, Richness=cumu.fall$richness, SD=cumu.fall$sd) %>%
  mutate(Season = "fall")

cumu <- bind_rows(cumu.winter,cumu.spring,cumu.summer,cumu.fall) #Combine all seasons into one data frame

# Plot cumulative prey curves (rarefaction curves)
ggplot(cumu, aes(x=Stomachs, y=Richness, colour=Season)) +
  geom_line(data=cumu) +
  geom_ribbon(data=cumu,aes(ymin=Richness-SD,ymax=Richness+SD, fill=Season),alpha=0.1) +
  theme_bw(base_size = 14)

# Stacked bar plots (proportion by weight)
proportion <- data %>%
  group_by(Season,PreyTaxa) %>%
  summarize(sum(PreyWt_g))

colnames(proportion)<-c("Season","PreyTaxa","Sum")
proportion <- proportion %>%
  mutate(Prop = Sum / sum(Sum))
proportion <- proportion[!proportion$PreyTaxa=="Empty",]

colorlist<-c('#e6194b', '#f58231', '#ffe119', '#bcf60c', '#3cb44b', '#46f0f0', 
             '#4363d8', '#911eb4', '#f032e6', '#800000', '#9A6324', 
             '#ffd8b1', '#808000', '#fffac8', '#000075', '#008080',   
             '#aaffc3', '#e6beff', '#a9a9a9') #Extra colors: '#fabebe'

ggplot(proportion, aes(x = Season, Prop, fill = PreyTaxa)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colorlist) +
  ylab("Proportion") + xlab("Season") +
  theme_bw(base_size = 14)
