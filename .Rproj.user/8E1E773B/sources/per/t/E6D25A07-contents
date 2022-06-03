#This is Catalina's File to work through the Diet Tutorial Lab

#Load Libraries
library(here)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(vegan)

#input datasets
data.raw <- read_csv(here("data", "DietTutorialRaw.csv"))
data.clean <- read_csv(here("data", "DietTutorialClean.csv"))
loc <- read_csv(here("data", "LocationTutorial.csv"))

#join datasets
data <-left_join(data.raw, loc)

#Part 1: Quality Control
#1a: expected number of lincod (n = 950), prey items (n = 1764) given info
#1b: actual number of lincod and prey items code below

dim(data) #this shows that we have the correct number of prey items
length(unique(data$LingID)) #this shows that we have the correct number of lincod

#2 Check to make sure data types in the spreadsheet align with data types described in metadata record.

head(data)
str(data)
class(data)
#LingID: double (incorrect), should be integer
#LingLength: double (correct)
#LingWt: double (correct)
#Season: character (correct)
#PreyTaxa: character (correct)
#PreyOccur: double (incorrect), should be logical or integer
#PreyWt: double (correct)
#PreyCode: double (correct)
#Site: character (correct)

#converting variables to correct type
data$LingID <- as.integer(data$LingID)
data$PreyOccur <- as.integer(data$PreyOccur)

#3. Check for inconsistencies in character fields
unique(data$PreyTaxa)
  #Teleostei unid and Teleostei NEI (keep)
  #suborder zoarcoidei (keep) and Zoarcoidea
unique(data$Season) #clean
unique(data$Site) #clean
unique(data$LingLength_cm) #there is one length of 3.3 cm should this have been 33cm?
unique(data$LingWt_g) #there is a 0 value for weight, should this be an N/A instead?
unique(data$PreyOccur) #there is a 2, I think the values should only be 1 or 0 for presence/absence.
unique(data$PreyWt_g) #there are some 0 values here as well that should be swapped to NAs

#sub out the errors
data$PreyTaxa <- gsub("Teleostei unid", "Teleostei NEI", data$PreyTaxa)
data$PreyTaxa <- gsub("Zoarcoidei", "Suborder Zoarcoidei", data$PreyTaxa)

#4. Check for anomalous or missing values.
#5. Check for logical inconsistancies
head(data)

#Checking: length
data %>% 
  ggplot(aes(x=LingID, y=LingLength_cm))+
  geom_point() ->
  fig
ggplotly(fig)

#Fixing: there is a lingcod that has a length of 3cm which I think is an error. I'm going to change it from 3.3 to 33
data$LingLength_cm[data$LingLength_cm == 3.3] <- 33

#Checking: weight
data %>% 
  ggplot(aes(x=LingWt_g, y=LingLength_cm))+
  geom_point() ->
  fig1
ggplotly(fig1)

#Fixing: I found a couple of lingcod that have no weight measurements. I'm going to replace 0 with NA values.
data$LingWt_g[data$LingWt_g == 0] <- NA

#Checking: Taxa
data %>% 
  filter(PreyTaxa == "Empty") %>% 
  ggplot(aes(x = PreyTaxa, y = PreyWt_g))+
  geom_point()
#didn't find any errors

#Checking: Occurance
data %>% 
  filter(PreyTaxa == "Empty") %>% 
  ggplot(aes(x = PreyTaxa, y = PreyOccur))+
  geom_point()
#I found a empty stomach that has a 1 for prey occurrence

#Plotting to figure out the LingID for the incorrect occurance values
data %>% 
  filter(PreyTaxa == "Empty") %>% 
  ggplot(aes(x = LingID, y = PreyOccur))+
  geom_point()->
  Fig3
Fig3
ggplotly(Fig3) #LingID = 1139, 1530

#Fixing: I'm replacing the 1 value with a 0
data %>% 
  filter(LingID ==1530) %>% 
  mutate(PreyOccur = recode(PreyOccur, '1' = 0)) ->
  data3

data %>% 
  filter(LingID ==1139) %>% 
  mutate(PreyOccur = recode(PreyOccur, '1' = 0)) ->
  data2

#Checking: Taxa
data %>% 
  filter(PreyTaxa == "Empty") %>% 
  ggplot(aes(x = PreyTaxa, y = PreyCode))+
  geom_point()
#There is also an empty stomach that has a 2 for prey code when it should be 1

#Plotting to figure out the lingID for incorrect Taxa value
data %>% 
  filter(PreyTaxa == "Empty") %>% 
  ggplot(aes(x = LingID, y = PreyCode))+
  geom_point()->
  Fig2
ggplotly(Fig2) #LingID = 578

#Fixing: I'm changing the prey code from 2 to 1
data %>% 
  filter(LingID ==578) %>% 
  mutate(PreyCode = recode(PreyCode, '2' = 1)) ->
  data1

data %>% 
  mutate(PreyCode = ifelse(LingID == 578, 1, PreyCode))->
  DataNew


# I need help subbing back in the correct values. For now I have saved the corrected values as seperate dataframes.

#-------------- PART 2: Integration ------------------

#I still had a couple datapoints to fix from Part 1, so I'm going to use the clean data from here on out.
#I also noticed that the clean data has an additional column for prey number that the raw data didn't have

final.data <-left_join(data.clean, loc)

#--------------- PART 3: Data Prep ----------------------
#filtering down for analysis
final.data %>% 
  filter(PreyCode > 3) %>% 
  filter(!PreyTaxa == "Teleostei NEI")->
  final.data.2

#I removed the mostly digested samples because they could have impacted the taxa ID
#I removed Teleostei because it is too large of a group. I want more detailed prey info.

#------------- PART 4: Visualization --------------------
#Research Question: How does lingcod diet vary in relation to body size
head(final.data.2)

#I wanted to start by looking at length to weight of Lingcod because there is a very clear power growth curve
#Plot length to weight
final.data.2 %>% 
  ggplot(aes(x=LingLength_cm, y = LingWt_g))+
  geom_point()+
  theme_minimal()+
  geom_smooth(method = "loess")

#Plot pred weight to prey weight
final.data.2 %>% 
  ggplot(aes(x=LingWt_g, y = PreyWt_g))+
  geom_point()+
  theme_minimal()
  #not a very clear relationship

#I need to calculate some summary statistics before I can really dive into this deeper
#I'm going to move on to the next section and make some new plots based on the summary stats I calculate

#---------------- PART 5: Summary Stats -----------------------
#I've decided to not use the filtered data because it is too limited

###Percent by Weight %W
#(sum weight of prey type i) / (total weight of all prey) x 100
#First find the sum weight of each prey type
Weight <- 
  as.data.frame(summarise(
    group_by(final.data, PreyTaxa),
    TotalWt = sum(PreyWt_g)))

#next find the total weight of all prey
sum(Weight$TotalWt) #Answer: 10396.51

#last calculate percent weight in new column
Weight$PW <- (Weight$TotalWt/10396.51)*100

###Percent by number %N
#(sum of number of individuals of prey type i) / (total number of all prey) * 100
#We know from the instructions that there were 1764 prey items sampled
#First find the sum of number of individuals of each prey type
Number <-
  as.data.frame(summarise(
    group_by(final.data, PreyTaxa),
    N = sum(PreyNum)
  ))

#Next find the total number of prey
sum(Number$N) #hmm this is different from what we were given, must be that some got removed in QC
  #A: 1443

#calculate percent weight in new column
Number$PN <- (Number$N/1443)*100

###Percent frequency of occurrence %O
#(number of lingcod that had at >= 1 of prey i) / (total number of lingcod sampled) * 100
#We know from the instructions that there were 950 lingcod sampled

#First calculate number of lingcod with more than one prey
Occur <- 
  as.data.frame(summarise(
    group_by(final.data, PreyTaxa),
    Occ = sum(unique(LingID)*0+1)
  ))

#next calculate total number of lingcod sampled
sum(unique(final.data$LingID)*0+1) #A: 950

#calculate percent occurance
Occur$PO <- (Occur$Occ/950)*100


#------------Additional Analysis----------------
#merge
DietSummary <- merge(Weight,Number) %>%
  merge(Occur)

#plotting proportion by weight vs freq of occurance
ggplot(DietSummary, aes(x=PO, y=PW)) +
  geom_point(size = 3, alpha = 0.5)+
  theme_minimal()
  #looks like there is kind of a positive relationship, although it is not a super clear relationship and there is an outlier 
  #there are several data points that make up a lot of the percent weight, but are rare

###I copy and pasted this code from Anne's answers template. I'm going to play around with the figures.
#I'm not going to lie, some of this code went over my head. Maybe worth asking Anne or coming back to it

# Calculate diet by season
dat <- final.data %>%
  group_by(Season, PreyTaxa) %>%
  summarize(sum(PreyWt_g))
colnames(dat)<-c("Season","PreyTaxa","PreyWt")
seasonal <- dat %>% spread(PreyTaxa,PreyWt)


# Calculate cumulative prey curves (rarefaction curves) and set up the data for plotting
dat.sub <- final.data[,c(1,4:5)]
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
#This figure shows that species richness increases with the number of stomach sampled and during different seasons.
#I think this refers back to what Anne was talking about that you need to sample enough stomachs that you capture the species richness.

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
             '#aaffc3', '#e6beff', '#a9a9a9', '#fabebe') #Extra colors: '#fabebe'

ggplot(proportion, aes(x = Season, Prop, fill = PreyTaxa)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colorlist) +
  ylab("Proportion") + xlab("Season") +
  theme_bw(base_size = 14)
#This figure shows that diet composition changes seasonally