#Load libraries
library(readr) 
library(tidyverse)
library(here)
library(patchwork)
library(igraph)
library(ggraph)

#Load Data
network_sc_filtered <- read_csv(here("output/source_data/network_sc_filtered.csv"))

groupings <- read_csv(here("output/groupings.csv"))

network_sc_filtered <- left_join(network_sc_filtered,groupings,by="Prey_Name")

network_simple <- network_sc_filtered %>% 
  filter(Prey_Name_Networks != "Empty") %>% 
  select(Pred_common, uniqueID, Prey_Name_Networks, Year, INPFC_AREA)

#pred_names <- unique(network_sc_filtered$Pred_common)
#prey_names <- unique(network_sc_filtered$Prey_Name)


#Write a function to calculate Percent Occurrence 
Occur_fun <- function(data) {
  data <- data %>% 
    group_by(Pred_common, Year, INPFC_AREA) %>% 
    mutate(TotalPredN = length(unique(uniqueID))) %>%
    group_by(Pred_common, Prey_Name_Networks, Year, INPFC_AREA) %>% 
    mutate(NStomachs = length(unique(uniqueID)), 
           Percent_Occur = NStomachs/TotalPredN*100) %>% 
    distinct(Pred_common, Prey_Name_Networks, Year, INPFC_AREA, TotalPredN, NStomachs, Percent_Occur)
 
   return(data)
}

#Create a dataframe with all the info on percent occurrence that I need for the networks
network_occurance <- network_simple %>% 
  Occur_fun()

#Plot the number of predators sampled vs the number of unique prey observed
network_occurance <- network_occurance %>% 
  group_by(Pred_common, Year, INPFC_AREA) %>% 
  mutate(unique_prey = sum(ifelse(NStomachs >= 1, 1, 0)))

ggplot(network_occurance, aes(x = TotalPredN, y = unique_prey, color = Pred_common)) +
  geom_point() +
  geom_smooth(method = "loess") +
  ylab("number of unique prey items") +
  xlab("number of stomachs sampled") +
  theme_classic()


#Get the dataframes into edge list and node list format
edges.df <- network_occurance %>% 
  select(Pred_common, Prey_Name_Networks, Percent_Occur, NStomachs, Year, INPFC_AREA) %>% 
  rename(from = Pred_common, to = Prey_Name_Networks, weight = Percent_Occur, n = NStomachs)

nodes.df.1 <- network_occurance %>% 
  ungroup() %>% 
  select(Prey_Name_Networks, Year, INPFC_AREA) %>% 
  distinct(Prey_Name_Networks, Year, INPFC_AREA) %>% 
  mutate(n = NA) %>% 
  rename(name = Prey_Name_Networks)

nodes.df.2 <- network_occurance %>% 
  ungroup() %>% 
  select(Pred_common, Year, INPFC_AREA, TotalPredN) %>% 
  distinct(Pred_common, Year, INPFC_AREA, TotalPredN) %>% 
  rename(name = Pred_common, n = TotalPredN)

  #I lost the number of predators sampled because I can't have repetative nodes. I think this is ok because
  #I have the table that shows the predator sample sizes for each region and year
nodes.df <- rbind(nodes.df.1, nodes.df.2) %>% 
  distinct(name, Year, INPFC_AREA)

#-------------------------------
#Now that everything is in the correct format I need to build networks. 
#I think in order to do this I will have to build loops (god help me).

  #practice
edges.df.test <- edges.df %>% 
  filter(Year == 1990 & INPFC_AREA == 610) %>% 
  ungroup() %>% 
  select(from, to)

nodes.df.test <- nodes.df %>% 
  filter(Year == 1990 & INPFC_AREA == 610) %>% 
  select(name)

g <- graph_from_data_frame(d = edges.df.test, vertices = nodes.df.test, directed = T)

plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray77',
     vertex.size = 0,
     edge.arrow.size = 0.1,
     layout = layout_nicely(g))

#I want to make a loop which will create a igraph object for each of the networks that I am building
#In order to do this I need to have vectors of the Years and regions that I want to loop the data through

year.region <- nodes.df.2 %>% 
  filter(name == "Walleye pollock") %>% 
  select(Year, INPFC_AREA)

year.region <- arrange(year.region, Year, INPFC_AREA)

Years <- year.region$Year
Regions <- year.region$INPFC_AREA

year.list <- vector('list', 45)

#WARNING: IT TAKES 1.15 min to run this loop!
      start.time <- Sys.time() #just to check the time on the loop

for(i in 1:length(Years)){
  for(j in 1:length(Regions)) {
  edges.df.test <- edges.df %>% 
    filter(Year == Years[i] & INPFC_AREA == Regions[j]) 
  
  nodes.df.test <- nodes.df %>% 
    filter(Year == Years[i] & INPFC_AREA == Regions[j]) 
  
  graph <- graph_from_data_frame(d = edges.df.test, vertices = nodes.df.test, directed = T)
  
  year.list[[i]] <- graph
  }
}
  #this little chunk is to see how long it takes my computer to run this
      end.time <-Sys.time()
      time.taken <- round(end.time - start.time, 2)
      time.taken #1.09 min

network1 <- year.list[[1]]
network2 <- year.list[[2]]
network40 <- year.list[[40]]

is_directed(network1, layout = layout_nicely())

#Test plots of the first network in my list
plot(network1)

plot(network1, 
     vertex.size = 0,
     edge.arrow.size = 0.1,
     layout = layout_nicely(network1))

plot(network2, 
     vertex.size = 0,
     edge.arrow.size = 0.1,
     layout = layout_nicely(network2))

plot(network40, 
     vertex.size = 0,
     edge.arrow.size = 0.1,
     layout = layout_nicely(network40))


#-----------------------------------------
#The next step is to output network statistics from each of these networks into a dataframe that can be used
#to run GLM

netstats.list <- vector('list', 45)

for(i in 1:length(year.list)) {
netstats.list[i] <- as.data.frame(strength(year.list[[i]],
                                           vids = V(year.list[[i]]),
                                           mode = c("in"),
                                           loops = T))
}

view(netstats.list[1])
netstats.list[2]

test <- as.data.frame(strength(network1,
         vids = V(network1),
         mode = c("in"),
         loops = T))

for(i in 1:length(Years)){
  for(j in 1:length(Regions)) {
    edges.df.test <- edges.df %>% 
      filter(Year == Years[i] & INPFC_AREA == Regions[j]) 
    
    nodes.df.test <- nodes.df %>% 
      filter(Year == Years[i] & INPFC_AREA == Regions[j]) 
    
    graph <- graph_from_data_frame(d = edges.df.test, vertices = nodes.df.test, directed = T)
    
    year.list[[i]] <- graph
  }
}







