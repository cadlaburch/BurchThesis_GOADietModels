#load libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(here)

#load data
lines <- read_csv(here("data/network practice/simpsons_script_lines.csv"))

raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
groupings <- read_csv(here("output/groupings.csv"))

nodes <- read.csv(here("data/network practice/Dataset1-Media-Example-NODES.csv"))
links <- read.csv(here("data/network practice/Dataset1-Media-Example-EDGES.csv"))

nodes2 <- read.csv(here("data/network practice/Dataset2-Media-User-Example-NODES.csv"))
links2 <- read.csv(here("data/network practice/Dataset2-Media-User-Example-EDGES.csv"))

sc_groupings <- left_join(raw_stomach_contents2021,groupings,by="Prey_Name")

#First create a edge list matrix
stom2021mat <- sc_groupings %>% 
  filter(Year == 2021, Prey_Name_Clean != "Empty") %>% 
  group_by(Pred_common, Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = Pred_common, edge = 1) %>% 
  select(Pred_common, Prey_Name_Clean, PW) %>% 
  spread(Prey_Name_Clean, PW, fill = 0)


#Next try plotting
test <- plot(stom2021mat)

#BELOW IS OUTDATED MESSY CODE
#First create a node dataframe
snodes <- sc_groupings %>% 
  filter(Year == 2021) %>% 
  summarise(ID = unique(c(OUTDATEDphylum_grouping, Pred_common)))

slinks <- sc_groupings  %>% 
  group_by(Pred_common) %>% 
  summarise(prey = unique(OUTDATEDphylum_grouping), lines = 1)

slinksmat <- slinks %>% 
  spread(prey, lines, fill=0)

snet2 <- graph_from_incidence_matrix(slinksmat)

plot(snet2, layout = layout.bipartite)

snet <- graph_from_data_frame(d = slinks, vertices = snodes, directed = T)

plot(snet, edge.arrow.size = .4)

ggraph(snet) +
  geom_edge_link() +
  geom_node_point()

stom2021 <- raw_stomach_contents2021 %>% 
  filter(Year == 2021, Prey_Name != "Empty") %>% 
  group_by(Pred_common, Prey_Name) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = Pred_common, edge = 1) %>% 
  select(Pred_common, Prey_Name, edge)



# Load scripts and remove non-speaking lines
speaking_lines <- lines %>% 
  filter(!is.na(raw_character_text))

# Limit analysis to re-occuring characters in 20 or more episodes
top_char <- speaking_lines %>% 
  group_by(raw_character_text) %>% 
  mutate(appearances=n_distinct(episode_id)) %>% 
  filter(appearances >= 20) %>%
  ungroup()

# Count characters lines per episode 
lines_per_ep <- top_char %>% 
  group_by(raw_character_text, episode_id) %>% 
  summarise(lines=n()) %>% 
  ungroup()

# Convert to matrix
char_df <- lines_per_ep %>% 
  spread(episode_id, lines, fill=0)

stom_df <- stom2021 %>% 
  spread(Prey_Name, edge, fill = 0)

char_mat <- as.matrix(select(char_df, -raw_character_text))
rownames(char_mat) <- char_df$raw_character_text

stom_mat <- as.matrix(select(stom_df, -Pred_common))
rownames(stom_mat) <- stom_df$Pred_common

plot(char_mat, layout = layout)

# Calculate cosine distance between characters
cosine_sim <- as.dist(char_mat %*% t(char_mat) / (sqrt(rowSums(char_mat^2) %*% t(rowSums(char_mat^2)))))

# Initial look at the network 
autograph(as.matrix(cosine_sim))


#Notes from video
##There's two types of data formating an Adjacency Matrix and an Edgelist
#There's also multiple different packages you can use. The first is igraph
#To use igraph you have to convert your data into igraph format
#This is how you take an edgelist and turn it into igraph format

edgelist <- sc_groupings  %>% 
  filter(Year == 2021) %>% 
  group_by(Pred_common) %>% 
  summarise(prey = unique(OUTDATEDphylum_grouping))

g <- graph.edgelist(as.matrix(edgelist), directed = T) #directed signals edges have no direction

g

#IGRAPH 4b43b8a UN-- 91 453 -- (first number = # of verticies, second number = # of edges)

V(g) #returns all vertices

E(g) #edges

gorder(g) #number of verticies
gsize(g) #number of edges

#baisc plot
plot(g)
#for some reason it wasn't plotting so I used this function dev.off()


#Vertex attributes
g
#attr: name (v/c) can be categorical or numerical (name of species)

#Edge attributes
#can be shown by weight/thickness. PW, PN

#manual coding of attributes
set_vertex_attr(dataframe, "attribute", value = dataframewithattributes)
set.edge.attribute()

#inspecting vertex attributes
V(dataframe)[[1:5]] #view the attributes fo the first 5 verticies in a dataframe

#or attributes can be included in your dataframes and converted to an igraph object
#see notes on the correct df formats for edges and vertices

#converting dataframes to igraph format
graph_from_data_frame(d = edges.df, verticies = vertices.df, directed = T)

#inspect igraph object
#subset edges
E(g)[[inc('E')]] #inspecting edges
E(g[[frequency = 3]]) #subsetting to edges with weight = 3

#Setting colors for vertex attributes
V(g)$color <- ifelse(
  V(g)$age > 22, "red", "white" 
)

plot(g,
     vertex.label.color = "black") #adding labels 
#hmm it didn't work maybe I need to have the data set up with attributes?


##############################
###############################
#DATA CAMP: Network Analysis in R
#Note: I decided to start over because it had been a while and I forgot what I had learned :(


##Chapter 1: Introduction to Networks

#-------
#1.1
#Packages
library(igraph)

  #social network refers to the visualization or the underlying data
  #vertex/nodes

  #Data Structure:
    #edges
    #Adjacency matrix. All nodes are on both y and x axis and 1 indicates edge between vertices.
    #edgelist: two column matrix or dataframe. each row represents edge between two verticies. Most common

#create example dataframe
df <- data.frame(first_column = c("A", "B", "C"),
                  second_column = c("B", "C", "A"))

#if you wanted to go from datrame to matrix use this function, as.matrix()

#transform dataframe into igraph object
g <- graph.edgelist(as.matrix(df), directed = F)
    
g
#IGRAPH 9217593 UN-- 3 3 -- (3 vertices, and 3 edges)

#Print verticies
V(g)

#Print edges
E(g)

#number of verticies
gorder(g)

#number of edges
gsize(g)

#plot network
plot(g)

#------
#1.2: Network Attributes
#vertex attributes: categorical or numerical, ex. age, gender, name

#edge attributes: relationship between vertices. ex. weight (thickness of line) 

#adding attributes to igraph objects manually

g <- set_vertex_attr(g,
                     "age",
                     value = c(12, 13, 14))

#view attributes
vertex_attr(g)

vertex.attributes(g)

#set edge attributes
g <- set_edge_attr(g, 
                   "frequency",
                  value = c(2, 3, 4))

edge_attr(g)

#adding attributes when they are in a dataframe format
#creating example dataframe

vertices.df <- data.frame(name = c("A", "B", "C"),
                 age = c(12, 13, 14))

edges.df <- data.frame(from = c("A", "B", "C"),
                       to = c("B", "C", "A"),
                       frequency = c(2, 3, 4))

g2 <- graph_from_data_frame(d = edges.df, vertices = vertices.df, 
                      directed = F)

#inspect igraph object to find certain verticies or edges with specific attributes
#subset edges of igraph object
  #all edges that include the vertex "B"
  E(g)[[inc('B')]]
  
  #all edges that have a frequency >=3
  E(g)[[frequency>=3]]
  
  #view attributes of first three vertices in dataframe
  V(g)[[1:3]]

  #useful in large networks to identify interesting relationships!
  

#Network Visualization

#adjust plot by adding parameters
  #create vertex attribute called color, make vertices > 13 years red and the others white
  V(g2)$color <- ifelse(V(g)$age > 13, "red", "white")
  
#plot
  plot(g2,
       vertex.label.color = "black") #???? I'm not sure why the names aren't coming up.
  
  
#--------------------
#1.3: Network Visualization
  
  #adjust size, color, text, layout
  #should immediately provide information to viewer
  
  #Commonly adjusted visuals
    #size, labels, color, shape, edges (thickness, color, linetype)
  #color is automatically included if you have a node attribute named color
  
#Pitfalls
  #too much text can make it hard to read
  #Color and shape are good for categorical information
  #make sure that you highlight the key pieces of information
  
#Layouts
  #use layout argument
  #algorythms follow general rules
    #edges don't cross, verticies dont overlap
    #edges should be equal length if possible
    #position key nodes towards the center
  #layout_in_circle()
  #layout_with_fr()
  #layout_as_tree()
  #layout_nicely() #chooses best layout
  
#plot
  plot(g2, layout = layout.fruchterman.reingold(g2))
  
#You can also stipulate the layout seperately by creating a matrix object
  m <- layout_as_tree(g2)
  
  plot(g2, layout = m)
  
  
#Visualizing edges

w1 <- E(g2)$frequency

m1 <- layout_nicely(g2)

plot(g2,
     vertex.label.color = "black",
     edge.color = "black",
     edge.width = w1,
     layout = m1)  

#you can also delete edges
g2 <- delete.edges(g2, E(g2)[frequency < 3])
  


#------------------------------ FEB 12
# Make a basic plot
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray77',
     vertex.size = 0,
     edge.arrow.size = 0.1,
     layout = layout_nicely(g))

plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1, #vertex size determined by betweenness score
     edge.arrow.size = 0.05,
     layout = layout_nicely(g))


#Caluclating different network statistics
diameter <- farthest_vertices(g)
test <- unlist(diameter)
get_diameter(g)
degree(g, mode = c("total"))
betweenness(g, directed = T)
o.deg <- degree(g, mode = c("in"))
table(o.deg)
hist(o.deg)
which.max(o.deg)
eigen_centrality(g)$vector
edge_density(g)
mean_distance(g)
plot(erdos.renyi.game(n=gorder(g), p.or.m = edge_density(g), type = "gnp"))



#Creating random networks that mimic the shape of your network
gl <- vector('list', 45)

for(i in 1:45) {
  gl[[i]] <- erdos.renyi.game(n = 10, p.or.m = .9, type = "gnp")
}



  