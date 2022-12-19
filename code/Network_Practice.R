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

