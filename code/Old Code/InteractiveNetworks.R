#INTERACTIVE NETWORKS

#Load Libraries
library(shiny)
library(tidyverse)
library(readr) 
library(tidyverse)
library(here)
library(patchwork)
library(igraph)
library(ggraph)
library(threejs) #interactive network package
library(networkD3) #another interactive network package more compatable with shiny?

#Load data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))


#Create interactive network
graphjs(g)

g <-set_vertex_attr(g,
                    "label",
                    value = V(g)$name)


#Color
x = edge.betweenness.community(g)
i <- membership(x)
g<-set_vertex_attr(g,
                   "color",
                   value = [i)


g2 <- igraph_to_networkD3(g, group = i)

fn <- forceNetwork(Links = g2$links, Nodes = g2$nodes, NodeID = 'name', Group = 'group', zoom = T)

fn$x$nodes$hyperlink <- paste0(
  'https://en.wikipedia.org/wiki/Cat',
  g2$name
)

fn$x$options$clickAction = 'window.open(d.hyperlink)'

#Create Shiny App
#create blank user interface (UI)
ui <- fluidPage(
  titlePanel("Gulf of Alaska Groundfish Food Web"),
  forceNetworkOutput('trend')
)

#create function for server

server <- function(input, output, session) {
  output$trend <- renderForceNetwork({
    fn
  })
}

shinyApp(ui = ui, server = server)

shiny::runGitHub('christophergandrud/networkD3-shiny-example')

