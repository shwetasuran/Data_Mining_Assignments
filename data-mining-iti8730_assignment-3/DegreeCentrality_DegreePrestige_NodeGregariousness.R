# Degree Centrality - Degree Prestige &Node Gregariousness


rm(list = ls())
graphics.off()
library("igraph")


Edges <- read.csv("Dataset1-Media-Example-EDGES.csv")
Nodes <- read.csv("Dataset1-Media-Example-NODES.csv")

NodeCount = nrow(Nodes)
EdgeCount = nrow(Edges)

NodeName = Nodes$media
NodeWeight = c(Nodes$audience.size)


GraphPlot <- vector(mode="character", 2*EdgeCount)

l = 1

for (m in 1:EdgeCount) {
  GraphPlot[l] = as.character(Edges$from[m])
  GraphPlot[l + 1] = as.character(Edges$to[m])
  l = l + 2
}

GGraph = graph(c(GraphPlot))
plot(GGraph, edge.arrow.size = 0.15, edge.color = "blue", vertex.size = 25,
     vertex.color = "yellow", vertex.frame.color = "blue", vertex.label.color = "black") 
title(main = "Graph_Of_Input")


NodeDegree <- array(0, NodeCount)

# Node_DEgree (Computation)
for (m in 1:NodeCount) {
  for (j in 1:EdgeCount) {
    if (m == as.numeric(substring(Edges$from[j], 2)) ||
        m == as.numeric(substring(Edges$to[j], 2))) {
      NodeDegree[m] = NodeDegree[m] + 1 }}}
  
In_NodeDegree <- array(0, NodeCount)
Out_NodeDegree <- array(0, NodeCount)

# calculate node degree
for (m in 1:NodeCount) {
  for (j in 1:EdgeCount) {
    if (m == as.numeric(substring(Edges$to[j], 2))) {
      In_NodeDegree[m] = In_NodeDegree[m] + 1
    }
  }
}

Out_NodeDegree = NodeDegree - In_NodeDegree

DegreeCentrality = NodeDegree/(NodeCount-1)
DegreePrestige = In_NodeDegree/(NodeCount-1)
NodeGregariousness = Out_NodeDegree/(NodeCount-1)


cat("\n Degree_Centrality:\n", DegreeCentrality)
cat("\n Degree_Prestige:\n", DegreePrestige)
cat("\n Node_Gregariousness:\n", NodeGregariousness)

plot(GGraph, edge.arrow.size = 0.15, edge.color = "blue", vertex.size = c(NodeDegree*5),
     vertex.color = "yellow", vertex.frame.color = "blue", vertex.label.color = "black") 
title(main = "Degree_Centrality")

plot(GGraph, edge.arrow.size = 0.15, edge.color = "blue", vertex.size = c(In_NodeDegree*5),
     vertex.color = "yellow", vertex.frame.color = "blue", vertex.label.color = "black") 
title(main = "Degree_Prestige")

plot(GGraph, edge.arrow.size = 0.15, edge.color = "blue", vertex.size = c(Out_NodeDegree*5),
     vertex.color = "yellow", vertex.frame.color = "blue", vertex.label.color = "black") 
title(main = "Node_Gregariousness")

