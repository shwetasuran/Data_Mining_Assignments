rm(list = ls())
graphics.off()
library("igraph")


Edges <- read.csv("Dataset1-Media-Example-EDGES.csv")
Nodes <- read.csv("Dataset1-Media-Example-NODES.csv")


# EdgeCount and NodeCount (Intialization)

NodeCount = nrow(Nodes)
EdgeCount = nrow(Edges)


# NodeNmae  and NodeCount (Intialization)

NodeName = Nodes$media
NodeWeight = c(Nodes$audience.size)

# Adjaceny_Mtrx

Adjacency_matrix <- array(0, c(NodeCount, NodeCount))

# Adjaceny_Mtrx (Computation)

for (j in 1:EdgeCount) {
  r = as.numeric(substring(Edges$from[j], 2))
  c = as.numeric(substring(Edges$to[j], 2))
  Adjacency_matrix[r, c] = 1
  Adjacency_matrix[c, r] = 1
}


#Graph Plotting of Input

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


#  Node_DEgree (Intialization)

NodeDegree <- array(0, NodeCount)

# Node_DEgree (Caluclation)

for (m in 1:NodeCount) {
  for (j in 1:EdgeCount) {if (m == as.numeric(substring(Edges$from[j], 2)) ||
        m == as.numeric(substring(Edges$to[j], 2))) { NodeDegree[m] = NodeDegree[m] + 1}}}

# Pair_of_Frnds--Who_r_Frnds ((Intialization))

Friend_Of_Friend <- array(0, NodeCount)

# Pair_of_Frnds--Who_r_Frnds (Computation)
for (m in 1:NodeCount) 
{
  p = which(Adjacency_matrix[m, ] == 1)
  l = length(p)
  if (l > 0) 
  {for (j in 1:l)
    { Intr = intersect(p, which(Adjacency_matrix[p[j], ] == 1))
      
      Friend_Of_Friend[m] = Friend_Of_Friend[m] + length(Intr) } }}

# Friend_Of_Friend (round the value)

Friend_Of_Friend = floor(Friend_Of_Friend/2)


LocalClusteringCoeff <- array(0, NodeCount)


LocalClusteringCoeff = round(Friend_Of_Friend/(NodeDegree*(NodeDegree - 1)), digits = 2)
cat("\nPairs_of_Friend_Of_Friend\n", Friend_Of_Friend)
cat("\nDegree\n", NodeDegree)
cat("\nLocal_Clustering_Coefficient :\n", LocalClusteringCoeff)

plot(GGraph, edge.arrow.size = 0.15, edge.color = "blue", vertex.size = c(LocalClusteringCoeff*100),
     vertex.color = "yellow", vertex.frame.color = "blue", vertex.label.color = "black") 
title(main = "Local_Clustering_Coefficient")
