

rm(list = ls())
graphics.off()
library("igraph")


Edges <- read.csv("Dataset1-Media-Example-EDGES.csv")
Nodes <- read.csv("Dataset1-Media-Example-NODES.csv")



NodeCount = nrow(Nodes)
EdgeCount = nrow(Edges)


GraphPlot <- vector(mode="character", 2*EdgeCount)

l = 1

for (m in 1:EdgeCount) {GraphPlot[l] = as.character(Edges$from[m])
  GraphPlot[l + 1] = as.character(Edges$to[m])
  l = l + 2}

GGraph = graph(c(GraphPlot))
plot(GGraph, edge.arrow.size = 0.15, edge.color = "blue", vertex.size = 25,
     vertex.color = "yellow", vertex.frame.color = "blue", vertex.label.color = "black") 
title(main = "Graph_Of_Input")



# eigenvectors_Computation
Tmp = eigen_centrality(GGraph, directed = TRUE)


cat("\n eigen_vectors\n")

print(Tmp$vector)

EigenVal <- as.vector(Tmp$vector)


EstradaIndex = 0

#EigenVal (computation)

for (m in 1:NodeCount) {
  EstradaIndex = EstradaIndex + exp(EigenVal[m])
}

cat("\n Estrada_Index ", EstradaIndex)