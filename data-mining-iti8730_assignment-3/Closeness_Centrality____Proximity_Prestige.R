
rm(list = ls())
graphics.off()
library("igraph")


Edges <- read.csv("Dataset1-Media-Example-EDGES.csv")
Nodes <- read.csv("Dataset1-Media-Example-NODES.csv")


NodeCount = nrow(Nodes)
EdgeCount = nrow(Edges)


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

# SumOfShrtDist (Intialization)

SumOfShrtDist <- array(0, NodeCount)
influence <- array(0, NodeCount)

# SumOfShrtDist (Computation)

for (m in 1:NodeCount) {from = as.character(Nodes$id[m])
  for (j in 1:NodeCount) {
    if (m != j) {to = as.character(Nodes$id[j])
      TempShortestPath = shortest_paths(GGraph, from, to)
      q = as.character(TempShortestPath$vpath[[1]])
      if (length(q) > 1) {for (l in 1:(length(q) - 1)) {f = as.character(names(TempShortestPath$vpath[[1]][l]))
          t = as.character(names(TempShortestPath$vpath[[1]][l + 1]))
          sD = min(Edges$weight[which((Edges$from == f) & (Edges$to == t))])
          SumOfShrtDist[m] = SumOfShrtDist[m] + sD
          influence[j] = influence[j] + 1
        }} else {SumOfShrtDist[m] = SumOfShrtDist[m] + Inf } } }}

# ClosenessCentrailty (Intialization)

ClosenessCentrailty <- array(0, NodeCount)

# ProximityPrestige (Intialization)

ProximityPrestige <- array(0, NodeCount)


# ClosenessCentrailty (Computation)

ClosenessCentrailty = (NodeCount - 1)/SumOfShrtDist

# ProximityPrestige (Computation)

ProximityPrestige = (influence ^ 2)/((NodeCount - 1) * SumOfShrtDist)

cat("\nClosenessCentrailty\n", ClosenessCentrailty)
cat("\nProximityPrestige\n", ProximityPrestige)

plot(GGraph, edge.arrow.size = 0.25, edge.color = "blue",
     vertex.size = c(ClosenessCentrailty*500), vertex.color = "yellow",
     vertex.frame.color = "blue", vertex.label.color = "black", vertex.label.dist = 2) 
title(main = "Closeness_Centrality")

plot(GGraph, edge.arrow.size = 0.25, edge.color = "blue",
     vertex.size = c(ProximityPrestige*50), vertex.color = "yellow",
     vertex.frame.color = "blue", vertex.label.color = "black", vertex.label.dist = 2) 
title(main = "Proximity_Prestige")