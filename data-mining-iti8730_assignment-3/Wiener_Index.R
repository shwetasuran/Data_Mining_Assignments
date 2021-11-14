


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
  l = l + 2
}

GGraph = graph(c(GraphPlot))
plot(GGraph, edge.arrow.size = 0.15, edge.color = "blue", vertex.size = 25,
     vertex.color = "yellow", vertex.frame.color = "blue", vertex.label.color = "black") 
title(main = "Graph_Of_Input")

wiener_index <- array(0, NodeCount)

# wiener_index (Computation)
for (m in 1:NodeCount) {from = as.character(Nodes$id[m])
  for (j in 1:NodeCount) {if (m != j) {to = as.character(Nodes$id[j])
      TempShortestPath = shortest_paths(GGraph, from, to)
      q = as.character(TempShortestPath$vpath[[1]])
      if (length(q) > 1) {for (i in 1:(length(q) - 1)) {f = as.character(names(TempShortestPath$vpath[[1]][i]))
          t = as.character(names(TempShortestPath$vpath[[1]][i + 1]))
          sD = min(Edges$weight[which((Edges$from == f) & (Edges$to == t))])
          wiener_index[m] = wiener_index[m] + sD }} else {
        wiener_index[m] = wiener_index[m] + 0 }}}}
 

cat("\n Wiener_Index", sum(wiener_index))

