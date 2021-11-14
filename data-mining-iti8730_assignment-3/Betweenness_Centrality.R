
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


# FactOfParts (intialization)

FactOfParts <- array(0, NodeCount)

# BetweenCent  (intialization)

BetweenCent <- array(0, NodeCount)

# calculate fraction of pairs

for (m in 1:NodeCount) {s = as.character(Nodes$id[m])
  for (j in 1:(NodeCount - 1)) {from = as.character(Nodes$id[j])
    for (k in (j + 1):NodeCount) {to = as.character(Nodes$id[k])
      if (j != m && k != m) {sp = shortest_paths(GGraph, from, to)$vpath[[1]]
        if (length(sp) > 0) { q = 1
          if (s %in% names(sp)) { qi = 1 }
          else { qi = 0 }
          FactOfParts[m] = FactOfParts[m] + (qi / q)}}}}}
       

# BetweenCent (Computation)

BetweenCent = FactOfParts / ((NodeCount - 1) * (NodeCount - 2))

cat("\n BETWEENNESS_CENTRALITY:\n", BetweenCent)

plot(GGraph, edge.arrow.size = 0.25, edge.color = "blue",
     vertex.size = c(BetweenCent*200), vertex.color = "yellow",
     vertex.frame.color = "blue", vertex.label.color = "blue", vertex.label.dist = 2) 
title(main = "Betweenness_Centrality")