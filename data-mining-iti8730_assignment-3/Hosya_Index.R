

rm(list = ls())
graphics.off()
library("igraph")


Edges <- read.csv("Dataset1-Media-Example-EDGES.csv")
Nodes <- read.csv("Dataset1-Media-Example-NODES.csv")


NodeCount = nrow(Nodes)
EdgeCount = nrow(Edges)


GraphPlot <- vector(mode="character", 2*EdgeCount)

l = 1

for (m in 1:EdgeCount) { GraphPlot[l] = as.character(Edges$from[m])
  GraphPlot[l + 1] = as.character(Edges$to[m])
  l = l + 2}

GGraph = graph(c(GraphPlot))
plot(GGraph, edge.arrow.size = 0.15, edge.color = "blue", vertex.size = 25,
     vertex.color = "yellow", vertex.frame.color = "blue", vertex.label.color = "black") 
title(main = "Graph_Of_Input")


library(tidyverse)

# DistinctEdges (Computation)

DistinctEdges = distinct(Edges[, 1:2])
l = nrow(DistinctEdges)


HosyaIndex <- array(1, l)
list <- array("", 4)

# HosyaIndex (Computation)
for (m in 1:l) {for (j in 1:m) {
    if (m != j) {list[1] = as.character(DistinctEdges$from[m])
      list[2] = as.character(DistinctEdges$to[m])
      list[3] = as.character(DistinctEdges$from[j])
      list[4] = as.character(DistinctEdges$to[j])
      if (length(unique(list)) == 4) { HosyaIndex[m] = HosyaIndex[m] + 1} else { }}}}
   
cat("\n Hosya_Index", sum(HosyaIndex))