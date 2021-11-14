

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

cat("\n Jaccard Measure_Out\n")

for (m in 1:NodeCount) {
  for (j in 1:NodeCount) {
    if (m != j) { Edge1 = Edges$to[c(which(Edges$from == as.character(Nodes$id[m])))]
      Edge2 = Edges$to[c(which(Edges$from == as.character(Nodes$id[j])))]
      on = intersect(Edge1, Edge2)
      off = union(Edge1, Edge2)
      
      if (length(on) > 0) {
        cat("[", as.character(Nodes$id[m]), ",", as.character(Nodes$id[j]), "] -- ")
        cat(length(on), "/", length(off))
        cat(" : ", length(on)/length(off), "\n") }}}}
 


cat("\n Jaccard Measure_In\n")

for (m in 1:NodeCount) {for (j in 1:NodeCount) {if (m != j) {Edge1 = Edges$from[c(which(Edges$to == as.character(Nodes$id[m])))]
      Edge2 = Edges$from[c(which(Edges$to == as.character(Nodes$id[j])))]
      on = intersect(Edge1, Edge2)
      off = union(Edge1, Edge2)
      
      if (length(on) > 0) { cat("[", as.character(Nodes$id[m]), ",", as.character(Nodes$id[j]), "] -- ")
        cat(length(on), "/", length(off))
        cat(" : ", length(on)/length(off), "\n") }}}}
  
