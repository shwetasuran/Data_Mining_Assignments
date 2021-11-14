
rm(list = ls())
graphics.off()


library("igraph")


Edges <- read.csv("Dataset1-Media-Example-EDGES.csv")
Nodes <- read.csv("Dataset1-Media-Example-NODES.csv")


GraphEd_Orginal = Edges[, 1:2]
GraphEd_Count = nrow(GraphEd_Orginal)
GraphPlot_Orginal <- vector(mode="character", 2*GraphEd_Count)

k = 1

for (i in 1:GraphEd_Count) {
  GraphPlot_Orginal[k] = as.character(GraphEd_Orginal$from[i])
  GraphPlot_Orginal[k + 1] = as.character(GraphEd_Orginal$to[i])
  k = k + 2
}

OriginalGraph = graph(c(GraphPlot_Orginal))


SubGraph_First <- induced_subgraph(OriginalGraph, c("s01", "s02", "s03","s04", "s05", "s06",
                       "s07", "s08", "s09", "s10", "s12" ,"s13", "s15", "s16", "s17"))
SubGraph_Second <- induced_subgraph(OriginalGraph, c("s01", "s02", "s03", "s05", "s06", "s08"
                                                    , "s09","s10", "s12" ,"s13", "s14", "s15", "s16"))

plot(OriginalGraph, edge.arrow.size = 0.30, edge.color = "blue",
     vertex.size = 30, vertex.color = "yellow",
     vertex.frame.color = "blue", vertex.label.color = "black", vertex.label.dist = 2) 
title(main = "Original")


plot(SubGraph_First, edge.arrow.size = 0.30, edge.color = "blue",
     vertex.size = 30, vertex.color = "yellow",
     vertex.frame.color = "blue", vertex.label.color = "black", vertex.label.dist = 2) 
title(main = "SubGraph-First")


plot(SubGraph_Second, edge.arrow.size = 0.30, edge.color = "blue",
     vertex.size = 30, vertex.color = "yellow",
     vertex.frame.color = "blue", vertex.label.color = "black", vertex.label.dist = 2) 
title(main = "SubGraph-Second")


GraphCliques1 = max_cliques(SubGraph_First)

Len1 = length(GraphCliques1)
CliqueOne <- array("", c(Len1, Len1))

for (i in 1:Len1) {
  n = names(GraphCliques1[[i]])
  for (j in 1:length(n)) {
    CliqueOne[i, j] = n[j]
  }
}


GraphCliques2 = max_cliques(SubGraph_Second)

Len2 = length(GraphCliques2)
CliqueTwo <- array("", c(Len2, Len2))

for (i in 1:Len2) {
  n = names(GraphCliques2[[i]])
  for (j in 1:length(n)) {
    CliqueTwo[i, j] = n[j]
  }
}

y <- as.character()
l = 1
for (i in 1:Len1) {
  for (j in 1:Len2) {
    b = setequal(CliqueOne[i, ], CliqueTwo[j, ])
    if (b) {
      e = CliqueOne[i, ]
      e = e[e != ""]
      s = length(e)
      for (k in 1:s) {
        y[l] = e[k]
        l = l + 1 }}}}


y = unique(y)

MaximumCommSubgrp <- induced_subgraph(OriginalGraph, c(y))


plot(MaximumCommSubgrp, edge.arrow.size = 0.30, edge.color = "blue",
     vertex.size = 30, vertex.color = "yellow",
     vertex.frame.color = "blue", vertex.label.color = "black", vertex.label.dist = 2) 
title(main = "MaximumCommSubgrp")
