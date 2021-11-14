# clear workspace
rm(list = ls())
graphics.off()
cat("\014")

#install.packages("igraph")
#install.packages("visNetwork")
library("igraph")
library(tidyverse)
library (CINNA)
library(visNetwork)
library(linkcomm)
library(tidygraph)
library(qgraph)
library(arsenal)

kangData <- read.csv(file = "kangaroo_data.csv")

#load("kangData.RData")


edgeCnt = nrow(kangData)

grphPlt <- vector(mode="character", 2*edgeCnt)

k = 1

for (i in 1:edgeCnt) {
  grphPlt[k] = as.character(kangData$From[i])
  grphPlt[k + 1] = as.character(kangData$To[i])
  k = k + 2
}

nodeLst <- unique(grphPlt)
nodeCnt = length(nodeLst)


grp = graph(c(grphPlt))


plot(grp, edge.arrow.size = 0.25, edge.color = "gray80",
     vertex.size = 20, vertex.color = rainbow(18),
     vertex.frame.color = "gray40", vertex.label.color = "gray40",  vertex.label.cex = 1.5, layout = layout_nicely) 
title(main = "InputGraph")


visIgraph(grp) %>%
  visNodes(size = 35, shape = "circle",font = list(size=28)) %>%
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE)
###################################################################################

#HITS Algorithm..........................................................

########HubScore
hubScore <- hub_score(grp)$vector
authSore <- authority_score(grp)$vector

hubScoreColor <- character(length = nodeCnt)
hsClus = kmeans(hubScore, 3)

for (i in 1:nodeCnt) {
  if (hsClus$cluster[i] == 1) {
    hubScoreColor[i] = "red"
  } else if (hsClus$cluster[i] == 2) {
    hubScoreColor[i] = "orange"
  }
  else { hubScoreColor[i] = "gold" }
}

par(mfrow=c(1,2))

plot(grp, edge.arrow.size = 0.5, edge.color = "gray",
     vertex.size = 25, vertex.color = c(hubScoreColor),
     vertex.label.color = "black", layout = layout_on_grid) 

title(main = "Clustering_In_HubScore")

plot(grp, edge.arrow.size = 0.25, edge.color = "gray", vertex.size = hubScore * 30, vertex.color = rainbow(18), vertex.frame.color = "white", vertex.label.color = "black",  vertex.label.cex = 1.5, layout = layout_on_grid) 

title(main = "HubScore")

#######AuthorityScore

aubScoreColor <- character(length = nodeCnt)
auClus = kmeans(hubScore, 3)

for (i in 1:nodeCnt) {
  if (auClus$cluster[i] == 1) {
    aubScoreColor[i] = "red"
  } else if (auClus$cluster[i] == 2) {
    aubScoreColor[i] = "orange"
  }
  else { aubScoreColor[i] = "gold" }
}

par(mfrow=c(1,2))

plot(grp, edge.arrow.size = 0.5, edge.color = "gray",
     vertex.size = 25, vertex.color = c(aubScoreColor),
     vertex.label.color = "black", layout = layout_on_grid) 

title(main = "Clustering_In_AuthorityScore")

plot(grp, edge.arrow.size = 0.25, edge.color = "gray", vertex.size = authSore  * 30, vertex.color = rainbow(18), vertex.frame.color = "white", vertex.label.color = "black",  vertex.label.cex = 1.5, layout = layout_on_grid) 

title(main = "AuthorityScore")

########################################################################################
#pagerank.........................................................................
pgR <- page_rank(grp , algo = c("prpack", "arpack", "power"),
                 vids = V(grp), directed = TRUE, damping = 0.85,
                 personalized = NULL, weights = NULL, options = NULL)

pgbScoreColor <- character(length = nodeCnt)
pgClus = kmeans(pgR, 3)

for (i in 1:nodeCnt) {
  if (pgClus$cluster[i] == 1) {
    pgbScoreColor[i] = "red"
  } else if (pgClus$cluster[i] == 2) {
    pgbScoreColor[i] = "orange"
  }
  else { pgbScoreColor[i] = "gold" }
}

par(mfrow=c(1,2))

plot(grp, edge.arrow.size = 0.5, edge.color = "gray",
     vertex.size = 25, vertex.color = c(pgbScoreColor),
     vertex.label.color = "black", layout = layout_on_grid) 

title(main = "Clustering_In_PageRanking")

plot(grp, edge.arrow.size = 0.25, edge.color = "gray", vertex.size = c(pgR$vector * 200), vertex.color = rainbow(18), vertex.frame.color = "white", vertex.label.color = "black",  vertex.label.cex = 1.5, layout = layout_on_grid) 

title(main = "PageRanking")


##########################################################################################
#Eigenvector centrality

eiGen <- eigen_centrality(grp)

eibScoreColor <- character(length = nodeCnt)
eiClus = kmeans(eiGen, 3)

for (i in 1:nodeCnt) {
  if (eiClus$cluster[i] == 1) {
    eibScoreColor[i] = "red"
  } else if (eiClus$cluster[i] == 2) {
    eibScoreColor[i] = "orange"
  }
  else { eibScoreColor[i] = "gold" }
}

par(mfrow=c(1,2))

plot(grp, edge.arrow.size = 0.5, edge.color = "gray",
     vertex.size = 25, vertex.color = c(eibScoreColor),
     vertex.label.color = "black", layout = layout_on_grid) 

title(main = "Clustering_In_EigenvectorCentrality")

plot(grp, edge.arrow.size = 0.25, edge.color = "gray", vertex.size = c(eiGen$vector * 45), vertex.color = rainbow(18), vertex.frame.color = "white", vertex.label.color = "black",  vertex.label.cex = 1.5, layout = layout_on_grid) 

title(main = "EigenvectorCentrality")


###########################################################################################
##closeness centraility
     
# initialize sum of shortest distance
#shrtDistSum <- array(0, nodeCnt)
#infl <- array(0, nodeCnt)

# calculate sum of shortest distance
#for (i in 1:nodeCnt) {
#from = as.character(nodeLst[i])
#for (j in 1:nodeCnt) {
#if (i != j) {
#to = as.character(nodeLst[j])
#tempShrtPth = shortest_paths(grp, from, to)
#p = as.character(tempShrtPth$vpath[[1]])
#cat(from, "->", to, "|", p, "|")
#if (length(p) > 1) {
# sD = length(p) - 1
#shrtDistSum[i] = shrtDistSum[i] + sD
#} else {
#shrtDistSum[i] = shrtDistSum[i] + Inf
#}
#cat("--->", sD)
#}
#cat("\n")
#}
  #cat("\n")
#}


       
# initialize closeness centrality
#clnsCntrlt <- array(0, nodeCnt)


# calculate closeness centrality
#clnsCntrlt = (nodeCnt - 1)/shrtDistSum


#cat("\nCLOSENESS CENTRALITY:\n", clnsCntrlt)

##plot(grp, edge.arrow.size = 0.25, edge.color = "gray80", vertex.size = 20, vertex.color = rainbow(18), vertex.frame.color = "gray40", vertex.label.color = "gray40", vertex.label.cex = 1.5,layout = layout_nicely)  title(main = "Closeness Centrality")


closenesscent <- closeness(grp, mode="all")
closenesscent

clbScoreColor <- character(length = nodeCnt)
clClus = kmeans(closenesscent, 3)

for (i in 1:nodeCnt) {
  if (clClus$cluster[i] == 1) {
    clbScoreColor[i] = "red"
  } else if (clClus$cluster[i] == 2) {
    clbScoreColor[i] = "orange"
  }
  else { clbScoreColor[i] = "gold" }
}

par(mfrow=c(1,2))

plot(grp, edge.arrow.size = 0.5, edge.color = "gray",
     vertex.size = 25, vertex.color = c(clbScoreColor),
     vertex.label.color = "black", layout = layout_on_grid) 

title(main = "Clustering_In_ClosenessCentraility")

plot(grp, edge.arrow.size = 0.25, edge.color = "gray", vertex.size = closenesscent * 400, vertex.color = rainbow(18), vertex.frame.color = "white", vertex.label.color = "black",  vertex.label.cex = 1.5, layout = layout_on_grid) 

title(main = "ClosenessCentraility")


#Betweness CEntarility.....................................................................
cat("\n")

# initialize fraction of pairs
#frctOfPrs <- array(0, nodeCnt)

# initialize betweenness centrality
#btwsCntrlt <- array(0, nodeCnt)

# calculate fraction of pairs
# for (i in 1:nodeCnt) {
# v = as.character(nodeLst[i])
# cat("\n", as.character(nodeLst[i]), ":\n")
# for (j in 1:(nodeCnt - 1)) {
# from = as.character(nodeLst[j])
# for (k in (j + 1):nodeCnt) {
# to = as.character(nodeLst[k])
# if (j != i && k != i) {
# cat(from, "-->", to, " ")
# sp = shortest_paths(grp, from, to)$vpath[[1]]
# if (length(sp) > 0) {
# q = 1
# if (v %in% names(sp)) { qi = 1 }
# else { qi = 0 }
# frctOfPrs[i] = frctOfPrs[i] + (qi / q)
# cat(qi, "/", q, "\n")
# }
# }
# }
# cat("\n")
# }
# }

# calculate betweenness centrality
#btwsCntrlt = frctOfPrs / ((nodeCnt - 1) * (nodeCnt - 2))

#cat("\nBETWEENNESS CENTRALITY:\n", btwsCntrlt)

#plot(grp, edge.arrow.size = 0.25, edge.color = "gray80", vertex.size = 20, vertex.color = rainbow(18), vertex.frame.color = "gray40", vertex.label.color = "gray40", vertex.label.cex = 1.5,layout = layout_nicely)title(main = "Betweenness Centrality")


betnCenter <- betweenness(grp) 
betnCenter

bebScoreColor <- character(length = nodeCnt)
beClus = kmeans(betnCenter, 3)

for (i in 1:nodeCnt) {
  if (beClus$cluster[i] == 1) {
    bebScoreColor[i] = "red"
  } else if (beClus$cluster[i] == 2) {
    bebScoreColor[i] = "orange"
  }
  else { bebScoreColor[i] = "gold" }
}

par(mfrow=c(1,2))

plot(grp, edge.arrow.size = 0.5, edge.color = "gray",
     vertex.size = 25, vertex.color = c(bebScoreColor),
     vertex.label.color = "black", layout = layout_on_grid) 

title(main = "Clustering_In_BetweennessCentrality")

plot(grp, edge.arrow.size = 0.25, edge.color = "gray", vertex.size = betnCenter * 4.5, vertex.color = rainbow(18), vertex.frame.color = "white", vertex.label.color = "black",  vertex.label.cex = 1.5, layout = layout_on_grid) 
title(main = "BetweennessCentrality")

#######################################################################################
#degree centraility

# initialize adjacency matrix
# adjMtrx <- array(0, c(nodeCnt, nodeCnt))



# calculate adjacency matrix
# for (j in 1:edgeCnt) {
# r = as.numeric(kangData$From[j])
# c = as.numeric(kangData$To[j])
# adjMtrx[r, c] = 1
#  adjMtrx[c, r] = 1
# }

# nodeCnt = length(nodeLst)


# initialize node degree
#  nodeDgres <- array(0, nodeCnt)

# calculate node degree
# calculate node degree
# for (i in 1:nodeCnt) {
# for (j in 1:edgeCnt) {
# if (i == as.numeric(kangData$From[j]) ||
#  i == as.numeric(kangData$To[j])) {
# nodeDgres[i] = nodeDgres[i] + 1
# }
# }
# }

# cat("\nDEGREE CENTRALITY:\n", (nodeDgres))

# plot(grp, edge.arrow.size = 0.25, edge.color = "gray80", vertex.size = c(nodeDgres), vertex.color = rainbow(18), vertex.frame.color = "gray40", vertex.label.color = "gray40", vertex.label.cex = 1.5, layout = layout.fruchterman.reingold) 
#title(main = "Degree Centrality")

degreecent <- centr_degree(grp, mode = "all", )
degreecent$res

#debScoreColor <- character(length = nodeCnt)
#deClus = kmeans(degreecent, 3)

#for (i in 1:nodeCnt) {
#if (deClus$cluster[i] == 1) {
#cdebScoreColor[i] = "red"
#} #else if (deClus$cluster[i] == 2) {
#debScoreColor[i] = "orange"
#}
#else { debScoreColor[i] = "gold" }
#}

#par(mfrow=c(1,2))

#plot(grp, edge.arrow.size = 0.5, edge.color = "gray",
#vertex.size = 25, vertex.color = c(clbScoreColor),
#vertex.label.color = "black", layout = layout_on_grid) 

#title(main = "Clustering_In_ClosenessCentraility")

#plot(grp, edge.arrow.size = 0.25, edge.color = "gray", vertex.size = closenesscent * 400, vertex.color = rainbow(18), vertex.frame.color = "white", vertex.label.color = "black",  vertex.label.cex = 1.5, layout = layout_on_grid) 

#title(main = "ClosenessCentraility")


plot(grp, edge.arrow.size = 0.25, edge.color = "gray80", vertex.size = V(grp), vertex.color = rainbow(18), vertex.frame.color = "gray40", vertex.label.color = "gray40", vertex.label.cex = 1.5, layout = layout.fruchterman.reingold)
title(main = "DegreeCentrality")




