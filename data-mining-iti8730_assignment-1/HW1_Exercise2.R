# to clear and close
rm(list=ls())
graphics.off()

library(clv)


load("irisdata.RData")

#intilizating centroids for the Kmeans
intializecentroid <- function(irisdata, clustercount) {
  
  cat("centroids intilization")
  
  #count the total number of elemnt in data
  totalelementcount <- length(irisdata[,1]) 
  
  #centroid intilization
  intializecent <- array(0, dim = c(clustercount, 2)) 
  
  #for the selection of the randomcenteroid
  for (j in 1:clustercount) {
    pos = as.integer(runif(1, 1, totalelementcount))
    
    intializecent[j, 1] = irisdata[pos,1]
    intializecent[j, 2] = irisdata[pos,2]
    
    #display the centoid
    cat("(", intializecent[j, 1], ",", intializecent[j, 2], ")")
  }
  
  return(intializecent)
}


#for kmean computation
kmean <- function(irisdata, cntroid, totaliteration, clustercount) {
  
  #compute the minimum and maximum   
  minofXa = min(irisdata[,1])
  maxofXa = max(irisdata[,1])
  minofYa = min(irisdata[,2])
  maxofYa = max(irisdata[,2])
  
  #total number of elemnts count in column1
  totalelementcount <- length(irisdata[,1]) 
  
  #distance of ele form the centroid
  distance <- array(0, dim = c(totalelementcount, clustercount))
  
  #label the data 
  datalabel <- array(0, dim = c(totalelementcount, 1)) 
  
  #colour the custers to differentiate 
  clustercolor <- array(1:3, dim = c(clustercount, 1)) 
  
  #run the loop until number of (150) iterations finihed
  for (l in 1:totaliteration) {
    
    # EUCLIDEAN distance: Compute
    for (m in 1:clustercount) {
      for (n in 1:totalelementcount) {
        distance[n, m] = (((irisdata[n, 1] - cntroid[m, 1])^2)+((irisdata[n, 2] - cntroid[m, 2])^2))^0.5
      }
    }
    
    #cluster the data
    for (m in 1:totalelementcount) {
      datalabel[m] <- which.min(distance[m,])
    }
    
    #centroid intialization
    cntroid <- array(0, dim = c(clustercount, 2)) 
    
    #now,count elemnts in that particular class (intilization)
    countelements <- array(0, dim = c(clustercount, 1)) 
    
    #now compute-sum of distances
    for (m in 1:totalelementcount) {
      if (datalabel[m] == 1) {
        countelements[1] = countelements[1] + 1
        cntroid[1,1] = cntroid[1,1] + irisdata[m,1]
        cntroid[1,2] = cntroid[1,2] + irisdata[m,2]
      } else if (datalabel[m] == 2) {
        countelements[2] = countelements[2] + 1
        cntroid[2,1] = cntroid[2,1] + irisdata[m,1]
        cntroid[2,2] = cntroid[2,2] + irisdata[m,2]
      } else {
        countelements[3] = countelements[3] + 1
        cntroid[3,1] = cntroid[3,1] + irisdata[m,1]
        cntroid[3,2] = cntroid[3,2] + irisdata[m,2]
      }
    }
    
    #use average distance or mean to identify new cluster
    for (m in 1:clustercount) {
      cntroid[m,1] = cntroid[m,1] / countelements[m]
      cntroid[m,2] = cntroid[m,2] / countelements[m]
    }
    
  }
  
  #plot the all (3) centroids
  for (m in 1:clustercount) {
    plot(cntroid[m, 1], cntroid[m, 2], col = clustercolor[m],
         xlab="", ylab="", xlim=c(minofX, maxofX), ylim=c(minofY, maxofY), pch = 8)
    par(new=TRUE)
  }
  
  #finally, plot the clustered data
  plot(irisdata[,1], irisdata[,2], col = datalabel, xlim=c(minofX, maxofX), ylim=c(minofY, maxofY),
       xlab="X", ylab="Y", main=paste("k-means totaliteration:", l), pch = 19)
  
  #diaplay centroids
  cat("\nk-means centroids...")
  for (m in 1:clustercount) {
    cat("(", cntroid[m, 1], ",", cntroid[m, 2], ")")
  }
  
  return(datalabel)
}

#for kmediodccomputation
kmedoid <- function(irisdata, cntroid, totaliteration, clustercount) {
  
  #compute the minimum and maximum   
  minofXa = min(irisdata[,1])
  maxofXa = max(irisdata[,1])
  minofYa = min(irisdata[,2])
  maxofYa = max(irisdata[,2])
  
  #total number of elemnts count in column1
  totalelementcount <- length(irisdata[,1]) 
  
  #distance of ele form the centroid
  distance <- array(0, dim = c(totalelementcount, clustercount)) 
  
  #label the data
  datalabel <- array(0, dim = c(totalelementcount, 1)) 
  
  #cost of the cluster (Manhattan)
  costofcluster <- as.integer(0) 
  
  # cost of the cluster (minimum)
  mincostofcluster <- as.integer(0) 
  
  # for the bestcentroid
  bestcentroid <- array(0, dim = c(clustercount, 2)) 
  
  #colour the custers to differentiate 
  clustercolor <- array(1:3, dim = c(clustercount, 1)) 
  
  #ccompute according to the MANHATTAN distance  
  for (m in 1:clustercount) {
    for (n in 1:totalelementcount) {
      distance[n, m] = (abs(irisdata[m, 1] - cntroid[m, 1])+abs(irisdata[m, 2] - cntroid[m, 2]))
    }
  }
  
  #calculate cost of cluster and datalabel
  for (m in 1:totalelementcount) {
    datalabel[m] <- which.min(distance[m,])
    costofcluster = costofcluster + min(distance[m,])
  }
  
  #lets suppose the cost of first cluster: Minimum
  mincostofcluster = costofcluster 
  
  #lets suppose the best cenroid is the first one
  bestcentroid = cntroid 
  l = 1
  
  #run the for loop to reach the termination
  repeat {
    
    costofcluster <- as.integer(0)
    
    #slecetion of new cntroid
    for (m in 1:clustercount) {
      pos = as.integer(runif(1, 1, totalelementcount))
      
      cntroid[m, 1] = irisdata[pos,1]
      cntroid[m, 2] = irisdata[pos,2]
    }
    
    #compute the distance according to the MANHATTAN
    for (m in 1:clustercount) {
      for (n in 1:totalelementcount) {
        distance[n, m] = (abs(irisdata[n, 1] - cntroid[m, 1])+abs(irisdata[n, 2] - cntroid[m, 2]))
      }
    }
    
    #  #calculate cost of cluster and datalabel
    for (m in 1:totalelementcount) {
      datalabel[m] <- which.min(distance[m,])
      costofcluster = costofcluster + min(distance[m,])
    }
    
    #aim of this loop to check the (final condition is fullfilled)
    
    if (l == totaliteration) {
      break
    } else if (costofcluster < mincostofcluster) {
      mincostofcluster = costofcluster 
      bestcentroid = cntroid 
      
    }
    
    l = l + 1
    
  }
  
  #plotting of the cntroid
  for (m in 1:clustercount) {
    plot(cntroid[m, 1], cntroid[m, 2], col = clustercolor[m],
         xlab="", ylab="", xlim=c(minofX, maxofX), ylim=c(minofY, maxofY), pch = 8)
    par(new=TRUE)
  }
  
  #plotting 
  plot(irisdata[,1], irisdata[,2], col = datalabel, xlim=c(minofX, maxofX), ylim=c(minofY, maxofY),
       xlab="X", ylab="Y",
       main=paste("k-medoid totaliteration:", totaliteration), pch = 19)
  
  #display 
  cat("\nk-medoid centroids...")
  for (m in 1:clustercount) {
    cat("(", cntroid[m, 1], ",", cntroid[m, 2], ")")
  }
  cat("\ncluster cost...",mincostofcluster)
  
  return(datalabel)
}



#compute the minimum and maximum to plot the graph
minofX = min(irisdata[,1])
maxofX = max(irisdata[,1])
minofY = min(irisdata[,2])
maxofY = max(irisdata[,2])

#number of clusters
clustercount <- as.integer(3) 

#total number of iteration
totaliteration <- as.integer(150) 



#plot the iris data
plot(irisdata[,1], irisdata[,2], xlim=c(minofX, maxofX), ylim=c(minofY, maxofY),
     xlab="X", ylab="Y", main="Input Data", pch = 19)



#first initialized the centroids
intializecent <- intializecentroid(irisdata, clustercount) 

cat("\nRunning for", totaliteration, "iterations\n")

#labels the kmenas cluster 
labelkmeanscluster <- kmean(irisdata, intializecent, totaliteration, clustercount) 



#labels the kmedoid cluster 
labelkmedoidcluster <- kmedoid(irisdata, intializecent, totaliteration, clustercount)

dissimilaritymatrixdata <- as.matrix(daisy(irisdata)) #dissimilarity matrix

silhouettecoefficient_kmeans <- silhouette(labelkmeanscluster, dissimilaritymatrixdata) #silhouettecoefficient_ for_Kmean
plot(silhouettecoefficient_kmeans)
silhouettecoefficient_kmedoids <- silhouette(labelkmedoidcluster, dissimilaritymatrixdata) #silhouettesilhouettecoefficient_for_kmedoid
plot(silhouettecoefficient_kmedoids)
 
