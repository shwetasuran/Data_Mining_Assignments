# clear and close everything
rm(list=ls())
graphics.off()



load("data.RData")
load("labels.RData")


distancefunction <- function(point, data, clst, p) {
  
  #count the total number of elemnt in data
  totalelementcount <- length(data[,1]) 
  
  intzeuclideandist <- array(0, dim = c(totalelementcount, 1)) 
  intmanhattandist <- array(0, dim = c(totalelementcount, 1)) 
  intchebyshevdist <- array(0, dim = c(totalelementcount, 1)) 
  intcanbdistdist <- array(0, dim = c(totalelementcount, 1))
  
  temporaryvalue = as.integer(0)
  
  labels = unique(clst)
  
  for (m in 1:totalelementcount) {
    intzeuclideandist[m] = (((data[m,1] - point[1])^2) + ((data[m,2] - point[2])^2))^0.5
    
    intmanhattandist[m] = abs(data[m,1] - point[1]) + abs(data[m,2] - point[2])
    
    intchebyshevdist[m] = max(c(abs(data[m,1] - point[1]), abs(data[m,2] - point[2])))
    
    intcanbdistdist[m] = abs(data[m,1] - point[1])/(abs(data[m,1]) + abs(point[1])) +
      abs(data[m,2] - point[2])/(abs(data[m,2]) + abs(point[2]))
  }
  
  x1 <- sort(intzeuclideandist,index.return = TRUE)$ix #returns all of the values in the column 
  listx1 <- x1[-(c(p+1):totalelementcount)]
  
  X2 <- sort(intmanhattandist,index.return = TRUE)$ix
  listX2 <- X2[-(c(p+1):totalelementcount)]
  
  X3 <- sort(intchebyshevdist,index.return = TRUE)$ix
  listX3 <- X3[-(c(p+1):totalelementcount)]
  
  X4 <- sort(intcanbdistdist,index.return = TRUE)$ix
  listX4 <- X4[-(c(p+1):totalelementcount)]
  
  listofcluster <- array(0, dim = c(p, 4))
  clusterfunction <- array(0, dim = c(1, 4))
  
  listofcluster[,1] <- clst[c(listx1)]
  clusterfunction[1] = names(which.max(table(listofcluster[,1])))
  cat("\nKNN for eucli", clusterfunction[1])
  
  listofcluster[,2] <- clst[c(listX2)]
  clusterfunction[2] = names(which.max(table(listofcluster[,2])))
  cat("\nKNN for manh", clusterfunction[2])
  
  listofcluster[,3] <- clst[c(listX3)]
  clusterfunction[3] = names(which.max(table(listofcluster[,3])))
  cat("\nKNN for cheb", clusterfunction[3])
  
  listofcluster[,4] <- clst[c(listX4)]
  clusterfunction[4] = names(which.max(table(listofcluster[,4])))
  cat("\nKNN for can", clusterfunction[4])
}




minofX = min(data[,1])
maxofX = max(data[,1])
minofY = min(data[,2])
maxofY = max(data[,2])

point = c(0,0)

repeat {
  
  cat("\nFavourable XrangeAxis", minofX, "-", maxofX)
  cat("\nFavourable YrangeAxis", minofY, "-", maxofY)
  
  cat("\nPut value-p (0 to exit):")
  
  p <- as.integer(readline(prompt="p? "))
  
  if (p == 0) {
    break
  }
  
  point[1] <- as.numeric(readline(prompt="enter x-coordinate: "))
  point[2] <- as.numeric(readline(prompt="enter y-coordinate: "))
  
  plot(data[,1], data[,2], col = kmeansclus, xlim=c(minofX, maxofX), ylim=c(minofY, maxofY),
       xlab="X", ylab="Y", pch = 20)
  par(new=TRUE)
  plot(point[1], point[2], xlim=c(minofX, maxofX), ylim=c(minofY, maxofY),
       xlab="X", ylab="Y", pch = 8)
  
  distancefunction(point, data, kmeansclus, p)
  
}