
rm(list = ls())
graphics.off()

library("plotrix")

l = 400
VectorX <- rnorm(l/2, mean = 0.5, sd = 1.5) 
VectorY <- rnorm(l/2, mean = 0.5, sd = 1.5) 

DaVectorX <- matrix(0, nrow = 1, ncol = l)
DaVectorY <- matrix(0, nrow = 1, ncol = l)

DaVectorX[1:400] = VectorX
DaVectorY[1:400] = VectorY

VectorX <- rnorm(l/2, mean = 1.5, sd = 1.0)
VectorY <- rnorm(l/2, mean = 1.5, sd = 1.0)

DaVectorX[201:400] = VectorX
DaVectorY[201:400] = VectorY


plot(
  DaVectorX,
  DaVectorY,
  pch = 16,
  col = "black",
  main = "Original Dataset"
)

print("Clustering for k = 3...")


distance <- matrix(0 , nrow = l, ncol = l)

for (a in 1:l) {
  for (b in 1:l) {
    distance[a,b] = (((DaVectorX[a] - DaVectorX[b])^2) + ((DaVectorY[a] - DaVectorY[b])^2))^0.5
  }
}

KN <- matrix(0 , nrow = l, ncol = 3)

for (j in 1:l) {
  clmn <- distance[,j]
  
  tmp = sapply(sort(clmn, index.return=TRUE), `[`, 2)
  KN[j, 1] = as.numeric(tmp[2])
  
  tmp = sapply(sort(clmn, index.return=TRUE), `[`, 3)
  KN[j, 2] = as.numeric(tmp[2])
  
  tmp = sapply(sort(clmn, index.return=TRUE), `[`, 4)
  KN[j, 3] = as.numeric(tmp[2])
}

ldr <- matrix(0 , nrow = l, ncol = 1)

for (j in 1:l) {
  Distancesum = 0
  
  P1 = c(KN[j, 1])
  P2 = c(KN[j, 2])
  P2 = c(KN[j, 3])
  Distance1 = max(distance[P1, (KN[P1, 3])], distance[j, P1])
  Distance2 = max(distance[P2, (KN[P2, 3])], distance[j, P2])
  Distance3 = max(distance[P2, (KN[P2, 3])], distance[j, P2])
  
  Distancesum = Distancesum + Distance1 + Distance2 + Distance3
  
  ldr[j] = 3/Distancesum
}

lof <- matrix(0 , nrow = l, ncol = 1)

for (j in 1:l) {
  lof[j] = 0
  
  P1 = c(KN[j, 1])
  P2 = c(KN[j, 2])
  P3 = c(KN[j, 3])
  
  lof[j] = lof[j] + (ldr[P1] + ldr[P2] + ldr[P2])
  
  Distance1 = max(distance[P1, (KN[P1, 3])], distance[j, P1])
  Distance2 = max(distance[P2, (KN[P2, 3])], distance[j, P2])
  Distance3 = max(distance[P2, (KN[P2, 3])], distance[j, P2])
  
  lof[j] = lof[j] * (Distance1 + Distance2 + Distance2)
  
  lof[j] = lof[j] / (3^2)
}


plot(
  DaVectorX[1:400],
  DaVectorY[1:400],
  pch = 4,
  cex = 0.5,
  col = "green",
  xlim = c(min(DaVectorX), max(DaVectorX)),
  ylim = c(min(DaVectorY), max(DaVectorY)),
  xlab="DaVectorX", ylab="DaVectorY",
  main = "LOF"
)

par(new = TRUE) 

plot(
  DaVectorX[201:400],
  DaVectorY[201:400],
  pch = 4,
  cex = 0.5,
  col = "green",
  xlim = c(min(DaVectorX), max(DaVectorX)),
  ylim = c(min(DaVectorY), max(DaVectorY)),
  xlab="DaVectorX", ylab="DaVectorY",
  main = "LOF"
)

for (j in 1:l) {
  if (j <= 200) {
    draw.circle(DaVectorX[j], DaVectorY[j], round(lof[j]/10, digits = 2), border = "red")
  }
  else {
    draw.circle(DaVectorX[j], DaVectorY[j], round(lof[j]/10, digits = 2), border = "blue")
  }
}