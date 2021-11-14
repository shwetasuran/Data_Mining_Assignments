
rm(list = ls())

graphics.off()


Data <- read.csv(file = "IstanbulStockExchange.csv")
#Data <- read.csv(file = "ECG200.csv")

VectorA <- Data[, 3]
VectorB <- Data[, 7]

#VectorA <- c(1,1,3,3,4,3,2,2,3,5,4,3,3,2,2) 
#VectorB <- c(0,0,0,1,1,0,0,0,1,0)

dyntimwrapVa <- c(1:length(VectorA))
dtwTimeVec2 <- c(1:length(VectorB))



xMax <-  max(c(dyntimwrapVa, dtwTimeVec2))
xMin <-  min(c(dyntimwrapVa, dtwTimeVec2))
yMax <-  max(c(VectorA, VectorB))
yMin <-  min(c(VectorA, VectorB))


plot(
  dyntimwrapVa,
  VectorA,
  pch = 18,
  col = "brown",
  xaxt = 'n',
  yaxt = 'n',
 ann = FALSE,
  xlim = c(xMin, xMax),
  ylim = c(yMin, yMax)
)


lines(dyntimwrapVa, VectorA, col = "blue")
par(new = TRUE) 
plot(
  dtwTimeVec2,
  VectorB,
  pch = 8,
  col = "blue",
  xaxt = 'n',
  yaxt = 'n',
 ann = FALSE,
  xlim = c(xMin, xMax),
  ylim = c(yMin, yMax)
)
lines(dtwTimeVec2, VectorB, col = "orange")



EDMatrix <-
  matrix(0, nrow = length(dtwTimeVec2), ncol = length(dyntimwrapVa))


for (p in dtwTimeVec2) {
  for (c in dyntimwrapVa) {
    EDMatrix[p, c] = abs(VectorB[p] - VectorA[c])
    if (p != 1 || c != 1) {
      if (p == 1) {
        EDMatrix[p, c] = EDMatrix[p, c] + EDMatrix[p, c - 1]
      } else if (c == 1) {
        EDMatrix[p, c] = EDMatrix[p, c] + EDMatrix[p - 1, c]
      } else {
        EDMatrix[p, c] = EDMatrix[p, c] +
          min(EDMatrix[p - 1, c - 1], EDMatrix[p - 1, c], EDMatrix[p, c - 1])
      }
    }
  }
}



EDpar <- matrix(0, nrow = 1, ncol = 2)

EDpar[1, 1] = length(dtwTimeVec2)
EDpar[1, 2] = length(dyntimwrapVa)

k = 1
stop = FALSE


for (p in rev(dtwTimeVec2)) {
  for (c in rev(dyntimwrapVa)) {
    
    if (EDpar[k, 1] == 1 && EDpar[k, 2] == 1) {
      stop = TRUE
      break
    }
    
    if (EDpar[k, 1] == p && EDpar[k, 2] == c) {
      
      k = k + 1
      
      if (p == 1) {
        EDpar <- rbind(EDpar, c(p, c - 1))
      } else if (c == 1) {
        EDpar <- rbind(EDpar, c(p - 1, c))
      } else {
        d = EDMatrix[p - 1, c - 1]
        a = EDMatrix[p, c - 1]
        b = EDMatrix[p - 1, c]
        
        if (d < a && d < b) {
          EDpar <- rbind(EDpar, c(p - 1, c - 1))
        } else if (a < b && a < d) {
          EDpar <- rbind(EDpar, c(p, c - 1))
        } else {
          EDpar <- rbind(EDpar, c(p - 1, c))
        }
      }
    }
  }
  if (stop) {break}
}

for (k in 1:length(EDpar[, 1])) {
  par(new = TRUE) 
  segments(EDpar[k, 1], VectorB[EDpar[k, 1]],
           EDpar[k, 2], VectorA[EDpar[k, 2]],
           col = "green", lty="solid")
}

