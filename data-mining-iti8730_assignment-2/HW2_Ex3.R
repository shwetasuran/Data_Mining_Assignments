
rm(list = ls())
graphics.off()

print("Data Stram\n")

xMin = 1
xMax = 10
yMin = 1
yMax = 10

p = as.integer(750000)

VectorX <- round(runif(p, xMin, xMax), digit = 2)
VectorY <- round(runif(p, yMin, yMax), digit = 2)

cat("Elements", p, "\n")

PVectorX <- VectorX[1:500]
PVectorY <- VectorY[1:500]

PVector <- data.frame(PVectorX,PVectorY)

plot(PVectorX, PVectorY, xlim = c(xMin, xMax), ylim = c(yMin, yMax),
     pch = 23, bg = "blue", col = "orange", lwd = 0.6, cex = 1.5,
     main = "Real_Reservoir")



Count = 0
Chng <- matrix(0, nrow = 1, ncol = 1)

for (j in 501:p) {
  
  Itp = 500/j
  RandomN <- runif(1, 0, 1)
  
  if(Itp > RandomN) {
    Count = Count + 1
    
    RandomReplacement <- sample(1:500, 1)
    PVectorX[RandomReplacement] = VectorX[j]
    PVectorY[RandomReplacement] = VectorY[j]
    
    
  }
  
  if (j %% 25000 == 0) {
    Chng <- rbind(Chng, c(Count))
    Count = 0
    
    text = paste("Reservoir_Streaming_at (", j, ")")
    plot(PVectorX, PVectorY, xlim = c(xMin, xMax), ylim = c(yMin, yMax),
         pch = 23, bg = "blue", col = "orange", lwd = 0.6, cex = 1.5,
         main = text)
  }
  
}

Chng = Chng[-1] 

plot(c(1:length(Chng)), Chng, pch = 20, col = "blue",
     xlab="TotalN_of_Itr-75000", ylab="Changes", main = "Num_Chng_Reservoir")
lines(c(1:length(Chng)), Chng, col = "red")