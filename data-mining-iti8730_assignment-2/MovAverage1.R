MovAverage1 <- function(datapnt) {
  l = length(datapnt)
  
  avgpnt = numeric(16)
  
  for (i in 3:15) {
    avgpnt[i] = (datapnt[i-2]+datapnt[i-1]+datapnt[i]+datapnt[i+1])/4
  }
  
  return(avgpnt)
}