MovAverage2 <- function(datapnt) {
  l = length(datapnt)
  
  avgpnt = numeric(16)
  
  for (i in 3:14) {
    avgpnt[i] = (datapnt[i]+datapnt[i+1])/2
  }
  
  return(avgpnt)
}