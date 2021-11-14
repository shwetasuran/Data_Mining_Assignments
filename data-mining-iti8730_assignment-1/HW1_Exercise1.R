# to clear and close 
rm(list=ls())

dimension <- as.integer(readline(prompt="Enter_the_Dimension"))

prompt <- "Enter the Point1 (with spaces)?"
Point1 <- as.numeric(strsplit(readline(prompt), " ")[[1]])

prompt <- "Enter the Point2 (with spaces)?"
Point2 <- as.numeric(strsplit(readline(prompt), " ")[[1]])

chc <- as.integer(readline
                  (prompt = "Select the distance_function (1. Minkowski_distance_funtion 2. Canberra_distance_funtion 3. Mahalanobis_distance funtion)-"))


distance = as.numeric(0)


if (chc == 1) {
  
  print("Compute the Minkowski_distance_funtion")
  pvalue <- as.integer(readline(prompt="pvalue: "))
  
  for (j in 1:dimension) {
    distance = distance + (abs(Point1[j] - Point2[j])^pvalue)
  }
  
  distance = distance^(1/pvalue)
  cat("Distance = ", distance)
  
}else if (chc == 2) {
  
    print("Compute the Canberra_distance_funtion")
    
    for (j in 1:dimension) {
      distance = distance +
        (abs(Point1[j] - Point2[j])/
           (abs(Point1[j]) + abs(Point2[j])))
    }
    
  cat("Distance = ", distance)
  
}else if (chc == 3) {
  distance <- mahafunc(Point1, Point2)
  cat("Distance = ", distance)
} else {
  print("Invalid choice!")
}

