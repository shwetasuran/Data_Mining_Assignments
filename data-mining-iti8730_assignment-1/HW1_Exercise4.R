rm(list=ls())
graphics.off()

inputdata = read.csv("iris.csv")

#run the loop for the 4 column

for (n in 1:4) {
  for (m in 1:4) {
    
    if (n != m) { #both column should be different
      tabledata = table(inputdata[,n],inputdata[,n])
      cat("\nElement", colnames(inputdata)[n], "&", colnames(inputdata)[m])
      fisher_score = fisher.test(tabledata,conf.int = TRUE,conf.level = 0.95,simulate.p.value=TRUE)
      print(fisher_score)
    }
    
  }
}
