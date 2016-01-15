# Author:       Uwe Hoenig
# Course:       15D012 - Advanced Computational Methods
# Last update:  15.01.16
# Type:         Problemset 1 - Exercise 1

### Clear workspace
rm(list = ls())
##This is my script for a generating-dataset function

#############################
#############################
#Task 1

genData <- function(datapoints=100, spirals=3, sd=0.05, savecsv=TRUE, savepdf=TRUE){
  if (!require("mlbench")) install.packages("mlbench"); library(mlbench)
  data <- mlbench.spirals(n=datapoints, cycles=spirals, sd=sd)#mydata
  mydf <- cbind(as.data.frame(data$x),as.data.frame(data$classes))
  names(mydf) <- c("milky", "way", "classes")

    if(savecsv){
      write.csv(mydf, file = "dataset.csv", row.names = FALSE)
  }
  
    if(savepdf){
      pdf("dataPlot.pdf")
      plot(data, main="Milkyway", xlab="X1", ylab="X2")
      dev.off()
  }
  return(mydf)
}
#############################
#############################