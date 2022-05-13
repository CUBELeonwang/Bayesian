# This code is for Latin hypercube sampling method
# the distribution of each parameter can be gaussian or uniform, depends on the input information

library(lhs)
library(data.table)
library(EnvStats)
library(tidyr)
library(ggplot2)
library(truncnorm)

fileName <- "lhs_inputs-NV-mechanical"
df <- read.csv(paste(fileName, ".csv", sep = "")) # read the input file
df$parameters <- NULL # drop the first column


# LHS sample
set.seed(1111)
paramNum <- ncol(df) # selected parameter number for sensitivity analysis
sampleNum <- paramNum * 55 # simulation times

x <- randomLHS(sampleNum,paramNum)
y <- x

for (i in seq(1, ncol(df), 1)) {
  if (df[4,i] == 1){
    y[, i] <- x[,i]*(df[2,i]-df[1,i]) + df[1, i]
  }else if (df[5,i] == 1 ){
    y[,i] <- qtruncnorm(x[,i], a=df[1,i], b=df[2,i], mean = (df[2,i] + df[1,i])/2, sd = (df[2,i] - df[1,i])/6)
      #qnorm(x[,i], (df[2,i] - df[1,i])/2, (df[2,i] - df[1,i])/6)
  } else{
    y[, i] <- qtri(x[,i], df[1, i], df[2,i], df[3,i])
  }
}


name_col <- colnames(df)
output <- data.frame(y)
setnames(output, old = colnames(output), new = colnames(df))

write.csv(output, file = paste(fileName, "_output_lhs.csv", sep = ""))



