library(QuantPsyc)
library(randomForest)
library(plyr)


df_1 <- read.csv("df_1.csv")

#mlrMonthly <- lm(df_1$Energy/10**6 ~ ., data = df_1)
#summary(mlrMonthly)
#src <- data.frame (lm.beta(mlrMonthly))
#tValue <- data.frame(summary(mlrMonthly)[["coefficients"]][, "t value"][2:(paraNum+1)])
#set.seed(2020)
#forest <- randomForest(df_1$Energy/10**6 ~ ., data = df_1, 
#ntree = 500, mtry = 4, importance = TRUE)
#ranFor <- data.frame(importance(forest)[,1])

saFunc <- function(data){
  mlrMonthly <- lm(data$Energy/10**6 ~ ., data = data)
  src <- data.frame (lm.beta(mlrMonthly))
  tValue <- data.frame(summary(mlrMonthly)[["coefficients"]][, "t value"][2:ncol(data)])
  set.seed(2020)
  forest <- randomForest(data$Energy/10**6 ~ ., data = data, 
                         ntree = 500, mtry = (ncol(data)-1), importance = TRUE)
  ranFor <- data.frame(importance(forest)[,1])
  saVal <- data.frame(src, ranFor, tValue)
  
  # rename the column name
  names(saVal)[1] <- "src"
  names(saVal)[2] <- "ranF"
  names(saVal)[3] <- "tvalue"
  
  # get absolute value of each element of saVal
  saVal <- abs(saVal)
  
  # calculate percentage
  saVal$Pct_src <- saVal$src / sum(saVal$src)*100
  saVal$Pct_ranF <- saVal$ranF / sum(saVal$ranF)*100
  saVal$Pct_tvalue <- saVal$tvalue / sum(saVal$tvalue)*100
  
  # calculate SVI
  saVal$svi <- (saVal$Pct_src + saVal$Pct_ranF + saVal$Pct_tvalue)/3
  
  # calculate rank
  saVal$rank[order(saVal$svi, decreasing = TRUE)] <- 1:nrow(saVal)
  
  return(saVal)
}

sa_1 <- saFunc(df_1)

# Sort by rank
sa_1_sort <-saVal[order(sa_1$rank),]

write.csv(sa_1, file = "SA_outputs.csv")
write.csv(sa_1_sort, file = "SA_outputs_sort.csv")