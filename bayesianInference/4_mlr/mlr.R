library(caret)
library(plyr)

buildArea <- 46320.38 # unit: m2
#load("~/R/districtCooling/doha_referenceBuild_Bayesian/totaData.RData")
totaData <- rename(totaData, c("elecData.elecJan...gasData.gasJan"="Jan", "elecData.elecFeb...gasData.gasFeb"="Feb",
                   "elecData.elecMar...gasData.gasMar"="Mar", "elecData.elecApr...gasData.gasApr"="Apr",
                   "elecData.elecMay...gasData.gasMay"="May", "elecData.elecJun...gasData.gasJun"="Jun",
                   "elecData.elecJul...gasData.gasJul"="Jul", "elecData.elecAug...gasData.gasAug"="Aug",
                   "elecData.elecSep...gasData.gasSep"="Sep", "elecData.elecOct...gasData.gasOct"="Oct",
                   "elecData.elecNov...gasData.gasNov"="Nov", "elecData.elecDec...gasData.gasDec"="Dec"))
# select train data set
trainData <- data.frame(totaData[c(1:400), c(1:12)], input_revise[c(1:400), c(1:14)])
# select validation data set
valData <- data.frame(totaData[c(401:700), c(1:12)], input_revise[c(401:700), c(1:14)])

# only use one parameter for mlr
lmFun_1 <- function(targetData, p1){
  a <- lm(targetData ~ p1)
  return(a)
}
mlr_1_1 <- lmFun_1(trainData$Jan/10**6/buildArea, trainData$EPD)
mlr_1_2 <- lmFun_1(trainData$Feb/10**6/buildArea, trainData$EPD)
mlr_1_3 <- lmFun_1(trainData$Mar/10**6/buildArea, trainData$EPD)
mlr_1_4 <- lmFun_1(trainData$Apr/10**6/buildArea, trainData$EPD)
mlr_1_5 <- lmFun_1(trainData$May/10**6/buildArea, trainData$EPD)
mlr_1_6 <- lmFun_1(trainData$Jun/10**6/buildArea, trainData$EPD)
mlr_1_7 <- lmFun_1(trainData$Jul/10**6/buildArea, trainData$EPD)
mlr_1_8 <- lmFun_1(trainData$Aug/10**6/buildArea, trainData$EPD)
mlr_1_9 <- lmFun_1(trainData$Sep/10**6/buildArea, trainData$EPD)
mlr_1_10 <- lmFun_1(trainData$Oct/10**6/buildArea, trainData$EPD)
mlr_1_11 <- lmFun_1(trainData$Nov/10**6/buildArea, trainData$EPD)
mlr_1_12 <- lmFun_1(trainData$Dec/10**6/buildArea, trainData$EPD)


r2_train_1 <- data.frame(summary(mlr_1_1)$r.squared, summary(mlr_1_2)$r.squared, summary(mlr_1_3)$r.squared, summary(mlr_1_4)$r.squared, 
                         summary(mlr_1_5)$r.squared, summary(mlr_1_6)$r.squared, summary(mlr_1_7)$r.squared, summary(mlr_1_8)$r.squared, 
                         summary(mlr_1_9)$r.squared, summary(mlr_1_10)$r.squared, summary(mlr_1_11)$r.squared, summary(mlr_1_12)$r.squared )

# use two parameter for mlr
lmFun_2 <- function(targetData, p1, p2){
  a <- lm(targetData ~ p1+p2)
  return(a)
}

mlr_2_1 <- lmFun_2(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_2 <- lmFun_2(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_3 <- lmFun_2(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_4 <- lmFun_2(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_5 <- lmFun_2(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_6 <- lmFun_2(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_7 <- lmFun_2(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_8 <- lmFun_2(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_9 <- lmFun_2(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_10 <- lmFun_2(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_11 <- lmFun_2(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP)
mlr_2_12 <- lmFun_2(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP)


r2_train_2 <- data.frame(summary(mlr_2_1)$r.squared, summary(mlr_2_2)$r.squared, summary(mlr_2_3)$r.squared, summary(mlr_2_4)$r.squared, 
                         summary(mlr_2_5)$r.squared, summary(mlr_2_6)$r.squared, summary(mlr_2_7)$r.squared, summary(mlr_2_8)$r.squared, 
                         summary(mlr_2_9)$r.squared, summary(mlr_2_10)$r.squared, summary(mlr_2_11)$r.squared, summary(mlr_2_12)$r.squared )

# use three parameter for mlr
lmFun_3 <- function(targetData, p1, p2, p3){
  a <- lm(targetData ~ p1+p2+p3)
  return(a)
}
mlr_3_1 <- lmFun_3(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_2 <- lmFun_3(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_3 <- lmFun_3(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_4 <- lmFun_3(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_5 <- lmFun_3(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_6 <- lmFun_3(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_7 <- lmFun_3(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_8 <- lmFun_3(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_9 <- lmFun_3(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_10 <- lmFun_3(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_11 <- lmFun_3(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)
mlr_3_12 <- lmFun_3(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD)

r2_train_3 <- data.frame(summary(mlr_3_1)$r.squared, summary(mlr_3_2)$r.squared, summary(mlr_3_3)$r.squared, summary(mlr_3_4)$r.squared, 
                         summary(mlr_3_5)$r.squared, summary(mlr_3_6)$r.squared, summary(mlr_3_7)$r.squared, summary(mlr_3_8)$r.squared, 
                         summary(mlr_3_9)$r.squared, summary(mlr_3_10)$r.squared, summary(mlr_3_11)$r.squared, summary(mlr_3_12)$r.squared )

# use four parameter for mlr
lmFun_4 <- function(targetData, p1, p2, p3, p4){
  a <- lm(targetData ~ p1+p2+p3+p4)
  return(a)
}
mlr_4_1 <- lmFun_4(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_2 <- lmFun_4(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_3 <- lmFun_4(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_4 <- lmFun_4(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_5 <- lmFun_4(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_6 <- lmFun_4(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_7 <- lmFun_4(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_8 <- lmFun_4(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_9 <- lmFun_4(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_10 <- lmFun_4(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_11 <- lmFun_4(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)
mlr_4_12 <- lmFun_4(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF)

r2_train_4 <- data.frame(summary(mlr_4_1)$r.squared, summary(mlr_4_2)$r.squared, summary(mlr_4_3)$r.squared, summary(mlr_4_4)$r.squared, 
                         summary(mlr_4_5)$r.squared, summary(mlr_4_6)$r.squared, summary(mlr_4_7)$r.squared, summary(mlr_4_8)$r.squared, 
                         summary(mlr_4_9)$r.squared, summary(mlr_4_10)$r.squared, summary(mlr_4_11)$r.squared, summary(mlr_4_12)$r.squared )

# use fIVE parameter for mlr
lmFun_5 <- function(targetData, p1, p2, p3, p4, p5){
  a <- lm(targetData ~ p1+p2+p3+p4+p5)
  return(a)
}
mlr_5_1 <- lmFun_5(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_2 <- lmFun_5(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_3 <- lmFun_5(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_4 <- lmFun_5(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_5 <- lmFun_5(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_6 <- lmFun_5(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_7 <- lmFun_5(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_8 <- lmFun_5(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_9 <- lmFun_5(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_10 <- lmFun_5(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_11 <- lmFun_5(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)
mlr_5_12 <- lmFun_5(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP)

r2_train_5 <- data.frame(summary(mlr_5_1)$r.squared, summary(mlr_5_2)$r.squared, summary(mlr_5_3)$r.squared, summary(mlr_5_4)$r.squared, 
                         summary(mlr_5_5)$r.squared, summary(mlr_5_6)$r.squared, summary(mlr_5_7)$r.squared, summary(mlr_5_8)$r.squared, 
                         summary(mlr_5_9)$r.squared, summary(mlr_5_10)$r.squared, summary(mlr_5_11)$r.squared, summary(mlr_5_12)$r.squared )

# use six parameter for mlr
lmFun_6 <- function(targetData, p1, p2, p3, p4, p5, p6){
  a <- lm(targetData ~ p1+p2+p3+p4+p5+p6)
  return(a)
}
mlr_6_1 <- lmFun_6(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_2 <- lmFun_6(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_3 <- lmFun_6(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_4 <- lmFun_6(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_5 <- lmFun_6(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_6 <- lmFun_6(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_7 <- lmFun_6(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_8 <- lmFun_6(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_9 <- lmFun_6(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_10 <- lmFun_6(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_11 <- lmFun_6(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)
mlr_6_12 <- lmFun_6(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC)

r2_train_6 <- data.frame(summary(mlr_6_1)$r.squared, summary(mlr_6_2)$r.squared, summary(mlr_6_3)$r.squared, summary(mlr_6_4)$r.squared, 
                         summary(mlr_6_5)$r.squared, summary(mlr_6_6)$r.squared, summary(mlr_6_7)$r.squared, summary(mlr_6_8)$r.squared, 
                         summary(mlr_6_9)$r.squared, summary(mlr_6_10)$r.squared, summary(mlr_6_11)$r.squared, summary(mlr_6_12)$r.squared )

# use sEVEN parameter for mlr
lmFun_7 <- function(targetData, p1, p2, p3, p4, p5, p6, p7){
  a <- lm(targetData ~ p1+p2+p3+p4+p5+p6+p7)
  return(a)
}
mlr_7_1 <- lmFun_7(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_2 <- lmFun_7(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_3 <- lmFun_7(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_4 <- lmFun_7(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_5 <- lmFun_7(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_6 <- lmFun_7(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_7 <- lmFun_7(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_8 <- lmFun_7(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_9 <- lmFun_7(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_10 <- lmFun_7(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_11 <- lmFun_7(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)
mlr_7_12 <- lmFun_7(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC)

r2_train_7 <- data.frame(summary(mlr_7_1)$r.squared, summary(mlr_7_2)$r.squared, summary(mlr_7_3)$r.squared, summary(mlr_7_4)$r.squared, 
                         summary(mlr_7_5)$r.squared, summary(mlr_7_6)$r.squared, summary(mlr_7_7)$r.squared, summary(mlr_7_8)$r.squared, 
                         summary(mlr_7_9)$r.squared, summary(mlr_7_10)$r.squared, summary(mlr_7_11)$r.squared, summary(mlr_7_12)$r.squared )

# use eight parameter for mlr
lmFun_8 <- function(targetData, p1, p2, p3, p4, p5, p6, p7,p8){
  a <- lm(targetData ~ p1+p2+p3+p4+p5+p6+p7+p8)
  return(a)
}
mlr_8_1 <- lmFun_8(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_2 <- lmFun_8(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_3 <- lmFun_8(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_4 <- lmFun_8(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_5 <- lmFun_8(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_6 <- lmFun_8(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_7 <- lmFun_8(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_8 <- lmFun_8(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_9 <- lmFun_8(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_10 <- lmFun_8(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_11 <-lmFun_8(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)
mlr_8_12 <- lmFun_8(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN)

r2_train_8 <- data.frame(summary(mlr_8_1)$r.squared, summary(mlr_8_2)$r.squared, summary(mlr_8_3)$r.squared, summary(mlr_8_4)$r.squared, 
                         summary(mlr_8_5)$r.squared, summary(mlr_8_6)$r.squared, summary(mlr_8_7)$r.squared, summary(mlr_8_8)$r.squared, 
                         summary(mlr_8_9)$r.squared, summary(mlr_8_10)$r.squared, summary(mlr_8_11)$r.squared, summary(mlr_8_12)$r.squared )

# use nine parameter for mlr
lmFun_9 <- function(targetData, p1, p2, p3, p4, p5, p6, p7,p8,p9){
  a <- lm(targetData ~ p1+p2+p3+p4+p5+p6+p7+p8+p9)
  return(a)
}
mlr_9_1 <- lmFun_9(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_2 <- lmFun_9(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_3 <- lmFun_9(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_4 <- lmFun_9(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_5 <- lmFun_9(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_6 <- lmFun_9(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_7 <- lmFun_9(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_8 <- lmFun_9(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_9 <- lmFun_9(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_10 <- lmFun_9(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_11 <- lmFun_9(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)
mlr_9_12 <- lmFun_9(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1)

r2_train_9 <- data.frame(summary(mlr_9_1)$r.squared, summary(mlr_9_2)$r.squared, summary(mlr_9_3)$r.squared, summary(mlr_9_4)$r.squared, 
                         summary(mlr_9_5)$r.squared, summary(mlr_9_6)$r.squared, summary(mlr_9_7)$r.squared, summary(mlr_9_8)$r.squared, 
                         summary(mlr_9_9)$r.squared, summary(mlr_9_10)$r.squared, summary(mlr_9_11)$r.squared, summary(mlr_9_12)$r.squared )


# use TEN parameter for mlr
lmFun_10 <- function(targetData, p1, p2, p3, p4, p5, p6, p7,p8,p9, p10){
  a <- lm(targetData ~ p1+p2+p3+p4+p5+p6+p7+p8+p9+p10)
  return(a)
}
mlr_10_1 <- lmFun_10(trainData$Jan/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_2 <- lmFun_10(trainData$Feb/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_3 <- lmFun_10(trainData$Mar/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_4 <- lmFun_10(trainData$Apr/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_5 <- lmFun_10(trainData$May/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_6 <- lmFun_10(trainData$Jun/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_7 <- lmFun_10(trainData$Jul/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_8 <- lmFun_10(trainData$Aug/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_9 <- lmFun_10(trainData$Sep/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_10 <- lmFun_10(trainData$Oct/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_11 <- lmFun_10(trainData$Nov/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)
mlr_10_12 <- lmFun_10(trainData$Dec/10**6/buildArea, trainData$EPD, trainData$COP, trainData$LPD, trainData$INF, trainData$CSP, trainData$SHGC, trainData$OCC, trainData$VEN, trainData$WALU.0.1, trainData$ROFU.0.1)

r2_train_10 <- data.frame(summary(mlr_10_1)$r.squared, summary(mlr_10_2)$r.squared, summary(mlr_10_3)$r.squared, summary(mlr_10_4)$r.squared, 
                         summary(mlr_10_5)$r.squared, summary(mlr_10_6)$r.squared, summary(mlr_10_7)$r.squared, summary(mlr_10_8)$r.squared, 
                         summary(mlr_10_9)$r.squared, summary(mlr_10_10)$r.squared, summary(mlr_10_11)$r.squared, summary(mlr_10_12)$r.squared)

# calculate validate dataset r2
# one parameter
model_1_1 <- coef(mlr_1_1)[1] + coef(mlr_1_1)[2]*valData$EPD
model_1_2 <- coef(mlr_1_2)[1] + coef(mlr_1_2)[2]*valData$EPD
model_1_3 <- coef(mlr_1_3)[1] + coef(mlr_1_3)[2]*valData$EPD
model_1_4 <- coef(mlr_1_4)[1] + coef(mlr_1_4)[2]*valData$EPD
model_1_5 <- coef(mlr_1_5)[1] + coef(mlr_1_5)[2]*valData$EPD
model_1_6 <- coef(mlr_1_6)[1] + coef(mlr_1_6)[2]*valData$EPD
model_1_7 <- coef(mlr_1_7)[1] + coef(mlr_1_7)[2]*valData$EPD
model_1_8 <- coef(mlr_1_8)[1] + coef(mlr_1_8)[2]*valData$EPD
model_1_9 <- coef(mlr_1_9)[1] + coef(mlr_1_9)[2]*valData$EPD
model_1_10 <- coef(mlr_1_10)[1] + coef(mlr_1_10)[2]*valData$EPD
model_1_11 <- coef(mlr_1_11)[1] + coef(mlr_1_11)[2]*valData$EPD
model_1_12 <- coef(mlr_1_12)[1] + coef(mlr_1_12)[2]*valData$EPD
r2_valid_1_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_1_1)[2]
r2_valid_1_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_1_2)[2]
r2_valid_1_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_1_3)[2]
r2_valid_1_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_1_4)[2]
r2_valid_1_5 <- caret::postResample(valData$May/10**6/buildArea, model_1_5)[2]
r2_valid_1_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_1_6)[2]
r2_valid_1_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_1_7)[2]
r2_valid_1_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_1_8)[2]
r2_valid_1_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_1_9)[2]
r2_valid_1_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_1_10)[2]
r2_valid_1_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_1_11)[2]
r2_valid_1_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_1_12)[2]
r2_valid_1 <- data.frame(r2_valid_1_1, r2_valid_1_2, r2_valid_1_3, r2_valid_1_4, r2_valid_1_5, r2_valid_1_6,
                         r2_valid_1_7, r2_valid_1_8, r2_valid_1_9, r2_valid_1_10, r2_valid_1_11, r2_valid_1_12)

# two parameter
mlr2 <- function(coef, p1,p2 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2
  return(a)
}
model_2_1 <- mlr2(coef(mlr_2_1), valData$EPD, valData$COP)
model_2_2 <- mlr2(coef(mlr_2_2), valData$EPD, valData$COP)
model_2_3 <- mlr2(coef(mlr_2_3), valData$EPD, valData$COP)
model_2_4 <- mlr2(coef(mlr_2_4), valData$EPD, valData$COP)
model_2_5 <- mlr2(coef(mlr_2_5), valData$EPD, valData$COP)
model_2_6 <- mlr2(coef(mlr_2_6), valData$EPD, valData$COP)
model_2_7 <- mlr2(coef(mlr_2_7), valData$EPD, valData$COP)
model_2_8 <- mlr2(coef(mlr_2_8), valData$EPD, valData$COP)
model_2_9 <- mlr2(coef(mlr_2_9), valData$EPD, valData$COP)
model_2_10 <- mlr2(coef(mlr_2_10), valData$EPD, valData$COP)
model_2_11 <- mlr2(coef(mlr_2_11), valData$EPD, valData$COP)
model_2_12 <-mlr2(coef(mlr_2_12), valData$EPD, valData$COP)
r2_valid_2_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_2_1)[2]
r2_valid_2_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_2_2)[2]
r2_valid_2_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_2_3)[2]
r2_valid_2_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_2_4)[2]
r2_valid_2_5 <- caret::postResample(valData$May/10**6/buildArea, model_2_5)[2]
r2_valid_2_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_2_6)[2]
r2_valid_2_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_2_7)[2]
r2_valid_2_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_2_8)[2]
r2_valid_2_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_2_9)[2]
r2_valid_2_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_2_10)[2]
r2_valid_2_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_2_11)[2]
r2_valid_2_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_2_12)[2]
r2_valid_2 <- data.frame(r2_valid_2_1, r2_valid_2_2, r2_valid_2_3, r2_valid_2_4, r2_valid_2_5, r2_valid_2_6,
                         r2_valid_2_7, r2_valid_2_8, r2_valid_2_9, r2_valid_2_10, r2_valid_2_11, r2_valid_2_12)

# THREE parameter
mlr3 <- function(coef, p1,p2, p3 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2 +coef[4]*p3
  return(a)
}

model_3_1 <- mlr3(coef(mlr_3_1), valData$EPD, valData$COP, valData$LPD)
model_3_2 <- mlr3(coef(mlr_3_2), valData$EPD, valData$COP, valData$LPD)
model_3_3 <- mlr3(coef(mlr_3_3), valData$EPD, valData$COP, valData$LPD)
model_3_4 <- mlr3(coef(mlr_3_4), valData$EPD, valData$COP, valData$LPD)
model_3_5 <- mlr3(coef(mlr_3_5), valData$EPD, valData$COP, valData$LPD)
model_3_6 <- mlr3(coef(mlr_3_6), valData$EPD, valData$COP, valData$LPD)
model_3_7 <- mlr3(coef(mlr_3_7), valData$EPD, valData$COP, valData$LPD)
model_3_8 <- mlr3(coef(mlr_3_8), valData$EPD, valData$COP, valData$LPD)
model_3_9 <- mlr3(coef(mlr_3_9), valData$EPD, valData$COP, valData$LPD)
model_3_10 <- mlr3(coef(mlr_3_10), valData$EPD, valData$COP, valData$LPD)
model_3_11 <- mlr3(coef(mlr_3_11), valData$EPD, valData$COP, valData$LPD)
model_3_12 <- mlr3(coef(mlr_3_12), valData$EPD, valData$COP, valData$LPD)
r2_valid_3_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_3_1)[2]
r2_valid_3_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_3_2)[2]
r2_valid_3_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_3_3)[2]
r2_valid_3_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_3_4)[2]
r2_valid_3_5 <- caret::postResample(valData$May/10**6/buildArea, model_3_5)[2]
r2_valid_3_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_3_6)[2]
r2_valid_3_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_3_7)[2]
r2_valid_3_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_3_8)[2]
r2_valid_3_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_3_9)[2]
r2_valid_3_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_3_10)[2]
r2_valid_3_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_3_11)[2]
r2_valid_3_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_3_12)[2]
r2_valid_3 <- data.frame(r2_valid_3_1, r2_valid_3_2, r2_valid_3_3, r2_valid_3_4, r2_valid_3_5, r2_valid_3_6,
                         r2_valid_3_7, r2_valid_3_8, r2_valid_3_9, r2_valid_3_10, r2_valid_3_11, r2_valid_3_12)

# four parameter
mlr4 <- function(coef, p1,p2, p3,p4 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2 +coef[4]*p3+coef[5]*p4
  return(a)
}
model_4_1 <- mlr4(coef(mlr_4_1), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_2 <- mlr4(coef(mlr_4_2), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_3 <- mlr4(coef(mlr_4_3), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_4 <- mlr4(coef(mlr_4_4), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_5 <- mlr4(coef(mlr_4_5), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_6 <- mlr4(coef(mlr_4_6), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_7 <- mlr4(coef(mlr_4_7), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_8 <- mlr4(coef(mlr_4_8), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_9 <- mlr4(coef(mlr_4_9), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_10 <- mlr4(coef(mlr_4_10), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_11 <- mlr4(coef(mlr_4_11), valData$EPD, valData$COP, valData$LPD, valData$INF)
model_4_12 <- mlr4(coef(mlr_4_12), valData$EPD, valData$COP, valData$LPD, valData$INF)
r2_valid_4_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_4_1)[2]
r2_valid_4_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_4_2)[2]
r2_valid_4_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_4_3)[2]
r2_valid_4_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_4_4)[2]
r2_valid_4_5 <- caret::postResample(valData$May/10**6/buildArea, model_4_5)[2]
r2_valid_4_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_4_6)[2]
r2_valid_4_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_4_7)[2]
r2_valid_4_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_4_8)[2]
r2_valid_4_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_4_9)[2]
r2_valid_4_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_4_10)[2]
r2_valid_4_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_4_11)[2]
r2_valid_4_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_4_12)[2]
r2_valid_4 <- data.frame(r2_valid_4_1, r2_valid_4_2, r2_valid_4_3, r2_valid_4_4, r2_valid_4_5, r2_valid_4_6,
                         r2_valid_4_7, r2_valid_4_8, r2_valid_4_9, r2_valid_4_10, r2_valid_4_11, r2_valid_4_12)


# fIVE parameter
mlr5 <- function(coef, p1,p2, p3,p4,p5 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2 +coef[4]*p3+coef[5]*p4+coef[6]*p5
  return(a)
}
model_5_1 <- mlr5(coef(mlr_5_1), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_2 <-  mlr5(coef(mlr_5_2), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_3 <-  mlr5(coef(mlr_5_3), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_4 <-  mlr5(coef(mlr_5_4), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_5 <-  mlr5(coef(mlr_5_5), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_6 <-  mlr5(coef(mlr_5_6), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_7 <-  mlr5(coef(mlr_5_7), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_8 <-  mlr5(coef(mlr_5_8), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_9 <-  mlr5(coef(mlr_5_9), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_10 <-  mlr5(coef(mlr_5_10), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_11 <-  mlr5(coef(mlr_5_11), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
model_5_12 <-  mlr5(coef(mlr_5_12), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP)
r2_valid_5_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_5_1)[2]
r2_valid_5_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_5_2)[2]
r2_valid_5_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_5_3)[2]
r2_valid_5_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_5_4)[2]
r2_valid_5_5 <- caret::postResample(valData$May/10**6/buildArea, model_5_5)[2]
r2_valid_5_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_5_6)[2]
r2_valid_5_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_5_7)[2]
r2_valid_5_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_5_8)[2]
r2_valid_5_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_5_9)[2]
r2_valid_5_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_5_10)[2]
r2_valid_5_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_5_11)[2]
r2_valid_5_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_5_12)[2]
r2_valid_5 <- data.frame(r2_valid_5_1, r2_valid_5_2, r2_valid_5_3, r2_valid_5_4, r2_valid_5_5, r2_valid_5_6,
                         r2_valid_5_7, r2_valid_5_8, r2_valid_5_9, r2_valid_5_10, r2_valid_5_11, r2_valid_5_12)


# six parameter
mlr6 <- function(coef, p1,p2, p3,p4,p5,p6 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2 +coef[4]*p3+coef[5]*p4+coef[6]*p5+coef[7]*p6
  return(a)
}
model_6_1 <- mlr6(coef(mlr_6_1), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_2 <- mlr6(coef(mlr_6_2), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_3 <- mlr6(coef(mlr_6_3), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_4 <- mlr6(coef(mlr_6_4), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_5 <- mlr6(coef(mlr_6_5), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_6 <- mlr6(coef(mlr_6_6), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_7 <- mlr6(coef(mlr_6_7), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_8 <- mlr6(coef(mlr_6_8), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_9 <- mlr6(coef(mlr_6_9), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_10 <- mlr6(coef(mlr_6_10), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_11 <- mlr6(coef(mlr_6_11), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
model_6_12 <- mlr6(coef(mlr_6_12), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC)
r2_valid_6_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_6_1)[2]
r2_valid_6_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_6_2)[2]
r2_valid_6_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_6_3)[2]
r2_valid_6_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_6_4)[2]
r2_valid_6_5 <- caret::postResample(valData$May/10**6/buildArea, model_6_5)[2]
r2_valid_6_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_6_6)[2]
r2_valid_6_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_6_7)[2]
r2_valid_6_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_6_8)[2]
r2_valid_6_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_6_9)[2]
r2_valid_6_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_6_10)[2]
r2_valid_6_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_6_11)[2]
r2_valid_6_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_6_12)[2]
r2_valid_6 <- data.frame(r2_valid_6_1, r2_valid_6_2, r2_valid_6_3, r2_valid_6_4, r2_valid_6_5, r2_valid_6_6,
                         r2_valid_6_7, r2_valid_6_8, r2_valid_6_9, r2_valid_6_10, r2_valid_6_11, r2_valid_6_12)


# seven parameter
mlr7 <- function(coef, p1,p2, p3,p4,p5,p6,p7 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2 +coef[4]*p3+coef[5]*p4+coef[6]*p5+coef[7]*p6+coef[8]*p7
  return(a)
}
model_7_1 <- mlr7(coef(mlr_7_1), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_2 <- mlr7(coef(mlr_7_2), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_3 <- mlr7(coef(mlr_7_3), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_4 <- mlr7(coef(mlr_7_4), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_5 <- mlr7(coef(mlr_7_5), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_6 <- mlr7(coef(mlr_7_6), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_7 <- mlr7(coef(mlr_7_7), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_8 <- mlr7(coef(mlr_7_8), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_9 <- mlr7(coef(mlr_7_9), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_10 <- mlr7(coef(mlr_7_10), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_11 <- mlr7(coef(mlr_7_11), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
model_7_12 <- mlr7(coef(mlr_7_12), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC)
r2_valid_7_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_7_1)[2]
r2_valid_7_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_7_2)[2]
r2_valid_7_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_7_3)[2]
r2_valid_7_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_7_4)[2]
r2_valid_7_5 <- caret::postResample(valData$May/10**6/buildArea, model_7_5)[2]
r2_valid_7_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_7_6)[2]
r2_valid_7_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_7_7)[2]
r2_valid_7_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_7_8)[2]
r2_valid_7_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_7_9)[2]
r2_valid_7_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_7_10)[2]
r2_valid_7_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_7_11)[2]
r2_valid_7_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_7_12)[2]
r2_valid_7 <- data.frame(r2_valid_7_1, r2_valid_7_2, r2_valid_7_3, r2_valid_7_4, r2_valid_7_5, r2_valid_7_6,
                         r2_valid_7_7, r2_valid_7_8, r2_valid_7_9, r2_valid_7_10, r2_valid_7_11, r2_valid_7_12)

# EIGHT parameter
mlr8 <- function(coef, p1,p2, p3,p4,p5,p6,p7,p8 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2 +coef[4]*p3+coef[5]*p4+coef[6]*p5+coef[7]*p6+coef[8]*p7+coef[9]*p8
  return(a)
}
model_8_1 <- mlr8(coef(mlr_8_1), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_2 <- mlr8(coef(mlr_8_2), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_3 <- mlr8(coef(mlr_8_3), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_4 <- mlr8(coef(mlr_8_4), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_5 <- mlr8(coef(mlr_8_5), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_6 <- mlr8(coef(mlr_8_6), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_7 <- mlr8(coef(mlr_8_7), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_8 <- mlr8(coef(mlr_8_8), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_9 <- mlr8(coef(mlr_8_9), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_10 <- mlr8(coef(mlr_8_10), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_11 <- mlr8(coef(mlr_8_11), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
model_8_12 <- mlr8(coef(mlr_8_12), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN)
r2_valid_8_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_8_1)[2]
r2_valid_8_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_8_2)[2]
r2_valid_8_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_8_3)[2]
r2_valid_8_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_8_4)[2]
r2_valid_8_5 <- caret::postResample(valData$May/10**6/buildArea, model_8_5)[2]
r2_valid_8_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_8_6)[2]
r2_valid_8_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_8_7)[2]
r2_valid_8_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_8_8)[2]
r2_valid_8_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_8_9)[2]
r2_valid_8_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_8_10)[2]
r2_valid_8_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_8_11)[2]
r2_valid_8_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_8_12)[2]
r2_valid_8 <- data.frame(r2_valid_8_1, r2_valid_8_2, r2_valid_8_3, r2_valid_8_4, r2_valid_8_5, r2_valid_8_6,
                         r2_valid_8_7, r2_valid_8_8, r2_valid_8_9, r2_valid_8_10, r2_valid_8_11, r2_valid_8_12)


# NINE parameter
mlr9 <- function(coef, p1,p2, p3,p4,p5,p6,p7,p8,p9 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2 +coef[4]*p3+coef[5]*p4+coef[6]*p5+coef[7]*p6+coef[8]*p7+coef[9]*p8+coef[10]*p9
  return(a)
}
model_9_1 <- mlr9(coef(mlr_9_1), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_2 <- mlr9(coef(mlr_9_2), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_3 <- mlr9(coef(mlr_9_3), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_4 <- mlr9(coef(mlr_9_4), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_5 <- mlr9(coef(mlr_9_5), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_6 <- mlr9(coef(mlr_9_6), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_7 <- mlr9(coef(mlr_9_7), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_8 <- mlr9(coef(mlr_9_8), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_9 <- mlr9(coef(mlr_9_9), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_10 <- mlr9(coef(mlr_9_10), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_11 <- mlr9(coef(mlr_9_11), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
model_9_12 <- mlr9(coef(mlr_9_12), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1)
r2_valid_9_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_9_1)[2]
r2_valid_9_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_9_2)[2]
r2_valid_9_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_9_3)[2]
r2_valid_9_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_9_4)[2]
r2_valid_9_5 <- caret::postResample(valData$May/10**6/buildArea, model_9_5)[2]
r2_valid_9_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_9_6)[2]
r2_valid_9_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_9_7)[2]
r2_valid_9_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_9_8)[2]
r2_valid_9_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_9_9)[2]
r2_valid_9_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_9_10)[2]
r2_valid_9_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_9_11)[2]
r2_valid_9_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_9_12)[2]
r2_valid_9 <- data.frame(r2_valid_9_1, r2_valid_9_2, r2_valid_9_3, r2_valid_9_4, r2_valid_9_5, r2_valid_9_6,
                         r2_valid_9_7, r2_valid_9_8, r2_valid_9_9, r2_valid_9_10, r2_valid_9_11, r2_valid_9_12)


# TEN parameter
mlr10 <- function(coef, p1,p2, p3,p4,p5,p6,p7,p8,p9,p10 ){
  a <- coef[1]+ coef[2]*p1 + coef[3]*p2 +coef[4]*p3+coef[5]*p4+coef[6]*p5+coef[7]*p6+coef[8]*p7+coef[9]*p8+coef[10]*p9+coef[11]*p10
  return(a)
}
model_10_1 <- mlr10(coef(mlr_10_1), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_2 <- mlr10(coef(mlr_10_2), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_3 <- mlr10(coef(mlr_10_3), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_4 <- mlr10(coef(mlr_10_4), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_5 <- mlr10(coef(mlr_10_5), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_6 <- mlr10(coef(mlr_10_6), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_7 <- mlr10(coef(mlr_10_7), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_8 <- mlr10(coef(mlr_10_8), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_9 <- mlr10(coef(mlr_10_9), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_10 <- mlr10(coef(mlr_10_10), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_11 <- mlr10(coef(mlr_10_11), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
model_10_12 <- mlr10(coef(mlr_10_12), valData$EPD, valData$COP, valData$LPD, valData$INF, valData$CSP, valData$SHGC, valData$OCC, valData$VEN,valData$WALU.0.1, valData$ROFU.0.1)
r2_valid_10_1 <- caret::postResample(valData$Jan/10**6/buildArea, model_10_1)[2]
r2_valid_10_2 <- caret::postResample(valData$Feb/10**6/buildArea, model_10_2)[2]
r2_valid_10_3 <- caret::postResample(valData$Mar/10**6/buildArea, model_10_3)[2]
r2_valid_10_4 <- caret::postResample(valData$Apr/10**6/buildArea, model_10_4)[2]
r2_valid_10_5 <- caret::postResample(valData$May/10**6/buildArea, model_10_5)[2]
r2_valid_10_6 <- caret::postResample(valData$Jun/10**6/buildArea, model_10_6)[2]
r2_valid_10_7 <- caret::postResample(valData$Jul/10**6/buildArea, model_10_7)[2]
r2_valid_10_8 <- caret::postResample(valData$Aug/10**6/buildArea, model_10_8)[2]
r2_valid_10_9 <- caret::postResample(valData$Sep/10**6/buildArea, model_10_9)[2]
r2_valid_10_10 <- caret::postResample(valData$Oct/10**6/buildArea, model_10_10)[2]
r2_valid_10_11 <- caret::postResample(valData$Nov/10**6/buildArea, model_10_11)[2]
r2_valid_10_12 <- caret::postResample(valData$Dec/10**6/buildArea, model_10_12)[2]
r2_valid_10 <- data.frame(r2_valid_10_1, r2_valid_10_2, r2_valid_10_3, r2_valid_10_4, r2_valid_10_5, r2_valid_10_6,
                         r2_valid_10_7, r2_valid_10_8, r2_valid_10_9, r2_valid_10_10, r2_valid_10_11, r2_valid_10_12)


mlr_1 <- data.frame(coef(mlr_1_1), coef(mlr_1_2), coef(mlr_1_3), coef(mlr_1_4), coef(mlr_1_5), coef(mlr_1_6), 
                    coef(mlr_1_7),coef(mlr_1_8), coef(mlr_1_9), coef(mlr_1_10), coef(mlr_1_11), coef(mlr_1_12))
save(mlr_1, file = 'mlr_1.RData')

mlr_2 <- data.frame(coef(mlr_2_1), coef(mlr_2_2), coef(mlr_2_3), coef(mlr_2_4), coef(mlr_2_5), coef(mlr_2_6), 
                    coef(mlr_2_7),coef(mlr_2_8), coef(mlr_2_9), coef(mlr_2_10), coef(mlr_2_11), coef(mlr_2_12))
save(mlr_2, file = 'mlr_2.RData')

mlr_3 <- data.frame(coef(mlr_3_1), coef(mlr_3_2), coef(mlr_3_3), coef(mlr_3_4), coef(mlr_3_5), coef(mlr_3_6), 
                    coef(mlr_3_7),coef(mlr_3_8), coef(mlr_3_9), coef(mlr_3_10), coef(mlr_3_11), coef(mlr_3_12))
save(mlr_3, file = 'mlr_3.RData')

mlr_4 <- data.frame(coef(mlr_4_1), coef(mlr_4_2), coef(mlr_4_3), coef(mlr_4_4), coef(mlr_4_5), coef(mlr_4_6), 
                    coef(mlr_4_7),coef(mlr_4_8), coef(mlr_4_9), coef(mlr_4_10), coef(mlr_4_11), coef(mlr_4_12))
save(mlr_4, file = 'mlr_4.RData')

mlr_5 <- data.frame(coef(mlr_5_1), coef(mlr_5_2), coef(mlr_5_3), coef(mlr_5_4), coef(mlr_5_5), coef(mlr_5_6), 
                    coef(mlr_5_7),coef(mlr_5_8), coef(mlr_5_9), coef(mlr_5_10), coef(mlr_5_11), coef(mlr_5_12))
save(mlr_5, file = 'mlr_5.RData')

mlr_6 <- data.frame(coef(mlr_6_1), coef(mlr_6_2), coef(mlr_6_3), coef(mlr_6_4), coef(mlr_6_5), coef(mlr_6_6),
                    coef(mlr_6_7),coef(mlr_6_8), coef(mlr_6_9), coef(mlr_6_10), coef(mlr_6_11), coef(mlr_6_12))
save(mlr_6, file = "mlr_6.RData")

mlr_7 <- data.frame(coef(mlr_7_1), coef(mlr_7_2), coef(mlr_7_3), coef(mlr_7_4), coef(mlr_7_5), coef(mlr_7_6), 
                    coef(mlr_7_7),coef(mlr_7_8), coef(mlr_7_9), coef(mlr_7_10), coef(mlr_7_11), coef(mlr_7_12))
save(mlr_7, file = "mlr_7.RData")
mlr_8 <- data.frame(coef(mlr_8_1), coef(mlr_8_2), coef(mlr_8_3), coef(mlr_8_4), coef(mlr_8_5), coef(mlr_8_6), 
                    coef(mlr_8_7),coef(mlr_8_8), coef(mlr_8_9), coef(mlr_8_10), coef(mlr_8_11), coef(mlr_8_12))
save(mlr_8, file = "mlr_8.RData")

mlr_9 <- data.frame(coef(mlr_9_1), coef(mlr_9_2), coef(mlr_9_3), coef(mlr_9_4), coef(mlr_9_5), coef(mlr_9_6), 
                    coef(mlr_9_7),coef(mlr_9_8), coef(mlr_9_9), coef(mlr_9_10), coef(mlr_9_11), coef(mlr_9_12))
save(mlr_9, file = "mlr_9.RData")

mlr_10 <- data.frame(coef(mlr_10_1), coef(mlr_10_2), coef(mlr_10_3), coef(mlr_10_4), coef(mlr_10_5), coef(mlr_10_6), 
                     coef(mlr_10_7),coef(mlr_10_8), coef(mlr_10_9), coef(mlr_10_10), coef(mlr_10_11), coef(mlr_10_12))
save(mlr_10, file = "mlr_10.RData")
