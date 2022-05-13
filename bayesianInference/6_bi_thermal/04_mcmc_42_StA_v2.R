library(ggplot2)
library(bayesplot)
library(readxl)
library(RSclient)

start_time <- Sys.time()

df <- read_excel("IndoorMeasurements_0523_0604.xlsx") #file has data up to 1152 [1:312,1]
df_to <- read_excel("outdoorTempEVcase01.xlsx")[1:312,1] 
betaMLR <- read.csv("coe_r2_param_3.csv")
lgb<-list(betaMLR$interception,betaMLR$SR1,betaMLR$SR2,betaMLR$SR4,betaMLR$SR5,betaMLR$INF,betaMLR$WT,betaMLR$RT)
lgu<-list("SR1"=c(0.4,0.9),"SR2"=c(0.4,0.9),"SR4"=c(0.4,0.9),
          "SR5"=c(0.4,0.9),"INF"=c(0.15,0.4),"WT"=c(0.054,0.095), "RT"=c(0.103,0.148)) #,
         # "IWG"=c(0.2,0.3),"PTM"=c(150,350),"RTM"=c(150,350),
         # "WTM"=c(150,350))
lgs<-list("mcsigma"=c(0,0.1))

seed<-2020

namea<-c("SR1","SR2","SR4","SR5","INF","WT", "RT", "sigma" )

lnk <- RS.connect(host="dlhou12.tongjilab.net",port=7010)
RS.login(lnk,"rlabu00","XQeHu3ahMO6L",authkey=RS.authkey(lnk))
RS.assign(lnk,"df",df,wait=TRUE)
RS.assign(lnk,"df_to",df_to,wait=TRUE)
RS.assign(lnk,"lgb",lgb,wait=TRUE)
RS.assign(lnk,"lgu",lgu,wait=TRUE)
RS.assign(lnk,"lgs",lgs,wait=TRUE)
RS.assign(lnk,"seed",seed,wait=TRUE)
RS.eval(lnk,library("DLHToolbox"),wait=TRUE)
res<-RS.eval(lnk,ga("mcobs",df$Average-df_to),wait=TRUE)
res<-RS.eval(lnk,gb(lgb),wait=TRUE)
res<-RS.eval(lnk,gu(lgu),wait=TRUE)
res<-RS.eval(lnk,gs(lgs),wait=TRUE)


res<-RS.eval(lnk,mca<-c(g_beta_0 + g_beta_1*SR1 + g_beta_2*SR2 + g_beta_3*SR4
                        + g_beta_4*SR5+ g_beta_5*INF + g_beta_6*WT +g_beta_7*RT),wait=TRUE)
res<-RS.eval(lnk,mcb<-c(SR1, SR2, SR4, SR5, INF, WT, RT, mcsigma),wait=TRUE)


res<-RS.eval(lnk,mc(mca,mcobs,mcsigma,mcb,seed),wait=TRUE)
RS.close(lnk)

a<-res["a"][[1]]
mat<-res["mat"][[1]]
draws<-res["draws"][[1]]
print(res["sur"][[1]])
end_time <- Sys.time()
end_time - start_time
write.csv(a, file = 'draws_4.csv')
save(draws, file = 'draw_4.RData')

names(mat) <- namea
ggplot(mat, aes(x=SR1)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")
ggplot(mat, aes(x=SR2)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")
ggplot(mat, aes(x=SR3)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")
ggplot(mat, aes(x=SR4)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")
ggplot(mat, aes(x=SR5)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")
ggplot(mat, aes(x=INF)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")
ggplot(mat, aes(x=WT)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")
ggplot(mat, aes(x=RT)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")
ggplot(mat, aes(x=sigma)) + geom_density(alpha=.5, fill="blue")+scale_color_brewer(palette = "Set1")


