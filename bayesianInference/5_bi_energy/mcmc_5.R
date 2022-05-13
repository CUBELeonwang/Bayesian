library(greta)
library(ggplot2)
library(bayesplot)
library(coda)
library(readxl)
start_time <- Sys.time()
df <- read_excel('measurements.xlsx', sheet = "meaData")
obs <- as_data(c(df$Jan, df$Feb, df$Mar, df$Apr, df$May, df$Jun, df$Jul, df$Aug, df$Sep, df$Oct, df$Nov, df$Dec))

mlr_1 <- na.omit(read.csv("mlr_coe_1 .csv"))
mlr_2 <- na.omit(read.csv("mlr_coe_2 .csv"))
mlr_3 <- na.omit(read.csv("mlr_coe_3 .csv"))
mlr_4 <- na.omit(read.csv("mlr_coe_4 .csv"))
mlr_5 <- na.omit(read.csv("mlr_coe_5 .csv"))
mlr_6 <- na.omit(read.csv("mlr_coe_6 .csv"))
mlr_7 <- na.omit(read.csv("mlr_coe_7 .csv"))
mlr_8 <- na.omit(read.csv("mlr_coe_8 .csv"))
mlr_9 <- na.omit(read.csv("mlr_coe_9 .csv"))
mlr_10 <- na.omit(read.csv("mlr_coe_10 .csv"))
mlr_11 <- na.omit(read.csv("mlr_coe_11 .csv"))
mlr_12 <- na.omit(read.csv("mlr_coe_12 .csv"))

betaMLR <- matrix(c(mlr_1$coe, 
                    mlr_2$coe, 
                    mlr_3$coe,
                    mlr_4$coe,
                    mlr_5$coe,
                    mlr_6$coe,
                    mlr_7$coe,
                    mlr_8$coe,
                    mlr_9$coe,
                    mlr_10$coe,
                    mlr_11$coe,
                    mlr_12$coe),nrow = 12, ncol = 6, byrow = TRUE)



# generate greta array
g_beta_0 <- as_data(betaMLR[,1])
g_beta_1 <- as_data(betaMLR[,2])
g_beta_2 <- as_data(betaMLR[,3])
g_beta_3 <- as_data(betaMLR[,4])
g_beta_4 <- as_data(betaMLR[,5])
g_beta_5 <- as_data(betaMLR[,6])

set.seed(2020)
# prior distribution
CSP <- uniform(21, 26)
EPD <- uniform(2, 8)
VEN <- uniform(0.0003, 0.0006)
SHGC <- uniform(0, 0.21)
LPD <- uniform(3, 6)
#sigma <- lognormal(0, 0.1)
sigma <- cauchy(0, 5);

# operation
mu <- g_beta_0 + g_beta_1 * CSP + g_beta_2 * EPD + g_beta_3 * VEN + g_beta_4 * SHGC  + g_beta_5 * LPD
dim(mu)

# likelihood
distribution(obs) <- normal(mu, sigma)

# defining the model
mod <- model(CSP, EPD, VEN, SHGC, LPD, sigma)
#str(mod,give.attr=FALSE,max.level=1)

# plotting
plot(mod)

# sampling
#draws <- mcmc(mod, warmup = 4000, n_samples = 5000)
#draws <- mcmc(mod, n_samples = 10000, warmup = 2000)
draws <- greta::mcmc(mod, warmup = 1000, n_samples = 2000)
summary(draws)
end_time <- Sys.time()
end_time - start_time
a <- as.matrix(draws)
write.csv(a, file = 'draws_5.csv')
save(draws, file = 'draw_5.RData')
gelman.diag(draws, confidence = 0.95, transform=FALSE)
gelman.plot(draws)

mcmc_trace(draws, facet_args = list(nrow = 6, ncol = 2))

mcmc_trace(draws, pars = c("CSP", "EPD", "VEN", "SHGC","LPD","sigma"),facet_args = list(nrow = 2, ncol = 2))

#mcmc_trace(draws, pars = c("COP"),window = c(1000,1050))+panel_bg(fill = "gray90", color = NA)

mcmc_intervals(draws, pars = c("COP", "VEN", "EPD","CSP", "sigma"))

mcmc_rank_overlay(draws, facet_args = list(nrow = 1, ncol = 2))
mcmc_trace_highlight(draws, pars = "sigma", highlight = 2, size = 2)
mcmc_areas( draws, pars = c("COP", "VEN", "EPD", "CSP","sigma"), prob = 0.8,point_est = "mean")

mat <- data.frame(matrix(draws[[1]],ncol=5))
names(mat) <- c("CSP","EPD", "VEN" ,"SHGC", "sigma")
ggplot(mat, aes(x=CSP)) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(mat, aes(x=CSP)) + geom_density(alpha=.5, fill="purple")+scale_color_brewer(palette = "Set1") 
ggplot(mat, aes(x=EPD)) + geom_density(alpha=.5, fill="purple")+scale_color_brewer(palette = "Set1") 
ggplot(mat, aes(x=VEN)) + geom_density(alpha=.5, fill="purple")+scale_color_brewer(palette = "Set1") 
ggplot(mat, aes(x=SHGC)) + geom_density(alpha=.5, fill="purple")+scale_color_brewer(palette = "Set1") 
ggplot(mat, aes(x=sigma)) + geom_density(alpha=.5, fill="purple")+scale_color_brewer(palette = "Set1") 

