library(rstan)
library(readxl)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

# read data

setwd('C:/Users/Olov/Documents/uu/math in industry/fini files') # change to your working directory

data = read_excel('ToxoplasmaSheep_4 tests.xls', range = 'C2:C119')
colnames(data) <- c('ODc2')
sub = subset(data, ODc2 > 0)
log = log10(sub)
num = log$ODc2

# compile model

model = stan_model('gm.stan')

# pass data to stan and run model

fit2 = sampling(model, list(N = 117, y = num, n_groups = 2), iter = 9000, chains = 4,
               control = list(adapt_delta = 0.9999, max_treedepth = 999)) # maybe change the parameters

# stuff

fit_summary2 <- summary(fit2)
summ2 = fit_summary2$summary
meanz2 = summ2[,"mean"]

print(meanz2)

# pass data to stan and run model

fit3 = sampling(model, list(N = 117, y = num, n_groups = 3), iter = 9000, chains = 4,
                control=list(adapt_delta = 0.9999, max_treedepth = 999)) # maybe change the parameters

# stuff

fit_summary3 <- summary(fit3)
summ3 = fit_summary3$summary
meanz3 = summ3[,"mean"]

print(meanz3)

# plots

ggplot(log, aes(x = ODc2)) +
  theme_bw() +
  geom_histogram(binwidth = 0.05, fill = "grey", size = 1) +
  stat_function(n = 10000, fun = function(x) (meanz2[5]*dnorm(x,meanz2[1],meanz2[3]))*117*0.05, size = 1) +
  stat_function(n = 10000, fun = function(x) (meanz2[6]*dnorm(x,meanz2[2],meanz2[4]))*117*0.05, size = 1) +
  xlim(-2,0.5) +
  xlab('corr OD')

ggplot(log, aes(x = ODc2)) +
  theme_bw() +
  geom_histogram(binwidth = 0.05, size = 1, fill = "grey") +
  stat_function(n = 10000, fun = function(x) (meanz2[5]*dnorm(x,meanz2[1],meanz2[3])+meanz2[6]*dnorm(x,meanz2[2],meanz2[4]))*117*0.05, size = 1) +
  xlim(-2,0.5) +
  xlab('corr OD')

ggplot(log, aes(x = ODc2)) +
  theme_bw() +
  geom_histogram(binwidth = 0.05, fill = "grey", size = 1) +
  stat_function(n = 10000, fun = function(x) (meanz3[7]*dnorm(x,meanz3[1],meanz3[4]))*117*0.05, size = 1) +
  stat_function(n = 10000, fun = function(x) (meanz3[8]*dnorm(x,meanz3[2],meanz3[5]))*117*0.05, size = 1) +
  stat_function(n = 10000, fun = function(x) (meanz3[9]*dnorm(x,meanz3[3],meanz3[6]))*117*0.05, size = 1) +
  xlim(-2,0.5) +
  xlab('corr OD')

ggplot(log, aes(x = ODc2)) +
  theme_bw() +
  geom_histogram(binwidth = 0.05, size = 1, fill = "grey") +
  stat_function(n = 10000, fun = function(x) (meanz3[7]*dnorm(x,meanz3[1],meanz3[4])+meanz3[8]*dnorm(x,meanz3[2],meanz3[5])+meanz3[9]*dnorm(x,meanz3[3],meanz3[6]))*117*0.05, size = 1) +
  xlim(-2,0.5) +
  xlab('corr OD')

# likelihoods

loglik2 = sum(log(meanz2[5]*dnorm(num,meanz2[1],meanz2[3])+meanz2[6]*dnorm(num,meanz2[2],meanz2[4])))
loglik3 = sum(log(meanz3[7]*dnorm(num,meanz3[1],meanz3[4])+meanz3[8]*dnorm(num,meanz3[2],meanz3[5])+meanz3[9]*dnorm(num,meanz3[3],meanz3[6])))

prior2on3 = exp(loglik3-loglik2)
