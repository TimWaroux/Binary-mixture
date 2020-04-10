library(rstan)
library(readxl)
library(evd)

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

model = stan_model('gumbel.stan')

# pass data to stan and run model

fit = sampling(model, list(N = 117, y = num, n_groups=2), iter = 9000, chains = 4,
               control = list(adapt_delta = 0.9999, max_treedepth = 999)) # maybe chnage the parameters

# stuff

fit_summary <- summary(fit)
summ = fit_summary$summary
meanz = summ[,"mean"]

print(meanz)

# plots

ggplot(log, aes(x = ODc2)) +
  theme_bw() +
  geom_histogram(binwidth = 0.05, fill = "grey", size = 1) +
  stat_function(n = 10000, fun = function(x) (meanz[5]*dgumbel(-x,meanz[1],meanz[3]))*117*0.05, size = 1) +
  stat_function(n = 10000, fun = function(x) (meazn[6]*dgumbel(-x,meanz[2],meanz[4]))*117*0.05, size = 1) +
  xlim(-2,0.5)

ggplot(log, aes(x = ODc2)) +
  theme_bw() +
  geom_histogram(binwidth = 0.05, size = 1, fill = "grey") +
  stat_function(n = 10000, fun = function(x) (meanz[5]*dgumbel(-x,meanz[1],meanz[3])+meanz[6]*dgumbel(-x,meanz[2],meanz[4]))*117*0.05, size = 1) +
  xlim(-2,0.5)

# likelihood

loglik = sum(log(meanz[5]*dgumbel(-num,meanz[1],meanz[3])+meanz[6]*dgumbel(-num,meanz[2],meanz[4])))
