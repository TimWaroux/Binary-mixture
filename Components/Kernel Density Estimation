## This R code presents the approach decribed within the report:

## 1. Implement GMM parameters
## 2. Create new parametric density start
## 3. Obtain optimal bandwidth 'h' through order derivative
## 4. Plug new optimal bandwidth with the startMix parametric start into kdensity
## 5. Plot

library( readxl )
library( kdensity )
library( kedd )
library( ggplot2 )
library( wesanderson )

# read data
setwd( "" )

source( "functions.r" )

data = read_excel('ELISAoverzichtjuni2019_v2.xlsx', range = cell_cols('D'))
sub = subset(data, ODc2 > 0)
log = log10(sub) ## take log of OD values
num = log$ODc2

## remove 'box' outline from plts
par(bty="n")

## colour palette
pal <- wes_palette("Rushmore", 5)

## initialize parameters from GMM
meanz= c(-1.1166050, 0.1882756, 0.5052600, 0.1431528, 0.7630198, 0.2369802)

## mixing distribution using initialized params
dmix = function(x) {
  meanz[5]*dnorm(x,meanz[1],meanz[3])+meanz[6]*dnorm(x,meanz[2],meanz[4])
}

## create mixing distribution start for kde
startMix = list(
  density = dmix,
  estimator = function(data) {
    c()
  },
  support = c(-Inf, Inf)
)

## histogram plots
ggplot(log, aes(x = ODc2)) +
  theme_bw() +
  geom_histogram(binwidth = 0.1, fill = "grey", position = "identity", size = 1) +
  stat_function(n = 10000, fun = function(x) (meanz[5]*dnorm(x,meanz[1],meanz[3]))*245*0.1, size = 1) +
  stat_function(n = 10000, fun = function(x) (meanz[6]*dnorm(x,meanz[2],meanz[4]))*245*0.1, size = 1)

ggplot(log, aes(x = ODc2)) +
  theme_bw() +
  geom_histogram(binwidth = 0.1, size = 1, fill = "grey") +
  stat_function(n = 10000, fun = function(x) (meanz[5]*dnorm(x,meanz[1],meanz[3])+meanz[6]*dnorm(x,meanz[2],meanz[4]))*245*0.1, size=1)

## begin plotting the kernel fits to the data
##

## build derivative orders
hatf <- dkde(log$ODc2, deriv.order = 0, na.rm=TRUE)

## correctly initialise mu and sigma
x <- log$ODc2 ## initiliase x
fx <- function(x) { meanz[5]*dnorm(x,meanz[1],meanz[3]) + meanz[6]*dnorm(x,meanz[2],meanz[4]) }

## plot derivative estimator against the order derivative of function
plot(hatf, fx = fx)

## find optimal 'h' bandwidth
## select the optimal bandwidth for kdensity function
## to call on bw= 'h' examples
## h.ucv.gaus0$h

## Unbiased cross validation

## Gaussian kernel
h.ucv.gaus0 <- h.ucv(log$ODc2, deriv.order= 0, kernel= "gaussian")

## Epanechnikov kernel
h.ucv.epan0 <- h.ucv(log$ODc2, deriv.order= 0, kernel= "epanechnikov")

## plot the UCV functions
## Gaussian
plot(h.ucv.gaus0, main= "UCV for Gaussian kernel bw", col= "#35274A", lw=2)
  abline(v= h.ucv.gaus0$h, col= "#0B775E", lw= 2) ## 'abline' to visually show the minimum value of which bw 'h' will take
## epanechnikov
##
plot(h.ucv.epan0, main= "UCV for Epanechnikov kernel bw", col= "#35274A", lw=2)
  abline(v= h.ucv.epan0$h, col= "#0B775E", lw=2) ## 'abline' to visually show the minimum value of which bw 'h' will take
  
## plot the distribution function against kernel density
## using NEW adjusted bw='h'

## plot original density against density with startMix and optimal bw 'h'
plot(kdensity(log$ODc2), ## no start
     main= "Original vs. Final", ylim= range(0, 0.8), col= "brown")
lines(kdensity(log$ODc2, start= startMix, bw= h.ucv.gaus0$h, kernel= "gaussian"),
      col= "darkgreen") ## startMix chosen
legend(-3, 0.8, legend= c("logOD", "Fitted distribution"), fill= c("brown", "darkgreen"), cex=0.5)
rug(log$ODc2, ticksize=0.05, lwd=1, col="darkblue")

logOD = log$ODc2
## comparing Guassian and startMix starts
## histogram
hist(logOD, prob= TRUE, main= "Density plots", ylim= range(0, 1), col= "lightgrey", breaks= 45)
lines(kdensity(log$ODc2), col= "#35274A", lwd=2) ## no parametric start
lines(kdensity(log$ODc2, start= "gaussian", bw= h.ucv.gaus0$h, kernel= "gaussian"), col= "#0B775E", lwd=2)
lines(kdensity(log$ODc2, start= startMix, bw= h.ucv.gaus0$h, kernel= "gaussian"), col= "#F2300F", lwd=2)
legend(-2.5, 1, legend= c("Original density", "User specified bw", "startMix Gauss"), fill= c("#35274A", "#0B775E", "#F2300F"), cex=0.6)
rug(log$ODc2, ticksize=-0.03, lwd=1, col="black")

## Compare Guassian vs. startMix starts
## plot
plot(kdensity(logOD),
     main= "Comparing starts", ylim= range(0, 0.8), col= "#35274A") ## no parametric start
lines(kdensity(log$ODc2, start= "gaussian", bw= h.ucv.gaus0$h, kernel= "gaussian"), col= "#0B775E", lwd=2)
lines(kdensity(log$ODc2, start= startMix, bw= h.ucv.gaus0$h, kernel= "gaussian"), col= "#F2300F", lwd=2)
legend(-3, 0.8, legend= c("Original density", "User specified bw", "startMix"), fill= c("#35274A", "#0B775E", "#F2300F"), cex=0.5)
rug(log$ODc2, ticksize=-0.03, lwd=1, col="black")



## END
