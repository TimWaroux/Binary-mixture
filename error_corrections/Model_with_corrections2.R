# Here, we correct the HEV_Varkens_studenten data and perform the binary mixture model at the same time (assuming two different sd's for f1 and f2)

library( tidyverse )
library( here )
library( readxl )
library( rstan )
library( tidybayes )
library( rio )

# Location file
getwd()
setwd("./Documents/R")

# Make one big dataframe from all sheets (possible because they have the same format)
data <- import_list("HEV_Varkens_studenten_adjusted.xlsx", setclass = "tbl", rbind = TRUE)
# View( data )

# Make subsets of the negative controls from each plate (CTL1 is the negative control)
data_ncontrol_1 <- data[c(1:4), c(1:9)]
data_ncontrol_2 <- data[c(97:100), c(1:9)]
data_ncontrol_3 <- data[c(193:196), c(1:9)]
data_ncontrol_4 <- data[c(289:292), c(1:9)]
data_ncontrol_5 <- data[c(385:388), c(1:9)]
data_ncontrol_6 <- data[c(481:484), c(1:9)]
data_ncontrol_7 <- data[c(577:580), c(1:9)]
data_ncontrol_8 <- data[c(673:676), c(1:9)]
data_ncontrol_9 <- data[c(769:772), c(1:9)]
data_ncontrol_10 <- data[c(865:868), c(1:9)]
data_ncontrol_11 <- data[c(961:964), c(1:9)]
data_ncontrol_12 <- data[c(1057:1060), c(1:9)]
data_ncontrol_13 <- data[c(1153:1156), c(1:9)]

# Make one big dataframe for all negative controls (CLT1) 
data_ncontrol <- rbind(data_ncontrol_1, data_ncontrol_2, data_ncontrol_3, data_ncontrol_4, data_ncontrol_5, data_ncontrol_6, data_ncontrol_7, data_ncontrol_8, data_ncontrol_9, data_ncontrol_10, data_ncontrol_11, data_ncontrol_12, data_ncontrol_13)
# View( data_ncontrol )

# Select the OD-value column and make logOD column
log_ncontrol <- data_ncontrol
log_ncontrol[,5] <- log(data_ncontrol[,5])
# View( log_ncontrol )

# Select non-empty wells of the negative controls (even) and remove some unimportant columns
data_ncontrols <- dplyr::filter(log_ncontrol, id %% 2 == 0) %>%
  select( logod = starts_with("450"), day_id = day_id, plate_id = plate_id)
# View( data_ncontrols )

# Make subsets of the positive controls from each plate (CTL2 is the positive control)
data_pcontrol_1 <- data[c(5:8), c(1:9)]
data_pcontrol_2 <- data[c(101:104), c(1:9)]
data_pcontrol_3 <- data[c(197:200), c(1:9)]
data_pcontrol_4 <- data[c(293:296), c(1:9)]
data_pcontrol_5 <- data[c(389:392), c(1:9)]
data_pcontrol_6 <- data[c(485:488), c(1:9)]
data_pcontrol_7 <- data[c(581:584), c(1:9)]
data_pcontrol_8 <- data[c(677:680), c(1:9)]
data_pcontrol_9 <- data[c(773:776), c(1:9)]
data_pcontrol_10 <- data[c(869:872), c(1:9)]
data_pcontrol_11 <- data[c(965:968), c(1:9)]
data_pcontrol_12 <- data[c(1061:1064), c(1:9)]
data_pcontrol_13 <- data[c(1157:1160), c(1:9)]

# Make one big dataframe for all positive controls (CLT2) 
data_pcontrol <- rbind(data_pcontrol_1, data_pcontrol_2, data_pcontrol_3, data_pcontrol_4, data_pcontrol_5, data_pcontrol_6, data_pcontrol_7, data_pcontrol_8, data_pcontrol_9, data_pcontrol_10, data_pcontrol_11, data_pcontrol_12, data_pcontrol_13)
# View( data_pcontrol )

# Select the OD-value column and make logOD column
log_pcontrol <- data_pcontrol
log_pcontrol[,5] <- log(data_pcontrol[,5])
# View( log_pcontrol )

# Select non-empty wells of the positive controls (even) and remove some unimportant columns
data_pcontrols <- dplyr::filter(log_pcontrol, id %% 2 == 0) %>%
  select( logod = starts_with("450"), day_id = day_id, plate_id = plate_id)
# View( data_pcontrols )

# Make subsets of binary mixture data from each plate
data_binary_1 <- data[c(17:96), c(1:9)]
data_binary_2 <- data[c(113:192), c(1:9)]
data_binary_3 <- data[c(209:288), c(1:9)]
data_binary_4 <- data[c(305:384), c(1:9)]
data_binary_5 <- data[c(401:480), c(1:9)]
data_binary_6 <- data[c(497:576), c(1:9)]
data_binary_7 <- data[c(593:672), c(1:9)]
data_binary_8 <- data[c(689:768), c(1:9)]
data_binary_9 <- data[c(785:864), c(1:9)]
data_binary_10 <- data[c(881:960), c(1:9)]
data_binary_11 <- data[c(977:1056), c(1:9)]
data_binary_12 <- data[c(1073:1152), c(1:9)]
data_binary_13 <- data[c(1169:1196), c(1:9)]

# Make one big dataframe for all binary mixture data
data_binary <- rbind(data_binary_1, data_binary_2, data_binary_3, data_binary_4, data_binary_5, data_binary_6, data_binary_7, data_binary_8, data_binary_9, data_binary_10, data_binary_11, data_binary_12, data_binary_13)
# View( data_binary )

# Select the OD-value column and make logOD column
log_binary <- data_binary
log_binary[,5] <- log(log_binary[,5])
# View( log_binary )

# Select non-empty wells of binary mixture data (even) and remove some unimportant columns
data2 <- dplyr::filter(log_binary, id %% 2 == 0) %>%
  select( logod = starts_with("450"), day_id = day_id, plate_id = plate_id)
# View( data2 )

# Negative controls: data_ncontrols
View( data_ncontrols )
# Positive controls: data_pcontrols
View( data_pcontrols )
# Binary mixture data: data2
View( data2 )

# Data: row 1-26 negative controls, row 27-52 positive controls, row 53-546 binary mixture data
plates.data <- rbind(data_ncontrols, data_pcontrols, data2)
id <- numeric(546)
for (i in 1:546){
  id[i] <- i
}
plates.data$id <- id # Add new unique id for all 546 data points
# View( plates.data )

# Make a histogram
hist(data2$logod, xlab = "logOD value", breaks = 40, main = "Histogram data")

# Number of plates
#n_plates_per_day <- c(2,2,2,1,3,3)
#total_n_plates = sum(n_plates_per_day)

datacomp=compose_data( plates.data, ctrls_neg=26, ctrls_pos=26)
#View( datacomp )

# Run the model
data.mod <- stan( "Binary_Mixture_with_corrections2.stan", data=datacomp, 
                  iter=6000,
                  control = list(max_treedepth = 15, adapt_delta = 0.99))

save.image(file="my_workspace.RData")

# Analyse results
summary( data.mod, par=c("mu_baseline", "sigma1", "sigma2", "sigma_plate", "delta", "p") )

# Verifying convergence for some parameters
traceplot( data.mod , pars=c("mu_baseline", "sigma1", "sigma2", "sigma_plate"))
traceplot( data.mod , pars=c("alpha[1]", "alpha[2]", "alpha[3]") ) # alpha[1] = day 1 error
traceplot( data.mod , pars=c("beta[1,1]", "beta[2,2]", "beta[4,3]")) # beta[3,1] = error of plate 1, day3
traceplot( data.mod , pars=c("delta", "p") ) # convergence of estimated delta and prevalence p

# Put results in handy format
result <- recover_types( data.mod ) %>% 
  spread_draws( mu_baseline, sigma1, sigma2, alpha[day_id],  beta[day_id, plate_id] , delta, p, sigma_plate)
colnames(result)
result_table <- aggregate(result[c(4:6, 8, 10:13)], list(result$plate_id, result$day_id), mean)
colnames(result_table)
result_table <- result_table[,c(2,1,3,4,5,6,7,8,9,10)] #reorder: exchange first(plate_id) & second (day_id) column 
colnames(result_table) <- c("day_id", "plate_id", "mu_baseline", "sigma1", "sigma2", "alpha", "beta", "delta_est", "p_est", "sigma_plate")
result_table
save(result_table, file="result_table.Rda")

# Verify correlation between parameters
pairs( data.mod, pars=c("mu_baseline", "sigma1") )
pairs( data.mod, pars=c("mu_baseline", "sigma2") )
pairs( data.mod, pars=c("mu_baseline", "sigma_plate") ) 
pairs( data.mod, pars=c("sigma_plate", "sigma1") ) 
pairs( data.mod, pars=c("mu_baseline", "alpha[1]") )
pairs( data.mod, pars=c("mu_baseline", "beta[1,1]") )
pairs( data.mod, pars=c("sigma1", "beta[1,1]") )
pairs( data.mod, pars=c("sigma_plate", "beta[2,2]") )
pairs( data.mod, pars=c("alpha[1]", "beta[1,1]") ) 

# No red dots! Yay!

result.corr <- recover_types( data.mod ) %>% 
  spread_draws( logod_corr ) 

# Show plot of corrected and original data
plt <- ggplot( )
plt <- plt + geom_density( data=result.corr, aes(x=logod_corr, color="Estimated"), color="black", lwd = 1, show.legend = TRUE )
plt <- plt + geom_density( data=tibble(logod=data2$logod), aes(x=logod, color="Original"), color="red", lwd = 1, show.legend = TRUE )
plt <- plt + annotate("text", x=-1.5, y=seq(from=0.4, to=0.5, by=0.1), label = c("Estimated density", "Original density"), color = c("black", "red"))
plt <- plt + ggtitle("Comparison of estimated and raw data density plots")
plt
ggsave("Comparison_estimated_raw_true_density.pdf", plot=plt)

# Looking at spread of mu_baseline estimation
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=mu_baseline, y=1) ) + ggtitle("Verifying mu_baseline estimate")
plt
ggsave("Mu_baseline_estimate.pdf", plot=plt)

# Looking at spread of sigma1 estimation
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=sigma1, y=1) ) + ggtitle("Verifying sigma_baseline estimate")
plt
ggsave("Sigma1_estimate.pdf", plot=plt)

# Looking at spread of sigma2 estimation
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=sigma2, y=1) ) + ggtitle("Verifying sigma_baseline estimate")
plt
ggsave("Sigma2_estimate.pdf", plot=plt)

# Looking at spread of delta estimation
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=delta, y=1) ) + ggtitle("Verifying delta estimate")
plt
ggsave("Delta_estimate.pdf", plot=plt)

# Looking at spread of prev estimation
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=p, y=1) ) + ggtitle("Verifying prev estimate")
plt
ggsave("Prev_estimate.pdf", plot=plt)

# Looking at spread of sigma_plate estimation
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=sigma_plate, y=1) ) + ggtitle("Verifying sigma_plate estimate")
plt
ggsave("Sigma_plate_estimate.pdf", plot=plt)

# Looking at spread of alpha (day error mean) estimations
plt <- ggplot(result)
plt <- plt + geom_halfeyeh( aes(x=alpha, y=day_id) ) + ggtitle("The spread of the alpha (day error) estimates)")
plt
ggsave("Alpha_estimates.pdf", plot=plt)

# Looking at spread of beta (plate error mean) estimations
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=beta, y=day_id) ) + ggtitle("The spread of the beta error estimates")
plt
ggsave("Beta_estimates.pdf", plot=plt)

# Summary
model_est <- c(round(mean(result$mu_baseline), digits=3), round(mean(result$sigma1), digits=3), round(mean(result$sigma2), digits=3), round(mean(result$delta), digits=3), round(mean(result$p), digits=3), round(mean(result$sigma_plate), digits=3))
model_summary <- data.frame(model_est)
rownames(model_summary) <- c("mu_baseline", "sigma1", "sigma2", "delta", "prev", "sigma_plate")
model_summary
save(model_summary, file="model_summary.Rda")
