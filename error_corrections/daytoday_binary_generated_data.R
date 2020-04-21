library( here )
library( rstan )
library( tidyverse )
library( tidybayes )
getwd()
#setwd("./Mathematics for Industry/daytoday_binary")


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


#### Options to generate data vor varying plates per day
#### are given as well (commented).
#### The evaluation has not been adjusted for this.


#### Hyperparameters for data generation ####

n_days <- 5 # number of days
n_plates_per_day <- 3 # setting fixed number for now to get Stan model working
#n_plates_per_day <- c(2,4,3,4,4) # vector of number of plates per day
total_n_plates = n_days*n_plates_per_day # sum(n_plates_per_day)

n_wells <- 88 # without controls: each plate has 96 wells
neg_ctrls_per_plate <- 4   # Number of negative controls per plate
pos_ctrls_per_plate <- 4   # Number of positive controls per plate

N <- n_wells*total_n_plates   # Total number of wells (excluding controls)
N_ctrls <- total_n_plates*(neg_ctrls_per_plate+pos_ctrls_per_plate)

mean_baseline <- -1 # mean of seronegative population
sd_baseline <- 0.5 # variance of both popuations
true_prev <- 0.3      # Prevalence in the population
true_delta <- 2   # Difference between means of the two components

sd_day_error <- 0.3 # variation of day error
sd_plate_error <- 0.1 # Variation of plate error



#### Generate "clean" data ####
logod_baseline <- rep(NA, N)
unif_samples <- runif(N)
for (i in 1:N){
  if(unif_samples[i] < true_prev){ # sample from seropositive population
    logod_baseline[i] <- rnorm(1, mean = mean_baseline+true_delta, sd = sd_baseline)
  }
  else{ # sample from seronegative population
    logod_baseline[i] <- rnorm(1, mean = mean_baseline, sd = sd_baseline)
  }
}


#### Generate errors and add to data ####
# day errors, centered around 0:
day_error <- rnorm(n_days, 0, sd_day_error)
day_error <- day_error - mean(day_error) # centering around zero (not strictly necessary)
# plate errors; plate error of a given day drawn from ~N with mean = that day's day_error
plate_error <- c()
for(d in 1:n_days){
  pl_err <- rnorm(n_plates_per_day, day_error[d], sd_plate_error)
  #pl_err <- rnorm(n_plates_per_day[d], day_error[d], sd_plate_error)
  #pl_err <- pl_err - mean(pl_err) + day_error[d] # centering around zero, not necessary if sd_plate is small
  plate_error <- c(plate_error, pl_err)
}
plate_error


# Add errors to clean data and plot raw data:
logod <- logod_baseline + rep(plate_error, each=n_wells)
dev.off()
plot(logod, main = "Raw data (with errors)")
dev.off()
par(mfrow = c(1,2))
hist(logod_baseline, breaks = 30, main = "Clean data") # histogram of original data
hist(logod, breaks = 30, main = "Data with day & plate errors") # histogram of data distorted by plate errors
par(mfrow = c(1,1))


#### Generate control data ####
# negative control data:
logod_neg_basectrl <- rnorm( neg_ctrls_per_plate*total_n_plates, 
                             mean=mean_baseline, sd=sd_baseline )
# add error, where the offset is the same as for the regular data
logod_neg_ctrl <- logod_neg_basectrl + rep( plate_error, each=neg_ctrls_per_plate)
# positive control data:
logod_pos_basectrl <- rnorm( pos_ctrls_per_plate*total_n_plates, 
                             mean=mean_baseline+true_delta, sd=sd_baseline )
# add error, where the offset is the same as for the regular data
logod_pos_ctrl <- logod_pos_basectrl + rep( plate_error, each=pos_ctrls_per_plate)

logod_ctrl <- c(logod_neg_ctrl,logod_pos_ctrl)
dev.off()
plot(logod_ctrl, main = "Control data")

# Combine control data and raw data:
plate_id_vec <- rep(1:n_plates_per_day, times = n_days)
#plate_id_vec <- c() # make vec of plate ids, restarting at 1 for each day (ex: 1,2,3,1,2,1,2,3)
#day_id_vec <- c()
#for(d in 1:n_days){ # code useful for when we have variable n_plates_per_day
#  plate_id_vec <- c(plate_id_vec, seq(1:n_plates_per_day[d]))
#  day_id_vec <- c(day_id_vec, rep(d, each=n_plates_per_day[d]))
#}
#plate_id_vec # testing vec
negctrls.data <- tibble( logod=logod_neg_ctrl, 
                       plate_id=as.factor(rep(plate_id_vec, each = neg_ctrls_per_plate)), # plate id per day, so restarts at 1 each day
                       day_id=as.factor(rep( 1:n_days, each = n_plates_per_day * neg_ctrls_per_plate)), 
                       id = 1:(total_n_plates*neg_ctrls_per_plate) ) # unique sample id
#negctrls.data <- tibble( logod=logod_neg_ctrl, 
#                         plate_id=as.factor(rep(plate_id_vec, each = neg_ctrls_per_plate)), # plate id per day, so restarts at 1 each day
#                         day_id=as.factor(rep( day_id_vec, each = neg_ctrls_per_plate)), 
#                         id = 1:(total_n_plates*neg_ctrls_per_plate) ) # unique sample id
posctrls.data <- tibble( logod=logod_pos_ctrl, 
                       plate_id=as.factor(rep(plate_id_vec, each = pos_ctrls_per_plate)), # plate id per day, so restarts at 1 each day
                       day_id=as.factor(rep( 1:n_days, each = n_plates_per_day * pos_ctrls_per_plate)), 
                       id = 1:(total_n_plates*pos_ctrls_per_plate) ) # unique sample id
#posctrls.data <- tibble( logod=logod_pos_ctrl, 
#                         plate_id=as.factor(rep(plate_id_vec, each = pos_ctrls_per_plate)), # plate id per day, so restarts at 1 each day
#                         day_id=as.factor(rep( day_id_vec, each = pos_ctrls_per_plate)), 
#                         id = 1:(total_n_plates*pos_ctrls_per_plate) ) # unique sample id
plates.data <- tibble( logod=logod, 
                       plate_id=as.factor(rep(plate_id_vec, each = n_wells)), # plate id per day, so restarts at 1 each day
                       day_id=as.factor(rep( 1:n_days, each = n_plates_per_day * n_wells)), 
                       id = 1:(total_n_plates*n_wells) ) # unique sample id
#plates.data <- tibble( logod=logod, 
#                       plate_id=as.factor(rep(plate_id_vec, each = n_wells)), # plate id per day, so restarts at 1 each day
#                       day_id=as.factor(rep( day_id_vec, each = n_wells)), 
#                       id = 1:(total_n_plates*n_wells) ) # unique sample id
total.data <- rbind(negctrls.data, posctrls.data, plates.data)

datacomp=compose_data( total.data,
                       ctrls_neg=neg_ctrls_per_plate*total_n_plates, ctrls_pos=pos_ctrls_per_plate*total_n_plates)


#### Run the model ####
plates.mod <- stan( "Binary_Mixture_GeneratedData.stan", data=datacomp, 
                    iter=6000,
                    control = list(max_treedepth = 15, adapt_delta = 0.99))

#save.image(file="my_workspace.RData")


#### Analyse results ####
summary( plates.mod, par=c("mu_baseline", "sigma_baseline", "sigma_plate", "delta", "p") )

# Verifying convergence for some parameters
traceplot( plates.mod , pars=c("mu_baseline", "sigma_baseline", "sigma_plate"))
traceplot( plates.mod , pars=c("alpha[1]", "alpha[2]", "alpha[3]") ) # alpha[1] = day 1 error
traceplot( plates.mod , pars=c("beta[1,1]", "beta[2,2]", "beta[3,3]")) # beta[3,1] = error of plate 1, day3
traceplot( plates.mod , pars=c("delta", "p" ) ) # convergence of estimated delta and prevalence p

# Put results in handy format
result <- recover_types( plates.mod ) %>% 
  spread_draws( mu_baseline, sigma_baseline, alpha[day_id],  beta[day_id, plate_id] , delta, p, sigma_plate)
colnames(result)
result_table <- aggregate(result[c(4:5, 7, 9:12)], list(result$plate_id, result$day_id), mean)
colnames(result_table)
result_table <- result_table[,c(2,1,3,4,5,6,7,8, 9)] #reorder: exchange first(plate_id) & second (day_id) column 
colnames(result_table) <- c("day_id", "plate_id", "mu_baseline", "sigma_baseline", "alpha", "beta", "delta_est", "p_est", "sigma_plate")
result_table
save(result_table, file="Results/result_table.Rda")

# Verify correlation between parameters:
pairs( plates.mod, pars=c("mu_baseline", "sigma_baseline") )
pairs( plates.mod, pars=c("mu_baseline", "sigma_plate") )
pairs( plates.mod, pars=c("sigma_plate", "sigma_baseline") )
pairs( plates.mod, pars=c("mu_baseline", "alpha[1]") )
pairs( plates.mod, pars=c("mu_baseline", "beta[1,1]") )
pairs( plates.mod, pars=c("sigma_baseline", "beta[1,1]") )
pairs( plates.mod, pars=c("sigma_plate", "beta[2,3]") )
pairs( plates.mod, pars=c("alpha[1]", "beta[1,1]") )

result.corr <- recover_types( plates.mod ) %>% 
  spread_draws( logod_corr ) 

corrected_data <- logod - rep(result_table[,6], each=n_wells)

# reconstructed, uncorrected, and original data
plt <- ggplot( )
plt <- plt + geom_density( data=result.corr, aes(x=logod_corr, color="Estimated"), color="turquoise4", lwd = 1, show.legend = TRUE ) # density curve of N(mu_baseline, sigma_baseline) estimates
plt <- plt + geom_density( data=tibble( logod=logod), aes(x=logod, color="Original"), color="red", lwd = 1, show.legend = TRUE )  # uncorrected (original) data
plt <- plt + geom_density( data=tibble( logod_baseline=logod_baseline), aes(x=logod_baseline, color="True data"), color = "black", lwd = 1, show.legend = TRUE ) # true data (logod without errors)
plt <- plt + ggtitle("Comparison of original, erroneous and estimated density plots")
plt <- plt + labs(x="logOD value")
plt
ggsave("Results/Comparison_estimation.png", plot=plt)


plt <- ggplot( )
plt <- plt + geom_density( data=tibble( logod_baseline=logod_baseline), aes(x=logod_baseline, color="True data"), color = "black", lwd = 1, show.legend = TRUE ) # original data without errors
plt <- plt + geom_density( data=tibble( logod=logod), aes(x=logod, color="Original"), color="red", lwd = 1, show.legend = TRUE )  # uncorrected (original) data
plt <- plt + geom_density( data=tibble( logod_corrected=corrected_data), aes(x=logod_corrected, color="Corrected"), color="turquoise4", lwd = 1, show.legend = TRUE ) # corrected data
plt <- plt + ggtitle("Comparison of original, erroneous and corrected density plots")
plt <- plt + labs(x="logOD value")
plt
ggsave("Results/Comparison_correction.png", plot=plt)


# checking estimate of mu_baseline
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=mu_baseline, y=1) ) + ggtitle("Verifying mu_baseline estimate")
plt <- plt + geom_vline( xintercept=mean_baseline, color="red", lwd=1.1) + annotate("text", x=mean_baseline, y=2, label="Red line is the true mean_baseline value")
plt
ggsave("Results/mu_baseline_estimate.pdf", plot=plt)

# looking at spread of sigma_baseline estimation
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=sigma_baseline, y=1) ) + ggtitle("Verifying sigma_baseline estimate")
plt <- plt + geom_vline( xintercept=sd_baseline, color="red", lwd=1.1 ) + annotate("text", x=sd_baseline, y=1.75, label="Red line is the true mean_baseline value")
plt
ggsave("Results/sigma_baseline_estimate.pdf", plot=plt)

# looking at spread of sigma_plate estimation
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=sigma_plate, y=1) ) + ggtitle("Verifying sigma_plate estimate")
plt <- plt + geom_vline( xintercept=sd_plate_error, color="red", lwd=1.1 ) + annotate("text", x=sd_plate_error, y=1.75, label="Red line is the true sd_plate_error value")
plt
ggsave("Results/sigma_plate_estimate.pdf", plot=plt)

# looking at spread of alpha (day mean) esimations
plt <- ggplot(result)
plt <- plt + geom_halfeyeh( aes(x=alpha, y=day_id) ) + ggtitle("The spread of the alpha (day error) estimates)")
plt <- plt + geom_point(data = data.frame(day_error, 1:n_days), mapping = aes(x=day_error, y=1:n_days), color = "red", cex =4, pch=20)
plt <- plt + annotate("text", x=0, y=2.5, label = "True day errors shown in red")
plt
ggsave("Results/alpha_estimates.pdf", plot=plt)

#spread of beta errors
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=beta, y=day_id) ) + ggtitle("The spread of the beta error estimates")
plt <- plt + geom_point(data = data.frame(plate_error, rep(1:n_days, each=n_plates_per_day)), mapping = aes(x=plate_error, y=rep(1:n_days, each=n_plates_per_day)), color="red", cex=4, pch=20)
plt <- plt + annotate("text", x=0, y=2.5, label = "True beta errors shown in red")
plt
ggsave("Results/beta_estimates.pdf", plot=plt)

# Estimated and real delta (difference between means of 2 components)
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=delta, y=1) ) + ggtitle("Verifying delta estimate")
plt <- plt + geom_vline( xintercept=true_delta, color="red", lwd=1.1) + annotate("text", x=true_delta, y=1.5, label="Red line is the true delta value")
plt
ggsave("Results/delta_estimate.pdf", plot=plt)

# Estimated and real prevalence
plt <- ggplot( result )
plt <- plt + geom_halfeyeh( aes(x=p, y=1) ) + ggtitle("Verifying prevalence estimate")
plt <- plt + geom_vline( xintercept=true_prev, color="red", lwd=1.1) + annotate("text", x=true_prev, y=1.5, label="Red line is the true prevalence")
plt
ggsave("Results/prev_estimate.pdf", plot=plt)

# Quantifying accuracy of model ####
# Looking at estimated vs true parameters:
# alpha errors:
alpha_param <- cbind( result %>% group_by( day_id ) %>% summarize( alpha_est=mean(alpha)),
       day_error)
alpha_param <- cbind(alpha_param, abs_diff = abs(alpha_param$alpha_est - alpha_param$day_error))
alpha_param
save(alpha_param, file="Results/alpha_estimates.Rda")

# beta errors (nested day & plate errors):
beta_param <- cbind( result %>% group_by( day_id, plate_id ) %>% summarize( beta_est=mean(beta)),
       plate_error = plate_error )
beta_param <- cbind(beta_param, abs_diff = abs(beta_param$beta_est-beta_param$plate_error))
beta_param
save(beta_param, file="Results/beta_estimates.Rda")

#summary
model_est <- c(round(mean(result$mu_baseline), digits=3), round(mean(result$sigma_baseline), digits=3), round(mean(result$delta), digits=3), round(mean(result$p), digits=3), round(mean(result$sigma_plate), digits=3))
true_value <- c(mean_baseline, sd_baseline, true_delta, true_prev, sd_plate_error) #assuming sigma_plate is sd_day error? or should it be sum of sd_day and sd_plate error?
model_summary <- data.frame(model_est, true_value)
model_summary <- cbind(model_summary,  abs_diff = c(round(abs(model_est-true_value), digits=3)))
alpha_summ <- c(round(mean(alpha_param$abs_diff), digits=3), "-", "-")
beta_summ <- c(round(mean(beta_param$abs_diff), digits=3), "-", "-")
model_summary <- rbind(model_summary, alpha_summ, beta_summ)
rownames(model_summary) <- c("mu_baseline", "sigma_baseline", "delta", "prev", "sigma_plate", "mean abs diff of alpha errors", "mean abs diff of beta errors")
model_summary
save(model_summary, file="Results/model_summary.Rda")

