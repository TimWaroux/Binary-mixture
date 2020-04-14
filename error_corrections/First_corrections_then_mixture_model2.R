# Here, we first derive the beta errors for the HEV_Varkens_studenten data, then subtract them from the original data, and finally perform the binary mixture model (assuming two different sd's for f1 and f2)

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


# Put results in handy format
result <- recover_types( data.mod ) %>% 
  spread_draws( mu_baseline, sigma1, sigma2, alpha[day_id],  beta[day_id, plate_id] , delta, p, sigma_plate)
colnames(result)
result_table <- aggregate(result[c(4:6, 8, 10:13)], list(result$plate_id, result$day_id), mean)
colnames(result_table)
result_table <- result_table[,c(2,1,3,4,5,6,7,8,9,10)] #reorder: exchange first(plate_id) & second (day_id) column 
colnames(result_table) <- c("day_id", "plate_id", "mu_baseline", "sigma_baseline", "alpha", "beta", "delta_est", "p_est", "sigma_plate")
result_table
save(result_table, file="result_table.Rda")

# Beta's for each plate and day (first column are plates, second column are days)
beta_result <- aggregate(result[c(10)], list(result$plate_id, result$day_id), mean)
beta_result

# Rememeber that n_plates_per_day <- c(2,2,2,1,3,3), so remove rows 3,6,9,11,12
beta_result_new <- rbind( beta_result[c(1:2),], beta_result[c(4:5),], beta_result[c(7:8),], beta_result[c(10),], beta_result[c(13:18),])

# Copy original data set 
data3 <- data2 

# Select the right data and substract the right errors (beta's)
data3[c(1:40),1] <- data3[c(1:40),1]-beta_result_new[1,3] # Day 1 plate 1
data3[c(41:80),1] <- data3[c(41:80),1]-beta_result_new[2,3] # Day 1 plate 2
data3[c(81:120),1] <- data3[c(81:120),1]-beta_result_new[3,3] # Day 2 plate 1
data3[c(121:160),1] <- data3[c(121:160),1]-beta_result_new[4,3] # Day 2 plate 2
data3[c(161:200),1] <- data3[c(161:200),1]-beta_result_new[5,3] # Day 3 plate 1
data3[c(201:240),1] <- data3[c(201:240),1]-beta_result_new[6,3] # Day 3 plate 2
data3[c(241:280),1] <- data3[c(241:280),1]-beta_result_new[7,3] # Day 4 plate 1
data3[c(281:320),1] <- data3[c(281:320),1]-beta_result_new[8,3] # Day 5 plate 1
data3[c(321:360),1] <- data3[c(321:360),1]-beta_result_new[9,3] # Day 5 plate 2
data3[c(361:400),1] <- data3[c(361:400),1]-beta_result_new[10,3] # Day 5 plate 3
data3[c(401:440),1] <- data3[c(401:440),1]-beta_result_new[11,3] # Day 6 plate 1
data3[c(441:480),1] <- data3[c(441:480),1]-beta_result_new[12,3] # Day 6 plate 2
data3[c(481:494),1] <- data3[c(481:494),1]-beta_result_new[13,3] # Day 6 plate 3
View( data3 )

# Make a histogram of original data
par(mfrow = c(1,2))
hist(data2$logod, xlab = "logOD value", breaks = 40, main = "Original data")
# Make a histogram of corrected data
hist(data3$logod, xlab = "logOD value", breaks = 40, main = "Corrected data")
par(mfrow = c(1,1))

# Show plot of corrected and original data
plt <- ggplot( )
# plt <- plt + geom_density( data=result.corr, aes(x=logod_corr, color="Estimated"), color="black", lwd = 1, show.legend = TRUE )
plt <- plt + geom_density( data=tibble(logod=data2$logod), aes(x=logod, color="Original"), color="red", lwd = 1, show.legend = TRUE )
plt <- plt + geom_density( data=tibble(logod=data3$logod), aes(x=logod, color="Corrected"), color="turquoise4", lwd = 1, show.legend = TRUE )
plt <- plt + annotate("text", x=-1.25, y=seq(from=0.3, to=0.4, by=0.1), label = c("Original density", "Corrected density"), color = c("red", "turquoise4"))
plt <- plt + ggtitle("Comparison of original and corrected data density plots")
plt
ggsave("Comparison_corrected_and_raw_true_density.pdf", plot=plt)

datacomp=compose_data( data3 )
# View( datacomp )

# Run the model
data.mod3 <- stan( "Binary_mixture2.stan", data=datacomp, 
                   iter=6000,
                   control = list(max_treedepth = 15, adapt_delta = 0.99))

save.image(file="my_workspace3.RData")

# Analyse results
summary( data.mod3, par=c("mu_baseline", "sigma1", "sigma2", "delta", "p") )

# Verifying convergence for some parameters
traceplot( data.mod3 , pars=c("mu_baseline", "sigma1", "sigma2"))
traceplot( data.mod3 , pars=c("delta", "p") ) # convergence of estimated delta and prevalence p

# Put results in handy format
result3 <- recover_types( data.mod3 ) %>% 
  spread_draws( mu_baseline, sigma1, sigma2, delta, p)

# Verify correlation between parameters
pairs( data.mod3, pars=c("mu_baseline", "sigma1") )
pairs( data.mod3, pars=c("mu_baseline", "sigma2") )
pairs( data.mod3, pars=c("sigma1", "sigma2") )
pairs( data.mod3, pars=c("sigma1", "p") )
pairs( data.mod3, pars=c("delta", "p") )

result.corr3 <- recover_types( data.mod3 ) %>% 
  spread_draws( logod_corr ) 

# Show plot of estimated, corrected and original data
plt3 <- ggplot( )
plt3 <- plt3 + geom_density( data=result.corr3, aes(x=logod_corr, color="Estimated"), color="black", lwd = 1, show.legend = TRUE )
plt3 <- plt3 + geom_density( data=tibble(logod=data2$logod), aes(x=logod, color="Original"), color="red", lwd = 1, show.legend = TRUE )
plt3 <- plt3 + geom_density( data=tibble(logod=data3$logod), aes(x=logod, color="Corrected"), color="turquoise4", lwd = 1, show.legend = TRUE )
plt3 <- plt3 + annotate("text", x=-1.35, y=seq(from=0.35, to=0.55, by=0.1), label = c("Estimated density", "Original density", "Corrected density"), color = c("black", "red", "turquoise4"))
plt3 <- plt3 + ggtitle("Comparison of original, estimated and corrected data density plots")
plt3
ggsave("Comparison_estimated_corrected_and_raw_true_density.pdf", plot=plt)

# Looking at spread of mu_baseline estimation
plt <- ggplot( result3 )
plt <- plt + geom_halfeyeh( aes(x=mu_baseline, y=1) ) + ggtitle("Verifying mu_baseline estimate")
plt
ggsave("Mu_baseline_estimate.pdf", plot=plt)

# Looking at spread of sigma1 estimation
plt <- ggplot( result3 )
plt <- plt + geom_halfeyeh( aes(x=sigma1, y=1) ) + ggtitle("Verifying sigma1 estimate")
plt
ggsave("Sigma1_estimate.pdf", plot=plt)

# Looking at spread of sigma2 estimation
plt <- ggplot( result3 )
plt <- plt + geom_halfeyeh( aes(x=sigma2, y=1) ) + ggtitle("Verifying sigma2 estimate")
plt
ggsave("Sigma2_estimate.pdf", plot=plt)

# Looking at spread of delta estimation
plt <- ggplot( result3 )
plt <- plt + geom_halfeyeh( aes(x=delta, y=1) ) + ggtitle("Verifying delta estimate")
plt
ggsave("Delta_estimate.pdf", plot=plt)

# Looking at spread of prev estimation
plt <- ggplot( result3 )
plt <- plt + geom_halfeyeh( aes(x=p, y=1) ) + ggtitle("Verifying prev estimate")
plt
ggsave("Prev_estimate.pdf", plot=plt)

# Summary
model_est3 <- c(round(mean(result3$mu_baseline), digits=3), round(mean(result3$sigma1), digits=3), round(mean(result3$sigma2), digits=3), round(mean(result3$delta), digits=3), round(mean(result3$p), digits=3))
model_summary3 <- data.frame(model_est3)
rownames(model_summary3) <- c("mu_baseline", "sigma1", "sigma2", "delta", "prev")
model_summary3
save(model_summary3, file="model_summary3.Rda")
