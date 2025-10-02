datapoint = data.frame(
  endyear = c(2021, 2020, 2019, 2018),
  carbon_endyear = c(63, 62, 61, 60),
  temp_endyear = c(172, 171, 170, 169),
  n = c(6, 5, 4, 3)
)
alpha_post_mean54_list  = list()
beta_post_mean54_list  = list()
cov_matrix54_list = list()
alpha_post_mean51_list  = list()
beta_post_mean51_list  = list()
cov_matrix51_list = list()

library(dplyr)
library(stats)
library(tidyr)
library(ggplot2)
library(MASS)
estimate_model <- function(df) {
  phi_k_new <- 1
  phi_k <- 0.5
  tol <- 1e-10
  iter <- 0
  while(abs(phi_k_new - phi_k) > tol) {
    
    # Calculate new variables
    y_prime <- df$temperature - phi_k * dplyr::lag(df$temperature)
    x_prime <- df$co2 - phi_k * dplyr::lag(df$co2)
    
    # remove NA
    temp_data <- data.frame(y_prime = y_prime, x_prime = x_prime) %>%
      na.omit() %>% filter(y_prime > 0, x_prime > 0)
    
    # OLS regression
    fit <- lm(y_prime ~ x_prime, data = temp_data)
    alpha_prime_k <- coef(fit)[1]
    beta_prime_k <- coef(fit)[2]
    alpha_k = alpha_prime_k/(1-phi_k)
    beta_k = beta_prime_k
    # calculate ut
    
    df$ut <- df$temperature - (alpha_k + beta_k * df$co2)
    phi_k_new <- cor(df$ut[-1], dplyr::lag(df$ut)[-1]) # check
    
    # Convergence Check
    phi_k <- phi_k_new
    iter <- iter + 1
  }
  
  vcov_matrix_prime <- vcov(fit)
  alpha_var <- (1 / (1 - phi_k))^2 * vcov_matrix_prime[1, 1]  # variance of alpha_k after transforming
  beta_var <- vcov_matrix_prime[2, 2]  # beta_k's variance
  alpha_beta_cov <- (1 / (1 - phi_k)) * vcov_matrix_prime[1, 2]
  
  list(alpha_k = alpha_k, beta_k = beta_k, phi_k = phi_k, iter = iter, alpha_var=alpha_var, beta_var = beta_var, alpha_beta_cov = alpha_beta_cov)
}
# function for calculating σ_k^2
calculate_sigma_k2 <- function(yt, xt, alpha_k, beta_k, phi_k) {
  # Calculate new variables
  yt_prime <- yt - phi_k * dplyr::lag(yt)
  xt_prime <- xt - phi_k * dplyr::lag(xt)
  
  # remove NA
  temp_data <- data.frame(yt_prime = yt_prime, xt_prime = xt_prime) %>%
    na.omit() 
  
  # calculate y'_t and x'_t
  y_t_prime <- temp_data$yt_prime
  x_t_prime <- temp_data$xt_prime
  
  alpha_prime_k <- alpha_k*(1-phi_k)
  # calculate ε_t,k
  epsilon_tk <- y_t_prime - (alpha_prime_k + beta_k * x_t_prime)
  
  # calculate σ_k^2
  sigma_k2 <- mean(epsilon_tk^2)
  
  return(sigma_k2)
}

compute_distr_deltaT <- function(delta_x, t1, phi_post, sigma_squared_post, beta_post_mean, cov_matrix) {
  # Initialize the variance vector
  m <- delta_x$Year - t1
  var_part1 <- numeric(length(m))
  
  # Compute the variance for each time point
  for (i in 1:length(m)) {
    mi <- m[i]
    
    var_part1[i] <- sum(
      sapply(1:1000, function(j) {
        phi_j <- phi_post[[j]]
        sigma_j2 <- sigma_squared_post[[j]]
        
        term1 <- sigma_j2 * (phi_j^mi - 1)^2
        term2 <- (1 - phi_j^(2 * (t1-1850))) / (1 - phi_j^2)
        term3 <- sigma_j2 * (1 - phi_j^(2 * mi)) / (1 - phi_j^2)
        
        term1 * term2 + term3
      })
    ) / 1000 
  }
  
  var_part2 <- delta_x$deltaCO2^2*cov_matrix[2,2]
  # Construct the output data frame
  distr_deltaT <- data.frame(
    Year = delta_x$Year,
    mean = delta_x$deltaCO2 * beta_post_mean,  # Assuming `deltaCO2` is a column in `delta_x`
    var = var_part1+var_part2
  )
  
  return(distr_deltaT)
}

generate_samples <- function(distr_deltaT, num_samples = 1000) {
  # Initialize a list to store samples for each year
  samples_list <- list()
  
  # Loop through each year in the data frame
  for (i in 1:nrow(distr_deltaT)) {
    # Extract the mean and variance for the current year
    current_mean <- distr_deltaT$mean[i]
    current_var <- distr_deltaT$var[i]
    
    # Calculate the standard deviation from the variance
    current_sd <- sqrt(current_var)
    
    # Generate 1000 normal samples for the current year
    samples <- rnorm(num_samples, mean = current_mean, sd = current_sd)
    
    # Store the samples in the list with the corresponding year as the name
    samples_list[[as.character(distr_deltaT$Year[i])]] <- samples
  }
  
  return(samples_list)
}
compute_quantiles <- function(samples_by_year, probs = c(0.025, 0.050, 0.500, 0.950, 0.975)) {
  quantiles_df <- data.frame(Year = numeric(), 
                             Q025 = numeric(), 
                             Q050 = numeric(), 
                             Q500 = numeric(), 
                             Q950 = numeric(), 
                             Q975 = numeric())
  
  # Loop through each year in the samples
  for (year in names(samples_by_year$temp.change.traj)) {
    # Get the samples for the current year
    samples <- samples_by_year$temp.change.traj[[year]]
    
    # Compute the quantiles
    quantiles <- quantile(samples, probs = probs)
    
    # Store the quantiles in the data frame
    quantiles_df <- rbind(quantiles_df, data.frame(Year = as.numeric(year),
                                                   Q025 = quantiles[1],
                                                   Q050 = quantiles[2],
                                                   Q500 = quantiles[3],
                                                   Q950 = quantiles[4],
                                                   Q975 = quantiles[5]))
  }
  
  return(quantiles_df)
}
# Plotting the median with 90% and 95% CI
plot_temp_change_with_CI <- function(samples_by_year, y_t, title) {
  temp_change_quants <- samples_by_year[["temp.quants"]]
  
  # Extract necessary data
  years <- temp_change_quants$Year
  median_values <- temp_change_quants$Q500
  ci_90_lower <- temp_change_quants$Q050
  ci_90_upper <- temp_change_quants$Q950
  ci_95_lower <- temp_change_quants$Q025
  ci_95_upper <- temp_change_quants$Q975
  
  # Plot the median line (disable default x-axis with xaxt = "n")
  plot(
    years, median_values, type = "l", col = "blue", lwd = 2,
    ylim = range(ci_95_lower, ci_95_upper), xlab = "Year", ylab = "Temperature (Celsius)", 
    xaxt = "n" # Disable the default x-axis
  )
  
  # Custom x-axis with only integer years
  axis(1, at = seq(floor(min(years)), ceiling(max(years)), by = 1), 
       labels = seq(floor(min(years)), ceiling(max(years)), by = 1))
  
  # Fill the 95% CI (polygon)
  polygon(
    x = c(years, rev(years)), # x-coordinates from left to right, then right to left
    y = c(ci_95_lower, rev(ci_95_upper)), # y-coordinates for lower and upper bounds
    col = rgb(0, 1, 0, alpha = 0.2), # Light green with transparency
    border = NA # No border
  )
  
  # Fill the 90% CI (polygon)
  polygon(
    x = c(years, rev(years)), 
    y = c(ci_90_lower, rev(ci_90_upper)), 
    col = rgb(1, 0, 0, alpha = 0.2), # Light red with transparency
    border = NA
  )
  
  # Add lines for the boundaries of CIs
  lines(years, ci_90_lower, col = rgb(1, 0, 0, alpha = 0.8), lwd = 1.5, lty = 2) # 90% CI lower
  lines(years, ci_90_upper, col = rgb(1, 0, 0, alpha = 0.8), lwd = 1.5, lty = 2) # 90% CI upper
  
  lines(years, ci_95_lower, col = rgb(0, 1, 0, alpha = 0.8), lwd = 1.5, lty = 2) # 95% CI lower
  lines(years, ci_95_upper, col = rgb(0, 1, 0, alpha = 0.8), lwd = 1.5, lty = 2) # 95% CI upper
  
  # Add the median line again on top
  lines(years, median_values, col = "blue", lwd = 2)
  
  # Add observed temperature line
  y_t_values <- y_t[-1]
  years_y_t <- samples_by_year[["temp.quants"]]$Year # Use the same years from the quantile data
  lines(years_y_t, y_t_values, type = "o", col = "purple", lwd = 2, pch = 16)
  
  # Add legend
  legend("topleft", legend = c("Median", "90% CI", "95% CI", "HadCrut5"), 
         col = c("blue", rgb(1, 0, 0, alpha = 0.8), rgb(0, 1, 0, alpha = 0.8), "purple"), 
         lty = c(1, 2, 2, 1), lwd = c(2, 1.5, 1.5, 2), pch = c(NA, NA, NA, 16), bg = "white")
  
  # Add title
  title(main = title)
  
  # Record and return the plot
  plot_record <- recordPlot()
  return(plot_record)
}


### 51 models

results <- list()
setwd("~/Desktop/BMA")
load("all_df_allmodel_new.RData")
all_df <- all_df %>% filter(!(model %in% c("ens", "mod", "one")))
all_models <- unique(all_df$model)

for (i in 1:length(all_models)) {
  model_data <- all_df %>% filter(model == all_models[i])
  if (any(is.na(model_data))) {
    #cat("model", i, "has NA \n")
    model_data = na.omit(model_data)
  } 
  model_data$co2 = model_data$co2/1000
  result <- estimate_model(model_data)
  results[[i]] <- result
}
# is based on Gt
####################
library(readxl)
setwd("~/Desktop/Supplementary/data_medium_updated_to_2024")
data.string.carbon1 <- paste0('National_Fossil_Carbon_Emissions_2024v1.xlsx') # with projection at year 2024
data.carbon1 <- read_excel(data.string.carbon1, skip = 11, sheet = 2) #yearly data
data.carbon1 <- data.carbon1[-(1:109), ] # start from 1960, i.e. 1959
names(data.carbon1)[1] <- "Year"
xt <- cumsum(data.carbon1$World[57:66])* 3.664/1000 # 57:66 means 2015-2024, /1000 means changing from Mt to Gt
xt <- xt/1000 # /1000 means changing from Gt to 1000Gt

#setwd("~/Documents/UW Courses/Research/CO2_Data_2024")
setwd("~/Desktop/BMA")
load('real_hadcrut5_updated_to_2024.Rda')
real_data = hadcrut5
yt <- real_data[166:175, 'V6'] # 166:year2015, 175:year2024

sigma_k2_results <- list()
for (i in 1:length(results)) {
  
  alpha_k <- results[[i]]$alpha_k
  beta_k <- results[[i]]$beta_k
  phi_k <- results[[i]]$phi_k
  
  # calculate σ_k^2
  sigma_k2 <- calculate_sigma_k2(yt, xt, alpha_k, beta_k, phi_k)
  sigma_k2_results[[i]] <- sigma_k2
}


sigma_k2_values <- unlist(sigma_k2_results)
n <- 9 # 9: 2016-2024

transformed_sigma_k2_values <- sigma_k2_values^(-n/2)
total_sum <- sum(transformed_sigma_k2_values)
normalized_sigma_k2 <- transformed_sigma_k2_values / total_sum
normalized_sigma_k2_results <- as.list(normalized_sigma_k2)

model_weights <- data.frame(
  model = all_models,
  weight = normalized_sigma_k2,
  beta = sapply(results, function(x) x$beta_k)
)
model_weights_sort <- model_weights %>%
  arrange(desc(weight))

model_weights_sort


# Posterior beta
alpha_post_mean <- 0
alpha_post_var <- 0
alpha_post_sd <- 0 

beta_post_mean <- 0
beta_post_var <- 0
beta_post_sd <- 0 

alpha_beta_post_cov <- 0

for (i in 1:length(results)) {
  alpha_post_mean <- alpha_post_mean+(results[[i]]$alpha_k)*normalized_sigma_k2_results[[i]]
  beta_post_mean <- beta_post_mean+(results[[i]]$beta_k)*normalized_sigma_k2_results[[i]]
}

for (i in 1:length(results)) {
  alpha_post_var <- alpha_post_var+(results[[i]]$alpha_var+ (results[[i]]$alpha_k-alpha_post_mean)^2)*normalized_sigma_k2_results[[i]]
  
  beta_post_var <- beta_post_var+(results[[i]]$beta_var+ (results[[i]]$beta_k-beta_post_mean)^2)*normalized_sigma_k2_results[[i]]
  
  alpha_beta_post_cov <- alpha_beta_post_cov+(results[[i]]$alpha_beta_cov + (results[[i]]$beta_k-beta_post_mean)*(results[[i]]$alpha_k-alpha_post_mean))*normalized_sigma_k2_results[[i]]
  
  alpha_post_sd <- sqrt(alpha_post_var)
  beta_post_sd <- sqrt(beta_post_var)
}

alpha_post_mean51 = alpha_post_mean
beta_post_mean51 = beta_post_mean

cat ("51 CIMP6 models \n")
cat ("Posterior mean [alpha, beta] is [", alpha_post_mean51,", ",beta_post_mean51, "] \n")


cov_matrix51 <- matrix(c(alpha_post_var, alpha_beta_post_cov,
                         alpha_beta_post_cov, beta_post_var), 
                       nrow = 2, ncol = 2)

cat("Covariance matrix is: \n")
print(cov_matrix51)


samples <- mvrnorm(n = 1000, mu = c(alpha_post_mean, beta_post_mean), Sigma = cov_matrix51)
plot(samples[,1], samples[,2], xlab = "alpha", ylab = "beta", main = "Samples from Joint Distribution of alpha and beta")

#################################
############ none ###############
#################################
if (!exists("distr_deltaT_none")) {
  
  # 51 CMIP6
  ############################
  ### obtain posterior sample
  x_t <- cumsum(data.carbon1$World[57:66])* 3.664/1000 # 57:66 means 2015-2024, /1000 means changing from Mt to Gt
  x_t <- x_t/1000 # changing from Gt to 1000 Gt
  
  y_t <- real_data[166:175, 'V6'] # 166:year2015, 175:year2024
  alpha_post_mean = alpha_post_mean51
  beta_post_mean = beta_post_mean51
  cov_matrix = cov_matrix51
  
  set.seed(2024)
  par_samples <- mvrnorm(n = 1000, mu = c(alpha_post_mean, beta_post_mean), Sigma = cov_matrix)
  alpha_sample = par_samples[,1]
  beta_sample = par_samples[,2]
  
  phi_post <- list()
  sigma_squared_post <- list()
  for (i in 1:1000){
    alpha = alpha_sample[i]
    beta = beta_sample[i]
    
    # Compute u_t
    u_t <- y_t - (alpha + beta * x_t)
    
    # Create lagged version of u_t
    u_lag <- dplyr::lag(u_t)
    
    # Remove NA values due to lag
    temp_data <- data.frame(u_t = u_t, u_lag = u_lag) %>% na.omit()
    
    # Estimate phi using OLS
    phi_model <- lm(u_t ~ u_lag, data = temp_data)
    
    # Get the estimated value of phi
    phi_post[i] <- coef(phi_model)[2]
    
    # Compute residuals of the AR(1) model
    epsilon_t <- residuals(phi_model)
    
    # Estimate sigma^2
    sigma_squared_post[i] <- var(epsilon_t)
  }
  #######################
  ###
  distr_deltaT_none = list()
  for (i in 1:1000){
    proj_co2_2024 <- data.frame(
      Year = proj.evals.2024.ar1.const$trajs.annual.worldwide[[i]]$Year,#i.e 2025:2100
      CO2 = proj.evals.2024.ar1.const$trajs.annual.worldwide[[i]]$CO2 / 1e12 # 1000 gigaton
    )
    proj_co2_2024$cumCO2 <- cumsum(proj_co2_2024$CO2) # based on 1000 Gt
    
    start_co2 <- proj_co2_2024[1,] # 2024 be the baseline
    delta_x <- data.frame(
      Year = proj_co2_2024$Year[2:dim(proj_co2_2024)[1]],#i.e 2025:2100
      deltaCO2 = proj_co2_2024$cumCO2[2:dim(proj_co2_2024)[1]] - start_co2$cumCO2 # 1 gigaton
    )
    
    
    parameters <- compute_distr_deltaT(delta_x=delta_x, 
                                       t1=2024, 
                                       phi_post=phi_post, 
                                       sigma_squared_post=sigma_squared_post, 
                                       beta_post_mean=beta_post_mean,
                                       cov_matrix=cov_matrix)
    
    # Sampling 1 value from the normal distribution for each year
    parameters$temp_change <- mapply(function(mean, var) {
      rnorm(1, mean, sqrt(var))
    }, parameters$mean, parameters$var)
    
    distr_deltaT_none$Year = delta_x$Year
    distr_deltaT_none$parameters[[i]]<- parameters[,c("Year", "mean", "var")]
    distr_deltaT_none$temp.change.traj[[i]] <- parameters$temp_change
    
  }
  
  
  
  all_samples <- do.call(cbind, distr_deltaT_none$temp.change.traj)
  quantiles_list <- apply(all_samples, 1, function(x) {
    c(
      Q025 = quantile(x, 0.025),
      Q050 = quantile(x, 0.050),
      Q500 = quantile(x, 0.500),
      Q950 = quantile(x, 0.950),
      Q975 = quantile(x, 0.975),
      mean = mean(x)
    )
  })
  
  
  distr_deltaT_none$temp.change.quantile <- data.frame(
    Year = distr_deltaT_none$Year,
    t(quantiles_list)
  )
  colnames(distr_deltaT_none$temp.change.quantile) <- c(
    "Year", "Q025", "Q050", "Q500", "Q950", "Q975", "mean"
  )
  
  head(distr_deltaT_none$temp.change.quantile)
}

summary_anomaly_1960 <- subset(summary_anomaly, year >= 1960)
other_estimates <- data.frame(year = 2006:2024, observed_anomaly = real_data$V6[157:175] - mean(real_data$V6[12:31])) # change to 175 due to 2024
other_quantiles <- other_estimates$observed_anomaly +
  matrix(rep(qnorm(c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)), each = 19), nrow=19) # change to 19
other_quantiles <- data.frame(other_quantiles)
names(other_quantiles) <- c("q025", "q050", "q100", "median", "q900", "q950", "q975")
other_estimates <- cbind(other_estimates, other_quantiles)
summary_anomaly_1960 <- rbind(summary_anomaly_1960, other_estimates)




samples_by_year<-list()
samples_by_year$t1 = 2024
samples_by_year$start.temp.change <- summary_anomaly_1960$observed_anomaly[nrow(summary_anomaly_1960)]
samples_by_year$hist.temp.change <- summary_anomaly_1960
samples_by_year$proj.temp.anomaly <- distr_deltaT_none$temp.change.quantile
samples_by_year$proj.temp.anomaly[, c("Q025", "Q050", "Q500", "Q950", "Q975","mean")] <-
  samples_by_year$proj.temp.anomaly[, c("Q025", "Q050", "Q500", "Q950", "Q975","mean")] +
  samples_by_year$start.temp.change


samples_by_year$proj.temp.anomaly$Year <- as.numeric(samples_by_year$proj.temp.anomaly$Year)
# Create a data frame for the historical and future data combined
historical_data <- samples_by_year$hist.temp.change
future_data <- samples_by_year$proj.temp.anomaly

library(ggplot2)

plot_combined_temp <- function(samples_by_year, summary_proj, title = 'Assembled Global Mean Temperature Forecast') {
  # Prepare data
  summary_proj_none <- subset(summary_proj, adjusted == "None")
  
  # Extract historical and future projection data from `samples_by_year`
  historical_data <- samples_by_year$hist.temp.change
  future_data <- samples_by_year$proj.temp.anomaly
  
  # Create combined plot
  p <- ggplot() +
    # Plot historical data (1960-2024) in black
    geom_line(data = historical_data, aes(x = year, y = observed_anomaly, color = "Historical Data"), size = 0.5) +
    # Plot future median projection from `samples_by_year` in red
    geom_line(data = future_data, aes(x = Year, y = Q500, color = "BMA median projection"), size = 1, linetype = "solid") +
    # Add 95% and 90% confidence intervals for `samples_by_year` in red
    geom_ribbon(data = future_data, aes(x = Year, ymin = Q025, ymax = Q975, fill = "95% CI (BMA)"), alpha = 0.2) +
    geom_ribbon(data = future_data, aes(x = Year, ymin = Q050, ymax = Q950, fill = "90% CI (BMA)"), alpha = 0.4) +
    # Plot TCRE projection from `combined2024` in #4daf4a green
    geom_line(data = summary_proj_none, aes(x = year, y = median, color = "TCRE median projection"), size = 1, linetype = "dashed") +
    # Add 95% and 90% confidence intervals for `combined2024` in #4daf4a green
    geom_ribbon(data = summary_proj_none, aes(x = year, ymin = q025, ymax = q975, fill = "95% CI (TCRE)"), alpha = 0.2) +
    geom_ribbon(data = summary_proj_none, aes(x = year, ymin = q050, ymax = q950, fill = "90% CI (TCRE)"), alpha = 0.4) +
    # Add a vertical dashed line at 2025
    geom_vline(xintercept = 2025, linetype = "dashed", color = "blue", size = 1) +
    # Add text annotation for 2025
    annotate("text", x = 2025, y = max(c(historical_data$observed_anomaly, future_data$mean)), label = "2025", color = "blue", vjust = -0.5) +
    # Labels and title
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)",
      title = title,
      color = "Legend",
      fill = "Confidence Intervals"
    ) +
    theme_minimal() +
    #ylim(-0.5, 4.5) +
    scale_x_continuous(limits = c(1960, 2100), breaks = c(1960, 2000, 2050, 2100)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    # Custom colors for lines and ribbons
    scale_color_manual(values = c("BMA median projection" = "red", "TCRE median projection" = "#4daf4a", "Historical Data" = "black")) +
    scale_fill_manual(values = c("95% CI (BMA)" = "red", "90% CI (BMA)" = "red", "95% CI (TCRE)" = "#4daf4a", "90% CI (TCRE)" = "#4daf4a"))
  
  return(p)
}

# Call the function to plot the combined data
plot_combined_temp(samples_by_year, combined)





#################################
############ adjust #############
#################################

# 51 CMIP6
### obtain posterior sample
if (!exists("distr_deltaT_adj")) {
  x_t <- cumsum(data.carbon1$World[57:66])* 3.664/1000 # 57:66 means 2015-2024, /1000 means changing from Mt to Gt
  x_t <- x_t /1000 # changing from Gt to 1000Gt
  y_t <- real_data[166:175, 'V6'] # 166:year2015, 175:year2024
  alpha_post_mean = alpha_post_mean51
  beta_post_mean = beta_post_mean51
  cov_matrix = cov_matrix51
  
  set.seed(2024)
  par_samples <- mvrnorm(n = 1000, mu = c(alpha_post_mean, beta_post_mean), Sigma = cov_matrix)
  alpha_sample = par_samples[,1]
  beta_sample = par_samples[,2]
  
  phi_post <- list()
  sigma_squared_post <- list()
  for (i in 1:1000){
    alpha = alpha_sample[i]
    beta = beta_sample[i]
    
    # Compute u_t
    u_t <- y_t - (alpha + beta * x_t)
    
    # Create lagged version of u_t
    u_lag <- dplyr::lag(u_t)
    
    # Remove NA values due to lag
    temp_data <- data.frame(u_t = u_t, u_lag = u_lag) %>% na.omit()
    
    # Estimate phi using OLS
    phi_model <- lm(u_t ~ u_lag, data = temp_data)
    
    # Get the estimated value of phi
    phi_post[i] <- coef(phi_model)[2]
    
    # Compute residuals of the AR(1) model
    epsilon_t <- residuals(phi_model)
    
    # Estimate sigma^2
    sigma_squared_post[i] <- var(epsilon_t)
  }
  #######################
  ###
  distr_deltaT_adj = list()
  for (i in 1:1000){
    proj_co2_2024 <- data.frame(
      Year = proj.evals.2024.adjusted$trajs.annual.worldwide[[i]]$Year,#i.e 2025:2100
      CO2 = proj.evals.2024.adjusted$trajs.annual.worldwide[[i]]$CO2 / 1e12 # 1000 gigaton
    )
    proj_co2_2024$cumCO2 <- cumsum(proj_co2_2024$CO2)
    
    start_co2 <- proj_co2_2024[1,] # 2024 be the baseline
    delta_x <- data.frame(
      Year = proj_co2_2024$Year[2:dim(proj_co2_2024)[1]],#i.e 2025:2100
      deltaCO2 = proj_co2_2024$cumCO2[2:dim(proj_co2_2024)[1]] - start_co2$cumCO2 # 1 gigaton
    )
    
    
    parameters <- compute_distr_deltaT(delta_x=delta_x, 
                                       t1=2024, 
                                       phi_post=phi_post, 
                                       sigma_squared_post=sigma_squared_post, 
                                       beta_post_mean=beta_post_mean,
                                       cov_matrix=cov_matrix)
    
    # Sampling 1 value from the normal distribution for each year
    parameters$temp_change <- mapply(function(mean, var) {
      rnorm(1, mean, sqrt(var))
    }, parameters$mean, parameters$var)
    
    distr_deltaT_adj$Year = delta_x$Year
    distr_deltaT_adj$parameters[[i]]<- parameters[,c("Year", "mean", "var")]
    distr_deltaT_adj$temp.change.traj[[i]] <- parameters$temp_change
    
  }
  
  
  
  all_samples <- do.call(cbind, distr_deltaT_adj$temp.change.traj)
  quantiles_list <- apply(all_samples, 1, function(x) {
    c(
      Q025 = quantile(x, 0.025),
      Q050 = quantile(x, 0.050),
      Q500 = quantile(x, 0.500),
      Q950 = quantile(x, 0.950),
      Q975 = quantile(x, 0.975),
      mean = mean(x)
    )
  })
  
  
  distr_deltaT_adj$temp.change.quantile <- data.frame(
    Year = distr_deltaT_adj$Year,
    t(quantiles_list)
  )
  colnames(distr_deltaT_adj$temp.change.quantile) <- c(
    "Year", "Q025", "Q050", "Q500", "Q950", "Q975", "mean"
  )
  
  head(distr_deltaT_adj$temp.change.quantile)
}


summary_anomaly_1960 <- subset(summary_anomaly, year >= 1960)
other_estimates <- data.frame(year = 2006:2024, observed_anomaly = real_data$V6[157:175] - mean(real_data$V6[12:31])) # change to 175 due to 2024
other_quantiles <- other_estimates$observed_anomaly +
  matrix(rep(qnorm(c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)), each = 19), nrow=19) # change to 19
other_quantiles <- data.frame(other_quantiles)
names(other_quantiles) <- c("q025", "q050", "q100", "median", "q900", "q950", "q975")
other_estimates <- cbind(other_estimates, other_quantiles)
summary_anomaly_1960 <- rbind(summary_anomaly_1960, other_estimates)




samples_by_year<-list()
samples_by_year$t1 = 2024
samples_by_year$start.temp.change <- summary_anomaly_1960$observed_anomaly[nrow(summary_anomaly_1960)]
samples_by_year$hist.temp.change <- summary_anomaly_1960
samples_by_year$proj.temp.anomaly <- distr_deltaT_adj$temp.change.quantile
samples_by_year$proj.temp.anomaly[, c("Q025", "Q050", "Q500", "Q950", "Q975","mean")] <-
  samples_by_year$proj.temp.anomaly[, c("Q025", "Q050", "Q500", "Q950", "Q975","mean")] +
  samples_by_year$start.temp.change


samples_by_year$proj.temp.anomaly$Year <- as.numeric(samples_by_year$proj.temp.anomaly$Year)
# Create a data frame for the historical and future data combined
historical_data <- samples_by_year$hist.temp.change
future_data <- samples_by_year$proj.temp.anomaly


library(ggplot2)

plot_combined_temp <- function(samples_by_year, summary_proj, title = 'Assembled Global Mean Temperature Forecast under Adjusted Scenario') {
  # Prepare data
  summary_proj_none <- subset(summary_proj, adjusted == "Adjusted")
  
  # Extract historical and future projection data from `samples_by_year`
  historical_data <- samples_by_year$hist.temp.change
  future_data <- samples_by_year$proj.temp.anomaly
  
  # Create combined plot
  p <- ggplot() +
    # Plot historical data (1960-2024) in black
    geom_line(data = historical_data, aes(x = year, y = observed_anomaly, color = "Historical Data"), size = 0.5) +
    # Plot future median projection from `samples_by_year` in red
    geom_line(data = future_data, aes(x = Year, y = Q500, color = "BMA median projection"), size = 1, linetype = "solid") +
    # Add 95% and 90% confidence intervals for `samples_by_year` in red
    geom_ribbon(data = future_data, aes(x = Year, ymin = Q025, ymax = Q975, fill = "95% CI (BMA)"), alpha = 0.2) +
    geom_ribbon(data = future_data, aes(x = Year, ymin = Q050, ymax = Q950, fill = "90% CI (BMA)"), alpha = 0.4) +
    # Plot TCRE projection from `combined2024` in #4daf4a green
    geom_line(data = summary_proj_none, aes(x = year, y = median, color = "TCRE median projection"), size = 1, linetype = "dashed") +
    # Add 95% and 90% confidence intervals for `combined2024` in #4daf4a green
    geom_ribbon(data = summary_proj_none, aes(x = year, ymin = q025, ymax = q975, fill = "95% CI (TCRE)"), alpha = 0.2) +
    geom_ribbon(data = summary_proj_none, aes(x = year, ymin = q050, ymax = q950, fill = "90% CI (TCRE)"), alpha = 0.4) +
    # Add a vertical dashed line at 2025
    geom_vline(xintercept = 2025, linetype = "dashed", color = "blue", size = 1) +
    # Add text annotation for 2025
    annotate("text", x = 2025, y = max(c(historical_data$observed_anomaly, future_data$mean)), label = "2025", color = "blue", vjust = -0.5) +
    # Labels and title
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)",
      title = title,
      color = "Legend",
      fill = "Confidence Intervals"
    ) +
    theme_minimal() +
    #ylim(-0.5, 4.5) +
    scale_x_continuous(limits = c(1960, 2100), breaks = c(1960, 2000, 2050, 2100)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    # Custom colors for lines and ribbons
    scale_color_manual(values = c("BMA median projection" = "red", "TCRE median projection" = "#4daf4a", "Historical Data" = "black")) +
    scale_fill_manual(values = c("95% CI (BMA)" = "red", "90% CI (BMA)" = "red", "95% CI (TCRE)" = "#4daf4a", "90% CI (TCRE)" = "#4daf4a"))
  
  return(p)
}

# Call the function to plot the combined data
plot_combined_temp(samples_by_year, combined)



#################################
############ continued #############
#################################
if (!exists("distr_deltaT_cont")) {
  x_t <- cumsum(data.carbon1$World[57:66])* 3.664/1000 # 57:66 means 2015-2024, /1000 means changing from Mt to Gt
  x_t <- x_t /1000 # changing from Gt to 1000Gt
  y_t <- real_data[166:175, 'V6'] # 166:year2015, 175:year2024
  alpha_post_mean = alpha_post_mean51
  beta_post_mean = beta_post_mean51
  cov_matrix = cov_matrix51
  
  set.seed(2024)
  par_samples <- mvrnorm(n = 1000, mu = c(alpha_post_mean, beta_post_mean), Sigma = cov_matrix)
  alpha_sample = par_samples[,1]
  beta_sample = par_samples[,2]
  
  phi_post <- list()
  sigma_squared_post <- list()
  for (i in 1:1000){
    alpha = alpha_sample[i]
    beta = beta_sample[i]
    
    # Compute u_t
    u_t <- y_t - (alpha + beta * x_t)
    
    # Create lagged version of u_t
    u_lag <- dplyr::lag(u_t)
    
    # Remove NA values due to lag
    temp_data <- data.frame(u_t = u_t, u_lag = u_lag) %>% na.omit()
    
    # Estimate phi using OLS
    phi_model <- lm(u_t ~ u_lag, data = temp_data)
    
    # Get the estimated value of phi
    phi_post[i] <- coef(phi_model)[2]
    
    # Compute residuals of the AR(1) model
    epsilon_t <- residuals(phi_model)
    
    # Estimate sigma^2
    sigma_squared_post[i] <- var(epsilon_t)
  }
  #######################
  ###
  distr_deltaT_cont = list()
  for (i in 1:1000){
    proj_co2_2024 <- data.frame(
      Year = proj.evals.2024.adjusted$trajs.annual.worldwide.cont[[i]]$Year,#i.e 2025:2100
      CO2 = proj.evals.2024.adjusted$trajs.annual.worldwide.cont[[i]]$CO2 / 1e12 # 1000 gigaton
    )
    proj_co2_2024$cumCO2 <- cumsum(proj_co2_2024$CO2)
    
    start_co2 <- proj_co2_2024[1,] # 2024 be the baseline
    delta_x <- data.frame(
      Year = proj_co2_2024$Year[2:dim(proj_co2_2024)[1]],#i.e 2025:2100
      deltaCO2 = proj_co2_2024$cumCO2[2:dim(proj_co2_2024)[1]] - start_co2$cumCO2 # 1 gigaton
    )
    
    
    parameters <- compute_distr_deltaT(delta_x=delta_x, 
                                       t1=2024, 
                                       phi_post=phi_post, 
                                       sigma_squared_post=sigma_squared_post, 
                                       beta_post_mean=beta_post_mean,
                                       cov_matrix=cov_matrix)
    
    # Sampling 1 value from the normal distribution for each year
    parameters$temp_change <- mapply(function(mean, var) {
      rnorm(1, mean, sqrt(var))
    }, parameters$mean, parameters$var)
    
    distr_deltaT_cont$Year = delta_x$Year
    distr_deltaT_cont$parameters[[i]]<- parameters[,c("Year", "mean", "var")]
    distr_deltaT_cont$temp.change.traj[[i]] <- parameters$temp_change
    
  }
  
  
  
  all_samples <- do.call(cbind, distr_deltaT_cont$temp.change.traj)
  quantiles_list <- apply(all_samples, 1, function(x) {
    c(
      Q025 = quantile(x, 0.025),
      Q050 = quantile(x, 0.050),
      Q500 = quantile(x, 0.500),
      Q950 = quantile(x, 0.950),
      Q975 = quantile(x, 0.975),
      mean = mean(x)
    )
  })
  
  
  distr_deltaT_cont$temp.change.quantile <- data.frame(
    Year = distr_deltaT_cont$Year,
    t(quantiles_list)
  )
  colnames(distr_deltaT_cont$temp.change.quantile) <- c(
    "Year", "Q025", "Q050", "Q500", "Q950", "Q975", "mean"
  )
  
  head(distr_deltaT_cont$temp.change.quantile)
}

#############################

summary_anomaly_1960 <- subset(summary_anomaly, year >= 1960)
other_estimates <- data.frame(year = 2006:2024, observed_anomaly = real_data$V6[157:175] - mean(real_data$V6[12:31])) # change to 175 due to 2024
other_quantiles <- other_estimates$observed_anomaly +
  matrix(rep(qnorm(c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)), each = 19), nrow=19) # change to 19
other_quantiles <- data.frame(other_quantiles)
names(other_quantiles) <- c("q025", "q050", "q100", "median", "q900", "q950", "q975")
other_estimates <- cbind(other_estimates, other_quantiles)
summary_anomaly_1960 <- rbind(summary_anomaly_1960, other_estimates)




samples_by_year<-list()
samples_by_year$t1 = 2024
samples_by_year$start.temp.change <- summary_anomaly_1960$observed_anomaly[nrow(summary_anomaly_1960)]
samples_by_year$hist.temp.change <- summary_anomaly_1960
samples_by_year$proj.temp.anomaly <- distr_deltaT_cont$temp.change.quantile
samples_by_year$proj.temp.anomaly[, c("Q025", "Q050", "Q500", "Q950", "Q975","mean")] <-
  samples_by_year$proj.temp.anomaly[, c("Q025", "Q050", "Q500", "Q950", "Q975","mean")] +
  samples_by_year$start.temp.change


samples_by_year$proj.temp.anomaly$Year <- as.numeric(samples_by_year$proj.temp.anomaly$Year)
# Create a data frame for the historical and future data combined
historical_data <- samples_by_year$hist.temp.change
future_data <- samples_by_year$proj.temp.anomaly


library(ggplot2)

plot_combined_temp <- function(samples_by_year, summary_proj, title = 'Assembled Global Mean Temperature Forecast under Continued Scenario') {
  # Prepare data
  summary_proj_none <- subset(summary_proj, adjusted == "Continued")
  
  # Extract historical and future projection data from `samples_by_year`
  historical_data <- samples_by_year$hist.temp.change
  future_data <- samples_by_year$proj.temp.anomaly
  
  # Create combined plot
  p <- ggplot() +
    # Plot historical data (1960-2024) in black
    geom_line(data = historical_data, aes(x = year, y = observed_anomaly, color = "Historical Data"), size = 0.5) +
    # Plot future median projection from `samples_by_year` in red
    geom_line(data = future_data, aes(x = Year, y = Q500, color = "BMA median projection"), size = 1, linetype = "solid") +
    # Add 95% and 90% confidence intervals for `samples_by_year` in red
    geom_ribbon(data = future_data, aes(x = Year, ymin = Q025, ymax = Q975, fill = "95% CI (BMA)"), alpha = 0.2) +
    geom_ribbon(data = future_data, aes(x = Year, ymin = Q050, ymax = Q950, fill = "90% CI (BMA)"), alpha = 0.4) +
    # Plot TCRE projection from `combined2024` in #4daf4a green
    geom_line(data = summary_proj_none, aes(x = year, y = median, color = "TCRE median projection"), size = 1, linetype = "dashed") +
    # Add 95% and 90% confidence intervals for `combined2024` in #4daf4a green
    geom_ribbon(data = summary_proj_none, aes(x = year, ymin = q025, ymax = q975, fill = "95% CI (TCRE)"), alpha = 0.2) +
    geom_ribbon(data = summary_proj_none, aes(x = year, ymin = q050, ymax = q950, fill = "90% CI (TCRE)"), alpha = 0.4) +
    # Add a vertical dashed line at 2025
    geom_vline(xintercept = 2025, linetype = "dashed", color = "blue", size = 1) +
    # Add text annotation for 2025
    annotate("text", x = 2025, y = max(c(historical_data$observed_anomaly, future_data$mean)), label = "2025", color = "blue", vjust = -0.5) +
    # Labels and title
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)",
      title = title,
      color = "Legend",
      fill = "Confidence Intervals"
    ) +
    theme_minimal() +
    #ylim(-0.5, 4.5) +
    scale_x_continuous(limits = c(1960, 2100), breaks = c(1960, 2000, 2050, 2100)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    # Custom colors for lines and ribbons
    scale_color_manual(values = c("BMA median projection" = "red", "TCRE median projection" = "#4daf4a", "Historical Data" = "black")) +
    scale_fill_manual(values = c("95% CI (BMA)" = "red", "90% CI (BMA)" = "red", "95% CI (TCRE)" = "#4daf4a", "90% CI (TCRE)" = "#4daf4a"))
  
  return(p)
}

# Call the function to plot the combined data
plot_combined_temp(samples_by_year, combined)


##############################
########## USA ###############
##############################
if (!exists("distr_deltaT_usa")) {
  x_t <- cumsum(data.carbon1$World[57:66])* 3.664/1000 # 57:66 means 2015-2024, /1000 means changing from Mt to Gt
  x_t <- x_t /1000 # changing from Gt to 1000Gt
  y_t <- real_data[166:175, 'V6'] # 166:year2015, 175:year2024
  alpha_post_mean = alpha_post_mean51
  beta_post_mean = beta_post_mean51
  cov_matrix = cov_matrix51
  
  set.seed(2024)
  par_samples <- mvrnorm(n = 1000, mu = c(alpha_post_mean, beta_post_mean), Sigma = cov_matrix)
  alpha_sample = par_samples[,1]
  beta_sample = par_samples[,2]
  
  phi_post <- list()
  sigma_squared_post <- list()
  for (i in 1:1000){
    alpha = alpha_sample[i]
    beta = beta_sample[i]
    
    # Compute u_t
    u_t <- y_t - (alpha + beta * x_t)
    
    # Create lagged version of u_t
    u_lag <- dplyr::lag(u_t)
    
    # Remove NA values due to lag
    temp_data <- data.frame(u_t = u_t, u_lag = u_lag) %>% na.omit()
    
    # Estimate phi using OLS
    phi_model <- lm(u_t ~ u_lag, data = temp_data)
    
    # Get the estimated value of phi
    phi_post[i] <- coef(phi_model)[2]
    
    # Compute residuals of the AR(1) model
    epsilon_t <- residuals(phi_model)
    
    # Estimate sigma^2
    sigma_squared_post[i] <- var(epsilon_t)
  }
  #######################
  ###
  distr_deltaT_usa = list()
  for (i in 1:1000){
    proj_co2_2024 <- data.frame(
      Year = proj.evals.2024.adjusted$trajs.annual.worldwide.usa[[i]]$Year,#i.e 2025:2100
      CO2 = proj.evals.2024.adjusted$trajs.annual.worldwide.usa[[i]]$CO2 / 1e12 # 1000 gigaton
    )
    proj_co2_2024$cumCO2 <- cumsum(proj_co2_2024$CO2)
    
    start_co2 <- proj_co2_2024[1,] # 2024 be the baseline
    delta_x <- data.frame(
      Year = proj_co2_2024$Year[2:dim(proj_co2_2024)[1]],#i.e 2025:2100
      deltaCO2 = proj_co2_2024$cumCO2[2:dim(proj_co2_2024)[1]] - start_co2$cumCO2 # 1 gigaton
    )
    
    
    parameters <- compute_distr_deltaT(delta_x=delta_x, 
                                       t1=2024, 
                                       phi_post=phi_post, 
                                       sigma_squared_post=sigma_squared_post, 
                                       beta_post_mean=beta_post_mean,
                                       cov_matrix=cov_matrix)
    
    # Sampling 1 value from the normal distribution for each year
    parameters$temp_change <- mapply(function(mean, var) {
      rnorm(1, mean, sqrt(var))
    }, parameters$mean, parameters$var)
    
    distr_deltaT_usa$Year = delta_x$Year
    distr_deltaT_usa$parameters[[i]]<- parameters[,c("Year", "mean", "var")]
    distr_deltaT_usa$temp.change.traj[[i]] <- parameters$temp_change
    
  }
  
  
  
  all_samples <- do.call(cbind, distr_deltaT_usa$temp.change.traj)
  quantiles_list <- apply(all_samples, 1, function(x) {
    c(
      Q025 = quantile(x, 0.025),
      Q050 = quantile(x, 0.050),
      Q500 = quantile(x, 0.500),
      Q950 = quantile(x, 0.950),
      Q975 = quantile(x, 0.975),
      mean = mean(x)
    )
  })
  
  
  distr_deltaT_usa$temp.change.quantile <- data.frame(
    Year = distr_deltaT_usa$Year,
    t(quantiles_list)
  )
  colnames(distr_deltaT_usa$temp.change.quantile) <- c(
    "Year", "Q025", "Q050", "Q500", "Q950", "Q975", "mean"
  )
  
  head(distr_deltaT_usa$temp.change.quantile)
}

#############################

summary_anomaly_1960 <- subset(summary_anomaly, year >= 1960)
other_estimates <- data.frame(year = 2006:2024, observed_anomaly = real_data$V6[157:175] - mean(real_data$V6[12:31])) # change to 175 due to 2024
other_quantiles <- other_estimates$observed_anomaly +
  matrix(rep(qnorm(c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)), each = 19), nrow=19) # change to 19
other_quantiles <- data.frame(other_quantiles)
names(other_quantiles) <- c("q025", "q050", "q100", "median", "q900", "q950", "q975")
other_estimates <- cbind(other_estimates, other_quantiles)
summary_anomaly_1960 <- rbind(summary_anomaly_1960, other_estimates)




samples_by_year<-list()
samples_by_year$t1 = 2024
samples_by_year$start.temp.change <- summary_anomaly_1960$observed_anomaly[nrow(summary_anomaly_1960)]
samples_by_year$hist.temp.change <- summary_anomaly_1960
samples_by_year$proj.temp.anomaly <- distr_deltaT_usa$temp.change.quantile
samples_by_year$proj.temp.anomaly[, c("Q025", "Q050", "Q500", "Q950", "Q975","mean")] <-
  samples_by_year$proj.temp.anomaly[, c("Q025", "Q050", "Q500", "Q950", "Q975","mean")] +
  samples_by_year$start.temp.change


samples_by_year$proj.temp.anomaly$Year <- as.numeric(samples_by_year$proj.temp.anomaly$Year)
# Create a data frame for the historical and future data combined
historical_data <- samples_by_year$hist.temp.change
future_data <- samples_by_year$proj.temp.anomaly

library(ggplot2)

plot_combined_temp <- function(samples_by_year, summary_proj, title = 'Assembled Global Mean Temperature Forecast under USA Excluded Scenario') {
  # Prepare data
  summary_proj_none <- subset(summary_proj, adjusted == "USA Excluded")
  
  # Extract historical and future projection data from `samples_by_year`
  historical_data <- samples_by_year$hist.temp.change
  future_data <- samples_by_year$proj.temp.anomaly
  
  # Create combined plot
  p <- ggplot() +
    # Plot historical data (1960-2024) in black
    geom_line(data = historical_data, aes(x = year, y = observed_anomaly, color = "Historical Data"), size = 0.5) +
    # Plot future median projection from `samples_by_year` in red
    geom_line(data = future_data, aes(x = Year, y = Q500, color = "BMA median projection"), size = 1, linetype = "solid") +
    # Add 95% and 90% confidence intervals for `samples_by_year` in red
    geom_ribbon(data = future_data, aes(x = Year, ymin = Q025, ymax = Q975, fill = "95% CI (BMA)"), alpha = 0.2) +
    geom_ribbon(data = future_data, aes(x = Year, ymin = Q050, ymax = Q950, fill = "90% CI (BMA)"), alpha = 0.4) +
    # Plot TCRE projection from `combined2024` in #4daf4a green
    geom_line(data = summary_proj_none, aes(x = year, y = median, color = "TCRE median projection"), size = 1, linetype = "dashed") +
    # Add 95% and 90% confidence intervals for `combined2024` in #4daf4a green
    geom_ribbon(data = summary_proj_none, aes(x = year, ymin = q025, ymax = q975, fill = "95% CI (TCRE)"), alpha = 0.2) +
    geom_ribbon(data = summary_proj_none, aes(x = year, ymin = q050, ymax = q950, fill = "90% CI (TCRE)"), alpha = 0.4) +
    # Add a vertical dashed line at 2025
    geom_vline(xintercept = 2025, linetype = "dashed", color = "blue", size = 1) +
    # Add text annotation for 2025
    annotate("text", x = 2025, y = max(c(historical_data$observed_anomaly, future_data$mean)), label = "2025", color = "blue", vjust = -0.5) +
    # Labels and title
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)",
      title = title,
      color = "Legend",
      fill = "Confidence Intervals"
    ) +
    theme_minimal() +
    #ylim(-0.5, 4.5) +
    scale_x_continuous(limits = c(1960, 2100), breaks = c(1960, 2000, 2050, 2100)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    # Custom colors for lines and ribbons
    scale_color_manual(values = c("BMA median projection" = "red", "TCRE median projection" = "#4daf4a", "Historical Data" = "black")) +
    scale_fill_manual(values = c("95% CI (BMA)" = "red", "90% CI (BMA)" = "red", "95% CI (TCRE)" = "#4daf4a", "90% CI (TCRE)" = "#4daf4a"))
  
  return(p)
}

# Call the function to plot the combined data
plot_combined_temp(samples_by_year, combined)

