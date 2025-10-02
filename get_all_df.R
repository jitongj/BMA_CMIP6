setwd("~/Desktop/BMA")
load("cleaned_all_data_bma_adjusted_cmip6.Rda")
cleaned_all_data <- cleaned_all_data_bma_adjusted # annual data, not cum

setwd("~/Documents/UW Courses/Research/CO2_Data_wpp2024")
data.location <- "NatureData/"

rcp.colors <- c("green", "red", "black", "purple")
library(readxl)
rcp.carbon.yearly.tmp <- read_excel(paste0(data.location, "ssp_db_carbon_emissions.xlsx"),
                                    n_max = 4)
rcp.carbon.yearly <- rcp.carbon.yearly.tmp[, c("Scenario", "Unit",
                                               c(2000, 2005, 2010, 2015, seq(2020, 2100, by=10)))] #changed

rcp.carbon.yearly[, -c(1,2)] <- (1/1000) * rcp.carbon.yearly[, -c(1,2)] # 1/1000 means Mt to Pg
rcp.carbon.yearly$Unit <- rep("PgCO2/yr", 4)
names(rcp.carbon.yearly)[3:length(names(rcp.carbon.yearly))] <- paste0("Carbon", names(rcp.carbon.yearly)[3:length(names(rcp.carbon.yearly))])

rcp.carbon.yearly$Scenario <- c("SSP3-7.0", "SSP2-4.5", "SSP1-2.6", "SSP5-8.5")

rcp.carbon.cum <- rcp.carbon.yearly[, c("Scenario", "Unit",
                                        paste0("Carbon", seq(2020, 2100, by=10)))] # changed: 2020

rcp.carbon.yearly.complete <- rcp.carbon.yearly[, 1:2]
for (year in 2015:2100)
{
  var.name <- paste0('Carbon', year)
  if (year == 2015 || (year %% 10 == 0 && year <= 2100))
  {
    rcp.carbon.yearly.complete[, var.name] <- rcp.carbon.yearly[, var.name]
  }
  else if (year < 2020)
  {
    rcp.carbon.yearly.complete[, var.name] <- (2020 - year)/5 * rcp.carbon.yearly[, 'Carbon2015'] + 
      (year - 2015)/5 * rcp.carbon.yearly[, 'Carbon2020']
  }
  else if (year < 2100)
  {
    rcp.carbon.yearly.complete[, var.name] <- (10 - year %% 10) / 10 * rcp.carbon.yearly[, paste0('Carbon', year - (year %% 10))] + 
      (year %% 10) / 10 * rcp.carbon.yearly[, paste0('Carbon', year - (year %% 10) + 10)]
  }
  else
  {
    rcp.carbon.yearly.complete[, var.name] <- rcp.carbon.yearly[, 'Carbon2100']
  }
}

rcp.carbon.cum.complete <- rcp.carbon.yearly.complete
# for (i in 1:4)
# {
#   rcp.carbon.cum.complete[i, 4:98] <- cumsum(as.numeric(rcp.carbon.yearly.complete[i, 4:88]))
# }

for (i in 1:4) {
  cumulative_values <- cumsum(as.numeric(rcp.carbon.yearly.complete[i, 4:88])) #changed to 88, 4:2016,88: 2100
  rcp.carbon.cum.complete[i, 4:88] <- matrix(cumulative_values, nrow = 1) #changed to 88
}

rcp.carbon.cum.complete <- rcp.carbon.cum.complete[,-3]

rcp.carbon.cum.complete.adjusted <- t(rcp.carbon.cum.complete[, -c(1,2)])
rcp.carbon.cum.complete.adjusted <- as.data.frame(rcp.carbon.cum.complete.adjusted)
rcp.carbon.cum.complete.adjusted <- rcp.carbon.cum.complete.adjusted[, c(3,2,1,4)]
rcp.carbon.cum.complete.adjusted$year <- 2016:2100
names(rcp.carbon.cum.complete.adjusted)[1:4] <- paste0('ssp', c(126, 245, 370, 585)) 

rcp.carbon.cum <- rcp.carbon.yearly[, c("Scenario", "Unit",
                                        paste0("Carbon", seq(2020, 2100, by=10)))]
carbon.cum <- rcp.carbon.yearly[, paste0('Carbon', 2015)] * 2.5 + rcp.carbon.yearly[, paste0('Carbon', 2020)] * 2.5 
for (year in seq(2020, 2100, by=10)) {
  var.name.tmp <- paste0("Carbon", year)
  if (year == 2020) {
    rcp.carbon.cum[, var.name.tmp] <- carbon.cum
    carbon.cum <- carbon.cum + 5*rcp.carbon.yearly[, var.name.tmp]
  } else if (year == 2100) {
    carbon.cum <- carbon.cum + 5*rcp.carbon.yearly[, var.name.tmp]
    rcp.carbon.cum[, var.name.tmp] <- carbon.cum
  } else {
    carbon.cum <- carbon.cum + 5*rcp.carbon.yearly[, var.name.tmp]
    rcp.carbon.cum[, var.name.tmp] <- carbon.cum
    carbon.cum <- carbon.cum + 5*rcp.carbon.yearly[, var.name.tmp]
  }
}


all_models_split <- unlist(strsplit(names(cleaned_all_data), '_s_'))[-1]
all_models <- unique(all_models_split[seq(2, length(all_models_split), 4)]) # 12110 = length(all_models_split)-7

# delete_models <-c("CIESM", "CMCC-CM2-SR5", "FIO-ESM-2-0", "GISS-E2-1-G-f2", "GISS-E2-1-G-p1","HadGEM3-GC31-LL", "HadGEM3-GC31-LL-f3",
#                   "HadGEM3-GC31-MM","HadGEM3-GC31-MM-f3","NESM3")
#all_models <- all_models[!all_models %in% delete_models]

selected_indices <- c()
for (model in all_models)
{
  all_trajectories <- all_models_split[which(all_models_split == model) + 2] # find .dat
  all_scenarios <- all_models_split[which(all_models_split == model) + 1] # find scenario, like "ssp126"
  indices <- (which(all_models_split == model) + 2) / 4 # find index of each model (full name)
  if ('ave.dat' %in% all_trajectories)
  {
  selected_indices <- c(selected_indices, indices[which(all_trajectories == 'ave.dat')]) #indices[which(all_trajectories == 'ave.dat')]: index of the model with
  }
  else if ('192ave.dat' %in% all_trajectories)
  {
  selected_indices <- c(selected_indices, indices[which(all_trajectories == '192ave.dat')])
  }
}


all_df <- data.frame()
for (index in (selected_indices + 1))
{
  temp_df <- cleaned_all_data[, c(1, index)]
  names(temp_df) <- c('year', 'temperature')
  #pre_industrial_avg <- mean(subset(temp_df, year >= 1995 & year <= 2014)$temperature)
  temp_df$temperature <- temp_df$temperature - 273.15#- pre_industrial_avg
  temp_df <- subset(temp_df, year >= 2016 & year <= 2100)
  val <- strsplit(names(cleaned_all_data)[index], split = '_s_')[[1]]
  temp_df$model <- val[2]
  temp_df$scenario <- val[3]
  temp_df$co2 <- rcp.carbon.cum.complete.adjusted[, val[3]]
  all_df <- rbind(all_df, temp_df)
}
setwd("~/Desktop/BMA")
#save(all_df, file = "all_df_allmodel.RData")
save(all_df, file = "all_df_allmodel_new.RData")
##
load('real_hadcrut5.Rda')
real_data = hadcrut5
hist_df_2016_2023 <- real_data[167:174, 'V6'] # 167:year2016, 174:year2023
hist_df_1995_2023 <- real_data[146:174, 'V6'] # 146:year1995, 166:year2015

