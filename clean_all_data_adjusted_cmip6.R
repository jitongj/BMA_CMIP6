setwd("~/Desktop")
load("cleaned_all_data_bma_cmip6.Rda")

cleaned_all_data_bma_adjusted <- cleaned_all_data_bma$Average[,1:(length(cleaned_all_data_bma$Average)-5)]
#CanESM5-CanOE
# ssp126
columns_to_average <- c('_s_CanESM5-CanOE_s_ssp126_s_000.dat', '_s_CanESM5-CanOE_s_ssp126_s_001.dat', '_s_CanESM5-CanOE_s_ssp126_s_002.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CanESM5-CanOE_s_ssp126_s_ave.dat` <- average_elements
# ssp245
columns_to_average <- c('_s_CanESM5-CanOE_s_ssp245_s_000.dat', '_s_CanESM5-CanOE_s_ssp245_s_001.dat', '_s_CanESM5-CanOE_s_ssp245_s_002.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CanESM5-CanOE_s_ssp245_s_ave.dat` <- average_elements
# ssp370
columns_to_average <- c('_s_CanESM5-CanOE_s_ssp370_s_000.dat', '_s_CanESM5-CanOE_s_ssp370_s_001.dat', '_s_CanESM5-CanOE_s_ssp370_s_002.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CanESM5-CanOE_s_ssp370_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_CanESM5-CanOE_s_ssp585_s_000.dat', '_s_CanESM5-CanOE_s_ssp585_s_001.dat', '_s_CanESM5-CanOE_s_ssp585_s_002.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CanESM5-CanOE_s_ssp585_s_ave.dat` <- average_elements


#CNRM-CM6-1
# ssp126
columns_to_average <- c('_s_CNRM-CM6-1_s_ssp126_s_000.dat', '_s_CNRM-CM6-1_s_ssp126_s_001.dat', '_s_CNRM-CM6-1_s_ssp126_s_002.dat', '_s_CNRM-CM6-1_s_ssp126_s_003.dat', '_s_CNRM-CM6-1_s_ssp126_s_004.dat', '_s_CNRM-CM6-1_s_ssp126_s_005.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CNRM-CM6-1_s_ssp126_s_ave.dat` <- average_elements
# ssp245
columns_to_average <- c('_s_CNRM-CM6-1_s_ssp245_s_000.dat', '_s_CNRM-CM6-1_s_ssp245_s_001.dat', '_s_CNRM-CM6-1_s_ssp245_s_002.dat', '_s_CNRM-CM6-1_s_ssp245_s_003.dat', '_s_CNRM-CM6-1_s_ssp245_s_004.dat', '_s_CNRM-CM6-1_s_ssp245_s_005.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CNRM-CM6-1_s_ssp245_s_ave.dat` <- average_elements
# ssp370
columns_to_average <- c('_s_CNRM-CM6-1_s_ssp370_s_000.dat', '_s_CNRM-CM6-1_s_ssp370_s_001.dat', '_s_CNRM-CM6-1_s_ssp370_s_002.dat', '_s_CNRM-CM6-1_s_ssp370_s_003.dat', '_s_CNRM-CM6-1_s_ssp370_s_004.dat', '_s_CNRM-CM6-1_s_ssp370_s_005.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CNRM-CM6-1_s_ssp370_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_CNRM-CM6-1_s_ssp585_s_000.dat', '_s_CNRM-CM6-1_s_ssp585_s_001.dat', '_s_CNRM-CM6-1_s_ssp585_s_002.dat', '_s_CNRM-CM6-1_s_ssp585_s_003.dat', '_s_CNRM-CM6-1_s_ssp585_s_004.dat', '_s_CNRM-CM6-1_s_ssp585_s_005.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CNRM-CM6-1_s_ssp585_s_ave.dat` <- average_elements

#CNRM-CM6-1-HR
# ssp126
columns_to_average <- c('_s_CNRM-CM6-1-HR_s_ssp126_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_CNRM-CM6-1-HR_s_ssp126_s_ave.dat` <- average_elements
# ssp245
columns_to_average <- c('_s_CNRM-CM6-1-HR_s_ssp245_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_CNRM-CM6-1-HR_s_ssp245_s_ave.dat` <- average_elements
# ssp370
columns_to_average <- c('_s_CNRM-CM6-1-HR_s_ssp370_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_CNRM-CM6-1-HR_s_ssp370_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_CNRM-CM6-1-HR_s_ssp585_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_CNRM-CM6-1-HR_s_ssp585_s_ave.dat` <- average_elements




#CNRM-ESM2-1
# ssp126
columns_to_average <- c('_s_CNRM-ESM2-1_s_ssp126_s_000.dat', '_s_CNRM-ESM2-1_s_ssp126_s_001.dat', '_s_CNRM-ESM2-1_s_ssp126_s_002.dat', '_s_CNRM-ESM2-1_s_ssp126_s_003.dat', '_s_CNRM-ESM2-1_s_ssp126_s_004.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CNRM-ESM2-1_s_ssp126_s_ave.dat` <- average_elements
# ssp245
columns_to_average <- c('_s_CNRM-ESM2-1_s_ssp245_s_000.dat', '_s_CNRM-ESM2-1_s_ssp245_s_001.dat', '_s_CNRM-ESM2-1_s_ssp245_s_002.dat', '_s_CNRM-ESM2-1_s_ssp245_s_003.dat', '_s_CNRM-ESM2-1_s_ssp245_s_004.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CNRM-ESM2-1_s_ssp245_s_ave.dat` <- average_elements
# ssp370
columns_to_average <- c('_s_CNRM-ESM2-1_s_ssp370_s_000.dat', '_s_CNRM-ESM2-1_s_ssp370_s_001.dat', '_s_CNRM-ESM2-1_s_ssp370_s_002.dat', '_s_CNRM-ESM2-1_s_ssp370_s_003.dat', '_s_CNRM-ESM2-1_s_ssp370_s_004.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CNRM-ESM2-1_s_ssp370_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_CNRM-ESM2-1_s_ssp585_s_000.dat', '_s_CNRM-ESM2-1_s_ssp585_s_001.dat', '_s_CNRM-ESM2-1_s_ssp585_s_002.dat', '_s_CNRM-ESM2-1_s_ssp585_s_003.dat', '_s_CNRM-ESM2-1_s_ssp585_s_004.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_CNRM-ESM2-1_s_ssp585_s_ave.dat` <- average_elements



#GISS-E2-1-G
# ssp126
columns_to_average <- c('_s_GISS-E2-1-G_s_ssp126_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_GISS-E2-1-G_s_ssp126_s_ave.dat` <- average_elements
# ssp245
columns_to_average <- c('_s_GISS-E2-1-G_s_ssp245_s_000.dat', '_s_GISS-E2-1-G_s_ssp245_s_001.dat', '_s_GISS-E2-1-G_s_ssp245_s_002.dat', '_s_GISS-E2-1-G_s_ssp245_s_003.dat', '_s_GISS-E2-1-G_s_ssp245_s_004.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_GISS-E2-1-G_s_ssp245_s_ave.dat` <- average_elements
# ssp370
columns_to_average <- c('_s_GISS-E2-1-G_s_ssp370_s_000.dat', '_s_GISS-E2-1-G_s_ssp370_s_001.dat', '_s_GISS-E2-1-G_s_ssp370_s_002.dat', '_s_GISS-E2-1-G_s_ssp370_s_003.dat', '_s_GISS-E2-1-G_s_ssp370_s_004.dat', '_s_GISS-E2-1-G_s_ssp370_s_005.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_GISS-E2-1-G_s_ssp370_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_GISS-E2-1-G_s_ssp585_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_GISS-E2-1-G_s_ssp585_s_ave.dat` <- average_elements



#GISS-E2-1-G-f2
# ssp370
columns_to_average <- c('_s_GISS-E2-1-G-f2_s_ssp370_s_192005.dat', '_s_GISS-E2-1-G-f2_s_ssp370_s_192006.dat', '_s_GISS-E2-1-G-f2_s_ssp370_s_192007.dat', '_s_GISS-E2-1-G-f2_s_ssp370_s_192008.dat', '_s_GISS-E2-1-G-f2_s_ssp370_s_192009.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_GISS-E2-1-G-f2_s_ssp370_s_192ave.dat` <- average_elements


#HadGEM3-GC31-LL
# ssp126
columns_to_average <- c('_s_HadGEM3-GC31-LL_s_ssp126_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_HadGEM3-GC31-LL_s_ssp126_s_ave.dat` <- average_elements
# ssp245
columns_to_average <- c('_s_HadGEM3-GC31-LL_s_ssp245_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_HadGEM3-GC31-LL_s_ssp245_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_HadGEM3-GC31-LL_s_ssp585_s_000.dat', '_s_HadGEM3-GC31-LL_s_ssp585_s_001.dat', '_s_HadGEM3-GC31-LL_s_ssp585_s_002.dat', '_s_HadGEM3-GC31-LL_s_ssp585_s_003.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_HadGEM3-GC31-LL_s_ssp585_s_ave.dat` <- average_elements



#HadGEM3-GC31-MM
# ssp126
columns_to_average <- c('_s_HadGEM3-GC31-MM_s_ssp126_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_HadGEM3-GC31-MM_s_ssp126_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_HadGEM3-GC31-MM_s_ssp585_s_000.dat','_s_HadGEM3-GC31-MM_s_ssp585_s_001.dat','_s_HadGEM3-GC31-MM_s_ssp585_s_002.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_HadGEM3-GC31-MM_s_ssp585_s_ave.dat` <- average_elements

#MCM-UA-1-0
# ssp245
columns_to_average <- c('_s_MCM-UA-1-0_s_ssp245_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_MCM-UA-1-0_s_ssp245_s_ave.dat` <- average_elements
# ssp370
columns_to_average <- c('_s_MCM-UA-1-0_s_ssp370_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_MCM-UA-1-0_s_ssp370_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_MCM-UA-1-0_s_ssp585_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_MCM-UA-1-0_s_ssp585_s_ave.dat` <- average_elements

#MIROC-ES2L
# ssp126
columns_to_average <- c('_s_MIROC-ES2L_s_ssp126_s_000.dat','_s_MIROC-ES2L_s_ssp126_s_001.dat','_s_MIROC-ES2L_s_ssp126_s_002.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_MIROC-ES2L_s_ssp126_s_ave.dat` <- average_elements
# ssp245
columns_to_average <- c('_s_MIROC-ES2L_s_ssp245_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_MIROC-ES2L_s_ssp245_s_ave.dat` <- average_elements
# ssp370
columns_to_average <- c('_s_MIROC-ES2L_s_ssp370_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_MIROC-ES2L_s_ssp370_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_MIROC-ES2L_s_ssp585_s_000.dat')
average_elements <- cleaned_all_data_bma$Average[ , columns_to_average]
cleaned_all_data_bma_adjusted$`_s_MIROC-ES2L_s_ssp585_s_ave.dat` <- average_elements



#UKESM1-0-LL
# ssp126
columns_to_average <- c('_s_UKESM1-0-LL_s_ssp126_s_000.dat','_s_UKESM1-0-LL_s_ssp126_s_001.dat','_s_UKESM1-0-LL_s_ssp126_s_002.dat','_s_UKESM1-0-LL_s_ssp126_s_003.dat','_s_UKESM1-0-LL_s_ssp126_s_004.dat','_s_UKESM1-0-LL_s_ssp126_s_005.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_UKESM1-0-LL_s_ssp126_s_ave.dat` <- average_elements
# ssp245
columns_to_average <- c('_s_UKESM1-0-LL_s_ssp245_s_000.dat','_s_UKESM1-0-LL_s_ssp245_s_001.dat','_s_UKESM1-0-LL_s_ssp245_s_002.dat','_s_UKESM1-0-LL_s_ssp245_s_003.dat','_s_UKESM1-0-LL_s_ssp245_s_004.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_UKESM1-0-LL_s_ssp245_s_ave.dat` <- average_elements
# ssp370
columns_to_average <- c('_s_UKESM1-0-LL_s_ssp370_s_000.dat','_s_UKESM1-0-LL_s_ssp370_s_001.dat','_s_UKESM1-0-LL_s_ssp370_s_002.dat','_s_UKESM1-0-LL_s_ssp370_s_003.dat','_s_UKESM1-0-LL_s_ssp370_s_004.dat',
                        '_s_UKESM1-0-LL_s_ssp370_s_005.dat','_s_UKESM1-0-LL_s_ssp370_s_006.dat', '_s_UKESM1-0-LL_s_ssp370_s_007.dat', '_s_UKESM1-0-LL_s_ssp370_s_008.dat', '_s_UKESM1-0-LL_s_ssp370_s_009.dat',
                        '_s_UKESM1-0-LL_s_ssp370_s_010.dat', '_s_UKESM1-0-LL_s_ssp370_s_011.dat', '_s_UKESM1-0-LL_s_ssp370_s_012.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_UKESM1-0-LL_s_ssp370_s_ave.dat` <- average_elements
# ssp585
columns_to_average <- c('_s_UKESM1-0-LL_s_ssp585_s_000.dat','_s_UKESM1-0-LL_s_ssp585_s_001.dat','_s_UKESM1-0-LL_s_ssp585_s_002.dat','_s_UKESM1-0-LL_s_ssp585_s_003.dat','_s_UKESM1-0-LL_s_ssp585_s_004.dat')
sum_elements <- rowSums(cleaned_all_data_bma$Average[ , columns_to_average])
average_elements <- sum_elements / length(columns_to_average)
cleaned_all_data_bma_adjusted$`_s_UKESM1-0-LL_s_ssp585_s_ave.dat` <- average_elements

path <- "~/Desktop/cleaned_all_data_bma_adjusted_cmip6.Rda"
save(cleaned_all_data_bma_adjusted, file = path)
