################## Water quality summary stats tables #########################
# Date: 10-26-22
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

#### D-fine functions ####
# summary stats table for burned vs. control, differentiating drainage vs. isolated
wq_summary_conn <- function(df, input_var){
  #df: data frame
  #input_var: water quality variable (column) in a data frame
  output <- df %>%
    group_by(Month, Group) %>%
    summarize(min=min(!!sym(input_var), na.rm=T), median=median(!!sym(input_var), na.rm=T),
              mean=mean(!!sym(input_var), na.rm=T), max=max(!!sym(input_var), na.rm=T), 
              sd=sd(!!sym(input_var), na.rm=T),nSamples=n(),
              .groups="keep")
  return(output)
}

# summary stats table for burned vs. control
wq_summary_basic <- function(df, input_var){
  #df: data frame
  #input_var: water quality variable (column) in a data frame
  output <- df %>%
    group_by(Month, Type) %>%
    summarize(min=min(!!sym(input_var), na.rm=T), median=median(!!sym(input_var), na.rm=T),
              mean=mean(!!sym(input_var), na.rm=T), max=max(!!sym(input_var), na.rm=T), 
              sd=sd(!!sym(input_var), na.rm=T),nSamples=n(),
              .groups="keep")
  return(output)
}
# original code on which function is based
# TP_summary <- waterquality %>%
#   group_by(Month_factor, Group) %>%
#   summarize(min=min(TP_ppb, na.rm=T), median=median(TP_ppb, na.rm=T),
#             mean=mean(TP_ppb, na.rm=T), max=max(TP_ppb, na.rm=T), sd=sd(TP_ppb),
#             nSamples=n(),.groups="keep")


# build overall summary table row by row (row=water quality variable)
wq_summary_row <- function(df, input_var) {
  #df: data frame
  #input_var: water quality variable (column) in a data frame
  my_row <- paste0('(',df$min, ', ', df$median, 
                   ', ', df$max, ')')
  row_burned <- subset(df, Type=='Burned')
  row_control <- subset(df, Type=='Control')
  
  allmon_min_burned <- min(row_burned$min, na.rm=T)
  allmon_med_burned <- median(row_burned$median, na.rm=T)
  allmon_max_burned <- max(row_burned$max, na.rm=T)
  allmon_all_burned <- paste0('(',allmon_min_burned, ', ', allmon_med_burned, 
                              ', ', allmon_max_burned, ')')
  
  allmon_min_control <- min(row_control$min, na.rm=T)
  allmon_med_control <- median(row_control$median, na.rm=T)
  allmon_max_control <- max(row_control$max, na.rm=T)
  allmon_all_control <- paste0('(',allmon_min_control, ', ', allmon_med_control, 
                               ', ', allmon_max_control, ')')
  full_row <- c(input_var, my_row, allmon_all_burned, allmon_all_control)
  return(full_row)
}

########## Main program ###########
TP_summary_basic <- wq_summary_basic(df=waterquality, input_var='TP_ppb')
TP_summary_conn <- wq_summary_conn(df=waterquality, input_var='TP_ppb')

TN_summary_basic <- wq_summary_basic(df=waterquality, input_var='TN_ppb')
TN_summary_conn <- wq_summary_conn(df=waterquality, input_var='TN_ppb')

DOC_summary_basic <- wq_summary_basic(df=waterquality, input_var='DOC_ppm')
DOC_summary_conn <- wq_summary_conn(df=waterquality, input_var='DOC_ppm')

TSS_summary_basic <- wq_summary_basic(df=waterquality, input_var='TSS_mgL')
TSS_summary_conn <- wq_summary_conn(df=waterquality, input_var='TSS_mgL')

Secchi_summary_basic <- wq_summary_basic(df=waterquality, input_var='SecchiDepth_m')
Secchi_summary_conn <- wq_summary_conn(df=waterquality, input_var='SecchiDepth_m')

Chloro_summary_basic <- wq_summary_basic(df=waterquality, input_var='Chloro_ppb')
Chloro_summary_conn <- wq_summary_conn(df=waterquality, input_var='Chloro_ppb')

pH_summary_basic <- wq_summary_basic(df=waterquality, input_var='pH')
pH_summary_conn <- wq_summary_conn(df=waterquality, input_var='pH')

Temp_summary_basic <- wq_summary_basic(df=waterquality, input_var='WaterTemp_C')
Temp_summary_conn <- wq_summary_conn(df=waterquality, input_var='WaterTemp_C')

ANC_summary_basic <- wq_summary_basic(df=waterquality, input_var='ANC_mgCaCO3L')
ANC_summary_conn <- wq_summary_conn(df=waterquality, input_var='ANC_mgCaCO3L')

zMax_summary_basic <- wq_summary_basic(df=waterquality, input_var='zMax_m')
zMax_summary_conn <- wq_summary_conn(df=waterquality, input_var='zMax_m')


#### Major overall summary table ####
columns <- c('Variable','MayBurned','MayControl','JunBurned','JunControl','JulBurned','JulControl',
             'AugBurned','AugControl','SepBurned','SepControl','AllMonthsBurned','AllMonthsControl')

major_df <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = 9,
                          ncol = 13))
colnames(major_df) <- columns

major_df[1,] <- wq_summary_row(df=TP_summary_basic, input_var='TP')
major_df[2,] <- wq_summary_row(df=TN_summary_basic, input_var='TN')
major_df[3,] <- wq_summary_row(df=DOC_summary_basic, input_var='DOC')
major_df[4,] <- wq_summary_row(df=TSS_summary_basic, input_var='TSS')
major_df[5,] <- wq_summary_row(df=Secchi_summary_basic, input_var='Secchi')
major_df[6,] <- wq_summary_row(df=Chloro_summary_basic, input_var='Chloro')
major_df[7,] <- wq_summary_row(df=pH_summary_basic, input_var='pH')
major_df[8,] <- wq_summary_row(df=Temp_summary_basic, input_var='Temp')
major_df[9,] <- wq_summary_row(df=ANC_summary_basic, input_var='ANC')

#write.csv(major_df, "Data/WaterQuality/minmedianmaxWQ_summary.csv", row.names=F)

