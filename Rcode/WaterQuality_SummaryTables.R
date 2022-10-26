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

wq_summary <- function(df, input_var){
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

########## Main program ###########
TP_summary <- wq_summary(df=waterquality, input_var='TP_ppb')
TP_summary_conn <- wq_summary_conn(df=waterquality, input_var='TP_ppb')
DOC_summary <- wq_summary(df=waterquality, input_var='DOC_ppm')
