#Code to carry out blood pressure analyses in the Philips eICU-CRD v2.0
#Making use of the nurseCharting data table
#note that con is the dbConnect() argument specific to our connection

library(tidyverse)
library(RPostgreSQL)

command <- "select nursingchartvalue, patientunitstayid, nursingchartoffset, nursingchartcelltypevalname from nurseCharting where nursingchartcelltypevallabel = 'Non-Invasive BP' and nursingchartvalue is not null;"
RNdatalong <- dbGetQuery(con, command) #the values here are strings
#Put sBP, dBP and MAP on the same line:
RNdataWIDE <- RNdatalong %>% group_by_at(vars(-nursingchartvalue))%>% mutate(row_id=1:n()) %>% ungroup() %>% spread(key = nursingchartcelltypevalname, value = nursingchartvalue) %>% select(-row_id)
#Rename the columns to work with the below formula
bpRN.1 <- rename(RNdataWIDE, systemicsystolic = "Non-Invasive BP Systolic", systemicdiastolic = "Non-Invasive BP Diastolic", systemicmean = "Non-Invasive BP Mean")
#change columns to numeric
bpRN.2 <- transform(bpRN.1, patientunitstayid = as.numeric(patientunitstayid), systemicsystolic = as.numeric(systemicsystolic), systemicdiastolic = as.numeric(systemicdiastolic), systemicmean = as.numeric(systemicmean))

#################################################

bperr_function <- function(bp){
  
  bp.2 <- na.omit(bp)
  
  bp.3 <- bp.2 %>%
    mutate(sys_too_high = systemicsystolic >= 300) %>%
    mutate(dias_too_high = systemicdiastolic >= 200) %>%
    mutate(sys_neg = systemicsystolic<0) %>%
    mutate(dias_neg = systemicdiastolic<0) %>%
    mutate(mean_neg = systemicmean<0) %>% 
    mutate(sys_LT_mean = systemicsystolic<systemicmean) %>%
    mutate(sys_LT_dias = systemicsystolic<systemicdiastolic) %>%
    mutate(mean_LT_dias = systemicmean<systemicdiastolic) %>% 
    mutate(sys_equal_dias = systemicsystolic==systemicdiastolic) %>%
    mutate(sys_close_dias = abs(systemicsystolic-systemicdiastolic)<6) %>%
    mutate(pulse_pressure = systemicsystolic-systemicdiastolic) %>%
    mutate(err = (sys_too_high | dias_too_high |sys_neg | dias_neg | mean_neg | sys_LT_dias | sys_LT_mean |
                          mean_LT_dias | sys_close_dias))
  return(bp.3)
}

vital_stats <- function(vital_data){
  
  #Raw data stats:
  ntotal <- nrow(vital_data) #Number of data points
  percent_orig = 100*ntotal/ntotal #Should be 100
  mean_orig_sys <- mean(vital_data[,5])
  mean_orig_dias <- mean(vital_data[,3])
  mean_orig_map <- mean(vital_data[,4])
  IQR_orig_sys <- IQR(vital_data[,5])
  IQR_orig_dias <- IQR(vital_data[,3])
  IQR_orig_map <- IQR(vital_data[,4])
  median_orig_sys <- median(vital_data[,5])
  median_orig_dias <- median(vital_data[,3])
  median_orig_map <- median(vital_data[,4])
  
  #Stats for the realistic data
  
  #vital_data <- mutate(vital_data, err = ((vital_data[,1] < lower_limit)|(vital_data[,1]> upper_limit)))
  real <- filter(vital_data, err == FALSE)
  nreal <- nrow(real) #Number of points that are within realistic limits
  percent_real = 100*nreal/ntotal
  
  mean_real_sys <- mean(real[,5])
  mean_real_dias <- mean(real[,3])
  mean_real_map <- mean(real[,4])
  IQR_real_sys <- IQR(real[,5])
  IQR_real_dias <- IQR(real[,3])
  IQR_real_map <- IQR(real[,4])
  median_real_sys <- median(real[,5])
  median_real_dias <- median(real[,3])
  median_real_map <- median(real[,4])
  
  #Stats for errors.
  nerr = ntotal - nreal #Number of points outside realistic limits
  percent_err = 100*nerr/ntotal
  
  output <- tibble(dataset = c("original", "realistic", "error"), 
                   N = c(ntotal, nreal, nerr), 
                   percent = c(percent_orig, percent_real, percent_err), 
                   mean_sys = c(mean_orig_sys, mean_real_sys, NA), 
                   mean_dias = c(mean_orig_dias, mean_real_dias, NA), 
                   mean_map = c(mean_orig_map, mean_real_map, NA), 
                   median_sys = c(median_orig_sys, median_real_sys, NA), 
                   median_dias = c(median_orig_dias, median_real_dias, NA), 
                   median_map = c(median_orig_map, median_real_map, NA),
                   IQR_sys = c(IQR_orig_sys, IQR_real_sys, NA), 
                   IQR_dias = c(IQR_orig_dias, IQR_real_dias, NA), 
                   IQR_map = c(IQR_orig_map, IQR_real_map, NA))
  output
}

#1 - calls error function (function of upper and lower limits)
data <- bperr_function(bpRN.2)

#2 - stats on the vitals themselves
vital_stats_output <-vital_stats(data)

