#Code to carry out the vital signs analyses in the Philips eICU-CRD v2.0
#Making use of the nurseCharting data tables
#This example uses central venous pressure from nurseCharting, but temperature, heart rate and oxygen saturation used the same format
#note that con is the dbConnect() argument specific to our connection

library(tidyverse)
library(RPostgreSQL)

command <- "select nursingchartvalue, patientUnitStayID from nurseCharting where nursingchartcelltypevallabel = 'CVP' and nursingchartvalue is not null;"
RNdata0 <- dbGetQuery(con, command)
RNdata <- transform(RNdata0, nursingchartvalue = as.numeric(nursingchartvalue))

lower_limit <- 1 
upper_limit <- 50

err_function <- function(vital_data, lower_limit, upper_limit){ 
  vital_data <- mutate(vital_data, err = ((vital_data[,1] < lower_limit)|(vital_data[,1]> upper_limit)))
  return(vital_data)
}

vital_stats <- function(vital_data){
  
  #Raw data stats:
  ntotal <- nrow(vital_data) #Number of data points
  percent_orig = 100*ntotal/ntotal #Should be 100
  mean_orig <- mean(vital_data[,1])
  IQR_orig <- IQR(vital_data[,1])
  median_orig <- median(vital_data[,1])
  
  #Stats for the realistic data
  
  #vital_data <- mutate(vital_data, err = ((vital_data[,1] < lower_limit)|(vital_data[,1]> upper_limit)))
  real <- filter(vital_data, err == FALSE)
  nreal <- nrow(real) #Number of points that are within realistic limits
  percent_real = 100*nreal/ntotal
  mean_real <- mean(real[,1])
  IQR_real <- IQR(real[,1])
  median_real <- median(real[,1])
  
  #Stats for errors.
  nerr = ntotal - nreal #Number of points outside realistic limits
  percent_err = 100*nerr/ntotal
  
  output <- tibble(dataset = c("original", "realistic", "error"), N = c(ntotal, nreal, nerr), percent = c(percent_orig, percent_real, percent_err), mean = c(mean_orig, mean_real, NA), median = c(median_orig, median_real, NA), IQR = c(IQR_orig, IQR_real, NA))
  output 
}

#1 - calls error function (function of upper and lower limits)
data <- err_function(RNdata, lower_limit, upper_limit)

#2 - stats on the vitals themselves
vital_stats_output <-vital_stats(data)
