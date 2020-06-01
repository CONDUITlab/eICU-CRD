

#############################################
# Analysis from the vitalsPeriodic table	#
# done via Postgresql (BigQuery)			#
#############################################

### heartrate 
# select count(heartrate) from `physionet-data.eicu_crd.vitalperiodic`
# 145979794
# select count(heartrate) from `physionet-data.eicu_crd.vitalperiodic` where heartrate > 300
# 0
# select count(heartrate) from `physionet-data.eicu_crd.vitalperiodic` where heartrate > 300
# 0

### sao2
# select count(sao2) from `physionet-data.eicu_crd.vitalperiodic`
# 132908266
# select count(sao2) from `physionet-data.eicu_crd.vitalperiodic` where sao2 < 50
# 65447
# select count(sao2) from `physionet-data.eicu_crd.vitalperiodic` where sao2 > 100

### temperature
# select count(temperature) from `physionet-data.eicu_crd.vitalperiodic`
# 13203289
# select count(temperature) from `physionet-data.eicu_crd.vitalperiodic` where temperature > 45
# 401283
# select count(temperature) from `physionet-data.eicu_crd.vitalperiodic` where temperature < 15
# 2647

### cvp
# select count(cvp) from `physionet-data.eicu_crd.vitalperiodic`
# 19157758
# select count(cvp) from `physionet-data.eicu_crd.vitalperiodic` where cvp > 50
# 2135240

### systolic
# select count(systemicsystolic) from `physionet-data.eicu_crd.vitalperiodic`
# 27834960
# select count(systemicsystolic) from `physionet-data.eicu_crd.vitalperiodic` where systemicsystolic > 300
# 4938
# select count(systemicsystolic) from `physionet-data.eicu_crd.vitalperiodic` where systemicsystolic < 0
# 4255

### diastolic
# select count(systemicdiastolic) from `physionet-data.eicu_crd.vitalperiodic`
# 27833847
# select count(systemicdiastolic) from `physionet-data.eicu_crd.vitalperiodic` where systemicdiastolic > 200
# 31071
# select count(systemicdiastolic) from `physionet-data.eicu_crd.vitalperiodic` where systemicdiastolic < 0
# 6015

### mean
# select count(systemicmean) from `physionet-data.eicu_crd.vitalperiodic`
# 28060870
# select count(systemicmean) from `physionet-data.eicu_crd.vitalperiodic` where systemicmean > 190
# 131990
# select count(systemicmean) from `physionet-data.eicu_crd.vitalperiodic` where systemicmean < 0
# 17110

### BP violations
# select count(systemicsystolic) from `physionet-data.eicu_crd.vitalperiodic` where systemicsystolic < systemicmean
# 46322
# select count(systemicsystolic) from `physionet-data.eicu_crd.vitalperiodic` where systemicsystolic < systemicdiastolic
# 721
# select count(systemicmean) from `physionet-data.eicu_crd.vitalperiodic` where systemicmean < systemicdiastolic


#################################
# For vitalAperiodic the NIBP	#
# values were queried and then	#
# loaded into rate 				#
#################################
 # SELECT patientunitstayid, noninvasivesystolic, noninvasivediastolic, noninvasivemean FROM `physionet-data.eicu_crd.vitalaperiodic`
# (saved as eicu_nibp.csv)
nibp <- read_csv("eicu_nibp.csv")
bp.2 <- na.omit(nibp)
bp.3 <- bp.2 %>%
  mutate(sys_too_high = noninvasivesystolic >= 300) %>%
  mutate(dias_too_high = noninvasivediastolic >= 200) %>%
  mutate(sys_neg = noninvasivesystolic<0) %>%
  mutate(dias_neg = noninvasivediastolic<0) %>%
  mutate(mean_neg = noninvasivemean<0) %>%
  mutate(sys_LT_mean = noninvasivesystolic<noninvasivemean) %>%
  mutate(sys_LT_dias = noninvasivesystolic<noninvasivediastolic) %>%
  mutate(mean_LT_dias = noninvasivemean<noninvasivediastolic) %>%
  mutate(sys_equal_dias = noninvasivesystolic==noninvasivediastolic) %>%
  mutate(sys_close_dias = abs(noninvasivesystolic-noninvasivediastolic)<6) %>%
  mutate(pulse_pressure = noninvasivesystolic-noninvasivediastolic) %>%
  mutate(any_error = (sys_too_high | dias_too_high | sys_neg | dias_neg | mean_neg | sys_LT_dias | sys_LT_mean |
                        mean_LT_dias | sys_close_dias))

mean(bp.3$noninvasivesystolic)
mean(bp.3$noninvasivediastolic)
bp.4 <- bp.3 %>% filter(any_error==FALSE)
mean(bp.4$noninvasivesystolic)
mean(bp.4$noninvasivediastolic)



### NURSECHARTING ###
nursignchartvalue
nursingchartcelltypevalname
Temperature (C)
Heart Rate
Invasive BP mean
O2 Saturation
Temperature (F)
Invasive BP Systolic
Invasive BP Diastolic

# heart rate
SELECT count(nursingchartvalue) FROM `physionet-data.eicu_crd.nursecharting` 
where nursingchartcelltypevalname = "Heart Rate"

