library(tidyverse)

#######################################
# Data tables derived from            #
# queries of the eICU-CRD             # 
# (see postgresql files).             #
# Tables generated as CSV files       #
# and uploaded into R.                #
# The queries generating the tables   #
# are provided prior to each          #
#######################################

# SELECT * FROM `physionet-data.eicu_crd.apachepatientresult`
# (file saved as "eicu_apachePatientResult.csv)
apachePatientResult <- read_csv('eicu_apachePatientResult.csv')

# SELECT * FROM `physionet-data.eicu_crd.patient`
# (file saved as "eicu_patient.csv")
patients <- read_csv('eicu_patient.csv')

# Add a column to indicate if providers are CCM specialists
apachePatientResult$ccm <- apachePatientResult$physicianspeciality %in% c('critical care medicine (CCM)', 'pulmonary/CCM', 'pulmonary', 'surgery-critical care', 'anesthesiology/CCM','anesthesiology')
# Add a column for tertiles of APACHE IV
apache_tertiles <- apachePatientResult %>%
  filter(apacheversion == 'IVa') %>%
  mutate(apache_tertile = ntile(apachescore, 3))
# Join patient demographic data with APACHE data
patient_data <- left_join(patients, apache_tertiles, by="patientunitstayid")

#####################################
# (N.B. remove ICUs with <5 stays   #  
#  when looking at per-ICU stuff)   #
#####################################              
# ICU types
patient_data %>%
  select(wardid, unittype) %>%
  distinct() %>%
  count(unittype) %>%
  mutate(perc=n/sum(n))

# encounters per ICU type
encount_per_icu <- patient_data %>%
  group_by(wardid, unittype) %>%
  count() %>%
  filter(n>4) %>%
  arrange(n)

no_encounters <- ggplot(encount_per_icu, aes(y=n, x=unittype)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width=0.2, alpha=0.4) +
  xlab('ICU type') + ylab("number of encounters")

# length of stay per ICU type
los_per_icu <- patient_data %>%
  group_by(wardid) %>%
  add_count() %>% group_by(wardid, unittype,n) %>%
  summarise(median_los_days = median(unitdischargeoffset/60/24),
            median_los_minutes = median(unitdischargeoffset))
summary(los_per_icu)
los_per_icu %>%
  filter(median_los_days >3) #There are 5 ICUs with median LOS > 3 days.
#One is a clear outlier with LOS 15.8 days

# The plot below exclude ONE unit with median LOS 15.8 days
los_per_unit_plot <- ggplot(filter(los_per_icu, median_los_days<10), aes(y=median_los_days, x=unittype, size=n)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width=0.2, alpha=0.4) +
  xlab('ICU type') + ylab("median length of stay (ICU)") +
  theme(legend.position = 'none')

los_per_unit_plot_2 <- ggplot(los_per_icu, aes(x=reorder(wardid, median_los_days), y=median_los_days)) +
  geom_bar(stat = 'identity', fill='light blue', colour='grey30') +
  xlab('ICU') + ylab('median ICU LOS (days)') +   theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) 

# Median APACHE per ICU type
soi_per_icu <- patient_data %>%
  group_by(wardid) %>%
  add_count() %>% group_by(wardid, unittype,n) %>%
  summarise(median_apache = median(na.omit(apachescore)))

soi_per_icu_plot <- ggplot(soi_per_icu, aes(x=unittype, y=median_apache, size=n)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha=0.5) +
  xlab('ICU type') + ylab("median APACHE IVa") +
  theme(legend.position = 'none')

# Median predicted mortality per ICU type
# (based on APACHE IV)
pred_mort_per_icu <- patient_data %>%
  group_by(wardid) %>%
  add_count() %>% group_by(wardid, unittype,n) %>%
  summarise(median_pred_mort = median(na.omit(predictedicumortality))) %>%
  filter(median_pred_mort>0)

# Actual mortality per ICU type
actual_mort_per_icu <- patient_data %>%
  group_by(wardid, unittype) %>%
  count(unitdischargestatus) %>%
  group_by(wardid) %>%
  mutate(actual_mort = n[2]/(n[1]+n[2]), total_n=n[1]+n[2]) %>%
  select(-unitdischargestatus, -n) %>%
  distinct()

# merge these
mortality_diff <- right_join(pred_mort_per_icu, actual_mort_per_icu) %>%
  mutate(diff = actual_mort-median_pred_mort)
summary(mortality_diff)

mortality_per_icu <- right_join(pred_mort_per_icu, actual_mort_per_icu) %>%
  gather(key=pred_v_actual, value=mortality, median_pred_mort, actual_mort)

mort_per_icu_plot <- ggplot(mortality_per_icu, 
                            aes(x=unittype, y=mortality, size=total_n)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha=0.5) +
  xlab('ICU type') + ylab("median ICU mortality") +
  theme(legend.position = 'none') +
  facet_wrap(~pred_v_actual, labeller=labeller(pred_v_actual = labels)) +
  theme(axis.text.x = element_text(angle=-45))


