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

# SELECT * FROM `physionet-data.eicu_crd.admissiondx`
# (file saved as "eicu_admissiondx.csv)
admissiondx <- read_csv('eicu_admissiondx.csv')

# SELECT * FROM `physionet-data.eicu_crd.diagnosis`
# (file saved as "eicu_diagnosis.csv")
diagnosis <- read_csv('eicu_diagnosis.csv')

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

# Diagnoses
# We get these from APACHE, and also from the "diagnosis" table
# 148,532 patients have an APACHE score, but 177,863 seem to have and APACHE admission dx
# Most patient have 3 APACHE diagnoses, some have 4 or 5. 
# There is one "diagnosis", and then various "yes/no" questions 
# eg. did the patient come from the OR?
# They all seem to have one dx that starts with:
# "admission diagnosis|All Diagnosis|..."
# We took this to be the main diagnosis
primary_apache_dx <- admissiondx %>%
  filter(
    str_detect(admitdxpath,"All Diagnosis")
  ) %>%
  group_by(admitdxname) %>%
  tally() %>%
  arrange(desc(n))

# Plot of APACHE diagnoses
# The number on the x-axis corresponds with the row number
# in primary_apache_dx
dx_plot <- ggplot(primary_apache_dx[1:20,], aes(x=reorder(admitdxname, -n), y=n)) +
  geom_bar(stat = 'identity') +
  theme_bw() + theme(panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=seq(1:20)) + xlab("diagnosis number") +ylab('count')

# Then there is the "diagnosis" table
# Here is the number of diagnoses assigned to each patient
dx_emr <- diagnosis %>%
  group_by(patientunitstayid) %>%
  count() %>%
  arrange(desc(n))

# So how many "primary" diagnoses are there per patient
primary_dx <- diagnosis %>%
  filter(diagnosispriority=='Primary') %>%
  group_by(patientunitstayid) %>%
  count() %>%
  arrange(desc(n))

primary_dx %>% group_by(n) %>% count()

# A histogram of the number of primary diagnoses
# per patient, excluding 236 patients with more than
# 50 primary diagnoses
dx_number_plot <- ggplot(filter(primary_dx, n<51), aes(n)) +
  geom_histogram(binwidth=1) +
  theme_bw() + xlab('number of Primary diagnoses')

primary_dx_tally <- diagnosis %>%
  filter(diagnosispriority=='Primary') %>%
  group_by(diagnosisstring) %>%
  count() %>%
  arrange(desc(n))

 # diagnoses per LOS and per SOI
dx_emr_comp <- left_join(dx_emr, patient_data)

apache_vs_num_dx_plot <- ggplot(dx_emr_comp, aes(x=n, y=apachescore)) +
  geom_point(alpha=0.3) +
  theme_bw() + xlab('number of diagnoses assigned') +
  ylab('APACHE IVa score')

los_vs_num_dx_plot <- ggplot(dx_emr_comp, aes(x=n, y=actualiculos)) +
  geom_point(alpha=0.3) +
  theme_bw() + xlab('number of diagnoses assigned') +
  ylab('ICU length of stay')

# What % of patients have NO DIAGNOSIS?
have_dx <- union(admissiondx$patientunitstayid, diagnosis$patientunitstayid)
no_dx <- setdiff(patients$patientunitstayid, have_dx)
length(no_dx)/nrow(patients)

