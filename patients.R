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

# SELECT * FROM `physionet-data.eicu_crd.admissiondrug`
# (file saved as "eicu_admissiondrug.csv")
admissiondrug <- read_csv("eicu_admisssiondrug.csv")

# SELECT * FROM `physionet-data.eicu_crd.allergy`
# (file saved as "eicu_allergy.csv")
allergy <- read_csv("eicu_allergy.csv")

# SELECT * FROM `physionet-data.eicu_crd.apacheapsvar`
# (file saved as "eicu_apacheApsVar.csv")
apacheApsVar <- read_csv('eicu_apacheApsVar.csv')

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



# Number of encounters per patient
enc_per_pat <- patient_data %>%
  group_by(uniquepid) %>%
  count()
summary(enc_per_pat$n)
# Demogaraphics
table(patients$gender)
table(patients$ethnicity)
table(patients$hospitaladmitsource)
table(patients$unittype)
table(patients$hospitaldischargelocation)
# Age has to be re-cast as numeric, with bulk category for > 89
age_young <- as.numeric(patients$age[patients$age != '>89'])
older <- rep(89, sum(na.omit(patients$age=='> 89')))
ages_numeric <- c(age_young, older)
# Plot of ages
ages_plot <- ggplot(as.tibble(ages_numeric), aes(value)) +
  geom_histogram(binwidth = 1, fill='grey80', colour='black') + 
  theme_bw() + xlab('Age (years)')
# Sub-plot of ages < 18
ages_subplot <- ggplot(filter(as.tibble(ages_numeric), value<18), aes(value)) +
  geom_histogram(binwidth = 1, fill='grey80', colour='black') + 
  theme_bw() + xlab('Age (years)')

# allergies 
allergies_per_encounter <- allergy %>%
  group_by(patientunitstayid) %>% tally()
summary(allergies_per_encounter$n)

# admissiondrug  
drugs_per_encounter <- admissiondrug %>%
  group_by(patientunitstayid) %>% tally()
summary(drugs_per_encounter$n)

