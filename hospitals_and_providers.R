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

# SELECT * FROM `physionet-data.eicu_crd.hospital`
# (file saved as "eicu_hospital.csv")
hospital <- read_csv('eicu_hospital.csv')

# SELECT * FROM `physionet-data.eicu_crd.patient`
# (file saved as "eicu_patient.csv")
patients <- read_csv('eicu_patient.csv')

# SELECT * FROM `physionet-data.eicu_crd.apachepatientresult`
# (file saved as "eicu_apachePatientResult.csv)
apachePatientResult <- read_csv('eicu_apachePatientResult.csv')

# Add a column to indicate if providers are CCM specialists
apachePatientResult$ccm <- apachePatientResult$physicianspeciality %in% c('critical care medicine (CCM)', 'pulmonary/CCM', 'pulmonary', 'surgery-critical care', 'anesthesiology/CCM','anesthesiology')
# Add a column for tertiles of APACHE IV
apache_tertiles <- apachePatientResult %>%
  filter(apacheversion == 'IVa') %>%
  mutate(apache_tertile = ntile(apachescore, 3))
# Join patient demographic data with APACHE data
patient_data <- left_join(patients, apache_tertiles, by="patientunitstayid")


#####################
# 1. Hospitals      #
#####################
hospital %>%
  group_by(region) %>%
  tally()
hospital %>%
  group_by(region, teachingstatus) %>%
  tally()
hospital %>%
  group_by(region,numbedscategory) %>%
  tally()

#####################
# 2. Providers      #
#####################
doctors <- patient_data %>%
  group_by(physicianspeciality) %>%
  tally()
# Number of providers of unknown specialty
unknown_docs <- filter(doctors, physicianspeciality %in% c('Specialty Not Specified', 'unknown'))
# Number of providers from CCM speciatlies
all_ccm_docs <- filter(doctors, physicianspeciality %in% c('critical care medicine (CCM)', 'pulmonary/CCM', 'pulmonary', 'surgery-critical care', 'anesthesiology/CCM','anesthesiology'))
# Number of providers from "striclty" CCM speciatlies
# (this excludes "pulmonary" and "anesthesiology")
strictly_ccm_docs <- filter(doctors, physicianspeciality %in% c('critical care medicine (CCM)', 'pulmonary/CCM', 'surgery-critical care', 'anesthesiology/CCM'))
new_denom <- 200859-52327-sum(unknown_docs$n)
# number of CCM docs
sum(all_ccm_docs$n)/new_denom
sum(strictly_ccm_docs$n)/new_denom
# plot of specialites (excludes "NA")
doctors$ccm <- doctors$physicianspeciality %in% c('critical care medicine (CCM)', 'pulmonary/CCM', 'pulmonary', 'surgery-critical care', 'anesthesiology/CCM','anesthesiology')
doctor_plot <- ggplot(na.omit(doctors), aes(x=reorder(physicianspeciality, -n), y=n, fill=ccm)) +
  geom_bar(stat='identity', colour='black') + theme_bw() +
  xlab('MD specialty') + ylab('No. of encounters') +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  theme(legend.position = 'none') +
  theme(panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = c('grey90', 'grey10'))

doc_table <- doctors[order(doctors$n, decreasing=T),]

