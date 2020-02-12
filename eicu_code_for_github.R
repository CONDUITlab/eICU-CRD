library(tidyverse)

#######################################
# Data tables derived from            #
# queries of the eICU-CRD.            #
# Tables generated as CSV files       #
# and uploaded into R.                #
# The queries generating the tables   #
# are provided prior to each          #
#######################################

# SELECT * FROM `physionet-data.eicu_crd.patient`
# (file saved as "eicu_patient.csv")
patients <- read_csv('eicu_patient.csv')
# SELECT * FROM `physionet-data.eicu_crd.diagnosis`
# (file saved as "eicu_diagnosis.csv")
diagnosis <- read_csv('eicu_diagnosis.csv')
# SELECT * FROM `physionet-data.eicu_crd.admissiondx`
# (file saved as "eicu_admissiondx.csv)
admissiondx <- read_csv('eicu_admissiondx.csv')
# SELECT * FROM `physionet-data.eicu_crd.apachepatientresult`
# (file saved as "eicu_apachePatientResult.csv)
apachePatientResult <- read_csv('eicu_apachePatientResult.csv')
# SELECT * FROM `physionet-data.eicu_crd.apacheapsvar`
# (file saved as "eicu_apacheApsVar.csv")
apacheApsVar <- read_csv('eicu_apacheApsVar.csv')
# SELECT * FROM `physionet-data.eicu_crd.hospital`
# (file saved as "eicu_hospital.csv")
hospital <- read_csv('eicu_hospital.csv')

#################################
# some modifiations to data     #
# tables to facilitate analysis #
#################################
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

#####################################
# 2. ICUs                           #
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

soi_per_icu <- ggplot(soi_per_icu, aes(x=unittype, y=median_apache, size=n)) +
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
actual_mort_icu <- patient_data %>%
group_by(wardid, unittype) %>%
count(unitdischargestatus) %>%
group_by(wardid) %>%
mutate(actual_mort = n[2]/(n[1]+n[2]), total_n=n[1]+n[2]) %>%
select(-unitdischargestatus, -n) %>%
distinct()

# merge these
mortality_diff <- right_join(pred_mort_per_icu, actual_mort_icu) %>%
  mutate(diff = actual_mort-median_pred_mort)
summary(mortality_diff)

mortality_per_icu <- right_join(pred_mort_per_icu, actual_mort_icu) %>%
  gather(key=pred_v_actual, value=mortality, median_pred_mort, actual_mort)

mort_per_icu_plot <- ggplot(mortality_per_icu, 
       aes(x=unittype, y=mortality, size=total_n)) +
	   geom_boxplot(outlier.shape = NA) +
	   geom_jitter(width = 0.2, alpha=0.5) +
	   xlab('ICU type') + ylab("median ICU mortality") +
	   theme(legend.position = 'none') +
	   facet_wrap(~pred_v_actual, labeller=labeller(pred_v_actual = labels)) +
	   theme(axis.text.x = element_text(angle=-45))


#########################
# 2. Providers			#
#########################
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


#########################
# 3. Patients           #
#########################

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

# Diagnoses
# We get these from APACHE, and also from the "diagnosis" table
# 148,532 patients have an APACHE score, but 177,863 seem to have and APACHE admission dx
# Most patient have 3 APACHE diagnoses, some have 4 or 5. 
# There is one "diagnosis", and then various "yes/no" answers
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

# APACHE score analyses
summary(patient_data$apachescore)
summary(patient_data$predictedicumortality)
summary(patient_data$predictediculos)

apacheApsVar %>%
  group_by(intubated) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

apacheApsVar %>%
  group_by(vent) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

apacheApsVar %>%
  group_by(dialysis) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

apacheApsVar %>%
  mutate(GCS = eyes+motor+verbal) %>%
  group_by(GCS) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Histograms of APACHE variables
apache_phys <- apacheApsVar %>%
select(-apacheapsvarid, -intubated, -vent, -dialysis,
       -eyes, -motor, -verbal, -meds) %>%
gather(var, value, -patientunitstayid) %>%
filter(value>0) # filter out all the "-1" which is the NA equivalent

apache_histograms <- ggplot(apache_phys, aes(value)) +
geom_histogram(bins = 30) +
facet_wrap(~var, scales = 'free') +
xlab('')


# Interventions by unit, broken into tertiles
apache_patient <- left_join(apacheApsVar, patients) %>%
  select(wardid, intubated, vent, dialysis) %>%
  gather(intervention, val, intubated, vent, dialysis)

apache_ward <- apache_patient %>%
  group_by(wardid, intervention) %>%
  summarise(n=n(), yes = sum(val)/n())
apache_ward$tertile = as.factor(ntile(apache_ward$n,3))
apache_ward$intervention <- str_to_upper(apache_ward$intervention)
apache_ward$intervention <- str_replace_all(apache_ward$intervention, "VENT", "VENTILATED")

interventions_plot <- ggplot(apache_ward, aes(x=tertile, y=yes, size=n, colour=tertile)) +
  geom_boxplot(outlier.shape = NA, colour='black') + 
  geom_jitter(width=0.2, alpha=0.2) +
  facet_wrap(~intervention) +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = 'none') + ylab("% of patients receiving indicated intervention") + xlab('Tertile (unit encounters)') +
  theme(strip.text = element_text(face='bold', size=12, colour='white')) +
  theme(strip.background.x = element_rect(colour='black', fill='black')) +
  theme(axis.title = element_text(size=14)) +
  theme(axis.text = element_text(size=12))

dialysis_plot <- ggplot(filter(apache_ward, intervention=='dialysis'), 
	aes(x=reorder(wardid, yes), y=yes)) +
	geom_bar(stat = 'identity', fill='yellow', colour='grey30') +
	ylim(0,1) + ylab('% dialysed') + xlab('unit') + theme(axis.text.x = element_blank(),
	axis.ticks.x = element_blank())
vented_plot <- ggplot(filter(apache_ward, intervention=='vent'), 
	aes(x=reorder(wardid, yes),y=yes)) +
	geom_bar(stat = 'identity', fill='purple', colour='grey30') +
	ylim(0,1) + ylab('% ventilated') + xlab('unit') + theme(axis.text.x = element_blank(), 	axis.ticks.x = element_blank())
intubated_plot <- ggplot(filter(apache_ward, intervention=='intubated'), 
	aes(x=reorder(wardid, yes), y=yes)) +
	geom_bar(stat = 'identity', fill='green', colour='grey30') +
	ylim(0,1) + ylab('% intubated') + xlab('unit') + theme(axis.text.x = element_blank(), 	axis.ticks.x = element_blank())

ward_terile_intervention <- apache_ward %>%
  group_by(intervention, tertile) %>%
  summarise(median=median(yes), iqr=IQR(yes))

# ICU length of stay
summary(patients$unitdischargeoffset/60/24)

los_by_icu <- patients %>%
  group_by(wardid) %>%
  summarize(median_los = median(unitdischargeoffset/60/24))

median_los_icu_plot <- ggplot(los_by_icu, aes(x=reorder(wardid, median_los), y=median_los)) +
  geom_bar(stat = 'identity', fill='yellow', colour='grey50') + theme_bw() +
  ylab('ICU mmedian length of stay (days)') + xlab('ICU') + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

# ICU mortality
mort_by_icu <- patients %>%
	group_by(wardid, unitdischargestatus) %>%
	summarise(n = n()) %>%
	mutate(freq = n / sum(n)) %>%
	filter(unitdischargestatus=='Expired')

mort_by_icu_plot <- ggplot(mort_by_icu, aes(x=reorder(wardid, freq), y=freq*100)) +
	geom_bar(stat = 'identity', fill='blue', colour='grey50') + theme_bw() +
	ylab('ICU mortality (%)') + xlab('ICU') + 
	theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
	theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

	# ICU outcomes per ICU tertile (size)
	ward_counts <- patients %>%
	  group_by(wardid) %>%
	  count()
	ward_counts$tertile <- as.factor(ntile(ward_counts$n, 3))

	pt_data_with_tert <- left_join(patient_data, ward_counts)

	los_tertiles <- pt_data_with_tert %>%
	  group_by(wardid, tertile) %>%
	  summarise(n=n(), median_icu_los=median(unitdischargeoffset/60/24))

# This plot excludes 4 small ICUs (54-171 encounters)
# that had a median LOS > 4 days (range 4.89-15.8)
# All other ICUs had a median LOS < 4 days
median_los_icu_tertile_plot <- ggplot(filter(los_tertiles, median_icu_los<4), aes(x=tertile, 	y=median_icu_los, size=n, colour=tertile)) +
  	geom_boxplot(outlier.shape = NA, colour='black') + 
  	geom_jitter(width=0.2, alpha=0.5) +
  	xlab('tertile (unit encounters)') +
  	ylab('median ICU LOS') +
  	theme(legend.position = 'none')


# mortality by tertile
# This plot excludes one ICU that had a mortality rate of 33%
mort_tertiles <- pt_data_with_tert %>%
  group_by(wardid, tertile) %>%
  summarise(n = n(), mort= sum(unitdischargestatus=="Expired")/n*100)

mortality_icu_tertile_plot <- ggplot(filter(mort_tertiles, mort<30), aes(x=tertile, y=mort, size=n, colour=tertile)) +
  geom_boxplot(outlier.shape = NA, colour='black') + 
  geom_jitter(width=0.2, alpha=0.5) +
  xlab('tertile (unit encounters)') +
  ylab('median ICU LOS') +
  theme(legend.position = 'none') +
  theme(axis.title = element_text(size=14)) +
  theme(axis.text = element_text(size=12))
