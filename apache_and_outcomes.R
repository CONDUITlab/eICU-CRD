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
  theme(axis.title = element_text(size=16)) +
  theme(axis.text = element_text(size=14)) +
  theme(strip.text.x = element_text(size = 18))

dialysis_plot <- ggplot(filter(apache_ward, intervention=='DIALYSIS'), 
                        aes(x=reorder(wardid, yes), y=yes)) +
  geom_bar(stat = 'identity', fill='yellow', colour='grey30') +
  ylim(0,1) + ylab('% dialysed') + xlab('unit') + theme(axis.text.x = element_blank(),
                                                        axis.ticks.x = element_blank())
vented_plot <- ggplot(filter(apache_ward, intervention=='VENTILATED'), 
                      aes(x=reorder(wardid, yes),y=yes)) +
  geom_bar(stat = 'identity', fill='purple', colour='grey30') +
  ylim(0,1) + ylab('% ventilated') + xlab('unit') + theme(axis.text.x = element_blank(), 	axis.ticks.x = element_blank())
intubated_plot <- ggplot(filter(apache_ward, intervention=='INTUBATED'), 
                         aes(x=reorder(wardid, yes), y=yes)) +
  geom_bar(stat = 'identity', fill='green', colour='grey30') +
  ylim(0,1) + ylab('% intubated') + xlab('unit') + theme(axis.text.x = element_blank(), 	axis.ticks.x = element_blank())

ward_terile_intervention <- apache_ward %>%
  group_by(intervention, tertile) %>%
  summarise(median=median(yes), iqr=IQR(yes))

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
  ylab('median ICU LOS (days)') +
  theme(legend.position = 'none')  +
  theme(axis.title = element_text(size=16)) +
  theme(axis.text = element_text(size=14))


# mortality by tertile
# This plot excludes one ICU that had a mortality rate of 33%
mort_tertiles <- pt_data_with_tert %>%
  group_by(wardid, tertile) %>%
  summarise(n = n(), mort= sum(unitdischargestatus=="Expired")/n*100)

mortality_icu_tertile_plot <- ggplot(filter(mort_tertiles, mort<30), aes(x=tertile, y=mort, size=n, colour=tertile)) +
  geom_boxplot(outlier.shape = NA, colour='black') + 
  geom_jitter(width=0.2, alpha=0.5) +
  xlab('tertile (unit encounters)') +
  ylab('ICU mortality (%)') +
  theme(legend.position = 'none') +
  theme(axis.title = element_text(size=16)) +
  theme(axis.text = element_text(size=14))

