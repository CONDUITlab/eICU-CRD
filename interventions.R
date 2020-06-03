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

# SELECT * FROM `physionet-data.eicu_crd.careplangeneral` 
# (file saved as "eicu_careplangeneral.csv)
careplan <- read_csv("eicu_careplangeneral.csv")

# SELECT * FROM `physionet-data.eicu_crd.respiratorycare`
# (file saved as "eicu_respiratoryCare.csv")
respcare <- read_csv('eicu_respiratorycare.csv')

# SELECT * FROM `physionet-data.eicu_crd.treatment` where treatmentstring like "pulmonary|ventilation and oxygenation|mechanical%"
# (file saved as "eicu_treatment_resp.csv")
treatment_resp <- read_csv("eicu_treatment_resp.csv")

# SELECT * FROM `physionet-data.eicu_crd.treatment` where treatmentstring like "pulmonary|ventilation and oxygenation|mechanical ventilation|non%"
# (file saved as "eicu_treatment_resp_niv.csv")
treatment_resp_niv <- read_csv("eicu_treatment_resp_niv.csv")

# SELECT * FROM `physionet-data.eicu_crd.infusiondrug` 
# where (drugname like "%ephrin%"
#        or drugname like "%itrogly%"
#        or drugname like "%iltiaz%"
#        or drugname like "%ilrinon%"
#        or drugname like "%olol%"
#        or drugname like "%obutam%"
#        or drugname like "%proteren%"
#        or drugname like "%evophed%"
#        or drugname like "%cardipine%"
#        or drugname like "%asopressin%"
#        or drugname like "%imacore%"
#        or drugname like "%prussid%"
#        or drugname like "%poporost%"
#        or drugname like "%opamine%
#        or drugname like "%miodaron%"
#        or drugname like "%abetalol%")
# (file saved as "eicu_infusiondrug.csv)
infusions <- read_csv("eicu_infusiondrug.csv")

# "physicalExam" table is queried in BigQuery
# total size of table = SELECT distinct patientunitstayid FROM `physionet-data.eicu_crd.physicalexam`
# 176379 rows in the table

### VENTILATION ###
#APACHE (41,348 stays [24% of total])
apache_vent <- apacheApsVar %>%
  filter(vent==1) %>% pull(patientunitstayid) %>% unique()
length(apache_vent)/length(unique(apacheApsVar$patientunitstayid))

# Careplan (65,419 stays [33% of total])
careplan_vent <- careplan %>% filter(cplitemvalue=="Ventilated - rapid wean/extubation" | 
                                       cplitemvalue=="Ventilated - with no daily extubation trial" |
                                       cplitemvalue=="Ventilated - chronic dependency" |
                                       cplitemvalue=="Non-invasive ventilation" |
                                       cplitemvalue=="Ventilated - with daily extubation evaluation") %>%
  pull(patientunitstayid) %>% unique()
length(careplan_vent)/length(unique(careplan$patientunitstayid))

# Careplan NIV (21,637 stays [11% of total])
careplan_niv <- careplan %>% filter(cplitemvalue=="Non-invasive ventilation") %>%
  pull(patientunitstayid) %>% unique()
length(careplan_niv)/length(unique(careplan$patientunitstayid))

# Careplan presumed invasive (50,490 stays )
careplan_vent_inv <- careplan %>% filter(cplitemvalue=="Ventilated - rapid wean/extubation" | 
                                           cplitemvalue=="Ventilated - with no daily extubation trial" |
                                           cplitemvalue=="Ventilated - chronic dependency" |
                                           cplitemvalue=="Ventilated - with daily extubation evaluation") %>%
  pull(patientunitstayid) %>% unique()

# N.B. 6708 are in both therefore 21637-6708 = 14929
14929/length(unique(careplan$patientunitstayid))

# respiratoryCare table
#If we assume all stays in this table are ventilated, then 
length(unique(respcare$patientunitstayid))
#44772
# There are lots of NA in the airwaytype field
sum(is.na(respcare$airwaytype))/nrow(respcare)
#89% missing

# Treatment
# There are 152429 patienunitstayid's in this table
length(unique(treatment_resp$patientunitstayid))  #40442
length(unique(treatment_resp_niv$patientunitstayid)) #1288

# from physicalExam
##################################################################################
# SELECT distinct patientunitstayid FROM `physionet-data.eicu_crd.physicalexam`	 #
# where (physicalexamvalue like "Vent Rate%"									 #
# OR physicalexamvalue like "ventilate%"										 #
# OR physicalexamvalue like "intubate%"											 #
# OR physicalexamvalue like "PEEP")												 #
##################################################################################

### Intubation ###
#APACHE (41348)
apache_intubated <- apacheApsVar %>%
  filter(intubated==1) %>% pull(patientunitstayid) %>% unique()
length(apache_intubated)/length(unique(apacheApsVar$patientunitstayid))
# 25398 stays

careplan_intubated <- careplan %>% filter(cplitemvalue=="Intubated/nasal ETT" | 
                                            cplitemvalue=="Intubated/nasal ETT - difficult" |
                                            cplitemvalue=="Intubated/oral ETT" |
                                            cplitemvalue=="Intubated/oral ETT - difficult") %>%
  pull(patientunitstayid) %>% unique()
# 48074 stays
length(careplan_intubated)/length(unique(careplan$patientunitstayid))


respcare_intubated <- respcare %>% filter(airwaytype=="Oral ETT"|
                                            airwaytype=="Nasal ETT"|
                                            airwaytype=="Double-Lumen Tube") %>% 
  pull(patientunitstayid) %>% unique()
# 8152 stays
# OR...we assume all stays in here are intubated
length(unique(respcare$patientunitstayid))
#44772

# from physicalExam
#################################################################################
# SELECT distinct patientunitstayid FROM `physionet-data.eicu_crd.physicalexam`	#
# where physicalexamvalue like "intubate%"										#
#################################################################################

### Dialysis ###
#APACHE
apache_dialysis <- apacheApsVar %>%
  filter(dialysis==1) %>% pull(patientunitstayid) %>% unique()
#6309
length(apache_dialysis)/length(unique(apacheApsVar$patientunitstayid))
# 3.7%

# from BigQuery
#####################################################################################
# SELECT distinct patientunitstayid FROM `physionet-data.eicu_crd.careplangeneral`	#
# where cplitemvalue="Hypervolemic - dialyze/filter"								#
#																					#
# SELECT distinct patientunitstayid FROM `physionet-data.eicu_crd.physicalexam`		#
# where physicalexamvalue like "Dialysis%"											#
#																					#
# SELECT distinct patientunitstayid FROM `physionet-data.eicu_crd.treatment` 		#
# where treatmentstring like 'renal|dialysis%'										#
#####################################################################################

### Infusions
# SELECT count(distinct patientunitstayid) FROM `physionet-data.eicu_crd.infusiondrug` 
# 73,547 encounters that inolved any infusions
length(unique(infusions$patientunitstayid))
#38,353 that involved vasoactive infusions

# Patients who required no interventions
vent_pressors <- union(respcare$patientunitstayid, infusions$patientunitstayid)
intub_dial <- union(careplan_intubated, apache_dialysis)
any_intervention <- union(vent_pressors, intub_dial)
length(setdiff(patients$patientunitstayid, any_intervention))

       