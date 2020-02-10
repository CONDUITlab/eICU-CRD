# For downloading the major tables
SELECT * FROM `physionet-data.eicu_crd.hospital`
SELECT * FROM `physionet-data.eicu_crd.apacheapsvar`
SELECT * FROM `physionet-data.eicu_crd.apachepatientresult`
SELECT * FROM `physionet-data.eicu_crd.admissiondx`
SELECT * FROM `physionet-data.eicu_crd.diagnosis`
SELECT * FROM `physionet-data.eicu_crd.patient`

# For vital signs
SELECT nursingChartCellTypeCat,
nursingChartCellTypeValLabel,
nursingChartCellTypeValName,
nursingChartValue
FROM `physionet-data.eicu_crd.nursecharting`
where nursingChartCellTypeValName = 'CVP'
  OR nursingChartCellTypeValName = 'Arterial Line MAP (mmHg)'		THIS IS NOTHING
  OR nursingChartCellTypeValName = 'Pulse'					THIS IS NOTHING
  OR nursingChartCellTypeValName = 'O2 Saturation'
  OR nursingChartCellTypeValName = 'CVP (mmHg)'				THIS IS NOTHING
  OR nursingChartCellTypeValName = 'SpO2'					THIS IS NOTHING
  OR nursingChartCellTypeValName = 'Heart Rate'
  OR nursingChartCellTypeValName = 'Invasive BP'
  OR nursingChartCellTypeValName = 'MAP (mmHg)'
  OR nursingChartCellTypeValName = 'Temperature'
  OR nursingChartCellTypeValName = 'Respiratoy Rate'
  OR nursingChartCellTypeValName = 'Non-Invasive BP'

SELECT nursingChartCellTypeCat,
nursingChartCellTypeValLabel,
nursingChartCellTypeValName,
nursingChartValue
FROM `physionet-data.eicu_crd.nursecharting`
where nursingChartCellTypeValName = 'CVP'
  OR nursingChartCellTypeValName = 'CVP (mmHg)'

