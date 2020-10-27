# Carehome_hospital_discharge_study
This repository contains the R code used to analyse data for the study led by PHW into care home discharges and COVID outbreaks. The study is entitled: 'Risk factors for outbreaks of COVID-19 in care homes following hospital discharge: a national cohort analysis' The study has been submitted for peer-review. A pre-print of the study is available from MedRvix here: https://www.medrxiv.org/content/10.1101/2020.08.24.20168955v1 

The study was carried out within Public Health Wales with the support of Swansea University Medical School. 

The code was written by Drew Turner, Information Analyst Specialist at Public Health Wales with support and supervision from Professor Michael Gravenor of Swansea University.

There are three R scripts avaialable to replicate this analysis: one to prepare the data (Discharge_study_data_prep), one to carry out the analysis (Discharge_study_final_analysis) and one to carry out a sensitivity analysis (Discharge_study_sensitivity_analysis). Scripts must be run in that order. 

As described in the data preparation script, three datasets are required for the analysis to work. It has been assumed these will be available as worksheets within a Microsoft Excel workbook entitled 'Discharge_study. 
1)'Results_data' sheet should have columns for the date of each result and for a unique identifier, linkable between worksheets, for the care home associated with the result
2) 'Discharge_data' sheet should have columns for the date of discharge to the care home and for a unique identifier, linkable between worksheets, for the care home associated with the discharge
3) 'Care_home_data' sheet should have a column for the unique identifier for each care home, with further columns for each care home characteristics investigated: the Health Board area (or equivalent) of the home, whether nursing is offered, whether there is provision for those with dementia, for those with mental health issues and for those with learning disabilities

The code is provided here for public health colleagues to apply to data in their own areas. The code is provided with the requirement that the authors of the code named aboave are acknowledged in any analysis that is made public, whether in the form of a report, peer-reviewed paper or by other means and that the original paper described above is cited as appropriate.

The code is further provided on condition that any code produced based on this work is also made publically available.
