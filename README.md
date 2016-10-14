# Scale drift quality control

# Motivation
The purpose of the study is to compare the performances of the change-point model (CPM), the regular hidden Markov model (HMM), and left-right hidden Markov model (HMM) on score stability monitoring in educational test administrations.

# Data
Data are simulated uisng 

* Date_50admin.R
* Date_100admin.R
* DataSim_NoSeason_1cp_Fun.R
* DataSim_Season_1cp_Fun.R
* DataSim_1cp.R

# Model comparison
Models are implemented using

* CPM.R
* GetAIC_RegHMM.R
* GetAIC_LRHMM.R
* ModelCompare_1cp_CPM.R
* ModelCompare_1cp_RegHMM.R
* ModelCompare_1cp_LRHMM_1.R
* ModelCompare_1cp_LRHMM_2.R
* ModelCompare_1cp_LRHMM_3.R
* ModelCompare_1cp_LRHMM_4.R
