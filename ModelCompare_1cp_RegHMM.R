#################################################################################
######## compare regular HMM model results for 100 admins yearly and 2 change points ########
#################################################################################
setwd("C:/Users/Yuyu/Dropbox/7.6 HMM project/data")
### import the source files for data modeling
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/GetAIC_RegHMM.R")
### load the data
load("GenData_NoSeason_100admin_1cp.Rdata")
load("GenRes_Season_100admin_1cp.Rdata")
load("GenData_NoSeason_50admin_1cp.Rdata")
load("GenRes_Season_50admin_1cp.Rdata")
### load the package
# install.packages("depmixS4")
library(depmixS4)

##############################################################################################
######### write a function to fit an optimal regular HMM for one condition
RegHMM_forOneCondition_forOneState = function(mydata, condition_to_use, num_states) {
    # create a vector to store the change points results for all series
    statechange_RegHMM_all = numeric()
    # caculate the number of rows to use
    rows_in_conditon = dim(mydata[[condition_to_use]])[1]
    for (i in 1:rows_in_conditon) {
        # get the best fitting model for each condition
        # best infor contains condition to use, num_states, seed_value, and AIC
        bestmodelInfor = Get_minAIC_RegHMM_for_one_state(mydata, condition_to_use, 
                                                         row_to_use=i, 
                                                         num_states)
        # fit the model again using the best fitting model parametes
        set.seed(bestmodelInfor["seed_value"])
        RegHMM = depmix(mydata[[condition_to_use]][i, ]~1, 
                        data=data.frame(mydata[[condition_to_use]][i, ]), 
                        nstates=bestmodelInfor["num_states"], 
                        family=gaussian(), 
                        trstart=runif((bestmodelInfor["num_states"])^2))
        RegHMM_fit = fit(RegHMM, verbose = FALSE)
        
        # get the estimated state for each time admin and find the change point
        RegHMM_esttrans = posterior(RegHMM_fit)
        statechange_RegHMM = 1+which(diff(RegHMM_esttrans$state)!=0)
        # concadinate the change points for all series
        statechange_RegHMM_all = c(statechange_RegHMM_all, statechange_RegHMM)
    }  
    return(statechange_RegHMM_all)
}

##############################################################################################
######### write a function to fit a regular HMM for one condition
RegHMM_forMultiConditions_forOneState = function(mydata, num_states){
    statechange_RegHMM_MultiConditions = list()
    num_of_conditions = length(mydata)
    for (i in 1:num_of_conditions){
        statechange_RegHMM_all_oneCondition = RegHMM_forOneCondition_forOneState(mydata, condition_to_use = i, num_states)
        statechange_RegHMM_MultiConditions[[i]] = statechange_RegHMM_all_oneCondition
    }
    return(statechange_RegHMM_MultiConditions)
}

##############################################################################################
######### get the state change for data GenData_NoSeason_100admin_1cp
begintime_RegHMM = Sys.time()
RegHMM_NoSeason_100admin_1cp_results = RegHMM_forMultiConditions_forOneState(mydata=GenData_NoSeason_100admin_1cp, num_states=2)
save(RegHMM_NoSeason_100admin_1cp_results, file="RegHMM_NoSeason_100admin_1cp_results.Rdata")

RegHMM_Season_100admin_1cp_results = RegHMM_forMultiConditions_forOneState(mydata=GenRes_Season_100admin_1cp, num_states=2)
save(RegHMM_Season_100admin_1cp_results, file="RegHMM_Season_100admin_1cp_results.Rdata")

RegHMM_NoSeason_50admin_1cp_results = RegHMM_forMultiConditions_forOneState(mydata=GenData_NoSeason_50admin_1cp, num_states=2)
save(RegHMM_NoSeason_50admin_1cp_results, file="RegHMM_NoSeason_50admin_1cp_results.Rdata")

RegHMM_Season_50admin_1cp_results = RegHMM_forMultiConditions_forOneState(mydata=GenRes_Season_50admin_1cp, num_states=2)
save(RegHMM_Season_50admin_1cp_results, file="RegHMM_Season_50admin_1cp_results.Rdata")

endtime_RegHMM = Sys.time()
# save the results 







