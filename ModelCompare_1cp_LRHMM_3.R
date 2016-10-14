#################################################################################
######## compare left-right HMM model results for 1 change point ################
#################################################################################
setwd("C:/Users/Yuyu/Dropbox/7.6 HMM project/data")
### import the source files for data modeling
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/GetAIC_LRHMM.R")
### load the data
load("GenData_NoSeason_100admin_1cp.Rdata")
load("GenRes_Season_100admin_1cp.Rdata")
load("GenData_NoSeason_50admin_1cp.Rdata")
load("GenRes_Season_50admin_1cp.Rdata")
### load the package
# install.packages("depmixS4")
library(depmixS4)

##############################################################################################
######### write a function to fit an optimal left-right HMM for one condition
LRHMM_forOneCondition_forOneState = function(mydata, condition_to_use, num_states) {
    # create a vector to store the change points results for all series
    statechange_LRHMM_all = numeric()
    # caculate the number of rows to use
    rows_in_conditon = dim(mydata[[condition_to_use]])[1]
    for (i in 1:rows_in_conditon) {
        # get the best fitting model for each condition
        # best infor contains condition to use, num_states, seed_value, and AIC
        bestmodelInfor = Get_minAIC_LRHMM_for_one_state(mydata, condition_to_use, 
                                                         row_to_use=i, 
                                                         num_states)
        # fit the model again using the best fitting model parametes
        #  if bestmodelInfor == "Error: No convergence for all models", then its length is 1
        if (class(bestmodelInfor) == "character") {
            
            statechange_LRHMM = NA
        } else {
            set.seed(bestmodelInfor["seed_value"])
            RegHMM = depmix(mydata[[condition_to_use]][i, ]~1, 
                           data=data.frame(mydata[[condition_to_use]][i, ]), 
                           nstates=bestmodelInfor["num_states"], 
                           family=gaussian(), 
                           trstart=runif((bestmodelInfor["num_states"])^2))
            paras_RegHMM = c(unlist(getpars(RegHMM)))
            paras_RegHMM[5] = 0
            paras_RegHMM[6] = 1
            LRHMM = setpars(RegHMM, paras_RegHMM)
            cat("current condition: ", condition_to_use, "; ", "curent row: ", i, "\n" )
            LRHMM_fit = fit(LRHMM, verbose = FALSE)
            
            # get the estimated state for each time admin and find the change point
            LRHMM_esttrans = posterior(LRHMM_fit)
            statechange_LRHMM = 1+which(diff(LRHMM_esttrans$state)!=0)
        }
        # concadinate the change points for all series
        statechange_LRHMM_all = c(statechange_LRHMM_all, statechange_LRHMM)
    }  
    return(statechange_LRHMM_all)
}

##############################################################################################
######### write a function to fit a left-right HMM for one condition
LRHMM_forMultiConditions_forOneState = function(mydata, num_states){
    statechange_LRHMM_MultiConditions = list()
    num_of_conditions = length(mydata)
    for (i in 1:num_of_conditions){
        statechange_LRHMM_all_oneCondition = LRHMM_forOneCondition_forOneState(mydata, condition_to_use = i, num_states)
        statechange_LRHMM_MultiConditions[[i]] = statechange_LRHMM_all_oneCondition
    }
    return(statechange_LRHMM_MultiConditions)
}

##############################################################################################
######### get the state change for data GenData_NoSeason_100admin_1cp
begintime_LRHMM = Sys.time()
# LRHMM_NoSeason_100admin_1cp_results = LRHMM_forMultiConditions_forOneState(mydata=GenData_NoSeason_100admin_1cp, num_states=2)
# save(LRHMM_NoSeason_100admin_1cp_results, file="LRHMM_NoSeason_100admin_1cp_results.Rdata")

# LRHMM_Season_100admin_1cp_results = LRHMM_forMultiConditions_forOneState(mydata=GenRes_Season_100admin_1cp, num_states=2)
# save(LRHMM_Season_100admin_1cp_results, file="LRHMM_Season_100admin_1cp_results.Rdata")

LRHMM_NoSeason_50admin_1cp_results = LRHMM_forMultiConditions_forOneState(mydata=GenData_NoSeason_50admin_1cp, num_states=2)
save(LRHMM_NoSeason_50admin_1cp_results, file="LRHMM_NoSeason_50admin_1cp_results.Rdata")

# LRHMM_Season_50admin_1cp_results = LRHMM_forMultiConditions_forOneState(mydata=GenRes_Season_50admin_1cp, num_states=2)
# save(LRHMM_Season_50admin_1cp_results, file="LRHMM_Season_50admin_1cp_results.Rdata")

endtime_LRHMM = Sys.time()
