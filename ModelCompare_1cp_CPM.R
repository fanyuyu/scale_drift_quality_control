################################################################################
################## get CPM model results for data with 1 cp ####################
################################################################################

setwd("C:/Users/Yuyu/Dropbox/7.6 HMM project/data")
### import the source files for data modeling
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/CPM.R")
### load results data of all models
load("GenData_NoSeason_100admin_1cp.Rdata")
load("GenData_NoSeason_50admin_1cp.Rdata")
load("GenRes_Season_100admin_1cp.Rdata")
load("GenRes_Season_50admin_1cp.Rdata")


### set the CP parameter vector/matrix
OneCP_100admin_CPPosition = c(20, 20, 50, 50, 80, 80,
                              20, 20, 50, 50, 80, 80)
OneCP_50admin_CPPosition = OneCP_100admin_CPPosition/2
# TwoCP_100admin_CPPosition = matrix(c(20, 50, 20, 50, 20, 50, 20, 50, 20, 50, 20, 50,
#                                      20, 80, 20, 80, 20, 80, 20, 80, 20, 80, 20, 80,
#                                      50, 80, 50, 80, 50, 80, 50, 80, 50, 80, 50, 80), 
#                                    nrow = 18, ncol = 2, byrow = T)
# TwoCP_50admin_CPPosition = TwoCP_100admin_CPPosition/2

###################### apply the CPM to each series in each condition ####################
########### write a function to apply CPM model on data with 1CP
CPM_on_dataOneCP = function(mydata){

    v = length(mydata) # v is the number of conditions in the data list
    w = dim(mydata[[1]])[1] # w is the number of replications/serieses in one condition
    n = dim(mydata[[1]])[2] # n is the number of test admins in one series
    
    onedata_tvalues_maxpos = vector()
    onedata_tvalues_max = vector()
    
    for (i in 1:v){
        onecondition_tvalues_maxpos = numeric()
        onecondition_tvalues_max = numeric()
        
        for (j in 1:w){
            oneseries_tvalues = t_stat_all(mydata[[i]][j, ], n)
            
            oneseries_tvalues_maxpos = which.max(abs(oneseries_tvalues))
            oneseries_tvalues_max = oneseries_tvalues[oneseries_tvalues_maxpos]
            
            onecondition_tvalues_maxpos = c(onecondition_tvalues_maxpos, oneseries_tvalues_maxpos)
            onecondition_tvalues_max = c(onecondition_tvalues_max, oneseries_tvalues_max)
        }
        
    onedata_tvalues_maxpos = rbind(onedata_tvalues_maxpos, onecondition_tvalues_maxpos)
    onedata_tvalues_max = rbind(onedata_tvalues_max, onecondition_tvalues_max)
    
    }
    return(list(onedata_tvalues_maxpos, onedata_tvalues_max))
}

############ get the prediction results for data without and with seasonality and 1cp
### CPM_NoSeason_100admin_1cp
CPM_NoSeason_100admin_1cp_results = CPM_on_dataOneCP(GenData_NoSeason_100admin_1cp)
CorrectPre_CPM_NoSeason_100admin_1cp = numeric()
for (i in 1:12){
    CPM_correctPre_onecondition =  sum(CPM_NoSeason_100admin_1cp_results[[1]][i, ] == 
                                           OneCP_100admin_CPPosition[i])
    CorrectPre_CPM_NoSeason_100admin_1cp = c(CorrectPre_CPM_NoSeason_100admin_1cp, 
                                             CPM_correctPre_onecondition)
}
CorrectPre_CPM_NoSeason_100admin_1cp
sum(abs(CPM_NoSeason_100admin_1cp_results[[2]]) < 4)


### CPM_NoSeason_50admin_1cp
CPM_NoSeason_50admin_1cp_results = CPM_on_dataOneCP(GenData_NoSeason_50admin_1cp)
CorrectPre_CPM_NoSeason_50admin_1cp = numeric()
for (i in 1:12){
    CPM_correctPre_onecondition =  sum(CPM_NoSeason_50admin_1cp_results[[1]][i, ] == 
                                           OneCP_50admin_CPPosition[i])
    CorrectPre_CPM_NoSeason_50admin_1cp = c(CorrectPre_CPM_NoSeason_50admin_1cp, 
                                            CPM_correctPre_onecondition)
}
CorrectPre_CPM_NoSeason_50admin_1cp
sum(abs(CPM_NoSeason_50admin_1cp_results[[2]]) < 4)


### CPM_Season_100admin_1cp
CPM_Season_100admin_1cp_results = CPM_on_dataOneCP(GenRes_Season_100admin_1cp)

CorrectPre_CPM_Season_100admin_1cp = numeric()
for (i in 1:12){
    CPM_correctPre_onecondition =  sum(CPM_Season_100admin_1cp_results[[1]][i, ] == 
                                           OneCP_100admin_CPPosition[i])
    CorrectPre_CPM_Season_100admin_1cp = c(CorrectPre_CPM_Season_100admin_1cp, 
                                           CPM_correctPre_onecondition)
}
CorrectPre_CPM_Season_100admin_1cp
sum(abs(CPM_Season_100admin_1cp_results[[2]]) < 4)


### CPM_Season_50admin_1cp
CPM_Season_50admin_1cp_results = CPM_on_dataOneCP(GenRes_Season_50admin_1cp)

CorrectPre_CPM_Season_50admin_1cp = numeric()
for (i in 1:12){
    CPM_correctPre_onecondition =  sum(CPM_Season_50admin_1cp_results[[1]][i, ] == 
                                           OneCP_50admin_CPPosition[i])
    CorrectPre_CPM_Season_50admin_1cp = c(CorrectPre_CPM_Season_50admin_1cp, 
                                          CPM_correctPre_onecondition)
}
CorrectPre_CPM_Season_50admin_1cp
sum(abs(CPM_Season_50admin_1cp_results[[2]]) < 4)









# ##############################################################################
# #### a small test for data with 2cp
# CPM_NoSeason_100admin_2cp_results <- vector()
# for (i in 1:1000){
#     results = t_stat_all(GenData_NoSeason_100admin_2cp[[10]][i, ], 100)
#     CPM_NoSeason_100admin_2cp_results = rbind(CPM_NoSeason_100admin_2cp_results, results)
# }
# CPM_NoSeason_100admin_2cp_results
# 
# CPM_NoSeason_100admin_2cp_results_max = numeric()
# for (i in 1:1000){
#     values = which.max(abs(CPM_NoSeason_100admin_2cp_results[i, ]))
#     CPM_NoSeason_100admin_2cp_results_max = c(CPM_NoSeason_100admin_2cp_results_max, values)
# }
# CPM_NoSeason_100admin_2cp_results_max
# 
# sum(CPM_NoSeason_100admin_2cp_results_max == 50)
# ##############################################################################


