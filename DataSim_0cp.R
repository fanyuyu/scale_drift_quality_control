#########################################################################
######## simulate data without any changes to get critial values ########
#########################################################################

setwd("C:/Users/Yuyu/Dropbox/7.6 HMM project/data")
### import the source files for data generation and modeling
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/DataSim_NoSeason_1cp_Fun.R")
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/DataSim_Season_1cp_Fun.R")
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/Date_50admin.R")
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/Date_100admin.R")

source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/CPM.R")


##################### simulate data with 100 admins ############################
# 1st, creat the parameters matrix need to be used in the data generation
# 50 is used becuase I could apply use equation for 1CP to generate 0CP
para_100admin_0cp = matrix(c(50, 20, 20,
                             50, 22, 22,
                             50, 25, 25), 3, 3, byrow=T)
colnames(para_100admin_0cp) = c("Cpos", "Mean1", "Mean2")
para_100admin_0cp = as.data.frame(para_100admin_0cp)

### simulate 1000 data series with 100 administrations yearly and 1 change point
GenData_NoSeason_100admin_0cp =list()
GenRes_Season_100admin_0cp = list()
for (i in 1:3){
    # generate date without seasonality
    GenData_NoSeason_100admin_0cp[[i]] = MData_NoSeason_1cp_sim(m=10000, n=100, seed=39247 + i*2, sdres=0.5,
                                                                cpos=para_100admin_0cp$Cpos[i], 
                                                                mean1=para_100admin_0cp$Mean1[i],
                                                                mean2=para_100admin_0cp$Mean2[i])
    # generate date with seasonality  
    GenRes_Season_100admin_0cp[[i]] = MRes_Season_1cp_sim(m=10000, n=100, 
                                                          date1314=date1314_100admin, 
                                                          date15=date15_100admin,
                                                          seed=93241 + i*2, sdres=0.5,
                                                          cpos=para_100admin_0cp$Cpos[i], 
                                                          mean1=para_100admin_0cp$Mean1[i],
                                                          mean2=para_100admin_0cp$Mean2[i])
}

# assign names to each condition
names(GenData_NoSeason_100admin_0cp) = paste("Data_NoSeason_100admin_0cp", "_mean", c(20, 22, 25), sep="")
names(GenRes_Season_100admin_0cp) = paste("Res_Season_100admin_0cp", "_mean_", c(20, 22, 25), sep="")

# check the data
plot(GenData_NoSeason_100admin_0cp[[1]][1, ])
plot(GenData_NoSeason_100admin_0cp[[3]][1, ])

plot(GenRes_Season_100admin_0cp[[1]][1, ])
plot(GenRes_Season_100admin_0cp[[3]][1, ])

# save the data files
save(GenData_NoSeason_100admin_0cp, file="GenData_NoSeason_100admin_0cp.Rdata")
save(GenRes_Season_100admin_0cp, file="GenRes_Season_100admin_0cp.Rdata")

# save the data files as a data frame (just for my reference)
# GenData_NoSeason_100admin_0cp_DataFrame <- do.call("rbind", lapply(GenData_NoSeason_100admin_0cp, as.data.frame))

##################### simulate data with 50 admins ############################
# 1st, creat the parameters matrix need to be used in the data generation
para_50admin_0cp = matrix(c(25, 20, 20,
                             25, 22, 22,
                             25, 25, 25), 3, 3, byrow=T)
colnames(para_50admin_0cp) = c("Cpos", "Mean1", "Mean2")
para_50admin_0cp = as.data.frame(para_50admin_0cp)

# 2nd simulate 1000 data series with 50 administrations yearly and 1 change point
GenData_NoSeason_50admin_0cp =list()
GenRes_Season_50admin_0cp = list()
for (i in 1:3){
    # generate date without seasonality
    GenData_NoSeason_50admin_0cp[[i]] = MData_NoSeason_1cp_sim(m=10000, n=50, seed=13241 + i*3, sdres=0.5,
                                                               cpos=para_50admin_0cp$Cpos[i], 
                                                               mean1=para_50admin_0cp$Mean1[i],
                                                               mean2=para_50admin_0cp$Mean2[i])
    # generate date with seasonality  
    GenRes_Season_50admin_0cp[[i]] = MRes_Season_1cp_sim(m=10000, n=50, 
                                                         date1314=date1314_50admin, 
                                                         date15=date15_50admin,
                                                         seed=35627 + i*3, sdres=0.5,
                                                         cpos=para_50admin_0cp$Cpos[i], 
                                                         mean1=para_50admin_0cp$Mean1[i],
                                                         mean2=para_50admin_0cp$Mean2[i])
}

# assign names to each condition 
names(GenData_NoSeason_50admin_0cp) = paste("Data_NoSeason_0cp", "_mean", c(20, 22, 25), sep="")
names(GenRes_Season_50admin_0cp) = paste("Res_Season_0cp", "_mean", c(20, 22, 25), sep="")

# check the data
plot(GenData_NoSeason_50admin_0cp[[1]][1, ])
plot(GenData_NoSeason_50admin_0cp[[1]][2, ])
plot(GenData_NoSeason_50admin_0cp[[2]][1, ])


# save the data files
save(GenData_NoSeason_50admin_0cp, file="GenData_NoSeason_50admin_0cp.Rdata")
save(GenRes_Season_50admin_0cp, file="GenRes_Season_50admin_0cp.Rdata")



#########################################################################
######## apply the t-test statistics to get the critial values ##########
#########################################################################
# apply the CPM test on the simulated series
CPM_on_data0CP_max = function(mydata){
    v = length(mydata) # v is the number of conditions in the data list
    w = dim(mydata[[1]])[1] # w is the number of replications/serieses in one condition
    n = dim(mydata[[1]])[2] # n is the number of test admins in one series
    onedata_tvalues_max = vector()
    for (i in 1:length(mydata)){
        onecondition_tvalues_max = numeric()
        for (j in 1:w){
            oneseries_tvalues = t_stat_all(mydata[[i]][j, ], n)
            oneseries_tvalues_max = oneseries_tvalues[which.max(abs(oneseries_tvalues))]
            onecondition_tvalues_max = c(onecondition_tvalues_max, oneseries_tvalues_max)
        }
        onedata_tvalues_max = rbind(onedata_tvalues_max, onecondition_tvalues_max)
    }
    return(onedata_tvalues_max)
}



################## ciritcal values for data with 50 dmins ######################
# NO seasonality
NoSeason_50admin_0cp_max = CPM_on_data0CP_max(GenData_NoSeason_50admin_0cp)

quantile(NoSeason_50admin_0cp_max[1, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.154969  3.167533 
quantile(NoSeason_50admin_0cp_max[2, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.127110  3.167032 
quantile(NoSeason_50admin_0cp_max[3, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.201382  3.165606 


# Seasonality
Season_50admin_0cp_max = CPM_on_data0CP_max(GenRes_Season_50admin_0cp)

quantile(Season_50admin_0cp_max[1, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.403969  3.459418 
quantile(Season_50admin_0cp_max[2, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.419478  3.406210 
quantile(Season_50admin_0cp_max[3, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.432077  3.392989 

################## ciritcal values for data with 100 dmins ######################
# No seasonality
NoSeason_100admin_0cp_max = CPM_on_data0CP_max(GenData_NoSeason_100admin_0cp)

quantile(NoSeason_100admin_0cp_max[1, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.101000  3.155134 
quantile(NoSeason_100admin_0cp_max[1, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.101000  3.155134
quantile(NoSeason_100admin_0cp_max[1, ], c(.025, .975))


# Seasonality
Season_100admin_0cp_max = CPM_on_data0CP_max(GenRes_Season_100admin_0cp)

quantile(Season_100admin_0cp_max[1, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.382654  3.336874 
quantile(Season_100admin_0cp_max[1, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.382654  3.336874 
quantile(Season_100admin_0cp_max[1, ], c(.025, .975))
#         2.5%     97.5% 
#     -3.382654  3.336874 

