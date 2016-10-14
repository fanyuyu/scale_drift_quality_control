##########################################################################################################
################ data generation:  100 test administrations yearly, and 2 change points  #################
##########################################################################################################
setwd("C:/Users/Yuyu/Dropbox/7.6 HMM project/data")
### import the source files for data simulation
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/Date_100admin.R") #dates for 100 admin
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/Date_50admin.R") #dates for 50 admin
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/DataSim_NoSeason_1cp_Fun.R")
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/DataSim_Season_1cp_Fun.R")

####################################################################################
############ simulate data with 100 admins #########################################
####################################################################################
# 1st, creat the parameters matrix need to be used in the data generation
para_100admin_1cp = matrix(c(20,	20,	22,
                             20,	20,	25,
                             50,	20,	22,
                             50,	20,	25,
                             80,	20,	22,
                             80,	20,	25,
                             20,	22,	20,
                             20,	25,	20,
                             50,	22,	20,
                             50,	25,	20,
                             80,	22,	20,
                             80,	25,	20), 12, 3, byrow=T)
colnames(para_100admin_1cp) = c("Cpos", "Mean1", "Mean2")
para_100admin_1cp = as.data.frame(para_100admin_1cp)

### simulate 1000 data series with 100 administrations yearly and 1 change point
GenData_NoSeason_100admin_1cp =list()
GenRes_Season_100admin_1cp = list()
for (i in 1:12){
    # generate date without seasonality
    GenData_NoSeason_100admin_1cp[[i]] = MData_NoSeason_1cp_sim(m=1000, n=100, seed=13241, sdres=0.5,
                                                                cpos=para_100admin_1cp$Cpos[i], 
                                                                mean1=para_100admin_1cp$Mean1[i],
                                                                mean2=para_100admin_1cp$Mean2[i])
    # generate date with seasonality  
    GenRes_Season_100admin_1cp[[i]] = MRes_Season_1cp_sim(m=1000, n=100, 
                                                          date1314=date1314_100admin, 
                                                          date15=date15_100admin,
                                                          seed=13241, sdres=0.5,
                                                          cpos=para_100admin_1cp$Cpos[i], 
                                                          mean1=para_100admin_1cp$Mean1[i],
                                                          mean2=para_100admin_1cp$Mean2[i])
}

## assign names to each condition
# specify the mean dif_100adminference (for ascending and descending)
difvalue_100admin = para_100admin_1cp$Mean2 - para_100admin_1cp$Mean1
dif_100admin = difvalue_100admin
dif_100admin[difvalue_100admin > 0] = paste("P", difvalue_100admin[difvalue_100admin > 0], sep="")
dif_100admin[difvalue_100admin < 0] = paste("N", abs(difvalue_100admin[difvalue_100admin < 0]), sep="")

# assign names to each condition 
names(GenData_NoSeason_100admin_1cp)  = paste("Data_NoSeason_100admin_1cp", "_", para_100admin_1cp$Cpos[1:12], "_", "dif_100admin", dif_100admin[1:12], sep="")
names(GenRes_Season_100admin_1cp) = paste("Res_Season_100admin_1cp", "_", para_100admin_1cp$Cpos[1:12], "_", "dif_100admin", dif_100admin[1:12], sep="")

# check the data
plot(GenData_NoSeason_100admin_1cp[[1]][1, ])
plot(GenData_NoSeason_100admin_1cp[[3]][1, ])
plot(GenData_NoSeason_100admin_1cp[[5]][1, ])
plot(GenData_NoSeason_100admin_1cp[[7]][1, ])
plot(GenData_NoSeason_100admin_1cp[[9]][1, ])
plot(GenData_NoSeason_100admin_1cp[[11]][1, ])

# save the data files
save(GenData_NoSeason_100admin_1cp, file="GenData_NoSeason_100admin_1cp.Rdata")
save(GenRes_Season_100admin_1cp, file="GenRes_Season_100admin_1cp.Rdata")

# save the data files as a data frame (just for my reference)
# GenData_NoSeason_100admin_1cp_DataFrame <- do.call("rbind", lapply(GenData_NoSeason_100admin_1cp, as.data.frame))

####################################################################################
############ simulate data with 50 admins #########################################
####################################################################################
# 1st, creat the parameters matrix need to be used in the data generation
para_50admin_1cp = matrix(c( 10,	20,	22,
                             10,	20,	25,
                             25,	20,	22,
                             25,	20,	25,
                             40,	20,	22,
                             40,	20,	25,
                             10,	22,	20,
                             10,	25,	20,
                             25,	22,	20,
                             25,	25,	20,
                             40,	22,	20,
                             40,	25,	20), 12, 3, byrow=T)
colnames(para_50admin_1cp) = c("Cpos", "Mean1", "Mean2")
para_50admin_1cp = as.data.frame(para_50admin_1cp)

# 2nd simulate 1000 data series with 50 administrations yearly and 1 change point
GenData_NoSeason_50admin_1cp =list()
GenRes_Season_50admin_1cp = list()
for (i in 1:12){
    # generate date without seasonality
    GenData_NoSeason_50admin_1cp[[i]] = MData_NoSeason_1cp_sim(m=1000, n=50, seed=13241, sdres=0.5,
                                                               cpos=para_50admin_1cp$Cpos[i], 
                                                               mean1=para_50admin_1cp$Mean1[i],
                                                               mean2=para_50admin_1cp$Mean2[i])
    # generate date with seasonality  
    GenRes_Season_50admin_1cp[[i]] = MRes_Season_1cp_sim(m=1000, n=50, 
                                                          date1314=date1314_50admin, 
                                                          date15=date15_50admin,
                                                          seed=35627, sdres=0.5,
                                                          cpos=para_50admin_1cp$Cpos[i], 
                                                          mean1=para_50admin_1cp$Mean1[i],
                                                          mean2=para_50admin_1cp$Mean2[i])
}

## assign names to each condition
# specify the mean dif_50adminference (for ascending and descending)
difvalue_50admin = para_50admin_1cp$Mean2 - para_50admin_1cp$Mean1
dif_50admin = difvalue_50admin
dif_50admin[difvalue_50admin > 0] = paste("P", difvalue_50admin[difvalue_50admin > 0], sep="")
dif_50admin[difvalue_50admin < 0] = paste("N", abs(difvalue_50admin[difvalue_50admin < 0]), sep="")

# assign names to each condition 
names(GenData_NoSeason_50admin_1cp)  = paste("Data_NoSeason_1cp", "_", para_50admin_1cp$Cpos[1:12], "_", "dif_50admin", dif_50admin[1:12], sep="")
names(GenRes_Season_50admin_1cp) = paste("Res_Season_1cp", "_", para_50admin_1cp$Cpos[1:12], "_", "dif_50admin", dif_50admin[1:12], sep="")

# check the data
plot(GenData_NoSeason_50admin_1cp[[1]][1, ])
plot(GenData_NoSeason_50admin_1cp[[3]][1, ])
plot(GenData_NoSeason_50admin_1cp[[5]][1, ])
plot(GenData_NoSeason_50admin_1cp[[7]][1, ])
plot(GenData_NoSeason_50admin_1cp[[9]][1, ])
plot(GenData_NoSeason_50admin_1cp[[11]][1, ])

# save the data files
save(GenData_NoSeason_50admin_1cp, file="GenData_NoSeason_50admin_1cp.Rdata")
save(GenRes_Season_50admin_1cp, file="GenRes_Season_50admin_1cp.Rdata")
