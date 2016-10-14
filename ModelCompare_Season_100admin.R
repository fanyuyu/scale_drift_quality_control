#################################################################################
### 100 replications, change point position 20/21, mean1 = 20, and mean2 = 22 ###
#####################  100 test administrations in each year  ###################
#################################################################################
setwd("C:/Users/Yuyu/Dropbox/7.6 HMM project/data")
### import the source files for data simulation and data modeling
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/Date_100admin.R")
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/CPM.R")
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/DataSimFun_NoSeason_2cp.R")
source("C:/Users/Yuyu/Dropbox/7.6 HMM project/data/DataSimFun_Season_2cp.R")

# 1st, creat the parameters matrix need to be used in the data generation
para_100admin = matrix(c(20,	20,	22,
                         20,	20,	25,
                         50,	20,	22,
                         50,	20,	25,
                         80,	20,	22,
                         80,	20,	25), 6, 3, byrow=T)
colnames(para_100admin) = c("Cpos", "Mean1", "Mean2")
para_100admin = as.data.frame(para_100admin)

### simulate 1000 data series with 100 administrations in each year
m = 1000
n = 100
GenData_NoSeason_2cp =list()
GenRes_Season_2cp = list()
for (i in 1:6){
  # generate date without seasonality
  GenData_NoSeason_2cp[[i]] = MData_NoSeason_2cp_sim(m=1000, n=100, seed=13241, sdres=0.5,
                                                    cpos=para_100admin$Cpos[i], 
                                                    mean1=para_100admin$Mean1[i],
                                                    mean2=para_100admin$Mean2[i])
  # generate date with seasonality  
  GenRes_Season_2cp[[i]] = MRes_Season_2cp_sim(m=1000, n=100, 
                                              date1314=date1314_100admin, 
                                              date15=date15_100admin,
                                              seed=13241, sdres=0.5,
                                              cpos=para_100admin$Cpos[i], 
                                              mean1=para_100admin$Mean1[i],
                                              mean2=para_100admin$Mean2[i])
}
## assign names to each condition
# specify the mean difference (for ascending and descending)
difvalue = para_100admin$Mean2 - para_100admin$Mean1
dif = difvalue
dif[difvalue > 0] = paste("P", difvalue[difvalue > 0], sep="")
dif[difvalue < 0] = paste("N", abs(difvalue[difvalue < 0]), sep="")
# assign names to each condition 
names(GenData_NoSeason_2cp)  = paste("Data_NoSeason_2cp", "_", para_100admin$Cpos[1:6], "_", "dif", dif[1:6], sep="")
names(GenRes_Season_2cp) = paste("Res_Season_2cp", "_", para_100admin$Cpos[1:6], "_", "dif", dif[1:6], sep="")


### apply the CPM to each series
results_Season_CPM = vector()
for (i in 1:1000){
    results = t_stat_all(GenData_NoSeason_2cp[[1]][i, ], 100)
    results_Season_CPM = rbind(results_Season_CPM, results)
}

results_Season_CPM_max = numeric()
for (i in 1:1000){
    values = which.max(abs(results_Season_CPM[i, ]))
    results_Season_CPM_max = c(results_Season_CPM_max, values)
}

sum(results_Season_CPM_max == 20)


### apply the regular HMM to ech series
#install.packages("depmixS4")
library(depmixS4)

# fit a regular HMM with 2 states
statechange_regularHMM_all = numeric()
statechange_LRHMM_all = numeric()
for (i in 1:m) {
    seedvalue = floor(runif(1, 100, 10000))
    set.seed(seedvalue)
    HMMmodel_Season_regular = depmix(GenData_NoSeason_2cp[[2]][i, ]~1, 
                                        data=data.frame(GenData_NoSeason_2cp[[2]][i, ]), 
                                        nstates=2, family=gaussian(), trstart=runif(4))
    RegularHMM_fit = fit(HMMmodel_Season_regular)
    
    # fit a left-right HMM with 2 states by fixing the transition prob from S2 to S1 as 0
    pars_Season = c(unlist(getpars(RegularHMM_fit)))
    pars_Season[5] = 0
    pars_Season[6] = 1
    
    HMMmodel_Season_LR = setpars(HMMmodel_Season_regular, pars_Season)
    LRHMM_fit = fit(HMMmodel_Season_LR)
    
    #get the estimated state for each time admin and find the change point
    regular_esttrans = posterior(RegularHMM_fit)
    statechange_regularHMM = 1+which(diff(regular_esttrans$state)!=0)
    LR_esttrans= posterior(LRHMM_fit)
    statechange_LRHMM = 1+which(diff(LR_esttrans$state)!=0)
    
    statechange_regularHMM_all = c(statechange_regularHMM_all, statechange_regularHMM)
    statechange_LRHMM_all = c(statechange_LRHMM_all, statechange_LRHMM)
}
statechange_regularHMM_all
#statechange_LRHMM_all

sum(statechange_regularHMM_all == 21)
#sum(statechange_LRHMM_all == 21)

