#####################################################################
##################### data generation function  #####################
#####################################################################

######## 1st, generate the function to recover parameters of the Harmonic Reg based on the frist two years data
##### (1) write a function that can generate seasonal data for year 2013 and 2014 (1314data)
## n refers to length of test admins in each year
## date1314 refers to the tests dates of year 2013 and year 2014
## seed refers to the seed for the residuls
## sd is also for the residuls 
One_Season_1314data = function(n, date1314, seed, sdres){
  ## simulate the seasonal effect with b1 = 0.04 and b2 = -0.28
  seasonal = 0.04*cos(2*pi*date1314/365) - 0.28*sin(2*pi*date1314/365)
  ## simulate the residuals with seed as a parameter
  set.seed(seed)
  w = rnorm(2*n, mean=0, sd=sdres)
  ## ssimulate the mean test scores: mean + seasonal + residuls
  x = seasonal + w
  return(x)
}
# check the function
#mydata1 = One_Season_1314data(n = 100, date1314=date1314_100admin, seed = 1157, sdres = 0.5)
#plot(mydata1)

##### (2) write a function to generate 2015 data 
## seed refers to the seed for the residuls
## sd is also for the residuls 
## cpos refers to the position of change point (1 to j, j+1 to n)
## mean1 refers to the mean of the first subsequence
## mean2 refers to the mean of the second subsequence
One_Season_1cp_15data = function(n, date15, seed, sdres, cpos, mean1, mean2){
    ## simulate the seasonal effect with b1 = 0.04 and b2 = -0.28
    seasonal = 0.04*cos(2*pi*date15/365) - 0.28*sin(2*pi*date15/365)
    ## simulate the residuals with seed as a parameter
    set.seed(seed+12871)
    w = rnorm(n, mean=0, sd=sdres)
    ## simulate the artifical abrupt changes with change point at cpos
    mean = c(rep(mean1, cpos), rep(mean2, (n - cpos)))
    ## ssimulate the mean test scores of year 2015: mean + seasonal + residuls
    x = mean + seasonal + w
    return(x)
}
# check the function
#mydata2 = One_Season_1cp_15data(100, date15_100admin, 1157, 0.5, 20, 20, 22)
#plot(mydata2)

##### (3) write a function to generate 2015 data residuals
One_Season_1cp_15res = function(date1314, date15, data1, data2){
  ## apply the harmonic function on 1314 data to recover the seasonal parameters
  HarModel = lm(data1 ~ I(cos(2*pi*date1314/365)) + I(sin(2*pi*date1314/365)))
  HarPara = coefficients(HarModel)
  #print(summary(HarModel))
  ## apply the 15 data on the fitted models of the 1314 data
  CosPart = cos(2*pi*date15/365)
  SinPart = sin(2*pi*date15/365)
  PredValues = HarPara[1] + HarPara[2]*CosPart + HarPara[3]*SinPart
  ResValues = data2 - PredValues
  return(ResValues)
}

# check the function
#Res <- One_Season_1cp_15res(date1314_100admin, date15_100admin, mydata1, mydata2)
#plot(Res)
#library(lmtest)
#dwtest(Res~1)
#dwtest(Res[1:20]~1)
#dwtest(Res[21:100]~1)

 
######## 2nd, creat a function that can generate m series of 100 mean test scores ########
## m is the number of replications
MRes_Season_1cp_sim = function(m, n, date1314, date15, seed, sdres, cpos, mean1, mean2){
    # create a vector that can store all the data
    Allseries = vector()  
    # give each series a name
    replications=paste("Res_", n, "_Season",1:m,sep="")
    # generate the data
    for(i in 1:m){
        seeds = seed + 11*i
        mydata1 = One_Season_1314data(n, date1314, seeds, sdres) # use ! seeds !
        mydata2 = One_Season_1cp_15data(n, date15, seeds, sdres, cpos, mean1, mean2) # use !seeds!
        res15 = One_Season_1cp_15res(date1314, date15, mydata1, mydata2)
        Allseries = rbind(Allseries, res15)
    }
    # add row names to the residual series
    row.names(Allseries) = replications
    # return all residual series
    return(Allseries)
}
# check the function
#Res_100admin_1cp_10series = MRes_Season_1cp_sim(m=10, n=100, 
#                    date1314=date1314_100admin, date15=date15_100admin,
#                    seed=1157, sdres=0.5, cpos=20, mean1=20, mean2=22)
#Res_100admin_1cp_10series
#for (i in 1:10){
#  dwResult1 = dwtest(Res_100admin_1cp_10series[1, 1:20]~1)
#  dwResult2 = dwtest(Res_100admin_1cp_10series[1, 21:100]~1)
#  print(dwResult1)
#  print(dwResult2)
#}



