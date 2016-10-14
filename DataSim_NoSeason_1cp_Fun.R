
####################################################################################
##################### data generation function (no seasonality) ####################
####################################################################################

######## a function that can generate one series of n mean test scores ########
## n refers to the number of test admins in each year (bacially in year 2015)
## seed refers to the seed for the residuls
## sd is also for the residuls 
## cpos refers to the position of change point (1 to j, j+1 to n)
## mean1 refers to the mean of the first subsequence
## mean2 refers to the mean of the second subsequence

## simulate 100 test administrations for year 2015 without seasonality 
One_NoSeason_1cp_15data = function (n, seed, sdres, cpos, mean1, mean2){
    ## simulate the residuals with seed as a parameter
    #set.seed(123)
    set.seed(seed)
    w = rnorm(n, mean=0, sd=sdres)
    ## simulate the artifical abrupt changes with change point at cpos
    mean = c(rep(mean1, cpos), rep(mean2, (n - cpos)))
    ## ssimulate the mean test scores: mean + seasonal + residuls
    x = mean + w
    return(x)
}


## check the function
#mydata = One_NoSeason_1cp_15data(30, 12348, 0.5, 10, 20, 25)
#plot(mydata)
#library(lmtest)
#dwtest(mydata~1)
#dwtest(mydata[1:10]~1)
#dwtest(mydata[11:30]~1)

######## a function that can generate m series of 100 mean test scores for each year ########
## m is the number of replications
MData_NoSeason_1cp_sim = function(m, n, seed, sdres, cpos, mean1, mean2){
    ## create a veactor that can store the data
    Allseries = vector()  
    replications = paste("Data_", n, "_NoSeason",1:m,sep="")
    
    for(i in 1:m){
        seeds = seed + 21*i
        mydata = One_NoSeason_1cp_15data(n, seeds, sdres, cpos, mean1, mean2)
        Allseries = rbind(Allseries, mydata)
    }
    
    row.names(Allseries) = replications
    
    return(Allseries)
}

## check the function
#Mmydata = MData_NoSeason_1cp_sim(10, 100, 123, 0.5, 50, 20, 25)

