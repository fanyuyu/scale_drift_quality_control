##############################################################
############# set up the test administration date ############
##############################################################

###########################################
############# 20 in each year #############
###########################################
## test date in 2013
# Tests are administrated on second Monday and second Wednesday of each month in 2013, start from 2015.1.14
#date_interval_2013_20admin = rep(c(9,21), 10)
#date13_20admin = numeric()
#date13_20admin[1] = 14
#for (i in 2:20) date13_20admin[i] = date_interval_2013_20admin[i-1] + date13_20admin[i-1]

## test date in 2014 
# Tests are administrated on second Monday and second Wednesday of each month in 2015, start from 2014.1.6
#date_interval_2014_20admin = rep(c(3,4), 50)
#date14_20admin = numeric()
#date14_20admin[1] = 6
#for (i in 2:100) date14_20admin[i] = date_interval_2014_20admin[i-1] + date14_20admin[i-1]

## combine test dates in 2013 and 2014
#date1314_20admin = c(date13_20admin, date14_20admin)

## test date in 2015
# Tests are administrated on second Monday and second Wednesday of each month in 2015, start from 2015.1.5
#date_interval_2015_20admin = c(rep(c(3,4), 49), 3)
#date15_20admin = numeric()
#date15_20admin[1] = 5
#for (i in 2:100) date15_20admin[i] = date_interval_2015_20admin[i-1] + date15_20admin[i-1]

###########################################
############# 100 in each year ############
###########################################
## test date in 2013
# Tests are administrated on every Monday and Thursday in 2013, start from 2015.1.7
date_interval_2013_100admin = rep(c(3,4), 50)
date13_100admin = numeric()
date13_100admin[1] = 7
for (i in 2:100) date13_100admin[i] = date_interval_2013_100admin[i-1] + date13_100admin[i-1]

## test date in 2014 
# Tests are administrated on every Monday and Thursday in 2015, start from 2014.1.6
date_interval_2014_100admin = rep(c(3,4), 50)
date14_100admin = numeric()
date14_100admin[1] = 6
for (i in 2:100) date14_100admin[i] = date_interval_2014_100admin[i-1] + date14_100admin[i-1]

## combine test dates in 2013 and 2014
date1314_100admin = c(date13_100admin, date14_100admin)

## test date in 2015
# Tests are administrated on every Monday and Thursday in 2015, start from 2015.1.5
date_interval_2015_100admin = c(rep(c(3,4), 49), 3)
date15_100admin = numeric()
date15_100admin[1] = 5
for (i in 2:100) date15_100admin[i] = date_interval_2015_100admin[i-1] + date15_100admin[i-1]

