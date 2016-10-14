##############################################################
############# set up the test administration date ############
##############################################################

###########################################
############# 50 in each year #############
###########################################
# test date in 2013
# Tests are administrated on Wednesday of each month in 2013, start from 2013.1.9
date_interval_2013_50admin = rep(7, 49)
date13_50admin = numeric()
date13_50admin[1] = 9
for (i in 2:50) date13_50admin[i] = date_interval_2013_50admin[i-1] + date13_50admin[i-1]
date13_50admin

# test date in 2014 
# Tests are administrated on Wednesday of each month in 2014, start from 2014.1.8
date_interval_2014_50admin = rep(7, 49)
date14_50admin = numeric()
date14_50admin[1] = 8
for (i in 2:50) date14_50admin[i] = date_interval_2014_50admin[i-1] + date14_50admin[i-1]
date14_50admin

# combine test dates in 2013 and 2014
date1314_50admin = c(date13_50admin, date14_50admin)

# test date in 2015
# Tests are administrated on Wednesday of each month in 2015, start from 2014.1.8
date_interval_2015_50admin = rep(7, 49)
date15_50admin = numeric()
date15_50admin[1] = 7
for (i in 2:50) date15_50admin[i] = date_interval_2015_50admin[i-1] + date15_50admin[i-1]
date15_50admin

