#########################################################################
###################### scale scores monitor_CPM model ###################
#########################################################################

################################################################################
########################## single-point CPM ############################
# calculate the mean of the left half
mean_seq1 <- function(x, j, n) {
    if (j > n)
        print("ERROR! index j out of range in function mean_seq1")
    else
        return(sum(x[1 : j]) / j)     
}


# calculate the mean of the right half
mean_seq2 <- function(x, j, n) {
    if (j >= n)
        print("ERROR! index j out of range in function mean_seq2")
    else
        return(sum(x[(j + 1) : n]) / (n-j))    
}

# calculate the deviation of the left half
dev1 <- function(x, j, n) {
    sum_squares = 0
    for (i in 1 : j){
        square_diff <- ((x[i] - mean_seq1(x, j, n)))^2 
        sum_squares <- sum_squares + square_diff
    }
    return (sum_squares)
}


# calculate the deviation of the right half
dev2 <- function(x, j, n) {
    sum_squares = 0
    for (i in (j + 1) : n) {
        square_diff <- ((x[i] - mean_seq2(x, j, n)))^2 
        sum_squares <- sum_squares + square_diff
    }
    return (sum_squares)
}

# calculat the pooled variance 
pool_var <- function(x, j, n) {
    if (n <= 2)
        print("ERROR! n must be bigger than 2")
    else {
        d1 <- dev1(x, j, n)
        d2 <- dev2(x, j, n)
        return((d1 + d2) / (n - 2))    
    }
}

# calculat the t statistic for a given point j
t_stat <- function(x, j, n) {
    coef <- sqrt(j * (n - j) / n) 
    d_std <- (mean_seq1(x, j, n) - mean_seq2(x, j, n)) / sqrt(pool_var(x, j, n))
    return(coef*d_std)        
}

# calculat the t statistic for every possible j
t_stat_all <- function(x, n) {
    t_statistics <- vector(length=(n-1))
    for (j in 1 : (n-1)) {
        t_statistics[j] <- t_stat(x, j, n)
        #cat(j, ": ", t_statistics[j], "\n")
    }
    return(t_statistics)
}

########################## TEST single-point CPM ########################
#subseq1 <- rnorm(4, mean=70, sd=2)
#subseq2 <- rnorm(5, mean=80, sd=2)
#seq <- c(subseq1, subseq2)
#seq
#t_stat_all(seq, 9)
#which.max(abs(t_stat_all(seq, 9)))




