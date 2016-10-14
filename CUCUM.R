#########################################################################
################## scale scores monitor_CUSUM chart #####################
#########################################################################

########################################################################
############################## CUSUM chart #############################
# calculate the mean of the left half (Mu1)
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

# determine the value of k(k_value), h(h_value), K(K_refernce), and H(H_refernce)
K_reference <- function(x, j, n){
    K_ref <- abs(mean_seq2(x, j, n) - mean_seq1(x, j, n)) / 2 
    return(K_ref)
}

H_reference <- function(x, j, n){
    kprime <- abs(mean_seq2(x, j, n) - mean_seq1(x, j, n)) / (2 * sqrt(pool_var(x, j, n)))
    kvector <- c(0.25, 0.5, 0.75, 1.0, 1.25, 1.5)
    hvector <- c(8.01, 4.77, 3.34, 2.52, 1.99, 1.61)
    kabsdiff <- abs(kvector - kprime)
    h_value = hvector[which.min(kabsdiff)]
    H_ref = h_value * sqrt(pool_var(x, j, n))
    return(H_ref)
}


# calculat the C_plus statistic for a given point i with change point j
C_plus <- function(x, index, j, n) {
    K = K_reference(x, j, n)
    if (index == 0) 
        return(0)
    else 
        value1 <- x[index] - (mean_seq1(x, j, n) + K) + C_plus(x, (index-1), j, n)
        value2 <- max(0, value1)
        return (value2)
}

C_minus <- function(x, index, j, n) {
    K = K_reference(x, j, n)
    if (index == 0) 
        return(0)
    else 
        value1 <- (-x[index]) + (mean_seq1(x, j, n) - K) + C_minus(x, (index-1), j, n)
        value2 <- max(0, value1)
        return (value2)
}
    
    


########################## TEST the CUSUM chart ########################
#subseq1 <- rnorm(200, mean=70, sd=2)
#subseq2 <- rnorm(150, mean=80, sd=2)
#seq <- c(subseq1, subseq2)
#seq
#mean_seq1(seq, 40, length(seq))
#mean_seq2(seq, 40, length(seq))
#pool_var(seq, 40, length(seq))
#K_reference(seq, 200, length(seq))
#H_reference(seq, 200, length(seq))
#C_plus(x=seq, index=330, j=40, n=length(seq))
#C_minus(x=seq, index=330, j=40, n=length(seq))





