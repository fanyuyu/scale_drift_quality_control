#######################################################################################################
############################# write a series of funcions that: ########################################
## 1. can get AIC for a left-right HMM model with one seed and one state 
## 2. can get min AIC for a left-right HMM model with a number of seeds (NUM_OF_SEED) for one state
## 3. can get min AIC for a left-right HMM model with a number of seeds (NUM_OF_SEED) across states

Get_minAIC_LRHMM_for_one_seed = function(mydata, condition_to_use, row_to_use, num_states, seed_value) {
    set.seed(seed_value)
    RegHMM = depmix(mydata[[condition_to_use]][row_to_use, ]~1, 
                                       data=data.frame(mydata[[condition_to_use]][row_to_use, ]), 
                                       nstates=num_states, 
                                       family=gaussian(), 
                                       trstart = runif(num_states^2))
    paras_RegHMM = c(unlist(getpars(RegHMM)))
    paras_RegHMM[5] = 0
    paras_RegHMM[6] = 1
    
    LRHMM = setpars(RegHMM, paras_RegHMM)
    LRHMM_fit = fit(LRHMM, verbose = FALSE)
    result = c(condition_to_use, num_states, seed_value, AIC(LRHMM_fit))
    names(result) = c("condition_to_use", "num_states", "seed_value", "AIC")
    return (result)
}

NUM_OF_SEED = 20
Get_minAIC_LRHMM_for_one_state = function(mydata, condition_to_use, row_to_use, num_states) {
    # for one state, iterate multiple seeds and find the minimum AIC
    num_of_seed = NUM_OF_SEED
    # print(str(mydata))
    # randomly generate NUM_OF_SEED seeds with fixed seed 27345
    set.seed(27668)
    seeds = floor(runif(num_of_seed, min=100, max=10000))
    all_result = vector() # all_result is a matrix, each row is AIC related row vector
    ## calculate all results for all seeds
    for (i in 1:num_of_seed) {
        current_seed = seeds[i]
        # print(paste("current seed: ", i, current_seed))
        tryCatch(
            {
                result = Get_minAIC_LRHMM_for_one_seed(mydata, condition_to_use, row_to_use, num_states, current_seed)
                all_result = rbind(all_result, result)
            },
            error=function(e) {
                print("Encounter an error, but we want to ignore!")
                #cat("ERROR :",conditionMessage(e), "\n")
            }
        )
    }
    ## if all_result is empty, return error message "No convergence for all models";
    ## if all_result is a vector, return all_result
    ## if all_result is a matrix, return rows_with_min_AIC
    if (is.vector(all_result)) {
        if (length(all_result)==0) {
            return("Error: No convergence for all models")
        } else if (length(all_result)==4) {
            return(all_result)
        }
    } else if (is.matrix(all_result)){
        ## return the row with the min AIC. If there are multiple mins, return the 1st one
        rows_with_min_AIC = all_result[which(all_result[ ,"AIC"]==min(all_result[ ,"AIC"])), ]
        ## check whether rows_with_min_AIC is a matrix or not
        ## if it is a matrix, return the first row; if a vector, return the vector
        ## !!! attention: dont put class() in the condition, or it would be a string
        if (is.matrix(rows_with_min_AIC)) {
            return(rows_with_min_AIC[1, ])
        } else {
            return(rows_with_min_AIC)
        }
    }
}

Get_minAIC_LRHMM_for_multi_states = function(mydata, condition_to_use, row_to_use){
    # starting from 2 states, we iterate up to 3, 4, 5, etc states to find minimum AIC
    # once one minimum is found, we immediately return it. Otherwise, keep iterating
    first_num_states = 2
    max_num_states = 10 # we don't consider states number larger than 10
    
    current_num_states = first_num_states
    current_min_AIC = Get_minAIC_LRHMM_for_one_state(mydata, condition_to_use, row_to_use, current_num_states)
    found_min = FALSE
    
    while (found_min == FALSE) {
        current_num_states = current_num_states + 1
        if (current_num_states > max_num_states) break
        
        next_min_AIC = Get_minAIC_LRHMM_for_one_state(mydata, condition_to_use, row_to_use, current_num_states)
        # cat("Current AIC", current_min_AIC["AIC"], "\n")
        # cat(", next AIC", next_min_AIC["AIC"], "\n")
        
        if (next_min_AIC["AIC"] < current_min_AIC["AIC"]) {
            current_min_AIC = next_min_AIC
        } else {
            found_min = TRUE
        }
    }
    
    if (found_min == TRUE) {
        cat("Found min AIC at state number: ", current_min_AIC["num_states"], "\n")
    } else {
        cat("We iterate from 2 all the way to ", max_num_states, "anyway, just return the last one as min!", "\n")
    }
    return (current_min_AIC)
}

# # check the functions 
# result_one_seed = Get_minAIC_LRHMM_for_one_seed(GenData_NoSeason_2cp, condition_to_use=1, row_to_use=4, num_states=2, seed_value=1234)
# result_one_seed
# #
# result_one_state = Get_minAIC_LRHMM_for_one_state(GenData_NoSeason_2cp, condition_to_use=1, row_to_use=4, num_states=2)
# result_one_state
# # 
# res_across_states = Get_minAIC_LRHMM_for_multi_states(GenData_NoSeason_2cp, condition_to_use=1, row_to_use=4)
# res_across_states
