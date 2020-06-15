getdataframe <- function(directory = 'rawdata', file_selector){
    # function returns a dataframe of the read csv file
    full_filepaths <- list.files(directory, full.names = TRUE)
    dat <- read.csv(full_filepaths[file_selector])
    return(dat)
}
# outcomes_dat <- getdataframe(file_selector = 2)
# outcomes_subset_colnames = c("Hospital.Name", "State",
#                              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
#                              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
#                              "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
# outcomes_dat <- outcomes_dat[ , outcomes_subset_colnames]
# outcomes_dat[ , 3:5] <- apply(outcomes_dat[ , 3:5], 2, function(entry){
#     as.numeric(entry)
# })
# print(str(outcomes_dat))

colnames_check <- function(outcome_arg){
    # function checks the validity of the outcome argument in the best function
    # and assigns it the corresponding column index in the dataframe
    arg_list <- c("heart attack", "heart failure", "pneumonia")
    if (outcome_arg == arg_list[1]){
        outcome_arg = 3L
    } else if (outcome_arg == arg_list[2]){
        outcome_arg = 4L
    } else if (outcome_arg == arg_list[3]){
        outcome_arg = 5L
    } else {
        stop("Invalid outcome")
    }
    return(outcome_arg)
}

state_check <- function(state_arg, outcomes_dat){
    # function checks if the state argument in the best function is within the
    # data frame's list of states
    if (state_arg %in% outcomes_dat[ , "State"]){
        return(TRUE)
    } else {
        stop("Invalid state")
    }
}

best <- function(state, outcome) {
    
    outcomes_dat <- getdataframe(file_selector = 2)
    outcomes_subset_colnames = c("Hospital.Name", "State",
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    outcomes_dat <- outcomes_dat[ , outcomes_subset_colnames]
    # convert the character numbers in the mortality rates to numeric class for
    # numerical comparison
    outcomes_dat[ , 3:5] <- apply(outcomes_dat[ , 3:5], 2, function(entry){
        as.numeric(entry)
    })
    
    # check the validity of the arguments
    outcome <- colnames_check(outcome)
    print(outcome)
    state_check(state, outcomes_dat)
    
    # commented out the debugging code
    # print(str(outcomes_dat))
    # below line finds the minimum mortality rate within the state's hospitals
    min_mortrate <- min(outcomes_dat[outcomes_dat$State == state, outcome], na.rm = T)
    # print(min_mortrate)
    
    # subsets the hospitals matching the mortrate first, then the state.
    best_hospital <- outcomes_dat[which(outcomes_dat[ , outcome] == min_mortrate) , ]
    best_hospital <- best_hospital[which(best_hospital$State == state), ]
    # retrieve the name and relevant mortrate requested
    hospital_name <- best_hospital[ , "Hospital.Name"]
    hosp_mortrate <- best_hospital[ , outcome]
    return(c(hospital_name, as.character(hosp_mortrate)))
}

#testing=======================================================================
print(best("TX", "heart attack"))