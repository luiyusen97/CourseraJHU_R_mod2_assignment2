getdataframe <- function(directory = 'rawdata', file_selector){
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
    arg_list <- c("heart attack", "heart failure", "pneumonia")
    # colnames_list <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    #                   "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    #                   "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
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

# state_check <- function(state_arg, outcomes_dat){
#     if !(state_arg %in% outcomes_dat[ , "State"]){
#         stop("Invalid state")
#     }
#     
# }

state_check <- function(state_arg, outcomes_dat){
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
    outcomes_dat[ , 3:5] <- apply(outcomes_dat[ , 3:5], 2, function(entry){
        as.numeric(entry)
    })
    
    outcome <- colnames_check(outcome)
    print(outcome)
    state_check(state, outcomes_dat)
    
    print(str(outcomes_dat))
    min_mortrate <- min(outcomes_dat[outcomes_dat$State == state, outcome], na.rm = T)
    print(min_mortrate)
    
    best_hospital <- outcomes_dat[which(outcomes_dat[ , outcome] == min_mortrate) , ]
    best_hospital <- best_hospital[which(best_hospital$State == state), ]
    hospital_name <- best_hospital[ , "Hospital.Name"]
    hosp_mortrate <- best_hospital[ , outcome]
    return(c(hospital_name, as.character(hosp_mortrate)))
}

#testing=======================================================================
print(best("TX", "heart attack"))