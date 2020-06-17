getdataframe <- function(directory = 'rawdata', file_selector){
    # function returns a dataframe of the read csv file
    full_filepaths <- list.files(directory, full.names = TRUE)
    dat <- read.csv(full_filepaths[file_selector])
    return(dat)
}    
# outcomes_dat <- getdataframe(file_selector = 2)
# # print(str(outcomes_dat))
# outcomes_subset <- outcomes_dat[
#     which(outcomes_dat["State"] == "MD"), 
#     c("Hospital.Name", "State", 
#       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"), 
#     drop = F]
# outcomes_subset[ , 3] <- apply(outcomes_subset[ , 3, drop = F], 2, 
#                                function(entry) as.numeric(entry))
# outcomes_subset <- outcomes_subset[order(outcomes_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
#                                          outcomes_subset$Hospital.Name), ]
# print(str(outcomes_subset))
# print(outcomes_subset)

rankhospital <- function(state, outcome, num = 1L){
    
    if (outcome == "heart attack"){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure"){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia"){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("Invalid outcome")
    }
    
    outcomes_dat <- getdataframe(file_selector = 2)
    
    if (state %in% outcomes_dat[ , "State"]){
        NULL
    } else {
        stop("Invalid state")
    }
    
    outcomes_subset <- outcomes_dat[
        which(outcomes_dat["State"] == state), 
        c("Hospital.Name", "State", 
          outcome), 
        drop = F]
    outcomes_subset[ , 3] <- apply(outcomes_subset[ , 3, drop = F], 2, function(entry){
        as.numeric(entry)
    })
    outcomes_subset <- outcomes_subset[order(outcomes_subset[ , 3],
                                             outcomes_subset$Hospital.Name), ]
    outcomes_subset <- na.omit(outcomes_subset)
    
    if (num == "best"){
        num <- 1L
    } else if (num == "worst"){
        num <- nrow(outcomes_subset)
    }
    if (num %in% 1:nrow(outcomes_subset)){
        NULL
    } else {
        return(NA)
    }
    
    ranked_hospital <- outcomes_subset[num, , drop = FALSE]
    # print(outcomes_subset)
    return(ranked_hospital)
}

print(rankhospital("MD", "heart failure", 5))
print(rankhospital("TX", "heart failure", 4))
print(rankhospital("MD", "heart attack", "worst"))
print(rankhospital("MN", "heart attack", 5000))