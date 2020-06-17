getdataframe <- function(directory = 'rawdata', file_selector){
    # function returns a dataframe of the read csv file
    full_filepaths <- list.files(directory, full.names = TRUE)
    dat <- read.csv(full_filepaths[file_selector])
    return(dat)
}
    
# outcomes_dat <- getdataframe(file_selector = 2)
# print(str(outcomes_dat))
# hospital_list_bystate <- split(outcomes_dat, outcomes_dat$State)
# hospital_list_bystate <- lapply(hospital_list_bystate, function(frame){
#     frame[ , "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- apply(frame[ ,
#                                                                                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
#                                                                                           drop = F], 2, function(entry) as.numeric(entry))
#     frame <- frame[order(frame[ , "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"], frame$Hospital.Name), ]
# })
# ranked_hospital_frame <- data.frame()
# for (frame in hospital_list_bystate){
#     ranked_hospital_frame <- rbind(ranked_hospital_frame, frame[2, ])
# }
# print(str(ranked_hospital_frame))

rankall <- function(outcome, num = "best") {
    
    outcomes_dat <- getdataframe(file_selector = 2)
    
    if (outcome == "heart attack"){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure"){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia"){
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("Invalid outcome")
    }

    hospital_list_bystate <- split(outcomes_dat, outcomes_dat$State)
    hospital_list_bystate <- lapply(hospital_list_bystate, function(frame){
        frame[ , outcome] <- apply(frame[ ,outcome, drop = F], 2, function(entry) as.numeric(entry))
        frame <- frame[order(frame[ , outcome], frame$Hospital.Name), ]
    })
    
    ranked_hospital_frame <- data.frame()
    for (frame in hospital_list_bystate){
        
        rank = NULL
        if (num == "best"){
            rank <- 1
        } else if (num == "worst"){
            rank <- nrow(frame)
        } else if (class(num) == "numeric"){
            rank <- as.integer(num)
        } else {
            stop("Invalid num")
        }
        
        if (rank %in% 1:nrow(frame)){
            ranked_hospital_frame <- rbind(ranked_hospital_frame, frame[rank, ])
        } else {
            NA_frame <- frame[1, , drop = FALSE]
            for (i in 11:ncol(frame)){
                NA_frame[1, i] <- NA
            }
            NA_frame[1, 2] <- NA
            ranked_hospital_frame<- rbind(ranked_hospital_frame, NA_frame)
        }
    }
    
    ranked_hospital_frame <- ranked_hospital_frame[ , c("Hospital.Name", "State", outcome)]
    ranked_hospital_frame <- ranked_hospital_frame[order(ranked_hospital_frame[ , outcome], decreasing = TRUE), ]
    return(ranked_hospital_frame)
}

print(rankall("heart attack", num = 2))
print(r <- rankall("heart attack", 4))
