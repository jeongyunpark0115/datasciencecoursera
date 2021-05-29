# C:/Users/Danbee/Desktop/datasciencecoursera/Programming Assignment 3

rankall <- function(outcome, num = "best") {
        
        # read the data
        df <- read.csv("C:/Users/Danbee/Desktop/datasciencecoursera/Programming Assignment 3/outcome-of-care-measures.csv")
        
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        
        # check if the outcome is valid
        if (outcome %in% outcome_list == FALSE) {
                
                stop()
                geterrmessage("invalid outcome")
                
        } else {
                
                # 1. change the col name of the outcome
                # 2. change the values of the outcome column in each data.frame of states_data
                # 3. remove NAs
                if (outcome == outcome_list[1]) {
                        
                        names(df)[names(df) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- outcome_list[1]
                        
                        as_num <- as.numeric(df[, 11])
                        df[, 11] <- as_num
                        
                        rm_na <- complete.cases(df[, 11])
                        na_removed <- df[rm_na, ]
                        
                } else if (outcome == outcome_list[2]) {
                        
                        names(df)[names(df) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- outcome_list[2]
                        
                        as_num <- as.numeric(df[, 17])
                        df[, 17] <- as_num
                        
                        rm_na <- complete.cases(df[, 17])
                        na_removed <- df[rm_na, ]
                        
                } else {
                        
                        names(df)[names(df) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- outcome_list[3]
                        
                        as_num <- as.numeric(df[, 23])
                        df[, 23] <- as_num
                        
                        rm_na <- complete.cases(df[, 23])
                        na_removed <- df[rm_na, ]
                        
                }
        }
                # split df by each states
                states_data <- split(na_removed, na_removed$State)
                
                # 1. rank them
                # for each element in states_data list,
                # apply the order function by outcome & by hospital name
                
                # 2. from ranked, get wanted data and make another data.frame, result
                
                # below works for a state
                # rankofAL <- states_data[[2]][order(states_data[[2]][outcome]), ]
                
                # create an empty data.frame
                result <- data.frame(hospital = character(), state = character(), stringsAsFactors =  FALSE)
                
                # if num is best
                if (num == "best") {
                  
                  # best
                  if (outcome == "heart attack"){
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"heart attack", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      result <- rbind(result, hospital = i[1])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  } else if (outcome == "heart failure"){
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"heart failure", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      result <- rbind(result, hospital = i[1])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  } else {
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"pneumonia", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      result <- rbind(result, hospital = i[1])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  }
                # else if num is worst  
                } else if (num == "worst") {
                  
                  # worst
                  if (outcome == "heart attack"){
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"heart attack", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      num <- length(hospitals[i])
                      result <- rbind(result, hospital = i[num])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  } else if (outcome == "heart failure"){
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"heart failure", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      num <- length(hospitals[i])
                      result <- rbind(result, hospital = i[num])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  } else {
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"pneumonia", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      num <- length(hospitals[i])
                      result <- rbind(result, hospital = i[num])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  }
                } else {
                  
                  # else num is numeric
                  if (outcome == "heart attack"){
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"heart attack", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      result <- rbind(result, hospital = i[num])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  } else if (outcome == "heart failure"){
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"heart failure", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      result <- rbind(result, hospital = i[num])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  } else {
                    
                    ranked <- lapply(states_data, function(x) x[order(x$"pneumonia", x$"Hospital.Name"), ])
                    
                    hospitals <- lapply(ranked, function(x) x$Hospital.Name)
                    
                    state <- sapply(ranked, function(x) x$State)
                    state1 <- unique(unlist(state))
                    count <- 0
                    
                    # in each state data frame in the hospital list, print the hospital name of the selected rank
                    # for(i in hospitals){result <- rbind(result, hospital = i[num])}
                    for(i in hospitals){
                      
                      result <- rbind(result, hospital = i[num])
                      count <- count + 1
                      
                    }
                    
                    # cbind hospital and state
                    result <- cbind(result, state1)
                    
                    # name each cols of the result df
                    names(result) <- c("hospital", "state")
                    
                    result
                    
                  }
                  
                }
                
        }


                
