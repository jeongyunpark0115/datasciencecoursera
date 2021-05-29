### 3. Ranking hospitals by outcome in a state
        
rankhospital <- function(state, outcome, num = "best") {
        
        ### Read outcome data
        df <- read.csv("outcome-of-care-measures.csv")
        
        ### Check that state and outcome are valid
        
        # make a list of states
        statelist <- unique(df$State)
        
        # make a list of outcome
        outcomelist <- c("heart attack", "heart failure", "pneumonia")
        
        # check if the state is valid
        if (state %in% statelist == FALSE) {
                
                return(NA)
                stop()
                geterrmessage("invalid state")
                
        # check if the outcome is valid
        } else if (outcome %in% outcomelist == FALSE) {
                
                return(NA)
                stop()
                geterrmessage("invalid outcome")
                
        } else {
                
                ### Return hospital name in that state with the given rank
                ### 30-day death rate
                
                # split the whole df by states to make a list containing multiple data.frames & assign it to a variable
                states_data <- split(df, df$State)
                
                # get the data.frame of the state & assign it to a variable
                matching_state <- states_data[[state]]
                
                # change col names for the three possible outcomes
                names(matching_state)[names(matching_state) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- outcomelist[1]
                names(matching_state)[names(matching_state) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- outcomelist[2]
                names(matching_state)[names(matching_state) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- outcomelist[3]
                
                # change class from character to numeric in the outcome column
                asnum_ha <- as.numeric(matching_state[["heart attack"]])
                asnum_hf <- as.numeric(matching_state[["heart failure"]])
                asnum_pn <- as.numeric(matching_state[["pneumonia"]])
                
                # give the data to each column in the matching_state data.frame
                matching_state[["heart attack"]] <- asnum_ha
                matching_state[["heart failure"]] <- asnum_hf
                matching_state[["pneumonia"]] <- asnum_pn
                
                
                # rank by outcome & hospital name
                ranked <- matching_state[with(matching_state, order(matching_state[[outcome]], matching_state$Hospital.Name, na.last = TRUE)), ]
                
                # remove NA
                na_removed <- complete.cases(ranked[, names(ranked[outcome])])
                result <- ranked[na_removed, ]
                
                # if num > the number of hospitals in the list
                if (num > nrow(result) & num != "best" & num != "worst") {
                        
                        return(NA)
                
                # if num == "best"
                } else if (num == "best") {
                        
                        result[1, 2]
                
                # if num == "worst"        
                } else if (num == "worst") {
                        
                        tail(result[, 2], n = 1)
                        
                # if num is within the number of rows in the list
                } else if (num <= nrow(result)) {
                        
                        result[num, 2]
                        
                # if the num value is weird
                } else {
                        
                        return(NA)
                        stop()
                        geterrmessage("invalid number")
                        
                }

        }
        
}

