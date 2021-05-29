

best <- function(state, outcome) {
        # read the outcome-of-care-measures.csv file
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # make a list of states in the df
        statelist <- unique(df$State)
        
        # split the file by state
        splitedbystate <- split(df, df$State)
        
        # check that state parameter is valid
        if (state %in% statelist == FALSE) {
                
                # if state input doesn't correspond to one of the element in statelist, print warning
                print("No such state. Check for typo.")
                
                # else: continue and get the rank by state and outcome
        } else {
                
                # find the state data in splitedbystate data.frame
                matching_state <- splitedbystate[[state]]
                
                # and get the outcome
                if (outcome == "heart attack") {
                        
                        # change the col content from character to numeric
                        asnum <- matching_state[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]
                        asnum <- as.numeric(asnum)
                        
                        # give asnum data to the column in matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                        matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- asnum
                        
                        # remove NAs
                        na_removed <- complete.cases(matching_state[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
                        matching_state <- matching_state[na_removed, ]
                        
                        # rank the data by the outcome and the hospital name
                        rank <- matching_state[with(matching_state, order(matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, matching_state$Hospital.Name)), ]

                        # result
                        rank[1, 2]
                        
                }
                else if (outcome == "heart failure"){
                        
                        # change the col content from character to numeric
                        asnum <- matching_state[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
                        asnum <- as.numeric(asnum)
                        
                        # give asnum data to the column in matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                        matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- asnum
                        
                        # remove NAs
                        na_removed <- complete.cases(matching_state[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])
                        matching_state <- matching_state[na_removed, ]
                        
                        # rank the data by the outcome and the hospital name
                        rank <- matching_state[with(matching_state, order(matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, matching_state$Hospital.Name)), ]
                        
                        # result
                        rank[1, 2]
                        
                }
                else if (outcome == "pneumonia") {
                        
                        # change the col content from character to numeric
                        asnum <- matching_state[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]
                        asnum <- as.numeric(asnum)
                        
                        # give asnum data to the column in matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                        matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- asnum
                        
                        # remove NAs
                        na_removed <- complete.cases(matching_state[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])
                        matching_state <- matching_state[na_removed, ]
                        
                        # rank the data by the outcome and the hospital name
                        rank <- matching_state[with(matching_state, order(matching_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, matching_state$Hospital.Name)), ]
                        
                        # result
                        rank[1, 2]
                        
                }
                else {
                        
                        print("No such disease. Check for typo.")
                        
                }
        }
}


