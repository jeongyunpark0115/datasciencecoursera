### 1. Plot the 30-day mortality rates for heart attack

### Read the outcome data into R via the read.csv function and look at the first few rows.
# getwd()
# setwd("C:/Users/Danbee/Desktop/datasciencecoursera/Programming Assignment 3")
# 
# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# look at the data and see how it's shaped
# head(outcome)
# ncol(outcome)
# names(outcome)
# str(outcome)

# make a histogram of the 30-day death rates from heart attack (col 11 in the outcome dataset)
# outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
# hist(outcome[, 11])


### 2. Finding the best hospital in a state

# read the outcome-of-care-measures.csv file

# return a character vector with the name of the hospital
# that has the best (lowest) 30-day mortality for the specified outcome in that state
# The hospital name is the name provided in the Hospital.Name variable

# The outcomes can be one of 'heart attack', 'heart failure', or 'pneumonia'.

# Hospitals that do not have data on a particular outcome should be excluded when deciding the rankings.

# If there is a tie for the best hospital for a given outcome,
# then the hospital names should be sorted in alphabetical order 

# a <- df[with(df, sort(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), ]
# b <- df[with(df, order(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.last = TRUE, decreasing = FALSE)), ]
# identical(a, b) # FALSE
# I'm not sure why the data.frame a is filled with NAs.
# b shows the result that I want


best <- function(state, outcome) {
  # read the outcome-of-care-measures.csv file
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # make a list of states in the df
  statelist <- unique(df$State)
  
  # split the file by state
  splitedbystate <- split(df, df$State)
  
  # check that state parameter is valid
  # state <- "AL"
  # state <- "KR"
  # state %in% statelist
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
  

### 4. Ranking hospitals in all states

