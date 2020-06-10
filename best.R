

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate

    data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
    
    ## Checking the validity of the state
    r <- F
    for (i in data$State) {
          
          if ( is.element(state, data$State)) {
            r <- T
            break
          }
    }
    
    if (r == F) stop("invalid state")
    
    
    ## finding the desired hospital
    if (outcome == "heart attack") {
      
          
          ## separate the desired state
          HospinState <- subset(data, 
                       State == state, 
                      select = c(Hospital.Name, State, 
                       Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
         
          ## preparing for evaluating the minimum 
          HA <- HospinState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
          
          ## evaluating the minimum
          m <- min(HA, na.rm = T)  
          
          ## finding the name(s) of the hospital with minimum death
          HospName <- subset(HospinState, 
                      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == as.character(m), 
                      select = c(Hospital.Name, State))
          
      
    }
    
    else if (outcome == "heart failure") {
      
      ## separate the desired state
      HospinState <- subset(data, 
                            State == state, 
                            select = c(Hospital.Name, State, 
                                       Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      
      ## preparing for evaluating the minimum 
      HA <- HospinState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
      
      ## evaluating the minimum
      m <- min(HA, na.rm = T)  
      
      ## finding the name(s) of the hospital with minimum death
      HospName <- subset(HospinState, 
                         Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == as.character(m), 
                         select = c(Hospital.Name, State))
      
        
    }
    
    else if (outcome == "pneumonia") {
      
      ## separate the desired state
      HospinState <- subset(data, 
                            State == state, 
                            select = c(Hospital.Name, State, 
                                       Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      
      ## preparing for evaluating the minimum 
      HA <- HospinState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
      
      ## evaluating the minimum
      m <- min(HA, na.rm = T)  
      
      ## finding the name(s) of the hospital with minimum death
      HospName <- subset(HospinState, 
                         Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == as.character(m), 
                         select = c(Hospital.Name, State))
      
      
    }
    
    else {
      
          stop("invalid outcome")
      
    }
    
    HospName <- as.character(HospName$Hospital.Name)
    HospName <- sort(HospName)
    HospName [1]
    
}