
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",
                   stringsAsFactors=FALSE)
  
  ## reading the outcome
  if (outcome == "heart attack") oc <- 3
  else if (outcome == "heart failure") oc <- 4
  else if (outcome == "pneumonia") oc <- 5
  else stop("invalid outcome")
  
  
  ## Checking the validity of the state
  r <- F
  for (i in data$State) {
    if ( is.element(state, data$State)) {
      r <- T
      break
    }
  }
  if (r == F) stop("invalid state")
  
  ## separate the desired state
  HospinState <- subset(data, State == state, select = c(Hospital.Name, State,
                 Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                 Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                 Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  
  ## removing NAs from the outcome
  HospinState <- HospinState[complete.cases(HospinState[,oc]),]
  
  ## order the data by the desired outcome
  HospinState <- HospinState[order(HospinState[,oc], HospinState$Hospital.Name),]
  
  ## return the hospital name with correct ranking
  if (num == "best") return(HospinState[,1][1])
  else if (num == "worst") return(HospinState[,1][length(HospinState[,1])])
  else return(HospinState[,1][num])
  
  
  
  
  
}
