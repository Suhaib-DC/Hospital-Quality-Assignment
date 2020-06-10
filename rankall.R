
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",
                            stringsAsFactors=FALSE)
  
  ## reading the outcome
  if (outcome == "heart attack") oc <- 3
  else if (outcome == "heart failure") oc <- 4
  else if (outcome == "pneumonia") oc <- 5
  else stop("invalid outcome")
  
  rank <- character()
  state <- character()
  
  FS <- as.factor(data$State)
  
  for (i in levels(FS)) {
      
    HD <- subset(data, State == i, select = c(Hospital.Name, State,
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
          Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      
      HD <- HD[complete.cases(HD[,oc]),]
      HD <- HD[order(HD[,oc], HD[,1]),]
      
      if (num == "best") rank <- c(rank, HD[,1][1])
      else if (num == "worst") rank <- c(rank, HD[,1][length(HD[,1])])
      else rank <- c(rank, HD[,1][num])
      
      state <- c(state, i)
 } 
  
  
  RT <- data.frame(hospital = rank, state = state)
}
