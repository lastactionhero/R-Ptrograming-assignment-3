## This function returns hospital name with least mortality rate in given outcome category for given state
## Read outcome data
## Check that state and outcome are valid

best <- function (state, outcome)
{
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  
  outcomeData <- suppressWarnings( data.table( read.csv("outcome-of-care-measures.csv",colClasses = "character")))

  
  if( !(state %in% outcomeData$State)){
    stop("invalid state")
  }
  if( !(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
    stop("invalid outcome")
  }
  filtered <- outcomeData[State==state]
  if(outcome=='heart attack')
  {
    
    
    filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <-suppressWarnings( as.numeric(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    filtered <- filtered[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)][1,]
    filtered$Hospital.Name
  }
  else if(outcome=='heart failure')
  {
    
    
    filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    filtered <- filtered[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)][1,]
    filtered$Hospital.Name
  }
  else if(outcome=='pneumonia')
  {
    
    
    filtered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    filtered <- filtered[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)][1,]
    filtered$Hospital.Name
  }
  
  
    
}