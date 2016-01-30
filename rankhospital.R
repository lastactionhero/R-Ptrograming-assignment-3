rankhospital <- function(state, outcome, num = "best")
{
  library(data.table)      
  outcomeData <- suppressWarnings( data.table( read.csv("outcome-of-care-measures.csv",colClasses = "character")))
  
  if( !(state %in% outcomeData$State)){
    stop("invalid state")
  }
  if( !(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
    stop("invalid outcome")
  }
  
  filtered <- outcomeData[State==state]
  colnm <- switch(outcome,
                  'heart attack' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
                  'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
                  'pneumonia' = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  )
  
  filtered <- filtered[!is.na(get(colnm))]
  filtered <- suppressWarnings(filtered[,dest:=as.numeric(get(colnm))])
  filtered <- filtered[!is.na(dest)]
  
  if(num=='best')
  {
    num <- 1
  }
  else if (num == 'worst')
  {
    num <- nrow(filtered)
  }
  num <- as.numeric(num)
  
  num <- as.numeric(num)
  
  filtered <- filtered[order(dest, Hospital.Name)][num,]
  filtered$Hospital.Name
  
  
  
}
