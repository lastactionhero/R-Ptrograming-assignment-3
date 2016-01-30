## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

rankall <- function(outcome, num = "best") {

        
        library(data.table)      
        outcomeData <- suppressWarnings( data.table( read.csv("outcome-of-care-measures.csv",colClasses = "character")))
        
        
        if( !(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
                stop("invalid outcome")
        }
        
        
        colnm <- switch(outcome,
                        'heart attack' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
                        'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
                        'pneumonia' = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
        )
        
        filtered <- outcomeData[!is.na(get(colnm))]
        filtered <- suppressWarnings(filtered[,dest:=as.numeric(get(colnm))])
        filtered <- filtered[!is.na(dest)]
        
        States <- unique(filtered,by='State')[,State]
        result <- data.table(Hospital.Name = as.character(), State = as.character())
        rnk <- num
        
        for (s in States)
                
        {
                
                statewise <- filtered[State==s]
                if(num=='best')
                {
                        rnk <- 1
                }
                else if (num == 'worst')
                {
                        rnk <- nrow(statewise)
                }
                statewise <- statewise[order(dest, Hospital.Name)][rnk,]
                result <- rbind(result, statewise[,c('Hospital.Name','State'),with=FALSE])
                result[,State:= ifelse(is.na(State),s,State)]
                
        }
        result <- result[order(State)]
        result
        
        
        
}