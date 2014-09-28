rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  outcomeCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  checkState <- unique(outcomeCSV[,7])
  if (! (state %in% checkState)) stop("invalid state")
  ## check num
  if (! (num=="best" || num=="worst" || is.numeric(num))) stop("invalid num")
  
  ## Colonne 7 c'est le state
  outcomeCSV <- subset(outcomeCSV, outcomeCSV[,7]==state)
  
  ## 2 = name, 7 = state, 11 = H Attack, 17 = H Failure, 23 = Pneumonia
  x <- if (outcome=="heart attack") {outcomeCSV[,c(2,11)]}
  else if (outcome=="heart failure")  {outcomeCSV[,c(2,17)]}
  else if (outcome=="pneumonia")  {outcomeCSV[,c(2,23)]}
  else stop("invalid outcome")
  
  ## on dégage les NA
  suppressWarnings( x[,2]<-as.numeric(x[,2]) )
  x<-subset(x,!is.na(x[,2]))
  
  ## le premier ,1 retourne la première colonne, on ordonne par valeur puis par hospital name
  x<-x[order(x[,2],x[,1]),1]
  
  x <- if (num=="best")  {head(x,1)}
  else if (num=="worst") {tail(x,1)}
  else x[num]
    
  return(x)
  
}