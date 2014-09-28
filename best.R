best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  outcomeCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  checkState <- unique(outcomeCSV[,7])
  
  if (! (state %in% checkState)) stop("invalid state")
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
  
  ## le premier ,1 retourne la première colonne, le deuxième ,1 fait porter le head sur la première ligne  
  return(head(x[order(x[,2],x[,1]),1],1))
}