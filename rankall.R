rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcomeCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (! (num=="best" || num=="worst" || is.numeric(num))) stop("invalid num")

  ## liste de référence des states
  listState <- as.data.frame(unique(outcomeCSV[,7]))
  colnames(listState) <- "State"
  
  ## 2 = name, 7 = state, 11 = H Attack, 17 = H Failure, 23 = Pneumonia
  x <- if (outcome=="heart attack") {outcomeCSV[,c(2,7,11)]}
  else if (outcome=="heart failure")  {outcomeCSV[,c(2,7,17)]}
  else if (outcome=="pneumonia")  {outcomeCSV[,c(2,7,23)]}
  else stop("invalid outcome")
  
  ## on dégage les NA
  suppressWarnings( x[,3]<-as.numeric(x[,3]) )
  x<-subset(x,!is.na(x[,3]))
  
  ## on ordonne par state
  xo <- x[order(x[,2]),]
  ## on ajoute une colonne de rang, dans chaque state
  xo$order.by.group <- unlist(with(xo,tapply(xo[,3],xo[,2], function(y) rank(y, ties.method="first"))))
  
  ##On joint la liste des states aux ranks pour obtenir le résultat
  w <- merge(x=listState,y = subset(w,w$order.by.group==num), by = "State", all.x=TRUE)
  return(w[,c(1,2)])
}