## best.R is a function that takes as arguments the state and a certain pathology. 
## The function returns the hospital that has the lowest mortality rates in a given state 
## and for a certain pathology

best <- function(state, pathology) {
  
  ## Read outcome/pathology data
   data <- read.csv("/Users/BertCarlier/Documents/R Working Directory/Coursera-Course-Work/Assignment 3 Hospital Quality/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")

  ## Check that state and pathology are valid
  validPathology = c("heart attack","heart failure","pneumonia")
  if (!pathology %in% validPathology) { stop("invalid pathology")}
  
  validState = unique(data[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert pathology name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(pathology,validPathology)]
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- data[data$State==state,]
  idx <- which.min(as.double(data.state[,colName]))
  data.state[idx,"Hospital.Name"]
}
