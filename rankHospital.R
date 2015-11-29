## rankHospital.R holds a function that takes as arguments a state, a pathology
## and a certain rank n. It returns as output the n-th ranked hospital in that
## state for that pathology

rankhospital <- function(state, pathology, num = "best") {
  ## Read pathology data
  data <- read.csv("/Users/BertCarlier/Documents/R Working Directory/Coursera-Course-Work/Assignment 3 Hospital Quality/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that state and pathology are valid
  validPathology = c("heart attack","heart failure","pneumonia")
  if (!pathology %in% validPathology) { stop("invalid pathology")}
  
  validState = unique(data[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert pathology name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(pathology,validPathology)]
  
  ## Return hospital name in that state with the given rank 30-day death rate
  data.state <- data[data$State==state,]
  
  # order data by pathology
  sorted.data.state <- data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  #handle num input
  if (num=="best") num = 1
  if (num=='worst') num = nrow(sorted.data.state)
  #will automatically return NA if num > nrow, as well as if it's some other text value
  # if someone passes num < 1, they'll get what's expected
  #if (is.numeric(num) & num > nrwo(sorted.data.state) return(NA)
  
  sorted.data.state[num,"Hospital.Name"]
}
