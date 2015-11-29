# Programming Assignment 3
Submission for Week 4 Assignment on Coursera R programming - Hospital Quality

#Part I - Finding the best hospital in a state

Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
outcome name. 

The function reads the outcome-of-care-measures.csv file and returns a character vector
with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
in that state. The hospital name is the name provided in the Hospital.Name variable. 

The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
and “f” are tied for best, then hospital “b” should be returned).

The function should use the following template.

>best <- function(state, outcome) {

>'## Read outcome data

>'## Check that state and outcome are valid

>'## Return hospital name in that state with lowest 30-day death

>'## rate

>}

The function should check the validity of its arguments. If an invalid state value is passed to best, the
function should throw an error via the stop function with the exact message “invalid state”. If an invalid
outcome value is passed to best, the function should throw an error via the stop function with the exact
message “invalid outcome”.

#Part II - Ranking hospitals by outcome in a state

Write a function called rankhospital that takes three arguments: 
* the 2-character abbreviated name of a state (state)
* an outcome (outcome)
* the ranking of a hospital in that state for that outcome (num).

The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
of the hospital that has the ranking specified by the num argument. For example, the call
rankhospital("MD", "heart failure", 5) would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking(smaller numbers are better). If the number given by num is larger than the number of hospitals in that state, then the function should return NA. Hospitals that do not have data on a particular outcome should
be excluded from the set of hospitals when deciding the rankings.

Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
the hospitals with lowest 30-day mortality rate for heart failure are shown here.

> head(texas)

> Hospital.Name Rate Rank

>3935 FORT DUNCAN MEDICAL CENTER 8.1 1

>4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2

>4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3

>3954 DETAR HOSPITAL NAVARRO 8.7 4

>4010 METHODIST HOSPITAL,THE 8.8 5

>3962 MISSION REGIONAL MEDICAL CENTER 8.8 6


Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate
(8.7). However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this
scheme and Detar is ranked number 4. One can use the order function to sort multiple vectors in this
manner (i.e. where one vector is used to break ties in another vector).

The function should use the following template.

>rankhospital <- function(state, outcome, num = "best") {

>'## Read outcome data

>'## Check that state and outcome are valid

>'## Return hospital name in that state with the given rank

>'## 30-day death rate

>}

The function should check the validity of its arguments. If an invalid state value is passed to best, the
function should throw an error via the stop function with the exact message “invalid state”. If an invalid
outcome value is passed to best, the function should throw an error via the stop function with the exact
message “invalid outcome”.

#Part III - Ranking hospitals in all states

Write a function called rankall that takes two arguments
* an outcome name (outcome) --> in this submission, the argument will be named pathology
* a hospital ranking (num). 

The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
containing the hospital in each state that has the ranking specified in num. 

For example the function call rankall("heart attack", "best") would return a data frame containing the names of the hospitals that are the best in their respective states for 30-day heart attack death rates. The function should return a value for every state (some may be NA). 

The first column in the data frame is named hospital, which contains he hospital name, and the second column is named state, which contains the 2-character abbreviation for the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
that the rankhospital function handles ties.

The function should use the following template.
>rankall <- function(outcome, num = "best") {

>'## Read outcome data

>'## Check that state and outcome are valid

>'## For each state, find the hospital of the given rank

>'## Return a data frame with the hospital names and the

>'## (abbreviated) state name

>'}

NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call
the rankhospital function from the previous section.
The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
the function should throw an error via the stop function with the exact message “invalid outcome”. The num
variable can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are better).
If the number given by num is larger than the number of hospitals in that state, then the function should
return NA.
