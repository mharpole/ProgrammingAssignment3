# 2 Finding the best hospital in a state
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are tied for best, then hospital “b” should be returned).
library(tidyverse)
best <- function(state, outcome) {
  # outcome <- "heart attack"
  # state <- "TX"
  ## Read outcome data
  myData <- read.csv("/Volumes/HarpoleFamilyNAS/Coursera/R_programming_course/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                     na.strings = "Not Available")
  ## Check that state and outcome are valid
  if(!(state %in% myData$State)){
    stop("invalid state")
  }
  if(!(outcome %in% c("heart attack", "heart failure","pneumonia"))){
    stop("invalid outcome")
  }

  mycolumns <- c("State",outcome,"Hospital.Name")
  myData2 <- myData %>% 
    rename(
           "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
           "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
           "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>% 
    select(mycolumns) %>% 
    filter(State==state) %>% 
    arrange_at(c(outcome,"Hospital.Name"))
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(myData2[1,3])
  
}
