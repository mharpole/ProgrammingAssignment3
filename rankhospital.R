# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the ranking specified by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  myData <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                     na.strings = "Not Available")
  ## Check that state and outcome are valid
  if(!(state %in% myData$State)){
    stop("invalid state")
  }
  if(!(outcome %in% c("heart attack", "heart failure","pneumonia"))){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  mycolumns <- c("State",outcome,"Hospital.Name")
  myData2 <- myData %>% 
    rename(
      "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
      "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
      "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>% 
    select(mycolumns) %>% 
    filter(State==state, complete.cases(.)) %>% 
    arrange_at(c(outcome,"Hospital.Name"))
  if(num == "best"){
  return(myData2[1,3])
  }else if(num == "worst"){
    return(myData2[dim(myData2)[1],3])
  }else if(num>dim(myData2)[1]){
    return(NA)
  }else{
    return(myData2[num,3])
  }
}