best <- function(state, outcome){
  outcome.table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  x <- is.element(state, outcome.table$State)
 
  if (x == FALSE){
    stop("invalid state")
    return(FALSE)
  }
  outcome.filtered <- outcome.table[outcome.table$State == state, ]
  
  if(outcome == "heart attack"){
    outcome_num = 11 
    
  }
  else if(outcome == "heart failure"){
    
    outcome_num = 17 
    
  }
  else if(outcome == "pneumonia"){
    
    outcome_num = 23 
    
  }
  else {
    
    stop("invalid outcome")
    return(FALSE)
  }
  
  max <- which.min(outcome.filtered[,outcome_num])
  return(outcome.filtered[max, 2])
}


rankhospital <- function(state, outcome, rank){
  
  
  outcome.table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  x <- is.element(state, outcome.table$State)
  
  if (x == FALSE){
    stop("invalid state")
    return(FALSE)
  }
  outcome.filtered <- outcome.table[outcome.table$State == state, ]
  
  if(outcome == "heart attack"){
    outcome_num = 11 
    
  }
  else if(outcome == "heart failure"){
    
    outcome_num = 17 
    
  }
  else if(outcome == "pneumonia"){
    
    outcome_num = 23 
    
  }
  else {
    
    stop("invalid outcome")
    return(FALSE)
  }
  
  #z <- outcome.filtered[,outcome_num]
  #zz <- z[z != "Not Available"]
  #y <- order(zz)
  # print(zz[y])
  
  ordered <- outcome.filtered[order(as.numeric(outcome.filtered[,outcome_num]), outcome.filtered$Hospital.Name),]
  order_list <- ordered[,outcome_num]
  
  #View(ordered)
  length_ordered <- order_list[order_list != "Not Available"]
 
  if (rank == "best"){
    rank = 1
    
  }
  if (rank == "worst"){
    rank = length(length_ordered)
    
  }
  return(ordered[rank,2])
}

rankall <- function(outcome, num) {
  
  outcome.table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  
  M <- matrix(ncol = 2)
  colnames(M) <- c("hospital", "states")
  
  for (x in unique(outcome.table$State)) {
  state <- x
  
  outcome.filtered <- outcome.table[outcome.table$State == state, ]
  
  if(outcome == "heart attack"){
    outcome_num = 11 
    
  }
  else if(outcome == "heart failure"){
    
    outcome_num = 17 
    
  }
  else if(outcome == "pneumonia"){
    
    outcome_num = 23 
    
  }
  else {
    
    stop("invalid outcome")
    return(FALSE)
  }
  
  
  
  ordered <- outcome.filtered[order(as.numeric(outcome.filtered[,outcome_num]), outcome.filtered$Hospital.Name),]
  order_list <- ordered[,outcome_num]
  
  
  length_ordered <- order_list[order_list != "Not Available"]
  if (num == "best"){
    num = 1
    
  }
  if (num == "worst"){
    num = length(length_ordered)
    
  }
  
  out <- (ordered[num,2])
  
  M <- rbind(M, c(out, x))
  }
  
  
  return(M)
}



