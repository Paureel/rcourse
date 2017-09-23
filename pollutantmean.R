pollutantmean <- function(directory, pollutant, id) {
  mean_temp <- 0
  directory <- paste(getwd(), directory,sep= "/")
  id_file <- sprintf("%03d", id)
  j <- 1
  for(i in id){
    dir <- paste(id_file[j], "csv", sep=".")
    dir <- paste(directory, dir, sep= "/")
    all <- read.csv2(dir, header = TRUE, sep= ",")
    filtered <- all[[pollutant]]
    bad <- is.na(filtered)
    good <- filtered[!bad]
    numeric_good <- as.numeric(levels(good))[good]
    mean_temp <- c(mean_temp, (numeric_good))
    j <- j + 1
    
  }
  
  all <- mean_temp[-1]
  
  
  return(mean(all))
  
}

complete <- function(directory,id){
  id_num <- 0
  length <- 0
  directory <- paste(getwd(), directory,sep= "/")
  id_file <- sprintf("%03d", id)
  j <- 1
  for(i in id){
    dir <- paste(id_file[j], "csv", sep=".")
    dir <- paste(directory, dir, sep= "/")
    all <- read.csv2(dir, header = TRUE, sep= ",")
    filtered <- complete.cases(all)
    good <- all[filtered,]
    id_num <- c(id_num,i)
    length <- c(length, nrow(good))
    
    j <- j + 1
    
  }
  
  id_num <- id_num[-1]
  length <- length[-1]
  
  output <- data.frame(id_num, length)
  
  return(output)
  
  
}