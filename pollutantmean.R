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
  
  id <- id_num[-1]
  nobs <- length[-1]
  
  output <- data.frame(id, nobs)
  
  return(output)
  
  
}

corr <- function(directory, threshold){
  correl <- 0
  
  id <- 1:332
  directory <- paste(getwd(), directory,sep= "/")
  id_file <- sprintf("%03d", id)
  
  j <- 1
  k <- 0
  for(i in id){
    dir <- paste(id_file[j], "csv", sep=".")
    dir <- paste(directory, dir, sep= "/")
    all <- read.csv2(dir, header = TRUE, sep= ",")
    filtered <- complete.cases(all)
    good <- all[filtered,]
    
    sulfate <- good$sulfate
    nitrate <- good$nitrate
    
    length <- nrow(good)
    
    if (length > threshold){
      
      numeric_good_sulfate <- as.numeric(levels(sulfate))[sulfate]
      numeric_good_nitrate <- as.numeric(levels(nitrate))[nitrate]
      correl_temp <- cor(numeric_good_sulfate, numeric_good_nitrate)
      correl <- c(correl, correl_temp)
      k <- k + 1
    }
    j <- j + 1
    
  }
  if (k == 0){
    return(0)
  }
 
  return(correl[-1])
    
  }
  

