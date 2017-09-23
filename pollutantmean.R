pollutantmean <- function(directory, pollutant, id) {
  mean_temp <- 0
  setwd(directory)
  id_file <- sprintf("%03d", id)
  j <- 1
  for(i in id){
    dir <- paste(id_file[j], "csv", sep=".")
    print(dir)
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