pollutantmean <- function(directory, pollutant, id) {
  mean_temp <- 0
  id_file <- sprintf("%03d", id)
  j <- 1
  for(i in id){
    setwd(directory)
    dir <- paste(id_file[j], "csv", sep=".")
    all <- read.csv2(dir, header = TRUE, sep= ",")
    filtered <- all[[pollutant]]
    bad <- is.na(filtered)
    good <- filtered[!bad]
    numeric_good <- as.numeric(levels(good))[good]
    mean_temp <- c(mean_temp, (mean(numeric_good)))
    j <- j + 1
    
  }
  all <- mean_temp[-1]
  bad <- is.na(all)
  good <- all[!bad]
  
  return(mean(good))
  
}