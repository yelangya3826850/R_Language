corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## extract ids meet the requirement
  
  
  data <- complete(directory, 1:332)
  corr <- as.numeric(c())
  id <- data$id[data$nobs > threshold]
  
  ## load data
  id_1 <- id[id > 0 & id < 10]
  id_2 <- id[id >= 10 & id < 100]
  id_3 <- id[id >= 100]
  file_part1 <- paste(rep("00",times = length(id_1)), id_1, rep(".csv",times = length(id_1)), sep = "")
  file_part2 <- paste(rep("0",times = length(id_2)), id_2, rep(".csv",times = length(id_2)), sep = "" )
  file_part3 <- paste(id_3, rep(".csv",times = length(id_3)), sep = "")
  file <- c(file_part1, file_part2, file_part3)
  
  file <- paste(rep(directory,times = length(file)), rep("/",times = length(file)), file, sep = "")
  

  ## get correlation
  for (filename in file)
  {
    data <- read.csv(filename)
    sulfate_temp <- data$sulfate[!is.na(data$sulfate) & !is.na(data$nitrate)]
    nitrate_temp <- data$nitrate[!is.na(data$sulfate) & !is.na(data$nitrate)]
    corr <- c(corr, cor(sulfate_temp, nitrate_temp))
  }
  

  ## return result
  corr
}
