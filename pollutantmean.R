pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## store current path
  ## get in to the path where we store data

  current_path <- getwd()
  ## load data
  id_1 <- id[id > 0 & id < 10]
  id_2 <- id[id >= 10 & id < 100]
  id_3 <- id[id >= 100]
  file_part1 <- paste(current_path, "/", directory, "/", rep("00",times = length(id_1)), id_1, rep(".csv",times = length(id_1)), sep = "")
  file_part2 <- paste(current_path, "/", directory, "/", rep("0",times = length(id_2)), id_2, rep(".csv",times = length(id_2)), sep = "" )
  file_part3 <- paste(current_path, "/", directory, "/", id_3, rep(".csv",times = length(id_3)), sep = "")
  file <- c(file_part1, file_part2, file_part3)
  
  ## extract data we need
  info_vector <- c()
  for (filename in file)
  {
    filename
      data <- read.csv(filename)
      if (pollutant == "sulfate")
      {
          temp <- data$sulfate[!is.na(data$sulfate)]
      }else if(pollutant == "nitrate")
      {
          temp <- data$nitrate[!is.na(data$nitrate)]
      }
      info_vector <- c(info_vector,temp)
  }

  ## return result
  mean(info_vector)
  
}
