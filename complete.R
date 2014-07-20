complete <- function(directory, id = 1:332) {
  data <- data.frame()
  id <- as.integer(id)
  nobs <- as.integer()
  for (i in id)
  {
    data <- rbind(data,read.csv(paste(directory, "/", sprintf("%.3d",i),".csv",sep="")));
  }
  x <- data[complete.cases(data),];
  #ID <- x$ID[!duplicated(x$ID)]
  for (i in id)
  {
      temp <- sum(x$ID == i)
      nobs <- c(nobs , temp)
  }

  id <- as.integer(id)
  data.frame(id,nobs)

}
