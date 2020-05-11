pollutantmean <- function(directory, pollutant, id = 1:332) {
  fileList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values <- numeric()
  
  for (i in id) {
    data <- read.csv(fileList[i])
    values <- c(values, data[[pollutant]])
  }
  mean(values, na.rm = TRUE)
}

#pollutantmean("specdata", "sulfate", 1:10)
#pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)

complete <- function(directory, id = 1:332) {
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  print(paste("##", "", "id", "nobs", sep = " "))
  counter <- 0
  for (i in id) {
    counter <- counter + 1
    data = read.csv(filelist[i])
    print(paste("##", counter, i, sum(complete.cases(data)), sep=" "))
  }
}

#complete("specdata", 1:332)

corr <- function(directory, threshold = 0) {
  filesList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  cors <- numeric()
  for (i in 1:332) {
    data <- read.csv(filesList[i])
    if (sum(complete.cases(data)) > threshold) {
      cors <- c(cors, cor(data[["sulfate"]], data[["nitrate"]], use = "complete.obs"))
    }
  }
  cors
}

#corr("specdata",100)

