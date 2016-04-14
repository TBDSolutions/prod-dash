combineServices <- function(directory) {
  ## 'directory' is a char vector of len 1 indicating location of CSV files
  files <- list.files(directory,full.names = TRUE) # make list of full file names
  n <- length(files)
  df <- data.frame() #create empty data frame
  for (i in 1:n) {
    # loop through files, rbinding them together
    library(readxl)
    x <- read_excel(files[i], na = "NULL")
    
    df <- rbind(df, x)
  }
  
  return(df)
}
