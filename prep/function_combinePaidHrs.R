combinePaidHrs2 <- function(directory) {
  ## 'directory' is a char vector of len 1 indicating location of CSV files
  files <- list.files(directory,full.names = TRUE) # make list of full file names
  n <- length(files)
  df <- data.frame() #create empty data frame
  for (i in 1:n) {
    # loop through files, rbinding them together
    library(readxl)
    x <- read_excel(files[i],
                    col_types = c("text","text","date","text","date","date",
                                  "text","date","date","numeric","numeric","text",
                                  "text","text","text","text","text","text",
                                  "numeric","numeric","numeric","numeric","text","text",
                                  "text","text","text","text","text","text",
                                  "text","text","text","text"))
    
    df <- rbind(df, x)
  }
  
  df <- df[c(1:29)]
  names(df)[27]<-"primary_site"
  names(df)[28]<-"cost_center"
  names(df)[29]<-"gl_program"
  
  return(df)
}