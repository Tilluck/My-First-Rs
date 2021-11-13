pollutantmean<- function(directory, pollutant, id = 1:332){
    list_files <- list.files(directory, full.names = TRUE)
    working_files<- list_files[id]
    table = data.frame()
      for( i in seq_along(working_files)) {
      read_file<- read.csv(working_files[i])
      temp <- read_file[[pollutant]]
      if( i == 1)
          table <- temp[!is.na(temp)]
      else table <- c(table, temp[!is.na(temp)])
    }

    mean(table)
}
