check_state<- function(abrv_state_name){
    read_file <- read.csv("outcome-of-care-measures.csv")
    state <<- "NULL"
    row_number<- nrow(read_file)
    state_list<- read_file[, 7]
    for(i in 1:row_number) {
         if (state_list[i] == abrv_state_name){
          state <<- abrv_state_name
          return(state)
          }
    else if(state == "NULL") {
          next
          }
    }
}

best <- function(abrv_state_name, outcome){
    read_file <- read.csv("outcome-of-care-measures.csv")
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    count <- 0
    for(i in 1:3) {
        if(outcome == outcomes[i])
        count<- 1
        }
    if(count == 0){
      stop("invalid outcome")
    }
    get_state <- "NULL"
    state <- "NULL"
         if(state == abrv_state_name) {
              print(state)
         }
         else {
          get_state<- check_state(abrv_state_name)
              if(is.null(get_state)) {
              stop("invalid state")
              }
          print(get_state)
         }
  list_1 <- which(read_file[, 7] == abrv_state_name)
  chunk <- data.frame()
  nar_chunk <- data.frame()
  ord_chunk <- data.frame()
  len <- length(list_1)
  for(i in 1 : len) {
      if( i == 1) {
        chunk<- (read_file[list_1[i], c(2,7,11,17,23) ])
      }
      else{
        chunk<- rbind(chunk, read_file[list_1[i], c(2,7,11,17,23) ])
      }
    }
  if(outcome == "heart attack"){
      to_index <- 3
    }
  else if(outcome == "heart failure"){
      to_index <- 4
      }
  else{
    to_index <- 5
    }
  nar_chunk <- as.data.frame(c(chunk[1], chunk[2], chunk[to_index] ))
  ord_chunk <- nar_chunk[order(nar_chunk[ ,3]), ]
  clean<- data.frame()
  list_ord <- which(ord_chunk[3] != "Not Available")
  len <- length(list_ord)
  for(i in 1:len) {
      if(i == 1) {
        clean <- ord_chunk[list_ord[i], ]
      }
      else{
        clean<- rbind(clean, ord_chunk[list_ord[i], ])
      }
  }
  if(to_index == 3){
         clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
       }
  if(to_index == 4) {
         clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
       }
  if(to_index == 5) {
         clean$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(clean$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
       }
  ord_clean <- clean[order(clean[ ,3] , clean[ ,1]), ]
  print(ord_clean[1,1])
}
