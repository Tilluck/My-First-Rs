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
rankhospital <- function(abrv_state_name, outcome, num){
    read_file <- read.csv("outcome-of-care-measures.csv")
    outcomes <- c("heart attack", "heart Failure", "pneumonia")
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
    ord_1 <- chunk[order(chunk[ ,to_index], chunk[ ,1]), ]
    ord_2 <- data.frame()
    list_2 <- which(ord_1[to_index] != "Not Available")
    len_2 <- length(list_2)
    for(i in 1 : len_2) {
        if( i == 1) {
          ord_2<- (ord_1[list_2[i], ])
        }
        else{
          ord_2<- rbind(ord_2, ord_1[list_2[i], ])
        }
      }

  len_2 <- nrow(ord_2)
  cls_num<- class(num)
  if(cls_num == "numeric"){
     if( num > len_2){
        print(NA)
     }
  }
  if(to_index == 3){
     ord_2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(ord_2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
   }
   else if(to_index == 4) {
     ord_2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(ord_2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
   }
   else {
     ord_2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(ord_2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
   }
  ord_3 <- ord_2[order(ord_2[ ,to_index] , ord_2[ ,1]), ]
  if(num == "best"){
    print(ord_3[1,1])
  }
  else if(num == "worst"){
    print(ord_3[len_2,1])
  }
  else{
    print(ord_3[num,1])
  }
  l_group<- len_2 - 2
  print(ord_3[c(1:12, l_group:len_2), ])
}
