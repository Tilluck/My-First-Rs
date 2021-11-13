rankall <- function(outcome, num){
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
  if(outcome == "heart attack"){
    to_index <- 11
  }
  else if(outcome == "heart failure"){
    to_index <- 17
    }
  else{
  to_index <- 23
  }
  read_file_2<- as.data.frame(c(read_file[2], read_file[7],read_file[to_index] ))

  read_file_3 <- read_file_2[order(read_file_2[ ,3]), ]
  clean<- data.frame()
  list_c <- which(read_file_3[3] != "Not Available")
  len_2 <- length(list_c)
  for(i in 1 : len_2) {
      if( i == 1) {
        clean<- read_file_3[list_c[i], ]
      }
      else{
        clean<- rbind(clean, read_file_3[list_c[i], ])
      }
  }
  if(to_index == 11){
         clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
       }
  if(to_index == 17) {
         clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
       }
  if(to_index == 23) {
         clean$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(clean$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
       }

  ord_clean <- clean[order(clean[ ,3] , clean[ ,1]), ]
  col_2 <- ord_clean[,2]
  table_1 <- table(col_2)
  state_list <- unique(col_2)
  len_3 <- length(state_list)
  if(num == "best"){
    cache_1 <- data.frame()
    cache_2 <- data.frame()
    cache_3 <- data.frame()
    for(i in 1:len_3){
         to_sort <- state_list[i]
         for(i in 1:nrow(ord_clean)){
                if(ord_clean[i,2] == to_sort){
                  cache_1 <- as.data.frame(ord_clean[i, c(1,2)])
                  invisible(cache_1)
                  break
                }
          }
          if(i == 1) {
            cache_2 <- as.data.frame(cache_1)
          }
          else {
            cache_2 <- rbind(cache_2, cache_1)
          }
      }

  }
  else if(num == "worst"){
    cache_1 <- data.frame()
    cache_2 <- data.frame()
    cache_3 <- data.frame()
    len_table <- length(table_1)
    for(i in 1:len_table){
        na_table <- names(table_1[i])
        nu_table <- as.numeric(table_1[[i]])
        count_2 <- 0
        for(i in 1:nrow(ord_clean)){
               if(ord_clean[i,2] == na_table){
                 count_2 <- count_2 + 1
               }
               if(count_2 == nu_table) {
               cache_1 <- as.data.frame(ord_clean[i, c(1,2)])
               invisible(cache_1)
               break
               }
         }
         if(i == 1) {
           cache_2 <- as.data.frame(cache_1)
         }
         else {
           cache_2 <- rbind(cache_2, cache_1)
         }
    }

  }
  cls_num<- class(num)
  if(cls_num != "numeric" ){
    cache_3<- cache_2[order(cache_2[ ,2]), ]
    return(cache_3)
  }
  if(num > 301){
    return("NA")
  }
  cache_1 <- data.frame()
  cache_2 <- data.frame()
  cache_3 <- data.frame()
  for(i in 1:len_3){
       to_sort <- state_list[i]
       count_2 <- 0
       if(i == 2) {
         cache_2 <- as.data.frame(cache_1)
       }
       else if(i > 2 ){
         cache_2 <- rbind(cache_2, cache_1)
       }
       for(i in 1:nrow(ord_clean)){
              if(ord_clean[i,2] == to_sort){
                count_2 <- count_2 + 1
              }
              if(count_2 == num) {
              cache_1 <- as.data.frame(ord_clean[i, c(1,2)])
              invisible(cache_1)
              break
              }
        }
  }
  cache_3<- cache_2[order(cache_2[ ,2]), ]
  print(cache_3)
}
