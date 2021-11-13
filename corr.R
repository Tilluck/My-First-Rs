corr<- function(directory, threshold = 0) {
    all_files <- list.files(directory, full.names = TRUE)
    d<- data.frame()
    for(i in seq_along(all_files)) {
       read_file4<- read.csv(all_files[i])
       complete<- complete.cases(read_file4)
       good<- read_file4[complete, ]
       e<- dim(good)
       f<- e[1]
           if(i == 1)
                d <- f
           else
               d<- rbind(d, f)
             }
    y<- vector(mode="numeric", length = 0)
    for( i in seq_along(d)) {
        if(d[i] > threshold) {
            read_file5<- read.csv(all_files[i])
            comp<- complete.cases(read_file5)
            na_rm<- read_file5[comp, ]
            x <- cor(na_rm[["sulfate"]], na_rm[["nitrate"]])
            y<- append(y,x)
          }
          else{
            next
          }
    }
    return(y)
}
