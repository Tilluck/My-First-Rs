complete<- function(directory, id = 1:332) {
    list_files2 <- list.files(directory, full.names = TRUE)
    new_list<- list_files2[id]
    list_files3 <- list.files(directory)
    count<- list_files3[id]
    len<-length(count)
# Get list on names of the files
    y<- data.frame()
    for(i in 1:len){
        no_ext<- substr(count[i], 1, nchar(count[i])-4)
        z<- data.frame(no_ext)
            if( i == 1)
               y <- z
            else
               (y <- rbind(y, z))
      }
# Get complete cases
    x<- data.frame()
    for(i in 1:len) {
       read_file2<- read.csv(new_list[i])
       complete<- complete.cases(read_file2)
       good<- read_file2[complete, ]
       z<- dim(good)
       a<- z[1]
           if(i == 1)
                x <- a
           else
               x<- rbind(x, a)

    }
# merge the two flies and rename the columns
t<- data.frame()
t<- cbind(y,x)
names(t)[names(t) == "no_ext"] <- "id"
names(t)[names(t) == "x"] <- "nobs"
print(t)
}
