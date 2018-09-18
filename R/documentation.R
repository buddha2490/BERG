documentation <- function(...){
     data<-list(...)
     # Take each input parameter and make list of values - then convert to character vector
     # [-1L] removes first element of list (always "List" in this case)
     names(data)<-as.character(as.list(substitute(list(...)))[-1L])
     for (i in 1:length(data)){
          df.name <- names(data[i])
          rows <- nrow(data[[i]])
          cols <- ncol(data[[i]])
          print(paste("Data frame", df.name, "has", rows, "observations and", cols, "variables"))
     }
}
