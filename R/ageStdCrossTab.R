
ageStdCrossTab <- function(dat,mar=2,agedist,age,var,strata){

  df <- dat

  if (class(df[[strata]]) != "factor") stop("strata is not a factor variable")



  # get weights

  weights <- weightlist[[agedist]]
  sum <- sum(weights$weights)
  weights$weights <- weights$weights/sum

  # categorize continuous age variable
  agecat <- factor(ifelse(df[[age]]<45,1, ifelse(
    df[[age]]<50,2, ifelse(
      df[[age]]<55,3, ifelse(
        df[[age]]<60,4, ifelse(
          df[[age]]<65,5, ifelse(
            df[[age]]<70,6, ifelse(
              df[[age]]<75,7, ifelse(
                df[[age]]<80,8, ifelse(
                  df[[age]]<85,9,10))))))))),1:10,
    weights$agegrp)

  # pull exposure/strata variable and factor levels
  expo <- df[[var]]
  strat <- df[[strata]]; strat <- factor(strat)
  cat.expo <- levels(expo)
  cat.strata <- levels(strat)

  #x <- cat.strata[1]
  columns <- function(x){
    one <- prop.table(table(agecat[strat==x],expo[strat==x]),1)
    one <- one*weights$weights
    tot.col <- apply(one,2,function(x) sum(x,na.rm=T))
    std.prop <- format(round(tot.col,4)*100,2)
    return(std.prop)
  }

  rows    <- function(y){
    one <- prop.table(table(agecat[expo==y], strat[expo==y]),1)
    one <- one*weights$weights
    tot.col <- apply(one,2,function(x) sum(x,na.rm=T))
    std.prop <- format(round(tot.col,4)*100,2)
  }

# row percents
if (mar==1){
  out <- lapply(cat.expo,rows)
  mat <- out[[1]]
  for (i in 2:length(out)) mat <- rbind(mat,out[[i]])
  final <- data.frame(cat.expo,mat,stringsAsFactors=F,row.names=NULL)
}

# column percents
if (mar==2){
  out <- lapply(cat.strata, columns)
  mat <- out[[1]]
  for (i in 2:length(out)) mat <- cbind(mat,out[[i]])
  final <- data.frame(row.names(mat),mat,stringsAsFactors=F,row.names=NULL)

}
  row.names(final) <- NULL
  final <- data.frame(c(var, rep(NA, nrow(final))),rbind(NA,final),stringsAsFactors=F)
  names(final) <- c("Variable","Category",cat.strata)
  return(final)
}
