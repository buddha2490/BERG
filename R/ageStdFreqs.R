
ageStdFreqs <- function(dat,agedist,age,var){

  df <- dat

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

  # pull exposure variable and factor levels
  expo <- df[[var]]
  cat <- levels(expo)

  # Raw Frequencies
  raw1 <- table(expo)
  raw2 <- prop.table(table(expo))
  raw <- data.frame(Categories=cat,
                    Frequency=format(raw1,big.mark=",", scientific=F),
                    Proportion=format(round(raw2,4)*100,2),stringsAsFactors=F)
  row.names(raw) <- NULL



  # standardized frequencies
standardize <- function(x){
  agefreq <- table(agecat[expo==x])  # raw age-specific frequencies for expo level
  agefreq <- agefreq*weights$weights # multiply frequencies by age-specific weights
  Freq.Adj <- sum(agefreq)  #  sum to get an adjusted frequency for expo level
  tot <- cbind(x,Freq.Adj)  # combine with the exposure level name
  return(tot)
}
out <- lapply(cat,standardize)  # run the function

# bind the results
adj <- data.frame(out[[1]],stringsAsFactors=F)
for (i in 2:length(out)) adj <- rbind(adj,out[[i]])

# take a total sum of the adjusted frequency - denominator for adjusted proportions
tot <- sum(as.numeric(adj$Freq.Adj))
adj$Std.Prop <- format(round(as.numeric(adj$Freq.Adj)/tot,4)*100,2)   # calculate the proportion.

# make the output look pretty.
freqs <- rbind(NA, cbind(raw,adj$Std.Prop))
final <- data.frame(Variable=c(var,rep(NA,nrow(freqs)-1)),freqs,stringsAsFactors=F)
names(final) <- c("Variable","Categories","Frequency","Proportion","Std.Proportion")
  return(final)
}
