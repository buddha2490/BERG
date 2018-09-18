
# dat - analytic dataset name
# agedist - which age-weighting distribution do you want
# agegrps - start age for collapsing 5-year age groups into larger groups
# start - start of followup variable, i.e. start92, start97, start82, etc
# bday - the BDAYDATE variable usually, unless you've renamed it
# failtime - fail failtime variable (dateft-start)
# outcome - Outcome variable, coded [0,1]
# expo - exposure of interest

incrate <- function(dat=df, agedist, agegrps=NULL, dtint, birthday,
                    failtime, outcome, expo){
newdf <- dat

load("data/nutweightlist.rdata")
weight <- weightlist[[agedist]]

# Age categories must be chosen.  Default is 40-44, 45-49, 50-54, etc.
# Custom age categories can be chosen by choosing the start age for each category
# Example, setting agegrps to c(40,65) would set two age groups: 40-64 and 65+
# options are:
# 40
# 45
# 50
# 55
# 60
# 65
# 70
# 75
# 80
# 85


# If you don't want the default 10 age groups, this little script will run
if (!is.null(agegrps)){
# Need to sum the appropriate age groups/weights
# There are a maximum of 10 age levels, so '11' indicates to end the loop
weight$startage <- as.numeric(substr(weight$agegrp,1,2))
obs <- c(match(agegrps,weight$startage),11)
agelist <- list()
for (i in 1:length(obs)){
     if (obs[i]==11){ break } else {
     agelist[[i]] <- weight[obs[i]:(obs[i+1]-1),]
     }}
names(agelist) <- paste0("Age",agegrps)

# This function will sum the various age groups weights and return a final set of weight
agelist2 <- lapply(names(agelist),function(x){
     foo <- agelist[[x]]
     foo$startage <- ifelse(foo$startage=="85","85P",foo$startage)
     newdf <- data.frame(agegrp=paste0(foo$startage[1],"-",foo$startage[nrow(foo)]),
                      weights=sum(foo$weights),
                      stringsAsFactors=F)
})
weight <- Reduce(function(x,y) rbind(x,y),agelist2)
rm(obs,agelist,agelist2,weightlist,i)
} else {
     agegrps <- seq(40,85,5)
}

# Create the tcut object
newdf$agedays <- as.numeric(newdf[[dtint]])-as.numeric(newdf[[birthday]])

# our "age 40" is actually "<40", so starting age at zero
newagegrps <- ifelse(agegrps==40,0,agegrps)

# The tcut object is failtime dependent age, in days
# Note: "200" used as the upper age cut because it is an arbitrary large number,
#        It basically just means "max"
newdf$agecat <- survival::tcut(newdf$agedays, c(newagegrps,200)*365.25,
                  labels=weight$agegrp)
rm(agegrps,newagegrps)



# Calculate age-specific rates --------------------------------------------

# This section uses the pyears() function to calculate the age-specific outcomes-person years
# Calculating the rates and confidence intervals are my contribution
  y <- formula(paste0("Surv(",failtime,",",outcome,")~ agecat + ",expo))
  pyrs <- survival::pyears(y,data=newdf,data.frame=T, scale=365.25)
  pyrs <- pyrs$data
  names(pyrs) <- c("tcut","expo","pyears","n","outcome")
  pyrs$rate <- (pyrs$outcome/pyrs$pyears)*100000
  merged <- merge(pyrs, weight, by.x="tcut",by.y="agegrp")
  merged$exp <- merged$weight * (merged$outcome/merged$pyears)
  merged$wsqvar <- merged$weight^2 * (merged$outcome/ merged$pyears^2)
  merged$ratevar <- merged$wsqvar * (100000^2) / (merged$weight^2)
  merged$rate.ll <- merged$rate - (1.96*sqrt(merged$ratevar))
  merged$rate.ul <- merged$rate + (1.96*sqrt(merged$ratevar))
 merged[,c("rate", "rate.ll", "rate.ul")] <- apply(merged[,c("rate", "rate.ll", "rate.ul")],2,function(x){round(x,2)})


# This section will do the actual age adjustment
 # It will provide exposure-specific deaths, person years, standardized rates with CI, and RR (95% CI)
# create a shell table to fill in with standardized weights
  cat <- names(table(merged$expo))
  shell <- data.frame(vector("character",length(cat)),
                      vector("numeric", length(cat)),
                      vector("numeric", length(cat)),
                      vector("numeric", length(cat)),
                      vector("numeric", length(cat)),
                      vector("numeric", length(cat)),
                      vector("numeric", length(cat)),
                      vector("numeric", length(cat)),
                      stringsAsFactors=F)
  names(shell) <- c("Exposure", "Deaths", "Person-years", "Std rate", "Rate LL", "Rate UL", "wsqvar", "exp")

  # fill in the shell table
  for (i in 1:length(cat)){
    shell[i,1] <- cat[i] # exposure level
    shell[i,2]<-sum(merged$outcome[(merged$expo)==cat[i]]) # deaths
    shell[i,3]<-sum(merged$pyears[(merged$expo)==cat[i]]) # pyears
    shell[i,4]<- sum(merged$exp[(merged$expo)==cat[i]]) /
      sum(merged$weight[(merged$expo)==cat[i]])*100000 # std rate
    shell[i,5]<-shell[i,4]-
      (1.96*(sqrt(sum(merged$wsqvar[(merged$expo)==cat[i]]))))*
      (100000^2/ (sum(merged$weight[(merged$expo)==cat[i]]))^2)
    shell[i,6]<-shell[i,4]+
      (1.96*(sqrt(sum(merged$wsqvar[(merged$expo)==cat[i]]))))*
      (100000^2/ (sum(merged$weight[(merged$expo)==cat[i]]))^2)

    shell[i,7] <- sum(merged$wsqvar[(merged$expo)==cat[i]])
    shell[i,8]<- sum(merged$exp[(merged$expo)==cat[i]])

  }
shell$srrvar <- shell$wsqvar/shell$exp^2
shell$RR[1] <- 1.00
shell$LL[1] <- NA
shell$UL[1]<- NA

  for (i in 2:nrow(shell)){
  shell$RR[i] <- round(shell[i,4]/shell[1,4],2)
  shell$LL[i] <- round(shell$RR[i] * exp(-1.96* sqrt(shell$srrvar[i]+shell$srrvar[1])),2)
  shell$UL[i] <- round(shell$RR[i] * exp(1.96* sqrt(shell$srrvar[i]+shell$srrvar[1])),2)
  i<- i+1
  }
shell <- shell[,!names(shell) %in% c("wsqvar","exp","srrvar")]
round <- c("Person-years","Std rate","Rate LL","Rate UL","RR","LL","UL")
shell[,round] <- apply(shell[,round],2,function(x){format(round(x,2),nsmall=2)})
shell <- rbind(shell, c("Total", apply(shell[,2:3],2,function(x) sum(as.numeric(x))), "", "", "", "", "",""))


# Now I'm going to create a list with all the age-specific outcomes/pyears/rates/RRs/etc


  final <- vector("list", length(cat)+1)
  names(final) <- c(cat, "Std.Rates")
  for (i in 1:length(cat)){
    final[[i]] <- merged[(merged$expo)==cat[i],]
    final[[i]] <- final[[i]][,c("tcut", "pyears", "outcome", "exp", "rate", "rate.ll", "rate.ul")]
    names(final[[i]]) <- c("Age", "Person years", "Deaths/Cases", "Expected", "Rate", "Rate LL", "Rate UL")
  }
  final[[(length(cat)+1)]] <- shell

cats <- length(names(final))

# Calculate age-specific RRs and 95% CIs
final[[1]]$RR <- ""
final[[1]]$LL <- ""
final[[1]]$UL <- ""
final[[1]][,"RR (95% CI)"] <- "1.00"


# I need to do this for all the list elements except for the last (the standardized rates)
# And the calculation is done line by line, compared to the same line in the reference group
for (x in 2:(cats-1)){
  rows <- nrow(final[[x]])
     final[[x]]$RR <- final[[x]]$Rate[1:rows]/final[[1]]$Rate[1:rows]
     final[[x]]$var <- (1/final[[1]][1:rows,3]) + (1/final[[x]][1:rows,3])
     final[[x]]$LL <- exp(log(final[[x]]$RR) - (1.96*(sqrt(final[[x]]$var))))
     final[[x]]$UL <- exp(log(final[[x]]$RR) + (1.96*(sqrt(final[[x]]$var))))
     final[[x]][,"RR (95% CI)"] <- paste(round(final[[x]]$RR,2), " (",
                                    round(final[[x]]$LL,2), ", ",
                                    round(final[[x]]$UL,2), ")",
                                    sep="")
     final[[x]] <- final[[x]][,c("Age", "Person years", "Deaths/Cases", "Rate", "Rate LL", "Rate UL",
                                      "RR (95% CI)")]
     }
final[[1]] <- final[[1]][,c("Age", "Person years", "Deaths/Cases", "Rate", "Rate LL", "Rate UL",
                            "RR (95% CI)")]

return(final)
}



