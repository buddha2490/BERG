\name{ageStdFreqs}
\alias{ageStdFreqs}

\title{
Age-standardized frequencies
}
\description{
This function calculates age-standardized one-variable frequencies.  A separate function is available to produce age-standardized 2x2 tables of two variables, which is probably more useful for typical analyses.
}
\usage{
ageStdFreqs(dat,agedist,age,var)
}

\arguments{
  \item{dat}{
data frame used for the analysis
}
  \item{agedist}{
Age-specific standardized weights.  See documentaion on ?nutweightlist for details
}
  \item{age}{
Character vector for your age at baseline variable.  This could be "AGE_INT" or "AGE92M", etc.
}
  \item{var}{
Character vector for the variable for which you need frequencies
}
}

}
\value{
Data frame including the raw frequencies and proportions, and a age-standardized proportion.
}

\author{
Maret Maliniak
}

\seealso{
\code{\link{nutweightlist}}
\code{\link{ageStdCrossTab}}
}
\examples{
example.data <- data.frame(bmicat=sample(c("<18.5","18.5-24.9","25-29.9","30+"),
                                   10000,replace=T),
                      age_int=sample(40:90,10000,replace=T),
                      smoking=sample(c("Never","Current","Former"),10000,replace=T))

mytable <- ageStdFreqs(dat=example.data,
                       agedist="YEARS00",
                       age="age_int",
                       var="bmicat")

# Run multiple frequencies using lapply()

manytables <- lapply(c("bmicat","smoking"),function(x){
     ageStdFreqs(dat=example.data,
                 agedist="YEARS00",
                 age="age_int",
                 var=x)
                 })
# create a single table from the results
final.frequencies <- Reduce(function(x,y) rbind(x,y), manytables)


