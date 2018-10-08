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


\value{
Data frame including the raw frequencies and proportions, and a age-standardized proportion.
}

\author{
Maret Maliniak
}

\seealso{
\code{\link{weightlist}}
\code{\link{ageStdCrossTab}}
}
\examples{
df <- example.data

mytable <- ageStdFreqs(dat=df,
                       agedist="YEARS00",
                       age="age92m",
                       var="bmicat92")

# Run multiple frequencies using lapply()

manytables <- lapply(c("bmicat92","smoke92"),function(x){
     ageStdFreqs(dat=df,
                 agedist="YEARS00",
                 age="age92m",
                 var=x)
                 })
# create a single table from the results
final.frequencies <- Reduce(function(x,y) rbind(x,y), manytables)


}
