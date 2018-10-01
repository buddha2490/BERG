\name{ageStdCrossTab}
\alias{ageStdCrossTab}

\title{
Age-standardized frequencies
}
\description{
This function calculates age-standardized cross-tab frequencies for two variables
}
\usage{
ageStdCrossTab(dat,mar=2,agedist,age,var,strata)
}

\arguments{
  \item{dat}{
data frame used for the analysis
}
  \item{mar}{
1= row percents, 2=column percents (default)
}
  \item{agedist}{
Age-specific standardized weights.  See documentaion on ?nutweightlist for details
}
  \item{age}{
Character vector for your age at baseline variable.  This could be "AGE_INT" or "AGE92M", etc.
}
  \item{var}{
Character vector for the primary frequency variable (typically formatted as the row variables)
}
  \item{strata}{
Character vector for the strata variable for which you need frequencies (typically formatted as your column variable)
}
}


\value{
Data frame including the only the age-adjusted row/column percents
}

\author{
Maret Maliniak
}

\seealso{
\code{\link{nutweightlist}}
}
\examples{

df <- example.data
mytable <- ageStdCrossTab(dat=df,
                         mar=2,
                         agedist="YEARS00",
                         age="age92m",
                         var="bmicat92",
                         strata="sex")


}

