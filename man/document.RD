\name{document}
\alias{document}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Documents number of observations and variables for mergind datasets
}
\description{
The document() function replicates some of the output found in the SAS log.  When loading or merging datasets, calling document() on any number of data frames will return the number of observations and number of variables.  This can be compared to the final merged dataset to spot-check that no variables or observations were dropped.
}
\usage{
document(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
data frames requiring documentation.  The user can insert as many dataframes as necessary to check, separated by a coma.
}
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Brian Carter and James Hodge
}

\examples{
mydata1 <- data.frame(x=1:5,y=x^2)
mydata2 <- data.frame(a=letters[1:10],b=rnorm(10,5))
document(mydata1,mydata2)


