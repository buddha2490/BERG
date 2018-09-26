\name{dxdateClean}
\alias{dxdateClean}
\title{
Cleans up diagnosis dates - 1997-2013
}
\description{
This standard code that cleans up diagnosis dates based on date of interview for each of the followup surveys.  If diagnosis dates are <180 after interview date, the date is recoded to DTINT-1.  If the diagnosis dates are >180 days after interview date, the date is recoded to 1900-01-01 and will be excluded from analyses or flagged for further review.
}
\usage{
dxdateClean(data,d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame used in the analysis
}
  \item{d}{
 Date of diagnosis variable
 }
}

\value{
Cleaned diagnosis date is returned, named identically to the one given to the function.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Brian Carter
}

