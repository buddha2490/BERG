\name{incrate}
\alias{incrate}

\title{
Age standardized rate analysis
}
\description{
This function calculates age-standardized incidence and mortality rates for CPSII Mortality and Nutrition Cohorts analyses.
}
\usage{
incrate(dat=df, agedist, agegrps=NULL, dtint, birthday,
                    failtime, outcome, expo)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data.frame used in the analysis (default is "df" for use in modeling functions)
}
  \item{agedist}{
Age distribution standard (see nutweightlist documentation for details)
}
  \item{agegrps}{
Vector of start ages for the analysis.  Default is 5-year age groups starting at age 40-44, 45-59,... 85+.  To collapse ages, provide a vector of the ages you want each group to start with.  For example, if you wanted to use 10-year age groups you would provide: c(40,50,60,70,80) to create age groups 40-49, 50-59, 60-69, 70-79, 80+.  For 2 age groups (<65 and 65+) you would provide the vector c(40,65).
}
  \item{dtint}{
Variable name for the start of followup.  Example: "dtint92", "dtint97", etc.
}
  \item{birthday}{
Variable name exact birthdate, typically "BDAYDATE" in our master file.
}
  \item{failtime}{
Variable name for fail time in days
}
  \item{outcome}{
Variable name for your outcome, formatted 0=control, 1=case
}
  \item{expo}{
Factor variable for your exposure.
}
}
\details{
These rates are calculated using the pyrs() function in the survival package. Age-specific time and events are tabulated as each CPSII participant moves through calendar time.  For example, if a participant enrolls in the study at age 49, they will contribute person time to the age 45-49 age group until their 50th birthday, and then start contributing person time to the 50-54 age group.  Events are assessed at this attained age.

The pyrs() function simply calculated age- and strata-specific person years and events.  The rest of the function calculates the age-adjustment and final rates.
}
\value{
The function returns a list of age-specific person-years and rates for each exposure level of your variable. Also includes:
  \item{Std.Rates }{Final age-standardized rates calculated for each exposure level}

}

\author{
Brian Carter
}


\seealso{
 \code{\link{weightlist}}
}

\examples{

example.data <- data.frame(enrolldate=as.Date("1992-01-01"),
                      bdaydate=sample(c(-15000:5000),10000,replace=T),
                      failnew=sample(1:7000,10000,replace=T),
                      lungcancer=sample(0:1,10000,replace=T),
                      smoking=sample(c("Never","Current","Former"),10000,replace=T))

rates <- incrate(dat=example.data,
                 agedist="YEARS00",
                 agegrps=c(40,65),
                 dtint="enrolldate",
                 birthday="bdaydate",
                 failtime="failnew",
                 outcome="lungcancer",
                 expo="smoking")

print(rates$Std.Rate)
  }

