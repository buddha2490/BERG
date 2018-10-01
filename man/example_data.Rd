\name{example_data}
\alias{example_data}
\docType{data}
\title{
This is an example dataset useful for demonstrating the functions in the BERG package.
}
\description{
The example_data data frame uses data from a recent analysis of BMI and myeloid leukemias.  The IDs and dates are fake, so please don't merge it with any existing data or believe and results that come out of it.  The data are only useful for learning the variaous BERG functions
}
\usage{data("example_data")}
\format{
  A data frame with 152,046 observations on the following 15 variables.
  \describe{
    \item{\code{racenew}}{Factor w/ 3 levels "White","Black",..: 1 1 1 1 1 1 1 1 3 2 ...}
    \item{\code{age92m}}{num  60 65 61 68 68 65 58 66 66 62 ...}
    \item{\code{sex}}{Factor w/ 2 levels "Men","Women": 1 1 1 1 1 1 1 1 1 1 ...}
    \item{\code{bmicat92}}{Factor w/ 3 levels "18.5-24.9","25-29.9",..: 1 1 1 2 1 2 2 1 1 2 ...}
    \item{\code{bmi92}}{num  23.7 24.8 20.3 25.4 23.2 ...}
    \item{\code{smoke92}}{Factor w/ 5 levels "Never","Current",..: 3 3 1 3 3 1 3 3 1 3 ...}
    \item{\code{dead}}{num  0 0 0 1 1 1 1 1 0 0 ...}
    \item{\code{myeloid}}{num  0 0 0 0 0 0 0 0 0 0 ...}
    \item{\code{bdaydate}}{Date, format: "1931-01-01" "1927-01-01" "1931-01-01" "1924-01-01" ...}
    \item{\code{dtint92}}{Date, format: "1992-01-01" "1992-01-01" "1992-01-01" "1992-01-01" ...}
    \item{\code{dateft}}{Date, format: "2010-03-26" "2013-03-28" "2007-01-08" "2007-10-31" ...}
    \item{\code{AML}}{num  0 0 0 0 0 0 0 0 0 0 ...}
    \item{\code{CML}}{ num  0 0 0 0 0 0 0 0 0 0 ...}
    \item{\code{educ}}{Factor w/ 4 levels "High school or less",..: 3 3 3 3 3 3 3 3 3 1 ...}
    \item{\code{id}}{Character vector similar to our ID variable}

  }
}
