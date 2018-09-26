\name{tbl1}
\alias{tbl1}
\title{
Creates and formats a typical Table 1 for categorical and continuous variables
}
\description{
The tbl1() function will create a typical descriptive table for categorical and continuous variables.  Frequencies are stratified by an exposure variable.  Percentages can be calculated as column or row percents.
}
\usage{
tbl1(dat, variable, stratvar, percents=2, freq.type=0)
}
\arguments{
  \item{dat}{
A data.frame in which to interpret the variables used in the function.
}
  \item{variable}{
  Main frequency variable or vector of variables.  In an N*N table, this would correspond to the rows
  }
  \item{stratvar}{
  Strata variable.  In an N*N table, this would correspond to the columns
  }
  \item{percents}{
  1=row percents; 2=column percents (default); 0=overall percents
  }
  \item{freq.type}{
  0=Both N and (percent); 1= N-only; 2=Percentages-only
  }
}
\details{
The tbl1() function will take any set of variables and calculate frequencies (N and percentages) across strata of another variable and format the output typical for BERG Table 1 publications.  Output for continuous variables will be mean (SD), categorical variables will be N (percent).  Total output is formatted so that continuous variables are at the top of the table, followed by results for categorical variables.  Final output is a data frame.}

\author{
Brian Carter
}

\examples{
# Create a simple test dataset
test1 <- data.frame(strata.variable=sample(0:1,100,replace=T),
	   	       continuous1 = runif(100,0,100),
		       continuous2 = runif(100,1,20),
		       categorical1 = sample(0:5,100,replace=T),
		       categorical2 = sample(1:3,100,replace=T)
		      )

table1 <- tbl1(dat=test1,
          variable=c("continuous1","continuous2","categorical1","categorical2"),
          stratvar="strata.variable")

}

