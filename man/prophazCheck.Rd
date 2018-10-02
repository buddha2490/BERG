\name{prophazCheck}
\alias{prophazCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check proportional hazards assumption and log-log survival plots.
}
\description{
prophazCheck quickly checks the proportional hazards assumption and returns a data frame of p-values and log-log survival plots.
}
\usage{
prophazCheck(dat,start,stop,outcome,age,expo,outcome.title=NULL,expo.title=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
     Data frame used in the analysis
}
  \item{start}{
     Character vector of the variable name for your start of followup.  Typically this would be "dtint92", "dtint97", etc.  Variable can be formatted as a date or numeric variable.  It is converted to a numeric variable within the function.
}
  \item{stop}{
     Character vector of the variable name for your end of followup.  Typically this would be "dateft", "datedd".  Variable can be formatted as a date or numeric variable, but is converted to numeric within the function to calculate continuous followup time.
}
  \item{outcome}{
     Character vector indicating the variable name for your censor variable.  Variable should be coded as a numeric variable, with 0=noncase, and 1=control
}
  \item{age}
     Character vector indicating your baseline age variable.  Cox models used the strata() function to stratify on single year of baseline age.  Typically this would correspond to variables "AGE_INT", "AGE92M", etc.
}
  \item{expo}{
     Character vector indicating your main exposure variable for which you want to assess proportional hazards.
}
  \item{outcome.title}{
     Provide a character vector describing your censor variable.  This will be used in the plots title.  The function defaults to the "outcome" argument, but you can call it anything you want.
}
  \item{expo.title}{
          Provide a character vector describing your exposure variable.  This will be used in the plots title.  The function defaults to the "expo" argument, but you can call it anything you want.
}

}
\details{
Proportional hazards are checked using the cox_zph() function in the survival package by using the Schoenfeld residuals against transformed time.  Log-log survival plots are also calculated and returned.
}
\value{
A list containing 2 objects
 \item{pval}{Data frame with three variables: exposure variable, outcome variable, and p-value for proportional hazards assumption}
 \item{plots}{log-log survival plots}

}

\author{
Brian Carter
}

\seealso{
\code{\link{cox.zph}}, ~~~
}
\examples{

foo <- prophazCheck(dat=example_data,
             start="dtint92",
             stop="dateft",
             outcome="myeloid",
             age="age92m",
             expo="bmicat92",
             outcome.title="All myeloid leukemias",
             expo.title="baseline BMI")
foo$plots  # draws plots

# You can save plots using the png() function

png(filename="My LogLog Plots.png", width=5,height=5,units="in",res=400)
foo$plots
dev.off()


}

