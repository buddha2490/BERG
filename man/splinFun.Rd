\name{splineFun}
\alias{splineFun}
\title{
Restricted cubic splines in R
}
\description{
The splineFun() function plots restricted cubic splines adjusted for covariates.
}
\usage{
splineFun(dat,expo,covariates=NULL,reference=NULL,knots,
                      failtime,outcome,agestrat,
                      expo.label=NULL,
                      outcome.text=NULL)
}

\arguments{
  \item{dat}{
Data frame used for the analysis
}
  \item{expo}{
Character vector of your main exposure variable.  Must be a continuous-numeric variable.
}
  \item{covariates}{
Character vector of your list of covariates.  Covariates can be a mixture of numeric, character, or factor variables.  Default is NULL, splines will only be age adjusted.
}
  \item{reference}{
The spline plot is centered on a reference value.  If left NULL, this reference value is computed as the median of your expo variable.
}
 \item{knots}{
Number of knots (must be >=3) calculated using default quantiles of expo.  For 3-5 knots, the outer quantiles used are 0.05 and 0.95.  For knots > 5, the outer quantiles are 0.025 and 0.975.  The remaining knots are equally spaced between these outer quantiles.
 }
  \item{failtime}{
Character vector of your failtime variable, must be coded as numeric.
}
  \item{outcome}{
Outcome variable.  Must be numeric and coded as 0=control, 1=case
}
  \item{agestrat}{
Character vector of continuous age.  BERG analyses stratify Cox models on single year of baseline age (i.e. AGE_INT, AGE92M, etc).  These splines are calculated using the same stratification procedure.
}
  \item{expo.label}{
Character label for your x-axis.  If left NULL it will default to the expo variable name.  Otherwise you can label your x-axis anything you want, i.e. "Baseline BMI", "Cigarettes per day", etc.
}
  \item{outcome.text}{
Character vector describing your outcome that will go into the figure title.  If left NULL, this value will default to the name of our outcome variable name.  Otherwise you can label your outcome any way you want, i.e. "Incident breast cancer", "Fatal lung cancer in men", etc.
}
}

\value{
A ggplot graph is returned after running the function.  The user can output this figure using any of the default graphic output functions included in base R.  The final figure is editable using normal ggplot2 commands.
}

\author{
Maret Maliniak and Brian Carter
}

\section{Warning}{
In datadist(df) : [variable] is constant.  The RMS package creates a a datadist object that includes summaries of all variables in a dataset and is required for adjusting the spline models for covariates. If any of the variables in your dataset are nonvarying, you will get this warning.  It is not necessarily a fatal error.  But if the variable is an important part of your model, you might want to consider whether it is coded correctly.
}

\section{Notes}{
The final plots are limited on the x and y axis.  On the Y-axis (Hazard ratios) range from 0-4.0.  The X-axis limits are based on the distribution of your data, with cutoffs at the 0.01 and 0.99 quantiles.  This may result in a warning message that some data have been dropped from the figure.  These values are dropped from the X-axis due to formatting issues in ggplot().  If this is a problem, it can be changed in the function, but it is recommended that you allow the truncating of the extreme ends of our exposure variable.
}

\seealso{
\code{\link{rcs}}
}
\examples{
example.data <- data.frame(failnew=sample(1:7000,10000,replace=T),
                           allcauses=sample(0:1,10000,replace=T),
                           BMI=runif(10000,18.5,50),
                           smoking=factor(sample(c("Never","Current","Former"),
                                                 10000,replace=T)),
                           age92m=sample(40:85,10000,replace=T))

figure <- splineFun(dat=example.data,
                    expo="BMI",
                    covariates=NULL,
                    reference=25,
                    knots=5,
                    failtime="failnew",
                    outcome="allcauses",
                    agestrat="age92m",
                    expo.label="Baseline BMI",
                    outcome.text="Death from all causes")

plot(figure)

}

