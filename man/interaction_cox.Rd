\name{interaction_cox}
\alias{interaction_cox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Runs single referent group cox models using interaction variables.
}
\description{
The interaction_cox() function runs single referent group interaction cox models.  The main interaction variable is coded using the interaction(var1, var2) function.
}
\usage{
interaction_cox(dat,start,stop,outcome,expoVar,strataVar,age,covariates=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data frame used in the analysis
}
  \item{start}{
     Character vector naming your start of followup time, can be numeric or date format.  Typically this is one of our DTINT variables.  For time dependent or late-entry models, this will be the name of your start time variable.
}
  \item{stop}{
     Character vector naming your end of followup time, can be numeric or date format.  Typically this is one of our DATEFT or DATEDD variables.  For time dependent or late-entry models, this will be the name of your stop time variable.
}
  \item{outcome}{
Character vector for the censor variable used in the analysis, must be coded as 0=control, 1=case
}
  \item{expoVar}{
Character vector for our main exposure variable, may be categorical or numeric
}
  \item{strataVar}{
Character vector for our stratification variable, must be categorical
}
  \item{age}{
Character vector for our age variable used for stratifying on single year of age
}
  \item{covariates}{
Character vector of covariates.  Default is NULL (age-adjusted analysis only)
}
}

\details{
The interaction_cox() function will run simple interaction models, format the results into a table, and return all model output in a list format that is suitable for program review. The function will run standard, time-dependent, or late-entry models, depending on how the data are structured prior to running the function

Interaction models are coded using the following formula:

y <- formula(Surv(start,stop,outcome)~ interaction(expoVar,strataVar) + covariates + strata(age))

To calculate a p-interaction, a reduced model is also calculated:

y <- formula(Surv(start,stop,outcome) strataVar + expoVar + covariates + strata(age))

The p-interaction is then calculated:

anova(interaction.model, reduced.model)

}
\value{
The function outputs a 3-level list including all the output from the analysis.
     \item{final}{A data frame containing the organized output of the function. Includes exposure name, categories, case numbers, stratified estimates and p-values, and a p-value for interaction.}
     \item{int.model}{All model output for the interaction model}
      \item{base.model}{All model output from the base model}
      }

\references{
G:/Intramural Research/Epidemiology Research/Analysts EPI/Memos, presentations, resources and code/Interaction/Memo, Interaction Methods.doc
}
\author{
Brian Carter
}

\seealso{
\code{\link{coxph}}, {\link{cox_models}}, {\link{stratified_cox}}
}


\examples{
df <- example_data

foo <- interaction_cox(dat=df,
                      start="dtint92",
                      stop="dateft",
                      outcome="dead",
                      expoVar="smoke92",
                      strataVar="bmicat92",
                      age="age92m",
                      covariates=NULL)

}

