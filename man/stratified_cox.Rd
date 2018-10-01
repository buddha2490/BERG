\name{stratified_cox}
\alias{stratified_cox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Runs multiple referent group cox models using interaction variables.
}
\description{
The stratified_cox() function runs multiple referent group interaction cox models.  The results will show the effect of our main exposure variable within each level of our stratification variable.
}
\usage{
stratified_cox(dat,start,stop,outcome,expoVar,strataVar,age,covariates=NULL)
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
Character vector for the outcome variable used in the analysis, must be coded as 0=control, 1=case
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
The stratified_cox() function will run your stratified interaction models, format the results into a table, and return all model output in a list format that is suitable for program review.  Function will work for standard, time-dependent, or late-entry models, depending on how the data are formatted prior to running the function.

Interaction models are coded using the following formula:

y <- formula(Surv(start,stop,outcome)~ strataVar + strataVar:expoVar + covariates + strata(age))

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
\code{\link{coxph}}, {\link{cox_models}}, {\link{interaction_cox}}
}


\examples{
df <- example_data

foo <- stratified_cox(dat=df,
                      start="dtint92",
                      stop="dateft",
                      outcome="dead",
                      expoVar="smoke92",
                      strataVar="bmicat92",
                      age="age92m",
                      covariates=NULL)


}
