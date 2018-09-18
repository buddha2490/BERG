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
interaction_cox(dat,failtime,outcome,expoVar,strataVar,age,covariates=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data frame used in the analysis
}
  \item{failtime}{
Failtime variable used in the analysis
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
The interaction_cox() function will run simple interaction models, format the results into a table, and return all model output in a list format that is suitable for program review.

Interaction models are coded using the following formula:

y <- formula(Surv(failtime,outcome)~ interaction(expoVar,strataVar) + covariates + strata(age))

To calculate a p-interaction, a reduced model is also calculated:

y <- formula(Surv(failtime,outcome) strataVar + expoVar + covariates + strata(age))

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
\code{\link{coxph}}, {\link{cox_models}}, {\link{stratified_models}}
}


\examples{
example.data <- data.frame(failnew=sample(1:7000,10000,replace=T),
                           allcauses=sample(0:1,10000,replace=T),
                           smoking=factor(sample(c("Never","Current","Former"),
                                                 10000,replace=T)),
                           bmicat=factor(sample(c("18.5-24.9","25-29.9","30+"),
                           10000,replace=T)),
                           age_int=round(sample(c(40:80),10000,replace=T)))

foo <- interaction_cox(dat=example.data,
                      failtime="failnew",
                      outcome="allcauses",
                      expoVar="smoking",
                      strataVar="bmicat",
                      age="age_int",
                      covariates=NULL)



