\name{prepJoint}
\alias{prepJoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Preps a cohort for a joint-cox regression model
}
\description{
Joint cox proportional hazards modeling in R requires that data be reformatted as long-form.  In short, one dataset is created for each subtype the user wants to model, indicator variables are defined, and then each subtype dataset is rbind() back together into one long dataset.  The result is a dataset with duplicates for each original observation.  The prepJoint() function simply automates this process.


}
\usage{
prepJoint(dat,subtypeVars)
}

\arguments{
  \item{dat}{
     Data frame used in the analysis.
}
  \item{subtypeVars}{
  Character vector of subtype variable names.  These subtypes should be dummy coded, 0=noncase, 1=case.  For example, if you are modeling ER-status breast cancer, you would want a dummy variable for "ERPOS" and "ERNEG", each coded [0,1].  The user can include as many of these subtype variables as needed.
  }
}
\details{
}
\value{
Returns a dataset structured for a joint-cox regression analysis.  The dataset will have duplicated observations for each subtype requested.  Two new variables will be added: "OUTCOME" which will match the subtype dummy codes, and "EVENT2" which indicates which subtype is the primary outcome used in the analysis.
}
\references{
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3565022/
}

\seealso{
\code{\link{jointCox}}
}

\author{
Brian Carter
}
\note{
This function expands the size of your analytic dataset.  It is unlikely that the user will run into memory issues restructuring the data using this function.  However, the dataset returned by prepJoint() will be used in the final modeling procedure which can be quite memory intensive.  Please consider subsetting your analytic dataset to only the variables you need prior to running the prepJoint() function.  Otherwise, you may run into memory overflow issues when fitting the joint cox models.

}

\examples{

example.data <- data.frame(ID=as.character(1:1000),
                           ERPOS=sample(c(0,0,0,1),10000,replace=T),
                           ERNEG=sample(c(0,0,0,0,0,1),10000,replace=T),
                           smoking=factor(sample(c("Never","Current","Former"),
                                                 10000,replace=T)),
                           start.time=0,
                           dateft=runif(10000,1,50000),
                           age92m=runif(10000,45,70))


joint <- prepJoint(example.data,c("ERPOS","ERNEG"))

}

