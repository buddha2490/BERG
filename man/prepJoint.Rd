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
prepJoint(dat,primarySubtype,otherSubtypes)
}

\arguments{
  \item{dat}{
     Data frame used in the analysis.
}
  \item{primarySubtype}{
  Character vector indicating your primary subtype.  This is the outcome for which you want risk estimates.  Variable should be coded as 0=noncase, 1=case.
  }
    \item{otherSubtypes}{
  Character vector indicating the other subtypes.  These variables should also be coded as 0=noncase, 1=case.  There is no limit on how many subtypes can be included in the model.
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

df <- example_data
joint <- prepJoint(df,primarySubtype="AML",otherSubtypes="CML")

}

