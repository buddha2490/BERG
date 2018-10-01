\name{jointCox}
\alias{jointCox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to run joint-cox proportional hazards models for subtype analyses
}
\description{
Joint-cox models examine associations with risk factors across multiple disease subtypes.  See details:
}
\usage{
jointCox(dat, primarySubtype, otherSubtypes, start, stop, expo, age, idvar, covariates = NULL)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{start}{
Character vector of the variable name used for the start of followup.  Typically this will be one of our date of interview variables, "dtint92","dtint97", etc.
}
  \item{stop}{
Character vector of the end of followup variable.  Typically this would be "dateft", "datedd", etc.
}
  \item{expo}{
Character vector of main exposure variable.  Must be a factor or numeric variable.
}
  \item{age}{
Character vector for baseline age.  Models stratify on single year of baseline age.
}
  \item{idvar}{
Character vector for the ID variable, typically "ID".  coxph() uses the cluster(ID) function to compute robust sandwhich variance estimators.
}
  \item{covariates}{
Character vector of covariates required for the analysis.
}
}
\details{
The function assumes common followup time. The joint-cox model requires that data be restructured as long-form with indicator variables delineating each subtype.  This function restructures the data, fits the models, and returns the formatted results and model output.  P-tumor heterogeneity is calculated as a liklihood ratio test comparing the interaction joint-cox model to a simple additive base model without interactions with subtype.

The function will only calculate risk estimates for the primarySubtype variable.  You will need to rerun the function for each subtype in the analysis.

It is important that you look at the p-heterogeneity calculated for each subtype.  If you are modeling the same set of subtypes (and covariates) in each model, the p-heterogeneity should be identical for each set.  If they are different, something has gone wrong.

}
\value{
A list of three objects
 \item{final }{Final formatted dataset of estimates}
 \item{interaction}{Model output for the joint-cox models}
 \item{base}{Model output for the base comparison models (used for p-heterogeneity calculation)}

}
\references{
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3565022/
}
\author{
Brian Carter
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{prepJoint}}
}
\examples{

df <- example_data


# Run the function with AML being our primary outcome
aml.results <- jointCox(dat=df,
              primarySubtype="AML",  # Acute myeloid
              otherSubtypes="CML",
              start="dtint92",
              stop="dateft",
              expo="smoke92",
              age="age92m",
              idvar="id",
              covariates = c("racenew","educ","sex"))

# Run with CML being the primary outcome
cml.results <- jointCox(dat=df,
              primarySubtype="CML", # chronic myeloid
              otherSubtypes="AML",
              start="dtint92",
              stop="dateft",
              expo="smoke92",
              age="age92m",
              idvar="id",
              covariates = c("racenew","educ","sex"))

# Bind the results into the final table
final.results <- rbind(aml.results$final,cml.results$final)
print(final.results)

}
