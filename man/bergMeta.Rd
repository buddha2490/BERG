\name{bergMeta}
\alias{bergMeta}
\title{
Simple meta-analysis function using the metafor package
}
\description{
The metafor package does all the hard work for metaanalyses.  This function simply takes a standardized input of relative risks and confidence intervals and calculates pooled estimates using a variety of methods.  Also computes study heterogeneity statistics.
}
\usage{
bergMeta(dat,method="DL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A data frame.  Must have THREE variables:  RR = relative risk estimates, LL=lower bound confidence limit, and UL=upper bound confidence limit.  The data frame can have anything else in it, as long as those three variables are present.
}
  \item{method}{
Random effects estimates method.  The metafor package provides several options:
  "DL" - DerSimonian-Laird - default
  "HS" - Hunter-Schmidt
  "SJ" - Sidik-Jonkman
  "ML" - Maximum-likelihood
  "REML" - restricted maximum-likelihood
}
}
\details{
bergMeta() calculates fixed and random effect spooled estimates using the rma() function in the metafor package.  It is written specifically for pooling relative risks.  The user should use this function if they are interested in quick results.  The metafor package contains a wide variety of methods for meta-analysis.  The bergMeta() function simply wraps the most standard methods and formats the results.  Users should refer to the metafor functions if they wish to do more advanced meta-analyses; however, this should work for most purposes.
}
\value{
A data frame with fixed and random effects estimates and an I2 statistic.
}

\author{
Brian Carter
}


\seealso{
\code{\link{rma}}, ~~~
}
\examples{

myRR <- data.frame(name=c("Brian","James","Maret"),
                   year=c("2000","2001","2002"),
                   RR=c(2.0,2.3,2.5),
                   LL=c(1.8,2.0,2.2),
                   UL=c(2.2,2.5,2.7))
foo <- bergMeta(myRR,"DL")
}

