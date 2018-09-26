\name{weightlist}
\alias{weightlist}
\docType{data}
\title{
Age-specific weights used for rate analyses and age-adjusted frequencies
}
\description{
This dataset contains age-specific weights that are required for running time-dependent rate analyses in the Nutrition Cohort.  Also required for running age-adjusted frequency tables.
}
\usage{weightlist[[x]])}
\format{
  A list with 71 data.frames.
  \describe{
    \item{\code{YEARS70}}{US 1970 population}
    \item{\code{YEARS80}}{US 1980 population}
    \item{\code{YEARS90}}{US 1990 population}
    \item{\code{YEARS00}}{US 2000 population}
    \item{\code{YEARSW70}}{World 1970 population}
    \item{\code{YEARS1N2}}{Unknown}
    \item{\code{YEARSC}}{Unknown}
    \item{\code{YEARS1Q2}}{Unknown}
    \item{\code{YEARS12M}}{Mortality cohort 12 year FU (men)}
    \item{\code{YEARS12F}}{Mortality cohort 12 year FU (women)}
    \item{\code{YEARS12B}}{Mortality cohort 12 year FU}
    \item{\code{YEARS14B}}{Mortality cohort 14 year FU}
    \item{\code{YEARS14F}}{Mortality cohort 14 year FU (women)}
    \item{\code{YEARS14M}}{Mortality cohort 14 year FU (men)}
    \item{\code{YEARS16B}}{Mortality cohort 16 year FU}
    \item{\code{YEARS16F}}{Mortality cohort 16 year FU (women)}
    \item{\code{YEARS16M}}{Mortality cohort 16 year FU (men)}
    \item{\code{YEARS18B}}{Mortality cohort 18 year FU}
    \item{\code{YEARS18F}}{Mortality cohort 18 year FU (women)}
    \item{\code{YEARS18M}}{Mortality cohort 18 year FU (men)}
    \item{\code{YEARS20B}}{Mortality cohort 20 year FU}
    \item{\code{YEARS20F}}{Mortality cohort 20 year FU (women)}
    \item{\code{YEARS20M}}{Mortality cohort 20 year FU (men)}
    \item{\code{YEARS22B}}{Mortality cohort 22 year FU}
    \item{\code{YEARS22F}}{Mortality cohort 22 year FU (women)}
    \item{\code{YEARS22M}}{Mortality cohort 22 year FU (men)}
    \item{\code{YEARS24B}}{Mortality cohort 24 year FU}
    \item{\code{YEARS24F}}{Mortality cohort 24 year FU (women)}
    \item{\code{YEARS24M}}{Mortality cohort 24 year FU (men)}
    \item{\code{YEARS26B}}{Mortality cohort 26 year FU}
    \item{\code{YEARS26F}}{Mortality cohort 26 year FU (women)}
    \item{\code{YEARS26M}}{Mortality cohort 26 year FU (men)}
    \item{\code{YEARS28B}}{Mortality cohort 28 year FU}
    \item{\code{YEARS28F}}{Mortality cohort 28 year FU (women)}
    \item{\code{YEARS28M}}{Mortality cohort 28 year FU (men)}
    \item{\code{YEARS30B}}{Mortality cohort 30 year FU}
    \item{\code{YEARS30F}}{Mortality cohort 30 year FU (women)}
    \item{\code{YEARS30M}}{Mortality cohort 30 year FU (men)}
    \item{\code{YEARSNIB}}{Nutrition cohort 22 year FU}
    \item{\code{YEARSNIF}}{Nutrition cohort 22 year FU (women)}
    \item{\code{YEARSNIM}}{Nutrition cohort 22 year FU (men)}
    \item{\code{YEARSNHB}}{Nutrition cohort 20 year FU}
    \item{\code{YEARSNHF}}{Nutrition cohort 20 year FU (women)}
    \item{\code{YEARSNHM}}{Nutrition cohort 20 year FU (men)}
    \item{\code{YEARSNGB}}{Nutrition cohort 18 year FU}
    \item{\code{YEARSNGF}}{Nutrition cohort 18 year FU (women)}
    \item{\code{YEARSNGM}}{Nutrition cohort 18 year FU (men)}
    \item{\code{YEARSNFB}}{Nutrition cohort 16 year FU}
    \item{\code{YEARSNFF}}{Nutrition cohort 16 year FU (women)}
    \item{\code{YEARSNFM}}{Nutrition cohort 16 year FU (men)}
    \item{\code{YEARSNEB}}{Nutrition cohort 14 year FU}
    \item{\code{YEARSNEF}}{Nutrition cohort 14 year FU (women)}
    \item{\code{YEARSNEM}}{Nutrition cohort 14 year FU (men)}
    \item{\code{YEARSNDB}}{Nutrition cohort 12 year FU}
    \item{\code{YEARSNDF}}{Nutrition cohort 12 year FU (women)}
    \item{\code{YEARSNDM}}{Nutrition cohort 12 year FU (men)}
    \item{\code{YEARSNCB}}{Nutrition cohort 10 year FU}
    \item{\code{YEARSNCF}}{Nutrition cohort 10 year FU (women)}
    \item{\code{YEARSNCM}}{Nutrition cohort 10 year FU (men)}
    \item{\code{YEARSNBB}}{Nutrition cohort 8 year FU}
    \item{\code{YEARSNBF}}{Nutrition cohort 8 year FU (women)}
    \item{\code{YEARSNBM}}{Nutrition cohort 8 year FU (men)}
    \item{\code{YEARSNBB}}{Nutrition cohort 6 year FU}
    \item{\code{YEARSNBF}}{Nutrition cohort 6 year FU (women)}
    \item{\code{YEARSNBM}}{Nutrition cohort 6 year FU (men)}
    \item{\code{YEARS30MLIM}}{Mortality (age limited) cohort 30 year FU (men)}
    \item{\code{YEARS30FLIM}}{Mortality (age limited) cohort 30 year FU (men)}
    \item{\code{YEARS30BLIM}}{Mortality (age limited) cohort 30 year FU (men)}
    \item{\code{YEARS32MLIM}}{Mortality (age limited) cohort 32 year FU (men)}
    \item{\code{YEARS32FLIM}}{Mortality (age limited) cohort 32 year FU (men)}
    \item{\code{YEARS32BLIM}}{Mortality (age limited) cohort 32 year FU (men)}

  }
}
\details{
Each dataset in weightlist corresponds to a person year or age distribution.  In rate analyses, the user must choose one of these age distributions to calculate the adjustment.  See documentation for rate and age-adjustments for details how to use these datasets in each function.
}

\examples{
weightlist[["YEARS00"]]
}
