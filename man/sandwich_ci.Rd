% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sandwich_ci.R
\name{sandwich_ci}
\alias{sandwich_ci}
\title{Takes a model fit and returns a data frame with variable, estimate, CI, and N}
\usage{
sandwich_ci(fit, antilog = FALSE, data, vnames = "names",
  outcome = NULL, exposure = NULL)
}
\arguments{
\item{fit}{model object}

\item{antilog}{boolean, set to `FALSE` for log scale or `TRUE` for antilog scale}

\item{data}{dataframe or datatable}

\item{vnames}{character, set to `"labels"` to use variable labels rather than variable names in the output}

\item{outcome}{character, outcome variable name}

\item{exposure}{character, exposure variable name}
}
\description{
Takes a model fit and returns a data frame with variable, estimate, CI, and N
}
\details{
Returns a dataframe with variable, estimate, and CI information from model fit
}
\examples{
# Not run:
# sandwich_ci(mod_obj, antilog = T, data = df, vnames = "labels")
# sandwich_ci(mod_obj, antilog = T, data = df, vnames = "labels", outcome = "flg_cmp_postop_ssi", exposure = "initial_approach")
}
\keyword{AHSQC,}
\keyword{sandwich}
