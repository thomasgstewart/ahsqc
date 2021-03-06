% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_entry.R
\name{cat_entry}
\alias{cat_entry}
\title{Creates a categorical entry for a table}
\usage{
cat_entry(out, x, y, dt, xlab = NULL, pvalue = TRUE,
  fmt = "norm_fmt", pvalue_fmt = function(x, test_method) {    
  formatp(x, digits = 3) \%|\% "<sup>" \%|\% test_method \%|\% "</sup>" })
}
\arguments{
\item{out}{empty vector}

\item{x}{variable for row in table}

\item{y}{grouping variable}

\item{dt}{data table}

\item{xlab}{Label for entry in table}

\item{pvalue}{Boolean}

\item{fmt}{Two values are "norm_fmt" or "count_fmt"}

\item{pvalue_fmt}{A function that takes a pvalue and a test method}
}
\description{
Creates a categorical entry for a table
}
\details{

}
\examples{
# Not run:
}
\keyword{AHSQC}
