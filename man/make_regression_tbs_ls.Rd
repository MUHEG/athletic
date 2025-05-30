% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_regression_tbs_ls}
\alias{make_regression_tbs_ls}
\title{Make regression tibbles list}
\usage{
make_regression_tbs_ls(
  models_ls_ls,
  outcomes_chr,
  tab_spanner_chr,
  dataset_1L_chr,
  labels_ls = NULL
)
}
\arguments{
\item{models_ls_ls}{Models (a list of lists)}

\item{outcomes_chr}{Outcomes (a character vector)}

\item{tab_spanner_chr}{Tab spanner (a character vector)}

\item{dataset_1L_chr}{Dataset (a character vector of length one)}

\item{labels_ls}{Labels (a list), Default: NULL}
}
\value{
Regression tables (a list)
}
\description{
make_regression_tbs_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make regression tibbles list. The function returns Regression tables (a list).
}
\keyword{internal}
