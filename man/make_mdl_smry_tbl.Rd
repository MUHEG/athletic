% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_mdl_smry_tbl}
\alias{make_mdl_smry_tbl}
\title{Make model summary table}
\usage{
make_mdl_smry_tbl(
  model_mdl,
  add_glance_1L_lgl = TRUE,
  statistic_1L_chr = "Coef (SE)",
  labels_ls = NULL
)
}
\arguments{
\item{model_mdl}{Model (a model)}

\item{add_glance_1L_lgl}{Add glance (a logical vector of length one), Default: TRUE}

\item{statistic_1L_chr}{Statistic (a character vector of length one), Default: 'Coef (SE)'}

\item{labels_ls}{Labels (a list), Default: NULL}
}
\value{
Table (an output object of multiple potential types)
}
\description{
make_mdl_smry_tbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make model summary table. The function returns Table (an output object of multiple potential types).
}
\keyword{internal}
