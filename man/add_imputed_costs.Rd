% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_imputed_costs}
\alias{add_imputed_costs}
\title{Add imputed costs}
\usage{
add_imputed_costs(
  data_tb,
  arrange_by_1L_chr = character(0),
  cost_var_1L_chr = "Cost",
  provider_id_1L_chr = "ProviderID"
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{arrange_by_1L_chr}{Arrange by (a character vector of length one), Default: character(0)}

\item{cost_var_1L_chr}{Cost variable (a character vector of length one), Default: 'Cost'}

\item{provider_id_1L_chr}{Provider identity (a character vector of length one), Default: 'ProviderID'}
}
\value{
Data (a tibble)
}
\description{
add_imputed_costs() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add imputed costs. The function returns Data (a tibble).
}
