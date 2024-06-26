% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sccoda.R
\name{sccoda_generate_data}
\alias{sccoda_generate_data}
\title{Synthetic data generation in scCODA}
\usage{
sccoda_generate_data(type = "case_control", ..., conda = NULL)
}
\arguments{
\item{type}{A string from following items.
\itemize{
\item \code{case_control}: Generates compositional data with binary covariates.
\item \code{bw}: Calculates intercepts and slopes from a starting count and an
absolute change for the first cell type.
\item \code{effect_matrix}: Generates a sparse effect matrix
\item \code{counts}: Calculates a count vector from a given first entry, length and
sum.
}}

\item{...}{Additional arguments to generate data.}

\item{conda}{A character vector of preferred python environment names to
search for and use. Details see \link{condaenv}.}
}
\description{
Synthetic data generation in scCODA
}
\seealso{
\url{https://sccoda.readthedocs.io/en/latest/api.html}
}
