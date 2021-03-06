% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coc_economic_indicators.R
\name{build_coc_public_program_use}
\alias{build_coc_public_program_use}
\title{CoC public program utilization rates}
\usage{
build_coc_public_program_use(acs_data, tract_crosswalk)
}
\arguments{
\item{acs_data}{A data frame with tract-level enrollment counts from
\code{\link[=fetch_public_program_use]{fetch_public_program_use()}}.}

\item{tract_crosswalk}{A data frame with a tract to CoC crosswalk from
\code{\link[=build_tract_crosswalk]{build_tract_crosswalk()}}.}
}
\value{
A data frame:
\itemize{
\item \code{coc_number}: CoC number
\item \code{year}: Year
\item \code{share_hh_with_snap}: Share of households receiving SNAP benefits
\item \code{share_hh_with_pub_assist}: Share of households receiving public assistance
benefits
\item \code{share_hh_with_ssi}: Share of households receiving SSI benefits
\item \code{share_with_medicaid}: Share of individuals age 19-64 enrolled in
Medicaid
}
}
\description{
Builds CoC public program utilization rates by summing total enrollment in
the Census Tracts within each CoC and dividing by the relevant total
population in the CoC.
}
