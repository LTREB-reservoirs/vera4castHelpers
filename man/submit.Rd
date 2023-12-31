% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/submit.R
\name{submit}
\alias{submit}
\title{Submit forecast to forecasting challenge}
\usage{
submit(
  forecast_file,
  ask = NULL,
  s3_region = "submit",
  s3_endpoint = "ltreb-reservoirs.org",
  first_submission = TRUE
)
}
\arguments{
\item{forecast_file}{forecast csv or csv.gz file}

\item{ask}{should we prompt for a go before submission?}

\item{s3_region}{subdomain of submission bucket}

\item{s3_endpoint}{root domain of submission bucket}

\item{first_submission}{flag if first submission.  set to FALSE if submitting multiple forecasts}
}
\description{
Submit forecast to forecasting challenge
}
