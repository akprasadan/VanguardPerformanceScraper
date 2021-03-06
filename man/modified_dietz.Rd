% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_cleaned_history.R
\name{modified_dietz}
\alias{modified_dietz}
\title{Compute the Modified Dietz return of a portfolio.}
\usage{
modified_dietz(aggregate_data, account_type, start_date, end_date)
}
\arguments{
\item{aggregate_data}{The portfolio level daily balance.}

\item{account_type}{Which account to analyze (account1, account2, etc.)}

\item{start_date}{The start date from which to calculate performance}

\item{end_date}{The date up to which performance is desired.}
}
\value{
The Modified Dietz Return
}
\description{
The Modified Dietz return is a measure of performance for a portfolio.
It takes an approach in between a purely money-weighted and purely
time-weighted rate of return. In the former, one ignores \emph{when} contributions
are made and only analyzes the net gains, while a
time-weighted rate of returns
takes into accounts the timing of investments. The implementation is based on
that used by the package 'spudr.'
}
