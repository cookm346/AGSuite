% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Novelty.R
\name{novelty}
\alias{novelty}
\title{Novelty}
\usage{
novelty(train, test, n_gram = 2, proportion = FALSE)
}
\arguments{
\item{train}{a vector of training strings}

\item{test}{a vector of test strings}

\item{n_gram}{the chunk sizes to analyze}

\item{proportion}{whether the output represents raw values or is proportional to length of the string}
}
\value{
a vector of the novelty of each test string
}
\description{
Computes test string novelty
}
