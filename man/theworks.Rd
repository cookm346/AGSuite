% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TheWorks.R
\name{theworks}
\alias{theworks}
\title{The Works}
\usage{
theworks(training, test)
}
\arguments{
\item{training}{a vector of training strings}

\item{test}{a vector of test strings}
}
\value{
a matrix containing string length, legal entry, four measures of novelty, four measures of ACS (Associative Chunk Strength), and four measures of Levenshtein (edit) distance
}
\description{
Computes all item-level statistics
}
