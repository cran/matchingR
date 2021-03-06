% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{cpp_wrapper_galeshapley_check_stability}
\alias{cpp_wrapper_galeshapley_check_stability}
\title{C++ Wrapper to Check Stability of Two-sided Matching}
\usage{
cpp_wrapper_galeshapley_check_stability(
  proposerUtils,
  reviewerUtils,
  proposals,
  engagements
)
}
\arguments{
\item{proposerUtils}{is a matrix with cardinal utilities of the proposing
side of the market. If there are \code{n} proposers and \code{m} reviewers,
then this matrix will be of dimension \code{m} by \code{n}. The
\code{i,j}th element refers to the payoff that individual \code{j} receives
from being matched to individual \code{i}.}

\item{reviewerUtils}{is a matrix with cardinal utilities of the courted side
of the market. If there are \code{n} proposers and \code{m} reviewers, then
this matrix will be of dimension \code{n} by \code{m}. The \code{i,j}th
element refers to the payoff that individual \code{j} receives from being
matched to individual \code{i}.}

\item{proposals}{is a matrix that contains the number of the reviewer that a
given proposer is matched to: the first row contains the number of the
reviewer that is matched with the first proposer (using C++ indexing), the
second row contains the id of the reviewer that is matched with the second
proposer, etc. The column dimension accommodates proposers with multiple
slots.}

\item{engagements}{is a matrix that contains the number of the proposer that
a given reviewer is matched to (using C++ indexing). The column dimension
accommodates reviewers with multiple slots.}
}
\value{
true if the matching is stable, false otherwise
}
\description{
This function checks if a given matching is stable for a particular set of
preferences. This function provides an R wrapper for the C++ backend. Users
should not call this function directly and instead use
\code{\link{galeShapley.checkStability}}.
}
