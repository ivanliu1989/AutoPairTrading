% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BackTesting.R
\name{BackTesting}
\alias{BackTesting}
\title{Back Testing Strategy}
\usage{
BackTesting(y, x, context, strategy, goLong, goShort, exitPoint)
}
\arguments{
\item{y}{long series}

\item{x}{short series}

\item{context}{trading context}

\item{strategy}{a vector of trading indicator calculated by user's strategy}

\item{goLong}{a vector of thresholds for going long}

\item{goShort}{a vector of thresholds for going short}
}
\value{
A \code{data.frame} with trading activities
}
\description{
Back Testing Strategy
}
\seealso{
\link{getUserTemplate}
}

