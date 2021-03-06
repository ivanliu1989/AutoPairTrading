% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PerformanceEvaluationEmail.R
\name{performanceEvaluationEmail}
\alias{performanceEvaluationEmail}
\title{Generate a pdf report and email users}
\usage{
performanceEvaluationEmail(performanceReport,
  to = c("ivan.liuyanfeng@gmail.com"), path = "PerformanceEvaluation.pdf",
  message = NULL)
}
\arguments{
\item{performanceReport}{an object returned from \code{performanceReport}}

\item{to}{a list of email addresses of report receivers}

\item{path}{report path}

\item{message}{additional message in email title}
}
\description{
Generate a pdf report and email users
}
\seealso{
\link{performanceReport}
}

