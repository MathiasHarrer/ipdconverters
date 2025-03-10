% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/converters.R
\name{hadsd}
\alias{hadsd}
\alias{hadsd.rev}
\title{Convert HADS-D scores to a depression common metric}
\usage{
hadsd(x)
hadsd.rev(x)
}
\arguments{
\item{x}{A scalar or vector of scores that should be converted.}
}
\description{
This function allows you to convert scores of the Hospital Anxiety and Depression Scale (HADS)
depression subscale
to a depression "common metric" score. Common metric scores
can be "reconverted" to the original scale by appending \code{.rev} to the respective
converter function.
}
\details{
Conversions are conducted using the crosswalk tables provided in
Wahl et al. (\href{https://doi.org/10.1016/j.jclinepi.2013.04.019}{2014}; supplement).
Common metric scores \emph{θ} are normed so that values of 50 indicate the general
population mean, with a population standard deviation of 10.
}
\examples{
\dontrun{
x <- runif(100,0,30)
hadsd(x)
}
}
