% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch.r
\name{fetch}
\alias{fetch}
\title{Download content from URLs.}
\usage{
fetch(urls, ..., timeout = 5)
}
\arguments{
\item{urls}{One or more URLs as a character vector.}

\item{...}{Additional URLs to add to \code{urls} with \code{c()}.}

\item{timeout}{Max time in seconds to wait for results.}
}
\value{
A character vector of the responses. Ordering and names are
preserved from \code{urls}.
}
\description{
URLs are retrieved asynchronously using curl.
}
\examples{
\dontshow{if (! webqueue:::is_cran_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

    library(webqueue)
    
    fetch('http://numbersapi.com/random')
    
    x <- fetch(c(
      uuid = 'https://www.uuidtools.com/api/generate/v1',
      zip  = 'https://ziptasticapi.com/77025' ))
    
    noquote(x)
\dontshow{\}) # examplesIf}
}
