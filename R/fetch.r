
#' Download content from URLs.
#' 
#' URLs are retrieved asynchronously using curl.
#' 
#' @param urls      One or more URLs as a character vector.
#' @param ...       Additional URLs to add to `urls` with `c()`.
#' @param timeout   Max time in seconds to wait for results.
#' 
#' @return A character vector of the responses. Ordering and names are 
#'         preserved from `urls`.  
#' 
#' @export
#' @examplesIf ! webqueue:::is_cran_check()
#' 
#'     library(webqueue)
#'     
#'     fetch('http://numbersapi.com/random')
#'     
#'     x <- fetch(c(
#'       uuid = 'https://www.uuidtools.com/api/generate/v1',
#'       zip  = 'https://ziptasticapi.com/77025' ))
#'     
#'     noquote(x)

fetch <- function (urls, ..., timeout = 5) {
  
  urls <- c(urls, ...)
  
  stopifnot(is.character(urls))
  stopifnot(length(urls) > 0)
  stopifnot(!any(is.na(urls)))
  stopifnot(all(nzchar(urls)))
  
  lapply(seq_along(urls), function (i) {
    done <- \(x) urls[[i]] <<- rawToChar(x$content)
    fail <- \(x) urls[[i]] <<- NA
    curl::curl_fetch_multi(urls[[i]], done, fail)
  })
  
  curl::multi_run(timeout = timeout)
  
  return (urls)
}
