
# webqueue

<!-- badges: start -->
[![cran](https://www.r-pkg.org/badges/version/webqueue)](https://CRAN.R-project.org/package=webqueue)
[![conda](https://anaconda.org/conda-forge/r-webqueue/badges/version.svg)](https://anaconda.org/conda-forge/r-webqueue)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/webqueue)](https://cranlogs.r-pkg.org/)
[![dev](https://github.com/cmmr/webqueue/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmmr/webqueue/actions/workflows/R-CMD-check.yaml)
[![covr](https://codecov.io/gh/cmmr/webqueue/graph/badge.svg)](https://app.codecov.io/gh/cmmr/webqueue)
<!-- badges: end -->


The goal of webqueue is to process HTTP requests on interruptible background processes.

Use cases:

* Prevent user-submitted jobs from excessively hogging compute resources.

* Stop tasks that are no longer needed.




## Installation

```r
# Install the latest stable version from CRAN:
install.packages("webqueue")

# Or the development version from GitHub:
install.packages("pak")
pak::pak("cmmr/webqueue")
```


## Example

```r
library(webqueue)

wq <- webqueue(~{ 'Hello world!\n' })

readLines('http://localhost:8080')
#> [1] "Hello world!"

wq$stop()
```


## Query Parameters

```r
wq <- webqueue(~{ jsonlite::toJSON(.$ARGS) })

cat(fetch('http://localhost:8080?myvar=123'))
#> {"myvar":["123"]}

wq$stop()
```

Accepts both GET and POST parameters.


## Simple API

```r
wq <- webqueue(
  handler = function (req) {
    switch(
      EXPR = req$PATH_INFO,
      '/date' = date(),
      '/req'  = ls.str(as.list(req)),
      '/args' = req$ARGS,
      '/cpus' = as.numeric(parallelly::availableCores()),
      404L )
})

fetch('http://localhost:8080/cpus')
#> [1] "6"

cat(fetch('http://localhost:8080/req?x=123&id=ABC'))
#> ARGS : List of 2
#>  $ x : chr "123"
#>  $ id: chr "ABC"
#> COOKIES :  Named list()
#> HEADERS : List of 1
#>  $ host: chr "localhost:8080"
#> PATH_INFO :  chr "/req"
#> REMOTE_ADDR :  chr "127.0.0.1"
#> REQUEST_METHOD :  chr "GET"
#> SERVER_NAME :  chr "127.0.0.1"
#> SERVER_PORT :  chr "8080"

wq$stop()
```



## Interrupting Requests

The strength of `webqueue` is its ability to cleanly abort a request at any 
point.

See `vignette('interrupts')` for more detailed examples.


### Set a time limit

```r
wq <- webqueue(
  handler = ~{ Sys.sleep(.$ARGS$sleep); 'Hello world!' }, 
  timeout = 1 )

fetch('http://localhost:8080?sleep=2')
#> [1] "timeout: total runtime exceeded 1 second\n"

fetch('http://localhost:8080?sleep=0')
#> [1] "Hello world!"

wq$stop()
```



### Merge duplicate requests

```r
# `copy_id` will be "/a" or "/b"
wq <- webqueue(
  handler = function (req) { Sys.sleep(1); req$ARGS$x }, 
  copy_id = function (job) job$req$PATH_INFO )

# Fetch three URLs at the same time. "/b" path is merged.
dput(fetch(
  'http://localhost:8080/a?x=first',
  'http://localhost:8080/b?x=second',
  'http://localhost:8080/b?x=third' ))
#> c("first", "second", "second")

wq$stop()
```


### Stop duplicate requests
```r
# `stop_id` will be "/a" or "/b"
wq <- webqueue(
  handler = function (req) { Sys.sleep(1); req$ARGS$x }, 
  stop_id = function (job) job$req$PATH_INFO )

# Fetch three URLs at the same time. "/b" path is stopped.
dput(fetch(
  'http://localhost:8080/a?x=first',
  'http://localhost:8080/b?x=second',
  'http://localhost:8080/b?x=third' ))
#> c("first", "superseded: duplicated stop_id\n", "third")

wq$stop()
```

