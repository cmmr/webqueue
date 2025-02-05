
# webqueue

<!-- badges: start -->
[![cran](https://www.r-pkg.org/badges/version/webqueue)](https://CRAN.R-project.org/package=webqueue)
[![conda](https://anaconda.org/conda-forge/r-webqueue/badges/version.svg)](https://anaconda.org/conda-forge/r-webqueue)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/webqueue)](https://cranlogs.r-pkg.org/)
[![dev](https://github.com/cmmr/webqueue/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmmr/webqueue/actions/workflows/R-CMD-check.yaml)
[![covr](https://codecov.io/gh/cmmr/webqueue/graph/badge.svg)](https://app.codecov.io/gh/cmmr/webqueue)
<!-- badges: end -->

The goal of webqueue is to process HTTP requests on interruptible background processes.


## Installation

``` r
# Install the latest stable version from CRAN:
install.packages("webqueue")

# Or the development version from GitHub:
install.packages("pak")
pak::pak("cmmr/webqueue")
```


## Example

``` r
library(webqueue)

svr <- WebQueue$new(~{ 'Hello world!' })
```

Visit `http://localhost:8080` with a web browser to see the `'Hello world!'` message.

Shutdown the server afterwards.

``` r
svr$stop()
```


## Demo

``` r
svr <- webqueue:::demo()   # triple colon here
#> Site available at <http://127.0.0.1:8080>
```
The demo application's code is available at https://github.com/cmmr/webqueue/blob/main/R/demo.r or by typing `webqueue:::demo`.


## Query Parameters

``` r
svr <- WebQueue$new(~{ jsonlite::toJSON(.$ARGS) })
```

Visit `http://localhost:8080?myvar=123` to see `'{"myvar":[123]}'`.

Accepts both GET and POST parameters.



## Interrupting Requests

The strength of webqueue is its ability to cleanly abort a request at any point.

See vignette('interrupts') for more detailed examples.


### Set a time limit

``` r
slow_hello <- ~{ Sys.sleep(runif(1) * 5); 'Hello' }

#                               vvvvvvv
svr <- WebQueue$new(slow_hello, timeout = 2.5)
```
Reload `http://localhost:8080` a few times.

Half the time it'll finish loading, and half the time it'll produce a timeout error.

Use case: prevent user-submitted jobs from excessively hogging compute resources.


### Merge duplicate requests

``` r
#                               vvvvvvv
svr <- WebQueue$new(slow_hello, copy_id = ~{ .$PATH_INFO })
```
Open several browser tabs for `http://localhost:8080/dup` in quick succession.

They will all finish loading at the exact same time.

Use case: mediate users' spamming of the 'submit' button.


### Stop duplicate requests
``` r
#                               vvvvvvv
svr <- WebQueue$new(slow_hello, stop_id = ~{ .$PATH_INFO })
```
Open several browser tabs for `http://localhost:8080/dup` in quick succession.

Only the first one will finish loading.

Use case: stop a task that the user no longer needs.




## Cautions

### R is single threaded

Do not try to interact with the server from the same R session.
The following code will hang forever:

``` r
## DON'T RUN
svr <- WebQueue$new(~{ 'Hello world!' })
download.file(url = 'http://localhost:8080', tempfile(), 'auto')
```

### Security considerations

As with all web-facing applications, you should take care to 
guard against remote code execution and other types of attacks.



