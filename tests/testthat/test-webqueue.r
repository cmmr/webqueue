test_that("webqueue", {
  
  # library(testthat); library(webqueue)
  
  withr::local_envvar(CURL_SSL_BACKEND = NA)
  
  globals <- list(
    
    fetch = function (url, cookies = NULL, post = NULL, query = NULL) {
      
      req <- httr2::request(url)
      req <- httr2::req_timeout(req, 5L)
      
      if (!is.null(cookies)) req <- httr2::req_headers(req, "Cookie" = cookies)
      if (!is.null(post))    req <- httr2::req_body_json(req, post)
      if (!is.null(query))   req <- httr2::req_url_query(req, !!!query)
      
      resp <- httr2::req_perform(req)
      
      return (httr2::resp_body_string(resp))
    }
  )
  
  
  expect_error(WebQueue$new(handler = NULL, onHeaders = ~{ NULL }))
  expect_error(WebQueue$new(handler = ~{ NULL }, globals = NULL))
  expect_error(WebQueue$new(handler = ~{ NULL }, parse = NA))
  
  tmp         <- tempfile()
  handler     <- ~{ .$COOKIES$txt %||% .$ARGS$txt %||% 'Hello World!' }
  staticPaths <- c('/tmp' = tmp)
  parse       <- ~{ if (is.null(.$ARGS$err)) . else stop() }
  
  svr <- WebQueue$new(handler, globals = globals, parse = parse, staticPaths = staticPaths)
  
  svr$jobqueue$wait()
  
  expect_s3_class(svr,              class = c('WebQueue', 'R6'))
  expect_s3_class(svr$jobqueue,     class = c('Queue', 'R6'))
  expect_s3_class(svr$httpuv,       class = c('WebServer', 'Server', 'R6'))
  expect_s3_class(svr$workers[[1]], class = c('Worker', 'R6'))
  expect_length(svr$jobs, n = 0)
  expect_identical(svr$host, '0.0.0.0')
  expect_identical(svr$port, 8080L)
  expect_no_error(suppressMessages(svr$print()))
  
  
  # HTTP client and server must be on separate R processes.
  worker <- jobqueue::Worker$new(globals = globals)$wait()
  
  job <- jobqueue::Job$new({ fetch('http://127.0.0.1:8080') })
  worker$run(job)
  expect_identical(job$result, 'Hello World!')
  
  job <- jobqueue::Job$new({ fetch('http://127.0.0.1:8080', 'xyz; b=5; txt=1=6') })
  worker$run(job)
  expect_identical(job$result, '1=6')
  
  job <- jobqueue::Job$new({ fetch('http://127.0.0.1:8080', query = list(txt='ABC')) })
  worker$run(job)
  expect_identical(job$result, 'ABC')
  
  job <- jobqueue::Job$new({ fetch('http://127.0.0.1:8080', post = list(txt='XYZ')) })
  worker$run(job)
  expect_identical(job$result, 'XYZ')
  
  cat(file = file.path(tmp, 'static.txt'), 'Static Content')
  job <- jobqueue::Job$new({ fetch('http://127.0.0.1:8080/tmp/static.txt') })
  worker$run(job)
  expect_identical(job$result, 'Static Content')
  
  job <- jobqueue::Job$new({ fetch('http://127.0.0.1:8080', post = list(err='1')) })
  worker$run(job)
  expect_s3_class(job$result, 'error')
  
  
  expect_no_error(svr$stop())
  expect_no_error(worker$stop())
  
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  
  expect_identical(format_200(I(200L)), I(200L))
  expect_identical(format_500(I(500L)), I(500L))
  
  expect_identical(format_500(500L)$status, 500L)
  expect_identical(format_500(errorCondition('x'))$status, 500L)
  expect_identical(format_500(errorCondition('x', class='timeout'))$status,    408L)
  expect_identical(format_500(errorCondition('x', class='superseded'))$status, 409L)
  expect_identical(format_500(errorCondition('x', class='interrupt'))$status,  499L)
  
})
