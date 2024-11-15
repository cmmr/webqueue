
# Using `hook` to alter the handler
# hook <- function (job, queue) {
#   job$vars$my_str <- function (x) capture.output(ls.str(x[order(names(x))]))
#   job$expr <- switch(
#     EXPR = job$vars$req$PATH_INFO,
#     '/hello'   = quote('Hello World'),
#     '/date'    = quote(date()),
#     '/sleep'   = quote({x <- date(); Sys.sleep(5); c(x, date())}),
#     '/req'     = quote(my_str(req)),
#     '/headers' = quote(my_str(as.list(req$HEADERS))),
#     '/query'   = quote(my_str(webutils::parse_query(req$QUERY_STRING))),
#     job$expr )
# }


Server <- R6Class(
  classname = "Server",
  cloneable = FALSE,
  
  public = list(
    
    initialize = function (
        handler,
        host      = '0.0.0.0',
        port      = 8080L,
        parse     = NULL,
        globals   = list(),
        packages  = NULL,
        init      = NULL,
        max_cpus  = detectCores(),
        workers   = ceiling(max_cpus * 1.2),
        timeout   = NULL,
        hooks     = NULL,
        reformat  = NULL,
        stop_id   = NULL,
        copy_id   = NULL,
        cross_cmp = NULL,
        quiet             = FALSE,
        onHeaders         = NULL,
        staticPaths       = NULL,
        staticPathOptions = NULL ) {
      
      # Convert lambda syntax to functions.
      if (is_formula(handler))   handler   <- as_function(handler)
      if (is_formula(parse))     parse     <- as_function(parse)
      if (is_formula(onHeaders)) onHeaders <- as_function(onHeaders)
      
      # Sanity check `handler` and `globals`.
      if (!is.function(handler)) cli_abort('`handler` must be a function, not {.type {handler}}.')
      if (!is.list(globals))     cli_abort('`globals` must be a list,     not {.type {globals}}.')
      
      # Custom parsing prior to queue submission.
      private$parse <- parse
      if (!(is.function(parse) || is.null(parse)))
        cli_abort('`parse` must be a function or NULL, not {.type {parse}}.')
      
      # Create static paths as needed.
      for (i in seq_along(staticPaths))
        if (is.character(fp <- staticPaths[[i]]))
          if (!file.exists(fp) && !dir.exists(fp))
            dir.create(fp, recursive = TRUE)
      
      # Start a Queue.
      private$.queue <- Queue$new(
        globals   = list(handler = handler, globals = globals),
        packages  = packages,
        init      = init,
        max_cpus  = max_cpus,
        workers   = workers,
        timeout   = timeout,
        hooks     = hooks,
        reformat  = reformat,
        signal    = TRUE,
        stop_id   = stop_id,
        copy_id   = copy_id,
        cross_cmp = cross_cmp )
      
      # Start a 'httpuv' http server.
      private$.httpuv <- startServer(
        host  = host,
        port  = port,
        quiet = quiet,
        app   = list(
          call              = private$app_call,
          onHeaders         = onHeaders,
          staticPaths       = staticPaths,
          staticPathOptions = staticPathOptions ))
      
      return (self)
    },
    
    
    print = function (...) {
      host <- self$host
      if (host == '0.0.0.0') host <- 'localhost'
      url <- paste0('http://', host, ':', self$port)
      cli_text('{.cls {class(self)}} on {.url {url}}')
    },
    
    
    stop = function (reason = 'server stopped') {
      private$finalize(reason)
      return (invisible(self))
    }
  ),
  
  private = list(
    
    .queue   = NULL,
    .httpuv  = NULL,
    handler  = NULL,
    parse    = NULL,
    
    app_call = function (req) {
      
      cnd <- catch_cnd({
        req$ARGS    <- parse_args(req)
        req$COOKIES <- parse_cookies(req)
        if (is.function(private$parse))
          req <- private$parse(req)
      })
      if (!is.null(cnd)) return (format_500(cnd))
      
      job <- private$.queue$run(
        expr = quote(do.call(handler, list(req, globals))),
        vars = list(req = req) )
      
      then(
        promise     = as.promise(job),
        onFulfilled = format_200,
        onRejected  = format_500 )
    },
    
    
    finalize = function (reason = 'server stopped') {
      private$.queue$stop(reason)
      private$.httpuv$stop()
      return (invisible(NULL))
    }
  ),
  
  active = list(
    queue   = function () private$.queue,
    httpuv  = function () private$.httpuv,
    workers = function () private$.queue$workers,
    jobs    = function () private$.queue$jobs,
    host    = function () private$.httpuv$getHost(),
    port    = function () private$.httpuv$getPort()
  )
)



parse_args <- function (req) {
  
  args <- list()
  
  if (hasName(req, 'REQUEST_METHOD') && isTRUE(nzchar(req[['REQUEST_METHOD']]))) {
    
    method <- toupper(req[['REQUEST_METHOD']])
    
    if (identical(method, 'GET')) {
      if (hasName(req, 'QUERY_STRING') && isTRUE(nzchar(req[['QUERY_STRING']])))
        args <- parse_query(req[['QUERY_STRING']])
      
    } else if (identical(method, 'POST')) {
      if (hasName(req, 'CONTENT_TYPE') && isTRUE(nzchar(req[['CONTENT_TYPE']])))
        if (is.function(req[['rook.input']]$read))
          args <- parse_http(req[['rook.input']]$read(), req[['CONTENT_TYPE']])
    }
  }
  
  return (args)
}


parse_cookies <- function (req) {
  
  cookies <- list()
  
  if (hasName(req, 'HTTP_COOKIE') && isTRUE(nzchar(req[['HTTP_COOKIE']]))) {
    for (cookie in strsplit(req[['HTTP_COOKIE']], ";", fixed = TRUE)[[1]]) {
      cookie <- trimws(strsplit(cookie, "=", fixed = TRUE)[[1]])
      if (length(cookie) == 1) cookie <- c('', cookie)
      if (length(cookie) >= 3) cookie <- c(cookie[[1]], paste(collapse = '=', cookie[-1]))
      cookies[[cookie[[1]]]] <- cookie[[2]]
    }
  }
  
  return (cookies)
}


format_200 <- function (resp) {
  if (length(resp) == 0)          return (list(status = 200L))
  if (is.list(resp))              return (resp)
  if (is_scalar_integerish(resp)) return (list(status = as.integer(resp)))
  list(status = 200L, body = paste(collapse='\n', as.character(resp)))
}


format_500 <- function (resp) {
  if (is_scalar_integerish(resp)) return (list(status = as.integer(resp)))
  list(status = 500L, body = ansi_strip(paste(collapse='\n', as.character(resp))))
}


