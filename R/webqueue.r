
#' Sends Web Traffic to Job Queue
#'
#' @name WebQueue
#'
#' @description
#' 
#' Connects the 'httpuv' and 'jobqueue' R packages.
#' 
#' 
#' @param handler  A `function (request, globals)` that will be run on a 
#'        background worker process. The returned value will be passed through 
#'        `reformat`, then sent as the server's response to the web client.
#' 
#' @param host   A string that is a valid IPv4 address that is owned by this 
#'        server, or `'0.0.0.0'` to listen on all IP addresses.
#' 
#' @param port   A number or integer that indicates the server port that should 
#'        be listened on. Note that on most Unix-like systems including Linux 
#'        and macOS, port numbers smaller than 1024 require root privileges.
#' 
#' @param parse  A `function (request)` that is run on the foreground process
#'        to transform the HTTP request prior to passing it to `handler`. Must
#'        return a `list()`-like object or signal an error. `request` is the 
#'        environment object provided by 'httpuv', amended with `$ARGS` and 
#'        `$COOKIES`.
#' 
#' @param globals  A list that is stored on the workers and provided as the 
#'        second argument to `handler`.
#'        
#' @param packages  Character vector of package names to load on workers.
#' 
#' @param init  A call or R expression wrapped in curly braces to evaluate on 
#'        each worker just once, immediately after start-up. Will have access 
#'        to variables defined by `globals` and assets from `packages`. 
#'        Returned value is ignored.
#' 
#' @param max_cpus  Total number of CPU cores that can be reserved by all 
#'        running Jobs (`sum(cpus)`). Does not enforce limits on actual CPU 
#'        utilization.
#' 
#' @param workers  How many background [Worker] processes to start. Set to more 
#'        than `max_cpus` to enable interrupted workers to be quickly swapped 
#'        out with standby Workers while a replacement Worker boots up.
#' 
#' @param timeout  A named numeric vector indicating the maximum number of 
#'        seconds allowed for each state the job passes through, or 'total' to
#'        apply a single timeout from 'submitted' to 'done'. Example:
#'        `timeout = c(total = 2.5, running = 1)`.
#' 
#' @param hooks  A list of functions to run when the Job state changes, of the 
#'        form `hooks = list(created = function (job) {...}, done = ~{...})`.
#'        See `vignette('hooks')`.
#' 
#' @param reformat  A `function (job)` that is run in the foreground process to 
#'        transform the output from `handler`. The default, `reformat = NULL`, 
#'        is essentially `function (job) { job$output }`.
#' 
#' @param stop_id  A `function (job)`. If two Jobs generate the same value from
#'        this function, then the earlier Job will be aborted. If the returned 
#'        value is `NULL`, no Jobs will be stopped.
#'                 
#' @param copy_id  A `function (job)`. If two Jobs generate the same value from
#'        this function, then the later Job will clone its output from the 
#'        earlier Job. If the returned value is `NULL`, no Jobs will be cloned.
#' 
#' @param quiet   If `TRUE`, suppress error messages from starting the 'httpuv' 
#'        server.
#' 
#' @param onHeaders   A `function (request)` triggered when headers are 
#'        received by 'httpuv'. Return NULL to continue normal processing of 
#'        the request, or a Rook response to send that response, stop 
#'        processing the request, and ask the client to close the connection. 
#'        (This can be used to implement upload size limits, for example.)
#' 
#' @param staticPaths   A named list of paths that will be served without 
#'        invoking `handler()` or `onHeaders()`. The name of each one is the 
#'        URL path, and the value is either a string referring to a local path, 
#'        or an object created by the `httpuv::staticPath()` function.
#' 
#' @param staticPathOptions   A set of default options to use when serving 
#'        static paths. If not set or NULL, then it will use the result from 
#'        calling `httpuv::staticPathOptions()` with no arguments.
#' 
#'
#' @export
#' 

WebQueue <- R6Class(
  classname = "WebQueue",
  cloneable = FALSE,
  
  public = list(
    
    #' @description
    #' Creates an httpuv::WebServer with requests handled by a jobqueue::Queue.
    #'
    #' @return A `WebQueue` object.
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
      private$.jobqueue <- Queue$new(
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
        copy_id   = copy_id )
      
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
    
    
    #' @description
    #' Print method for a WebQueue.
    #' @param ... Arguments are not used currently.
    print = function (...) {
      host <- self$host
      if (host == '0.0.0.0') host <- 'localhost'
      url <- paste0('http://', host, ':', self$port)
      cli_text('{.cls {class(self)}} on {.url {url}}')
    },
    
    
    #' @description
    #' Shuts down the WebQueue and all associated subprocesses. Stopped Jobs
    #' will have their `$output` set to a object of class `<interrupt/condition>`
    #' 
    #' @param reason   A brief message for the condition object.
    #' 
    #' @return This WebQueue, invisibly.
    stop = function (reason = 'server stopped') {
      private$finalize(reason)
      return (invisible(self))
    }
  ),
  
  private = list(
    
    .jobqueue = NULL,
    .httpuv   = NULL,
    handler   = NULL,
    parse     = NULL,
    
    app_call = function (req) {
      
      cnd <- catch_cnd({
        req$ARGS    <- parse_args(req)
        req$COOKIES <- parse_cookies(req)
        req$HEADERS <- as.list(req[['HEADERS']])
        if (is.function(private$parse))
          req <- private$parse(req)
        req <- as.list(req)
      })
      if (!is.null(cnd)) return (format_500(cnd))
      
      job <- private$.jobqueue$run(
        expr = quote(do.call(handler, list(request, globals))),
        vars = list(request = req) )
      
      then(
        promise     = as.promise(job),
        onFulfilled = format_200,
        onRejected  = format_500 )
    },
    
    
    finalize = function (reason = 'server stopped') {
      private$.jobqueue$stop(reason)
      private$.httpuv$stop()
      invisible()
    }
  ),
  
  active = list(
    
    #' @field jobqueue
    #' The `jobqueue::Queue`.
    jobqueue = function () private$.jobqueue,
    
    #' @field httpuv
    #' The `httpuv::WebServer`.
    httpuv = function () private$.httpuv,
    
    #' @field workers
    #' List of `jobqueue::Worker`s used by `$jobqueue`.
    workers = function () private$.jobqueue$workers,
    
    #' @field jobs
    #' List of `jobqueue::Job`s currently in `$jobqueue`.
    jobs = function () private$.jobqueue$jobs,
    
    #' @field host
    #' Host bound by `$httpuv`.
    host = function () private$.httpuv$getHost(),
    
    #' @field port
    #' Port bound by `$httpuv`.
    port = function () private$.httpuv$getPort()
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


format_200 <- function (result) {
  
  #________________________________________________________
  # Ensure result is a properly formatted HTTP response.
  #________________________________________________________
  if (identical(sort(names(result)), c('body', 'headers', 'status'))) {
    
    resp <- result
    
  } else {
    
    if (length(result) == 0) result <- 200L
    
    if (is_scalar_integerish(result)) {
      
      resp <- list(
        headers = list(),
        status  = as.integer(result), 
        body    = character(0) )
    
    } else if (is.list(result)) {
      
      resp <- list(
        headers = list('Content-Type' = 'application/json'),
        status  = 200L, 
        body    = toJSON(result, na="null") )
      
    } else {
      
      resp <- list(
        headers = list('Content-Type' = 'text/html; charset=utf-8'),
        status  = 200L,
        body    = paste(collapse='\n', as.character(result)) )
    }
  }
  
  return (resp)
}


format_500 <- function (result) {
  
  if (is_scalar_integerish(result))        { status <- as.integer(result); result <- '' }
  else if (inherits(result, 'timeout'))    { status <- 408L } # Request Timeout
  else if (inherits(result, 'superseded')) { status <- 409L } # Conflict
  else if (inherits(result, 'interrupt'))  { status <- 499L } # Client Closed Request
  else                                     { status <- 500L } # Internal Server Error
  
  resp <- list(
    status  = status, 
    headers = list(), 
    body    = ansi_strip(paste(collapse='\n', as.character(result))) )
  
  return (resp)
}


# Using `hooks` to alter the handler
# hooks <- function (job, queue) {
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

