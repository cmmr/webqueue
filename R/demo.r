


demo <- function () {

  # A functions that will be available on the worker processes.
  globals <- list(

    template = function (header, text) {
      paste0('
        <!doctype html>
        <html lang="en">
          <head>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <meta name="color-scheme" content="light dark">
            <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css">
            <title>JobQueue | WebQueue Demo</title>
          </head>
          <body>
            <header class="container">
              <nav>
                <ul>
                  <li><strong>WebQueue Demo</strong></li>
                </ul>
                <ul>
                  <li><a href="date">Date</a></li>
                  <li><a href="greet">Greet</a></li>
                  <li><a href="req">Request</a></li>
                  <li><a href="sleep">Sleep</a></li>
                </ul>
              </nav>
            </header>
            <main class="container">
              <h2>', header, '</h2>
              ', text, '
            </main>
            <footer class="container">
              <small>
                See also <a href="https://cmmr.github.io/jobqueue/" class="secondary">https://cmmr.github.io/jobqueue/</a>
              </small>
            </footer>
          </body>
        </html>
      ')
    }, # /template

    form = function (name, type, value, button, action = NULL) {
      value  <- shQuote(ifelse(is.null(value), '', value))
      method <- ifelse(is.null(action), '', ' method = "POST"')
      action <- ifelse(is.null(action), '', paste0(' action = "', action, '"'))
      paste0('
        <form', method, action, '>
          <fieldset role="group">
          <input name="', name, '" type="', type, '" value=', value, '>
          <input type="submit" value="', button, '">
          </fieldset>
        </form> ')
    }
  )


  # Code that gets run on the worker process.
  handler <- function (req, globals) {

    page <- req$PATH_INFO

    if (page == '/date') {
      header <- "Curent Date and Time"
      text   <- paste0('<pre><code>', Sys.time(), '</code></pre>')

    } else if (page == '/greet') {
      header <- "Interactive Greeting"
      text   <- paste0(
        ifelse(
          test = is.null(req$ARGS$name),
          yes  = "Enter your name to be greeted.",
          no   = paste0('<article>Hello, <b>', req$ARGS$name, '</b>!</article>\n') ),
        '<blockquote>',
        '<table>',
        '<tr>',
        '<td><h4>Using GET:</h4></td>',
        '<td>', globals$form('name', 'text', req$ARGS$name, 'Greet Me!'), '</td>',
        '</tr><tr>',
        '<td><h4>Using POST:</h4></td>',
        '<td>', globals$form('name', 'text', req$ARGS$name, 'Greet Me!', '/greet'), '</td>',
        '</tr>',
        '</table>',
        '</blockquote>'
      )

    } else if (page == '/req') {

      escape <- function (nm) {
        val <- get(nm, req)
        pre <- TRUE

        if (is.character(val) && length(val) > 1) {
          val <- mapply(paste, names(val), '=', unname(val))
          val <- paste(collapse = '\n', val)
        } else if (!is.character(val) || length(val) != 1) {
          val <- capture.output(str(val))
          val <- paste(collapse = '\n', val)
        } else {
          pre <- FALSE
        }
        val <- gsub('<', '&lt;', val, fixed = TRUE)
        val <- gsub('>', '&gt;', val, fixed = TRUE)
        if (pre) val <- paste0('<pre>', val, '</pre>')
        paste0('<small><code>', val, '</code></small>')
      }

      rows <- paste(collapse = '', sapply(sort(ls(req)), function (nm) {
        paste0('<tr><td><small>', nm, '</small></td><td>', escape(nm), '</td></tr>') }))

      header <- "Content of this HTTP Request"
      text   <- paste0('<table>', rows, '</table>')

    } else if (startsWith(page, '/sleep')) {

      wait <- ifelse(is.null(req$ARGS$wait), 0, as.integer(req$ARGS$wait))

      t1 <- as.character(Sys.time())
      Sys.sleep(wait)
      t2 <- as.character(Sys.time())

      header <- paste('Waited', wait, 'seconds')
      text   <- paste0(
        '<pre>',
        '<code>',
        'Started:  ', t1, '\n',
        'Finished: ', t2,
        '</code>',
        '</pre>',
        '<hr>',
        globals$form('wait', 'number', wait, 'Sleep'),
        'If more than the maximum of 10 seconds, the job will be automatically stopped before finishing.')


    } else {
      header <- "Welcome to WebQueue"
      text   <- "Select a link above to explore what R's request handler can see and do."
    }

    html <- globals$template(header, text)
    return (html)
  }

  # Ignore requests for a website fav icon.
  hook <- function (job) {
    if (job$vars$req$PATH_INFO == '/favicon.ico') job$stop(404L)
  }


  svr <- Server$new(
    handler = handler,
    host    = '127.0.0.1',
    port    = 8080L,
    timeout = 10,
    hooks   = list(created = hook),
    stop_id = ~{ .$GET$stop_id },
    copy_id = ~{ .$GET$copy_id },
    globals = globals,
    workers = 4L )

  cli_text("Site available at {.url http://127.0.0.1:8080}")

  return (svr)
}
