
#' Compile an HTTP response.
#' 
#' If your WebQueue's `handler` function returns a list, json object, character
#' vector, or scalar integer, `response()` will be used to transform that 
#' result into an HTTP response.\cr\cr
#' You may also call `response()` within your handler to better customize the 
#' HTTP response. Or, return a result of class 'AsIs' to have that object 
#' passed directly on to 'httpuv'.
#' 
#' @param body      The content. A list will be encoded as JSON. A scalar 
#'                  integer will be interpreted as a status. A character vector
#'                  will be concatenated with no separator.
#' @param status    A HTTP response status code.
#' @param headers   A named character vector of HTTP headers. A list-like
#'                  object is acceptable if all elements are simple strings.
#' @param cookies   A named character vector of cookies to set.
#'                  `cookie()` can aid in making these strings.
#' 
#' @return A `<response/AsIs>` object. Essentially a list with elements named
#'         `body`, `status`, and `headers` formatted as 'httpuv' expects.
#' 
#' @export
#' @examples
#' 
#'     library(webqueue)
#'     
#'     response(list(name = unbox('Peter'), pi = pi))
#'          
#'     response(307L, headers = c(Location = '/new/file.html'))
#'     
#'     response(cookies = c(id = cookie(123, http_only = TRUE)))
#'     

response <- function (body = NULL, status = 200L, headers = NULL, cookies = NULL) {
  
  if (is_int(body)) { status <- body; body <- NULL }
  else              { status <- as.integer(status) }
  stopifnot(status >= 100L, status < 600L)
  
  headers <- as.list(headers, all.names = TRUE)
  cookies <- as.list(cookies, all.names = TRUE)
  stopifnot(all_named(headers), all(sapply(headers, is_string)))
  stopifnot(all_named(cookies), all(sapply(cookies, is_string)))
  
  if (!inherits(body, c('list', 'json', 'character', 'NULL')))
    cli_abort('`body` must be of class list, character, json, or NULL, not {.type {body}}.')
  
  for (i in seq_along(cookies)) {
    cookie_name <- names(cookies)[[i]]
    stopifnot(valid_string(cookie_name, no = ' ()<>@,;:\\"/[]?={}'))
    cookie  <- paste0(cookie_name, '=', cookies[[i]])
    headers <- c(headers, list('Set-Cookie' = cookie))
  }
  
  if (!'content-type' %in% tolower(names(headers)))
    headers[['Content-Type']] <- {
      if (inherits(body, c('list', 'json'))) { 'application/json; charset=utf-8' }
      else if (inherits(body, 'character'))  { 'text/html; charset=utf-8'        }
    }
  
  if (inherits(body, 'list')) body <- toJSON(body, null = 'null')
  if (inherits(body, 'json')) body <- as.character(body)
  if (!is.null(body))         body <- paste0(body, collapse = '')
  
  resp <- structure(
    .Data = list(body = body, status = status, headers = headers),
    class = c('response', 'AsIs') )
  
  return (resp)
}


#' Print a response object.
#' 
#' @param x   An object of class `response`.
#' @param ...   Not used.
#' 
#' @noRd
#' @keywords internal
#' @export
print.response <- function (x, ...) {
  
  stopifnot(
    is.list(x),
    length(x) == 3,
    is_int(x$status),
    is.list(x$headers),
    all_named(x$headers),
    is_string(x$body, null_ok = TRUE),
    all(sapply(x$headers, is_string))
  )
  
  msg <- code_to_msg[[as.character(x$status)]]
  cat(style_bold(paste('HTTP/1.1', x$status, msg)), '\n', sep = '')
  
  for (i in seq_along(x$headers))
    cat(paste0(names(x$headers)[[i]], ': ', x$headers[[i]]), '\n', sep = '')
  
  if (!is.null(x$body)) {
    
    cat('\n')
    
    if (nchar(x$body) > 50) {
      ct <- which(tolower(names(x$headers)) == 'content-type')
      ct <- if (length(ct) > 0) tolower(x$headers[[ct[[1]]]]) else ''
      if (startsWith(ct, 'application/json')) x$body <- prettify(x$body)
    }
    
    x$body <- strsplit(x$body, '\n', fixed = TRUE)[[1]]
    width  <- min(getOption('width', 100L), 100L) - 5L
    
    for (i in seq_len(min(length(x$body), 10))) {
      line <- x$body[[i]]
      post <- if (nchar(line) > width) style_italic(col_grey('...'))
      cat(substr(line, 1, width), post, '\n', sep = '')
    }
    
    if (length(x$body) > 10) {
      line <- paste('___', length(x$body) - 10, 'more lines.', '___')
      cat(style_italic(col_grey(line)))
    }
    
  }
  
  invisible()
}


#' Ensure a list becomes a JSON object.
#' 
#' This function returns a list that `jsonlite::toJSON()` will always encode as 
#' `{}`.
#' 
#' @param x   A list, or list-like object.
#' 
#' @return A list with the names attribute set.
#' 
#' @export
#' @examples
#' 
#'     library(webqueue)
#'     
#'     updates <- list()
#'     
#'     response(list(updates = updates))
#'     
#'     response(list(updates = js_obj(updates)))
#'     

js_obj <- function (x = list()) {
  x <- as.list(x, all.names = TRUE)
  if (is.null(names(x)))
    names(x) <- character(length(x))
  return (x)
}



#' Assemble an HTTP cookie.
#' 
#' See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie for 
#' a more in-depth description of each parameter's purpose.
#' 
#' @param cookie_value   The value (string) to assign to the cookie. 
#'                       Cannot contain white space, double quotes, commas, 
#'                       semicolons, or slashes (`" , ; \\`).
#' 
#' @param max_age      The number of seconds until expiration. 
#'                     Omit to create a session cookie,
#' 
#' @param domain       Send with requests to this host.
#' 
#' @param path         Send with requests to this path.
#' 
#' @param same_site    `'Strict'`, `'Lax'`, or `'None'`.
#'                     `secure` required for `'None'`.
#' 
#' @param secure        Only send over HTTPS.
#' 
#' @param http_only     Disallow javascript access.
#' 
#' @param partitioned   Use partitioned storage. `secure` required.
#' 
#' @return A string.
#' 
#' @export
#' @examples
#' 
#'     library(webqueue)
#'     
#'     cookie('xyz', max_age = 3600, http_only = TRUE)
#'     
#'     response(cookies = c(token = cookie('randomstring123')))
#'     

cookie <- function (
    cookie_value, 
    max_age = NULL, domain = NULL, path = NULL, same_site = 'Lax', 
    secure = FALSE, http_only = FALSE, partitioned = FALSE ) {
  
  cookie_value <- as.character(cookie_value)
  if (!is.null(max_age)) max_age <- as.integer(max_age)
  
  stopifnot(
    valid_string(cookie_value, no = ' ",;\\', null_ok = FALSE),
    is.null(max_age) || is_int(max_age),
    valid_string(domain, ok = '.-'),
    valid_string(path,   ok = ".-_~!$&'()*+,=:@%/"),
    same_site %in% c('None', 'Lax', 'Strict'),
    is_bool(secure),
    is_bool(http_only),
    is_bool(partitioned)
  )
  
  str <- paste(collapse = '; ', c(
    cookie_value,
    if (!is.null(max_age))  paste0('Max-Age=',  max_age),
    if (!is.null(domain))   paste0('Domain=',   domain),
    if (!is.null(path))     paste0('Path=',     path),
    if (same_site != 'Lax') paste0('SameSite=', same_site),
    if (secure)             'Secure',
    if (http_only)          'HttpOnly',
    if (partitioned)        'Partitioned'
  ))
  
  return (str)
}
