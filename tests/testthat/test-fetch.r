test_that("fetch", {
  
  skip_on_cran()
  
  withr::local_envvar(CURL_SSL_BACKEND = NA)
  
  wq <- webqueue(~{ 'Hi' }, workers = 1L, bg = TRUE)
  
  expect_error(fetch(1))
  expect_error(fetch(character(0)))
  expect_error(fetch(NA_character_))
  expect_error(fetch(''))
  expect_identical(fetch('http://127.0.0.1:8080'), 'Hi')
  
  expect_no_error(wq$stop())
})
