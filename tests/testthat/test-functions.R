set_discordr_username('')
set_discordr_webhook('')

context("Environment Variables")

test_that("empty environment variable returns empty string and warning", {
  expect_message(get_discordr_username(), 'username not set')
  expect_equal('', get_discordr_username())

  expect_message(get_discordr_webhook(), 'webhook not set')
  expect_equal('', get_discordr_webhook())
})

test_that("setting username and checking it exists", {
  set_discordr_username('discordr-bot')
  expect_equal('discordr-bot', get_discordr_username())

  set_discordr_webhook('https://test.webhook/')
  expect_equal('https://test.webhook/', get_discordr_webhook())
})

test_that("messages when overwriting existing environment variables", {
  # No message on same string
  expect_message(set_discordr_username('discordr-bot'), regexp = NA)
  expect_message(set_discordr_webhook('https://test.webhook/'), regexp = NA)

  # Show message on different string
  expect_message(set_discordr_username('discordr-bot2'), 'Overwriting existing username: discordr-bot')
  expect_message(set_discordr_webhook('https://test2.webhook/'), 'Overwriting existing webhook: https://test.webhook/')
})

context("Send Message")

test_that("empty strings do not get sent", {
  expect_null(send_message(''))
  expect_message(send_message(''), 'Empty message provided.')
})

test_that("204 response for sent messages", {
  set_discordr_webhook(Sys.getenv("DISCORDR_TEST_WEBHOOK_URL"))
  set_discordr_username('Travis CI')

  response <- send_message('testing...')
  expect_equal(response$status_code, 204)
})

context("Send File")

test_that("stop function if file does not exist", {
  expect_error(send_file('does_not_exist.abc'), regexp = 'File not found.')
})

test_that("200 response for sent files", {
  set_discordr_webhook(Sys.getenv("DISCORDR_TEST_WEBHOOK_URL"))
  set_discordr_username('Travis CI')

  # create example file
  filename <- tempfile(pattern = 'discordr')
  write.csv(x = rnorm(5), file = filename)

  response <- send_file(filename)
  expect_equal(response$status_code, 200)

  # remove example file
  file.remove(filename)
})

context("Send Image (Base Graphics)")

test_that("stop function if no plot exists", {
  expect_error(send_current_plot(), regexp = "No plots found.")
})

test_that("200 response for sent plots", {
  filename = tempfile(pattern = 'discordr', fileext = '.png')

  #Manually Setup Graphics Device; setup quartz for OSX
  if(Sys.info()[['sysname']] == 'Darwin'){
    grDevices::quartz()
  }
  else {
    grDevices::png(filename = filename)
  }

  plot(rnorm(5), rnorm(5))

  response <- send_current_plot(filename = filename)
  expect_equal(response$status_code, 200)

  # Remove files
  file.remove(filename)
  file.remove('Rplots.pdf')
})

context("Send Image (ggplot2 Graphics)")

test_that("stop function if no plot exists", {
  expect_error(send_current_ggplot(), regexp = "No ggplots found in Plots pane.")
})

test_that("200 response for sent plots", {
  ggplot2::ggplot(data = dplyr::tibble(x = rnorm(5), y = rnorm(5)), ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
  response <- send_current_ggplot()
  expect_equal(response$status_code, 200)
})

context('Send Console Output')

test_that("stop function if no calls provided.", {
  set_discordr_webhook(Sys.getenv("DISCORDR_TEST_WEBHOOK_URL"))
  set_discordr_username('Travis CI')

  expect_message(send_console(), 'No calls provided.')
})

test_that("stop function if no console output provided", {

  expect_message(send_console(invisible()), 'No console output from provided functions.')
})

test_that("204 response for console output", {
  # Basic response
  response <- send_console(5 + 5)
  expect_equal(response$status_code, 204)

  # 2000+ character output
  options(max.print = 5000)
  response_list <- send_console(paste(replicate(2000, "a"), collapse = ""))
  options(max.print = 1000)

  # two responses expected
  expect_equal(length(response_list), 2)

  # extract response status codes
  response_status_codes <- rep(0, length(response_list))
  for(response_index in 1:length(response_list)){
    response_status_codes[response_index] <- response$status_code
  }

  # all status codes should be 204
  expect_setequal(response_status_codes, rep(204, length(response_list)))
})

context('Send R Object')

test_that("stop function if no objects provided",{
  set_discordr_webhook(Sys.getenv("DISCORDR_TEST_WEBHOOK_URL"))
  set_discordr_username('Travis CI')

  response <- expect_message(send_robject(), 'No objects provided.')
  expect_null(response)
})

test_that("200 response for sent Rdata file", {
  set_discordr_webhook(Sys.getenv("DISCORDR_TEST_WEBHOOK_URL"))
  set_discordr_username('Travis CI')

  x <<- c(1,2,3,4,5)
  y <<- matrix(rep(0, 20), nrow = 4)
  z <<- dplyr::tibble(a = c(1,2,3,4,5), b = c(1,2,3,4,5))

  response <- send_robject(x, y, z)
  expect_equal(response$status_code, 200)
})

context('Send Tex Image')

test_that("stop function if no tex string provided", {
  set_discordr_webhook(Sys.getenv("DISCORDR_TEST_WEBHOOK_URL"))
  set_discordr_username('Travis CI')

  expect_message(send_tex(""))
})

test_that("200 response for sent tex image", {
  set_discordr_webhook(Sys.getenv("DISCORDR_TEST_WEBHOOK_URL"))
  set_discordr_username('Travis CI')

  tex_string <- "$\\int^a_b \\frac{1}{3}x^3 dx$"

  response <- send_tex(tex_string)
  expect_equal(response$status_code, 200)
})
