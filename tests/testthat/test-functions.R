set_default_discord_username('')

context("Set Default Username")

test_that("message and empty string when no username is set", {
  function_result <- expect_message(get_default_discord_username(), 'username not set')
  expect_equal('', function_result)
})

test_that("setting default username and checking it exists", {
  set_default_discord_username('discordr-bot')
  expect_equal('discordr-bot', get_default_discord_username())
})

test_that("overwriting an existing username creates a message", {
  expect_message(set_default_discord_username('discordr-bot2'), 'Overwriting existing username')
  expect_equal('discordr-bot2', get_default_discord_username())
})

test_that("removing default username creates message but accepts", {
  expect_message(set_default_discord_username(''), 'Default username is set to an empty string')
  expect_equal('', get_default_discord_username())
})

context("Creating Discord Connections")

test_that("create discord connection object", {
  conn_obj <- create_discord_connection(webhook_string = 'https://google.com', username = 'test')
  expect_equal(conn_obj$webhook, 'https://google.com')
  expect_equal(conn_obj$username, 'test')
})

test_that("zero character usernames", {
  expect_error(create_discord_connection(webhook_string = 'test', username = ''))
})

conn_obj <- create_discord_connection(webhook = Sys.getenv("DISCORDR_TEST_WEBHOOK_URL"), username = 'Travis CI')

context("Send Message")

test_that("empty strings do not get sent", {
  expect_null(send_message('', conn = conn_obj))
  expect_message(send_message(''), 'Empty message provided.')
})

test_that("204 response for sent messages", {
  response <- send_message('testing...', conn = conn_obj)
  expect_equal(response$status_code, 204)
})

context("Send File")

test_that("stop function if file does not exist", {
  expect_error(send_file('does_not_exist.abc', conn = conn_obj), regexp = 'File not found.')
})

test_that("200 response for sent files", {
  # create example file
  filename <- tempfile(pattern = 'discordr')
  write.csv(x = rnorm(5), file = filename)

  response <- send_file(filename, conn = conn_obj)
  expect_equal(response$status_code, 200)

  # remove example file
  file.remove(filename)
})

context("Send Image (Base Graphics)")

test_that("stop function if no plot exists", {
  expect_error(send_current_plot(conn = conn_obj), regexp = "No plots found.")
})

test_that("200 response for sent plots", {
  filename = tempfile(pattern = 'discordr', fileext = '.png')

  print(dev.list())
  grDevices::png(filename = filename)
  print(dev.list())
  plot(rnorm(5), rnorm(5))
  print(dev.list())
  response <- send_current_plot(filename = filename, conn = conn_obj)
  expect_equal(response$status_code, 200)

  # Remove files
  file.remove(filename)
  file.remove('Rplots.pdf')
})

context("Send Image (ggplot2 Graphics)")

test_that("stop function if no plot exists", {
  expect_error(send_current_ggplot(conn = conn_obj), regexp = "No ggplots found in Plots pane.")
})

test_that("200 response for sent plots", {
  ggplot2::ggplot(data = dplyr::tibble(x = rnorm(5), y = rnorm(5)), ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
  response <- send_current_ggplot(conn = conn_obj)
  expect_equal(response$status_code, 200)
})

context('Send Console Output')

test_that("stop function if no calls provided.", {
  expect_message(send_console(conn = conn_obj), 'No calls provided.')
})

test_that("stop function if no console output provided", {

  expect_message(send_console(invisible(), conn = conn_obj), 'No console output from provided functions.')
})

test_that("204 response for console output", {
  # Basic response
  response <- send_console(5 + 5, conn = conn_obj)
  expect_equal(response$status_code, 204)

  # 2000+ character output
  options(max.print = 5000)
  response_list <- send_console(paste(replicate(2000, "a"), collapse = ""), conn = conn_obj)
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
  response <- expect_message(send_robject(conn = conn_obj), 'No objects provided.')
  expect_null(response)
})

test_that("200 response for sent Rdata file", {
  x <<- c(1,2,3,4,5)
  y <<- matrix(rep(0, 20), nrow = 4)
  z <<- dplyr::tibble(a = c(1,2,3,4,5), b = c(1,2,3,4,5))

  response <- send_robject(x, y, z, conn = conn_obj)
  expect_equal(response$status_code, 200)
})

context('Send Tex Image')

test_that("stop function if no tex string provided", {
  expect_message(send_tex("", conn = conn_obj))
})

test_that("200 response for sent tex image", {

  tex_string <- "$\\int^a_b \\frac{1}{3}x^3 dx$"

  response <- send_tex(tex_string, conn = conn_obj)
  expect_equal(response$status_code, 200)
})
