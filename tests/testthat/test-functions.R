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
  set_discordr_webhook('https://discordapp.com/api/webhooks/685200292941267120/E0xM8Ipe7TkZiTBIeFtp289NMqjejB2q2aj50B-jbYRafaFbF0o2PsUZux0ZpizfWcKV')
  set_discordr_username('Travis CI')

  response <- send_message('testing...')
  expect_equal(response$status_code, 204)
})

context("Send File")

test_that("stop function if file does not exist", {
  expect_error(send_file('does_not_exist.abc'), regexp = 'File not found.')
})

test_that("200 response for sent files", {
  set_discordr_webhook('https://discordapp.com/api/webhooks/685200292941267120/E0xM8Ipe7TkZiTBIeFtp289NMqjejB2q2aj50B-jbYRafaFbF0o2PsUZux0ZpizfWcKV')
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

  # Manually Setup Graphics Device; setup quartz for OSX
  if(sys.info()[['sysname']] == 'Darwin'){
    grDevices::quartz()
  }
  grDevices::png(filename = filename)
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

context('Send R Object')


