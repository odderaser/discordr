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
