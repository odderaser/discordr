library(httr)
library(ggplot2)

#' Set Default Discordr Username
#'
#' Sets a default username to be used for discord communication. Use \code{\link{get_discordr_username}} to check currently set default username.
#'
#' @param username Username to be used as default discord name
#'
#' @return
#' @export
#'
#' @examples
#' set_discordr_username("dataman")
set_discordr_username <- function(username){
  Sys.setenv(DISCORDR_USERNAME = username)
}

#' Get Default Discordr Username
#'
#' Obtains the currently set default username or returns an error if it not set within the current environment. If username is not set, use \code{\link{set_discordr_username}} to set default environment username.
#'
#' @return
#' @export
#'
#' @examples
#' get_discordr_username()
get_discordr_username <- function(){
  username <- Sys.getenv("DISCORDR_USERNAME")
  if(nchar(username) == 0){
    stop("Default discordr username not set. Use set_discordr_username to set a default webhook as an environment variable.")
  }
  return(username)
}

#' Set Default Discordr Webhook
#'
#' Sets a default webhook to be used for discord communication. Use \code{\link{set_discordr_webhook}} to check currently set default webhook.
#'
#' @param webhook_address
#'
#' @return
#' @export
#'
#' @examples
#' set_discordr_webhook("https://discord.com/etc")
set_discordr_webhook <- function(webhook_address){
  Sys.setenv(DISCORDR_WEBHOOK = webhook_address)
}

#' Get Default Discordr Webhook
#'
#' Obtains the currently set default username or returns an error if it not set within the current environment. If a default webhook is not set, Use \code{\link{set_discordr_webhook}} to set default environment webhook.
#'
#' @return webhook_address
#' @export
#'
#' @examples
#' get_discordr_webhook()
get_discordr_webhook <- function(){
  webhook_address <- Sys.getenv("DISCORDR_WEBHOOK")
  if(nchar(webhook_address) == 0){
    stop("Default discordr webhook not set. Use set_discordr_webhook to set a default webhook as an environment variable.")
  }
  return(webhook_address)
}

#' Send Message
#'
#' Sends a message using the username provided to the channel of the webhook provided
#'
#' @param message Character string of message to send
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#'
#' @return None
#' @export
#'
#' @examples
#' send_message("Hello World!")
#' send_message("Hello World!", username = "dataman")
#' send_message("Hello World!", webhook = "https://discordapp.com/api/webhooks/<your-webhook-here>")
#' send_message("Hello World!", username = "dataman", webhook = "https://discordapp.com/api/webhooks/<your-webhook-here>")
#'
#' @seealso \code{\link{send_file}}, \code{\link{send_current_plot}}, \code{\link{send_current_ggplot}}
send_message <- function(message, username = get_discordr_username(), webhook = get_discordr_webhook()){
  body_data <- list(content = message,
                    username = username)

  POST(url = webhook,
       body = body_data,
       encode = "json")
}

#' Send File
#'
#' Send file from the given filename to the username and webhook provided
#'
#' @param filename filepath and filename of the file to be sent
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#'
#' @return
#' @export
#'
#' @examples
#' send_file('image.jpg')
#'
#' @seealso
#' \code{\link{send_file}}, \code{\link{send_current_plot}}, \code{\link{send_current_ggplot}}
send_file <- function(filename, username = get_discordr_username(), webhook = get_discordr_webhook()){

  if(file.exists(filename)){
    body_data <- list(content = upload_file(filename),
                      username = username)

    POST(url = webhook,
         body = body_data,
         encode = 'multipart')
  }
}

#' Send Current Plot
#'
#' Sends the last plot displayed within the plots tab in RStudio. This function will display both ggplot and base graphics
#' with a lower resolution than only ggplot graphics. In order to save and send the file, a random name for the image will
#' be generated and saved temporarily.
#'
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#'
#' @return
#' @export
#'
#' @examples
#' send_current_plot()
#'
#' @seealso
#' \code{\link{send_current_ggplot}}, \code{\link{send_file}}, \code{\link{send_message}}
send_current_plot <- function(username = get_discordr_username(), webhook = get_discordr_webhook()){
  random_filename <- paste(paste(sample(LETTERS, 15, replace = TRUE), collapse = ''), '.png', sep = '')

  image_dimensions <- grDevices::dev.size("px")
  rstudioapi::savePlotAsImage(file = random_filename, width = image_dimensions[1], height = image_dimensions[2])

  body_data <- list(content = upload_file(random_filename),
                    username = username)

  POST(url = webhook,
       body = body_data,
       encode = "multipart")

  if(file.exists(random_filename)){
    file.remove(random_filename)
  }
}

#' Send Current Plot (ggplot version)
#'
#' This function works identical to \code{\link{send_current_plot}} except that it appears to provide a higher resolution and only work for plots
#' constructed using a ggplot workflow.
#'
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#'
#' @return
#' @export
#'
#' @examples
#' send_current_ggplot()
#'
#' @seealso
#' \code{\link{send_current_plot}}, \code{\link{send_file}}, \code{\link{send_message}}
send_current_ggplot <- function(username = get_discordr_username(), webhook = get_discordr_webhook()){
  random_filename <- paste(paste(sample(LETTERS, 15, replace = TRUE), collapse = ''), '.png', sep = '')

  if(!is.null(last_plot())){
    ggsave(random_filename)
  }
  else {
    stop('No previous ggplot found.')
  }

  body_data <- list(content = upload_file(random_filename),
                    username = username)

  POST(url = webhook,
       body = body_data,
       encode = "multipart")

  if(file.exists(random_filename)){
    file.remove(random_filename)
  }
}

