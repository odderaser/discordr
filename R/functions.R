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

#' Discordr Setup Wizard
#'
#' Walks the user through checking if an existing configuration exists, changing an existing configuration, or setting up a new configuration.
#'
#' @return
#' @export
#'
#' @examples
#' discordr_setup()
discordr_setup <- function(){
  nice_print <- function(x = '') paste(x, '\n') %>% cat()

  nice_print('Welcome to discordr!')
  nice_print('Would you like to setup a new configuration or check for an existing one?')
  setup_menu_choice <- menu(c('Check for existing configuration', 'Setup new configuration'), graphics = FALSE, title = NULL)

  if(as.integer(setup_menu_choice) == 1){
    webhook_address <- Sys.getenv("DISCORDR_WEBHOOK")
    if(nchar(webhook_address) == 0){
      nice_print('No default environment webhook found. Would you like to setup a default webhook?')
      webhook_menu_choice <- menu(c('Uh huh.', 'Nope!'), graphics = FALSE, title = NULL)
      if(as.integer(webhook_menu_choice) == 1){
        webhook_address <- readline(prompt = "Enter Webhook URL:")
        set_discordr_webhook(webhook_address)
      }
    }
    else {
      nice_print(paste("We found the following default environment webhook set: ", webhook_address))
      nice_print("Would you like to change it?")
      change_webhook_choice <- menu(c("Yes, please!", "No thanks."), graphics = FALSE, title = NULL)
      if(change_webhook_choice == 1){
        webhook_address <- readline(prompt = "Enter Webhook (enter empty string to remove existing default):")
        set_discordr_username(webhook_address)
      }
    }

    username <- Sys.getenv("DISORDR_USERNAME")
    if(nchar(username) == 0){
      nice_print('No default environment username found. Would you like to setup a default username?')
      webhook_menu_choice <- menu(c('You bet!', 'Nah.'), graphics = FALSE, title = NULL)
      if(as.integer(webhook_menu_choice) == 1){
        username <- readline(prompt = "Enter Username:")
        set_discordr_username(username)
      }
    }
    else {
      nice_print(paste("We found the following default environment username set: ", username))
      nice_print("Would you like to change it?")
      change_username_choice <- menu(c("Yes, please!", "No thanks."), graphics = FALSE, title = NULL)
      if(change_username_choice == 1){
        username <- readline(prompt = "Enter Username (enter empty string to remove existing default):")
        set_discordr_username(username)
      }
    }

    nice_print("Great! You're set it looks like. Use send_message('Hello World!') to test connectivity and include webhook and/or username manually if you left them empty during configuration.")
  }
  else if(as.integer(setup_menu_choice) == 2){

    nice_print('Enter the webhook URL for the channel you would like to communicate with.')
    webhook_address <- readline(prompt = "Enter Webhook URL:")
    set_discordr_webhook(webhook_address)

    nice_print('Enter a name to be attached to communication sent using this package.')
    username <- readline(prompt = "Enter Username:")
    set_discordr_username(username)

    nice_print("Great! You're all set! Use the send_message('Hello World!') function to test connectivity.")
  }
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
#' @seealso \code{\link{send_file}}, \code{\link{send_current_plot}}, \code{\link{send_current_ggplot}}, \code{\link{send_console}}
send_message <- function(message, username = get_discordr_username(), webhook = get_discordr_webhook()){
  if(nchar(message) > 0){
    body_data <- list(content = message,
                      username = username)

    res <- httr::POST(url = webhook,
                     body = body_data,
                     encode = "json")
  }

  invisible(res)
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
#' \code{\link{send_file}}, \code{\link{send_current_plot}}, \code{\link{send_current_ggplot}}, \code{\link{send_console}}
send_file <- function(filename, username = get_discordr_username(), webhook = get_discordr_webhook()){

  if(file.exists(filename)){
    body_data <- list(content = httr::upload_file(filename),
                      username = username)

    res <- httr::POST(url = webhook,
                     body = body_data,
                     encode = 'multipart')

    invisible(res)
  }
  else {
    stop("File not found.")
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
#' @param filename Optional - Filepath indicating where to save image; Provide to manually override the temporary directory and filename
#'
#' @return
#' @export
#'
#' @examples
#' send_current_plot()
#'
#' @seealso
#' \code{\link{send_current_ggplot}}, \code{\link{send_file}}, \code{\link{send_message}}, \code{\link{send_console}}
send_current_plot <- function(username = get_discordr_username(), webhook = get_discordr_webhook(), filename = tempfile(pattern = 'discordr', fileext = '.png')){

  image_dimensions <- grDevices::dev.size("px")
  rstudioapi::savePlotAsImage(file = filename, width = image_dimensions[1], height = image_dimensions[2])

  body_data <- list(content = httr::upload_file(filename),
                    username = username)

  res <- httr::POST(url = webhook,
                   body = body_data,
                   encode = "multipart")

  invisible(res)
}

#' Send Current Plot (ggplot version)
#'
#' This function works identical to \code{\link{send_current_plot}} except that it appears to provide a higher resolution and only work for plots
#' constructed using a ggplot workflow.
#'
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#' @param filename Optional - Filepath indicating where to save image; Provide to manually override the temporary directory and filename
#'
#' @return
#' @export
#'
#' @examples
#' send_current_ggplot()
#'
#' @seealso
#' \code{\link{send_current_plot}}, \code{\link{send_file}}, \code{\link{send_message}}, \code{\link{send_console}}
send_current_ggplot <- function(username = get_discordr_username(), webhook = get_discordr_webhook(), filename = tempfile(pattern = 'discordr', fileext = '.png')){
  filename

  if(!is.null(last_plot())){
    ggplot2::ggsave(filename)
  }
  else {
    stop('No previous ggplot found.')
  }

  body_data <- list(content = httr::upload_file(filename),
                    username = username)

  res <- httr::POST(url = webhook,
                   body = body_data,
                   encode = "multipart")

  invisble(res)
}

#' Send Console Output
#'
#' This functions accepts an expression whose console output you would like to send to the specified discord channel
#'
#' @param ... A single or set of expressions to be evaluated for console output
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#'
#' @return
#' @export
#'
#' @examples
#' send_console(2 + 2)
#' #' @seealso
#' \code{\link{send_message}}, \code{\link{send_file}}, \code{\link{send_current_plot}}, \code{\link{send_current_ggplot}}
send_console <- function(..., username = get_discordr_username(), webhook = get_discordr_webhook(), filename = tempfile(pattern = 'discordr')){

  sink(file = filename, split = TRUE)

  # code heavily inspired by capture.output
  pf = parent.frame()
  args <- substitute(list(...))[-1L]
  evalVis <- function(expr) withVisible(eval(expr, pf))
  for(i in 1:seq_along(args)){
    expr <- args[[1]]
    tmp <- switch(mode(expr), expression = lapply(expr, evalVis), call = , name = list(evalVis(expr)), stop("bad argument"))
    for (item in tmp) if (item$visible)
      print(item$value)
  }

  sink()

  console_output <- readChar(filename, file.info(filename)$size)

  ### Clean up this code
  # Create easy function for padding message with code quotes, maybe in send_message?
  # Streamline loop process by removing initialization,
  # move check for empty string to else case,
  # remove subsetting in forloop
  # name variables better

  if(file.info(filename)$size > 1990){
    console_output_split <- unlist(stringr::str_split(console_output, '\n'))
    current_console_output_split <- console_output_split[1]
    temp_total <- nchar(current_console_output_split) + 2
    for(console_output_substr in console_output_split[2:length(console_output_split)]){
      if(temp_total + nchar(console_output_substr) > 1990){
        current_console_output_split <- paste('```', current_console_output_split, '```', sep = '\n')
        send_message(current_console_output_split, username = username, webhook = webhook)

        current_console_output_split <- console_output_substr
        temp_total <- nchar(console_output_substr) + 2
      }
      else {
        temp_total <- temp_total + nchar(console_output_substr) + 2
        current_console_output_split <- paste(current_console_output_split, console_output_substr, sep = '\n')
      }
    }
    current_console_output_split <- paste('```', current_console_output_split, '```', sep = '\n')
    send_message(current_console_output_split, username = username, webhook = webhook)
  }
  else {
    console_output <- paste('```', console_output, '```', sep = '\n')
    send_message(console_output, username = username, webhook = webhook)
  }
}

#' Send R Objects
#'
#' @param ... Single or Multiple R Objects to be contained within a single RData file
#' @param filename Default is a random string saved in the temporary directory; change this if you would like the RData file to be human-readable and in a different location.
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#'
#' @return
#' @export
#'
#' @examples
#' x <- c(1,2,3,4,5)
#' y <- matrix(rep(0, 4), rows = 2, cols = 2)
#' send_robject(x, y, filename = 'test_data.RData')
send_robject <- function(..., filename = tempfile(pattern = 'discordr', fileext = '.RData'), username = get_discordr_username(), webhook = get_discordr_webhook()){

  save(..., file = filename)

  res <-send_file(filename, username = username, webhook = webhook)
  invisible(res)
}

