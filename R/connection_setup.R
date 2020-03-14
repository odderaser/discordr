#' Set Default Discordr Username
#'
#' Sets a default username to be used for discord communication. Use \code{\link{get_default_discord_username}} to check currently set default username.
#'
#' @param username Username to be used as default discord name
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' set_default_discord_username("dataman")
#' }
set_default_discord_username <- function(username){
  existing_username <- get_default_discord_username(verbose = FALSE)
  if(nchar(username) == 0){
    message("Default username is set to an empty string.")
  }
  else if(nchar(existing_username) > 0 && existing_username != username){
    message(paste('Overwriting existing username:', existing_username))
  }
  Sys.setenv(DISCORD_USERNAME = username)
}

#' Get Default Discordr Username
#'
#' Obtains the currently set default username or returns an error if it not set within the current environment. If username is not set, use \code{\link{set_default_discord_username}} to set default environment username.
#'
#' @param verbose Return detailed messages on if username is currently set.
#'
#' @return Currently set discordr username environment variable; Will return an empty string and message if no username has been set.
#' @export
#'
#' @examples
#' \dontrun{
#' get_default_discord_username()
#' }
get_default_discord_username <- function(verbose = TRUE){
  username <- Sys.getenv("DISCORD_USERNAME")
  if(nchar(username) == 0 && verbose){
    message("Default discordr username not set. Use set_default_discord_username to set a default username as an environment variable.")
  }
  return(username)
}

#' Create Discord Connection Object
#'
#' @param webhook_string Webhook URL for sending discord comments
#' @param username Defaults to \code{get_default_discord_username()}
#' @param server_name Optional - Used for discriminating webhooks in a human readable format
#' @param channel_name Optional - Used for discriminating webhooks in a human readable format
#'
#' @return DiscordR connection object containing provided information
#' @export
#'
#' @examples
create_discord_connection <- function(webhook_string, username = get_default_discord_username(verbose = FALSE), server_name = NULL, channel_name = NULL){
  # check if username is not empty
  if(nchar(username) == 0){
    stop('Zero character username provided.')
  }

  connection_object <- NULL
  connection_object$webhook = webhook_string
  connection_object$username = username
  connection_object$server_name = server_name
  connection_object$channel_name = channel_name

  return(connection_object)
}

#' Discordr Setup Wizard
#'
#' Walks the user through checking if an existing configuration exists, changing an existing configuration, or setting up a new configuration.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' discordr_setup()
#' }
#' @importFrom utils menu
discordr_setup <- function(){
  nice_print <- function(x = ''){
    cat(paste(x, '\n'))
  }

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
