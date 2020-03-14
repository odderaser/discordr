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
#' @param set_default Optional - Set created connection as the default connection
#'
#' @return DiscordR connection object containing provided information
#' @export
#'
#' @examples
#' \dontrun{
#' conn_obj <- create_discord_connection(webhook = 'https://google.com', username = 'test')
#' }
create_discord_connection <- function(webhook_string, username = get_default_discord_username(verbose = FALSE), server_name = '', channel_name = '', set_default = FALSE){
  # check if username is not empty
  if(nchar(username) == 0){
    stop('Zero character username provided.')
  }

  connection_object <- NULL
  connection_object$webhook = webhook_string
  connection_object$username = username
  connection_object$server_name = server_name
  connection_object$channel_name = channel_name

  if(set_default){
    set_default_discord_connection(connection_object)
  }

  return(connection_object)
}

#' Get Default Discord Connnection
#'
#' Retreives Default Connection Object from R Options if set
#'
#' @return DiscordR Connection Object
#' @export
#'
#' @examples
#' \dontrun{
#' get_default_discord_connection()
#' }
get_default_discord_connection <- function(){
  conn <- getOption('default_discordr_connection')
  if(is.null(conn)){
    stop("No default discord connection set.")
  }

  return(conn)
}

#' Set Default Discord Connection
#'
#' @param conn Discord Connection Object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' conn_obj <- create_discord_connection(webhook = 'https://google.com', username = 'test')
#' set_default_discord_connection(conn_obj)
#' }
set_default_discord_connection <- function(conn){
  options(default_discordr_connection = conn)
}

#' Export Discord Connections
#'
#' @param conn Connection Object to Export
#' @param filepath Filepath to new or existing connections file
#' @param append Optional; Assumed to be TRUE to add connection objects, otherwise will overwrite existing file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' conn_obj <- create_discord_connection(webhook = 'https://google.com', username = 'test')
#' export_discord_connection(conn_obj, 'discord_connections.csv')
#' }
export_discord_connection <- function(conn, filepath, append = TRUE){
  if(file.exists(filepath) && append){
    conn_file <- readr::read_csv(filepath)
    conn_file <- dplyr::bind_rows(conn_file, tibble::tibble(server_name = conn$server_name, channel_name = conn$channel_name, username = conn$username, webhook = conn$webhook))
    readr::write_csv(conn_file, filepath)
  }
  else {
    if(file.exists(filepath)){
      file.remove(filepath)
    }
    conn_file <- readr::write_csv(tibble::tibble(server_name = conn$server_name, channel_name = conn$channel_name, username = conn$username, webhook = conn$webhook), filepath)
  }
}

#' Import Discord Connections From File
#'
#' @param filepath Path to Discord Connections File
#'
#' @return List of Connection Objects
#' @export
#'
#' @examples
#' \dontrun{
#' import_discord_connections('test.csv')
#' }
import_discord_connections <- function(filepath){
  if(!file.exists(filepath)){
    stop('connection file does not exist.')
  }

  conn_file <- readr::read_csv(filepath)
  connections <- list()

  for(i in 1:nrow(conn_file)){
    connections[[i]] <- create_discord_connection(webhook_string = conn_file$webhook[i], username = conn_file$username[i], server_name = conn_file$server_name[i], channel_name = conn_file$channel_name[i])
  }

  return(connections)
}
