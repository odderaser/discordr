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
#' \dontrun{
#' send_message("Hello World!")
#' }
#'
#' @seealso \code{\link{send_file}}, \code{\link{send_current_plot}}, \code{\link{send_current_ggplot}}, \code{\link{send_console}}
send_message <- function(message, conn = get_default_discord_connection()){
  res <- NULL

  if(nchar(message) > 0){
    body_data <- list(content = message,
                      username = conn$username)

    res <- httr::POST(url = conn$webhook,
                     body = body_data,
                     encode = "json")
  }
  else {
    message('Empty message provided.')
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
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' send_file('image.jpg')
#' }
#' @seealso
#' \code{\link{send_file}}, \code{\link{send_current_plot}}, \code{\link{send_current_ggplot}}, \code{\link{send_console}}
send_file <- function(filename, conn = get_default_discord_connection()){
  res <- NULL

  if(file.exists(filename)){
    body_data <- list(content = httr::upload_file(filename),
                      username = conn$username)

    res <- httr::POST(url = conn$webhook,
                     body = body_data,
                     encode = 'multipart')

    invisible(res)
  }
  else {
    stop("File not found.")
  }

  invisible(res)
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
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' send_current_plot()
#' }
#' @seealso
#' \code{\link{send_current_ggplot}}, \code{\link{send_file}}, \code{\link{send_message}}, \code{\link{send_console}}
send_current_plot <- function(conn = get_default_discord_connection(), filename = tempfile(pattern = 'discordr', fileext = '.png')){

  image_dimensions <- grDevices::dev.size("px")

  if(Sys.info()[['sysname']] == 'Darwin'){
    print(dev.cur())
    grDevices::quartz.save(file = filename, width = image_dimensions[1], height = image_dimensions[2])
    grDevices::dev.off()
  }
  else {
    print(dev.cur())
    grDevices::dev.copy(grDevices::png, filename = filename, width = image_dimensions[1], height = image_dimensions[2])
    grDevices::dev.off()
  }

  if(file.exists(filename)){
    body_data <- list(content = httr::upload_file(filename),
                      username = conn$username)

    res <- httr::POST(url = conn$webhook,
                     body = body_data,
                     encode = "multipart")
  }
  else {
    stop('No plots found.')
  }

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
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' send_current_ggplot()
#' }
#' @seealso
#' \code{\link{send_current_plot}}, \code{\link{send_file}}, \code{\link{send_message}}, \code{\link{send_console}}
send_current_ggplot <- function(conn = get_default_discord_connection(), filename = tempfile(pattern = 'discordr', fileext = '.png')){

  if(!is.null(ggplot2::last_plot())){
    ggplot2::ggsave(filename)
  }
  else {
    stop("No ggplots found in Plots pane.")
  }

  body_data <- list(content = httr::upload_file(filename),
                    username = conn$username)

  res <- httr::POST(url = conn$webhook,
                   body = body_data,
                   encode = "multipart")

  invisible(res)
}

#' Send Console Output
#'
#' This functions accepts an expression whose console output you would like to send to the specified discord channel
#'
#' @param ... A single or set of expressions to be evaluated for console output
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#' @param filename Optional - Save console output to a file, defaults to a temporary file
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' send_console(2 + 2)
#' }
#' @seealso
#' \code{\link{send_message}}, \code{\link{send_file}}, \code{\link{send_current_plot}}, \code{\link{send_current_ggplot}}
send_console <- function(..., conn = get_default_discord_connection(), filename = tempfile(pattern = 'discordr')){
  res <- NULL

  if(length(list(...)) == 0){
    message('No calls provided.')
  }
  else {

    sink(file = filename, split = TRUE)

    # code heavily inspired by capture.output
    pf = parent.frame()
    args <- substitute(list(...))[-1L]
    evalVis <- function(expr) withVisible(eval(expr, pf))
    for(i in 1:seq_along(args)){
      expr <- args[[1]]
      tmp <- switch(mode(expr),
                    expression = lapply(expr, evalVis),
                    call = ,
                    name = list(evalVis(expr)),
                    numeric = list(evalVis(expr)),
                    stop("bad argument"))
      for (item in tmp) if (item$visible)
        print(item$value)
    }

    sink()

    if(file.info(filename)$size == 0){
      message('No console output from provided functions.')
    }
    else {

      console_output <- readChar(filename, file.info(filename)$size)

      nchar_split <- 1500
      if(nchar(console_output) > nchar_split){
        split_console_output <- substring(console_output, seq(1, nchar(console_output), nchar_split), seq(nchar_split, nchar(console_output), nchar_split))
        res <- list()

        for(console_output_index in 1:length(split_console_output)){
          current_console_output_split <- paste('```', split_console_output[console_output_index], '```', sep = '\n')
          res[[console_output_index]] <- send_message(current_console_output_split, conn = conn)

          #avoid timeout errors
          Sys.sleep(1)
        }
      }
      else {
        console_output <- paste('```', console_output, '```', sep = '\n')
        res <- send_message(console_output, conn = conn)
      }
    }
  }

  invisible(res)
}

#' Send R Objects
#'
#' @param ... Single or Multiple R Objects to be contained within a single RData file
#' @param filename Default is a random string saved in the temporary directory; change this if you would like the RData file to be human-readable and in a different location.
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(1,2,3,4,5)
#' y <- matrix(rep(0, 4), rows = 2, cols = 2)
#' send_robject(x, y, filename = 'test_data.RData')
#' }
send_robject <- function(..., filename = tempfile(pattern = 'discordr', fileext = '.RData'), conn = get_default_discord_connection()){
  res <- NULL

  if(length(list(...)) > 0){
    save(..., file = filename)
    res <- send_file(filename, conn = conn)
  }
  else {
    message('No objects provided.')
  }

  invisible(res)
}

#' Send Rendered Latex Images
#'
#' In order to use this function, you will need to install the texPreview package manually.
#'
#' @param tex_string Character string of compileable latex code. Ensure you are using double slashes ('\\') for commands.
#' @param filename Default is a random string saved in the temporary directory; change this if you would like the RData file to be human-readable and in a different location.
#' @param density Density of latex image to be saved. Default is 250.
#' @param username Username to use for sender of message, defaults to environment set username
#' @param webhook Webhook to which the message should be sent, defaults to environment set webhook
#'
#' @return None
#' @export
#'
#' @examples
send_tex <- function(tex_string, filename = tempfile(pattern = 'discordr'), density = 250, conn = get_default_discord_connection()){

  res <- NULL

  if(nchar(tex_string) == 0){
    message("No tex string provided.")
  }
  else {
    texPreview::tex_preview(tex_string, stem = basename(filename), fileDir = dirname(filename), imgFormat = 'png', density = density)
    res <- send_file(filename = paste(filename, '.png', sep = ''), conn = conn)
  }

  invisible(res)
}
