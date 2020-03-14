#' Send Message
#'
#' Sends a message using the username provided to the channel of the webhook provided
#'
#' @param message Character string of message to send
#' @param conn Discord Connection Object containing Webhook and Username
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' send_message("Hello World!")
#' }
#'
#' @seealso \code{\link{send_file}}, \code{\link{send_plot_code}}, \code{\link{send_current_ggplot}}, \code{\link{send_console}}
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
#' @param conn Discord Connection Object containing Webhook and Username
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' send_file('image.jpg')
#' }
#' @seealso
#' \code{\link{send_file}}, \code{\link{send_plot_code}}, \code{\link{send_current_ggplot}}, \code{\link{send_console}}
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

#' Send Plot Code
#'
#' Runs and saves the plot code provided. In order to save and send the file, a random name
#' for the image will be generated and saved temporarily.
#'
#' @param ... Plot code to run and save
#' @param conn Discord Connection Object containing Webhook and Username
#' @param filename Optional - Filepath indicating where to save image; Provide to manually override the temporary directory and filename
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' send_plot_code(plot(rnorm(5), rnorm(5), conn = conn_obj))()
#' }
#' @seealso
#' \code{\link{send_current_ggplot}}, \code{\link{send_file}}, \code{\link{send_message}}, \code{\link{send_console}}
send_plot_code <- function(..., conn = get_default_discord_connection(), filename = tempfile(pattern = 'discordr', fileext = '.png')){

  res <- NULL

  if(length(list(...)) == 0){
    stop('No plot code provided.')
  }
  else {
      grDevices::png(filename = filename)

      # code heavily inspired by capture.output
      pf = parent.frame()
      args <- substitute(list(...))[-1L]
      evalVis <- function(expr) withVisible(eval(expr, pf))
      for(i in 1:length(substitute(list(...))[-1L])){
        expr <- args[[i]]
        tmp <- switch(mode(expr),
                      expression = lapply(expr, evalVis),
                      call = ,
                      name = list(evalVis(expr)),
                      stop("bad argument"))
      }

      grDevices::dev.off()

    if(file.exists(filename)){
      body_data <- list(content = httr::upload_file(filename),
                        username = conn$username)

      res <- httr::POST(url = conn$webhook,
                       body = body_data,
                       encode = "multipart")
    }
    else {
      stop('Plot output not saved.')
    }

    invisible(res)
  }
}

#' Send Current GGPlot
#'
#' Send images of GGplot which are currently shown in the plots pane of RStudio or elsewhere.
#'
#' @param conn Discord Connection Object containing Webhook and Username
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
#' \code{\link{send_plot_code}}, \code{\link{send_file}}, \code{\link{send_message}}, \code{\link{send_console}}
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
#' @param filename Alternative path to route console output; defaults to tempfile
#' @param conn Discord Connection Object containing Webhook and Username
#' @param tibble_formatting By Default this is set to False, Use this option to format linebreaks specifically on newlines for large tibbles
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' send_console(2 + 2)
#' }
#' @seealso
#' \code{\link{send_message}}, \code{\link{send_file}}, \code{\link{send_plot_code}}, \code{\link{send_current_ggplot}}
send_console <- function(..., conn = get_default_discord_connection(), filename = tempfile(pattern = 'discordr'), tibble_formatting = FALSE){
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
      if(!tibble_formatting){
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
      else {
        console_output <- readChar(filename, file.info(filename)$size)
        res <- list()

        new_line_locations <- stringr::str_locate_all(console_output, '[\n]')
        num_breaks <- ceiling(max(new_line_locations[[1]][,1]) / 1500)
        break_locations <- tibble::tibble('index' = new_line_locations[[1]][,1], 'mod_value' = new_line_locations[[1]][,1] %% 1500)
        break_indices <- sort(dplyr::arrange(break_locations, dplyr::desc(!!dplyr::sym('mod_value')))$index[1:(num_breaks - 1)])
        actual_break_indices <- tibble::tibble(start = c(0, break_indices), stop = c(break_indices, break_indices[length(break_indices)] + 1500))

        for(row_index in 1:nrow(actual_break_indices)){
          formatted_console_substr <- paste('```', substr(console_output, actual_break_indices$start[row_index], actual_break_indices$stop[row_index] - 2), '```')
          res[[row_index]] <- send_message(formatted_console_substr, conn = conn)
          Sys.sleep(1)
        }
      }
    }
  }

  invisible(res)
}

#' Send R Objects
#'
#' @param ... Single or Multiple R Objects to be contained within a single RData file
#' @param filename Default is a random string saved in the temporary directory; change this if you would like the RData file to be human-readable and in a different location.
#' @param conn Discord Connection Object containing Webhook and Username
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
#' @param conn Discord Connection Object containing Webhook and Username
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' tex_string <- "$\\int^a_b \\frac{1}{3}x^3 dx$"
#' send_tex(tex_string, conn = conn_obj)
#' }

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
