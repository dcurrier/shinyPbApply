##############################################################
#' Sapply with an auto-generated shiny progress bar.
#'
#' When called within a shiny app, this function will auto-
#' generate a progress bar, update the progress bar as evaluation
#' progresses, and close the progress bar upon completion.
#'
#' @param X  a vector (atomic or list) or an expression object.
#' Other objects (including classed objects) will be coerced by
#' \code{base::as.list}.
#'
#' @param FUN The function to apply to the elements of \code{X}.
#'
#' @param session The Shiny session object, as provided by
#' shinyServer to the server function.
#'
#' @param message A single-element character vector; the message
#' to be displayed to the user, or \code{NULL} to hide the current
#' message (if any).
#'
#' @param detail A single-element character vector; the detail
#' message to be displayed to the user, or \code{NULL} to hide the
#' current detail message (if any). The detail message will be
#' shown with a de-emphasized appearance relative to \code{message}.
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @param simplify logical or character string; should the result
#' be simplified to a vector, matrix or higher dimensional array if
#' possible? The default value, \code{TRUE}, returns a vector or
#' matrix if appropriate, whereas if \code{simplify = "array"} the
#' result may be an array of “rank” \code{=length(dim(.)}) one
#' higher than the result of \code{FUN(X[[i]])}.
#'
#' @param USE.NAMES logical; use names if the first \code{...} argument
#' has names, or if it is a character vector, use that character
#' vector as the names.
#'
#' @return A list of results from \code{FUN} applied over \code{X}.
#'
#' @seealso \code{\link{pbapply}} and \code{\link{pblapply}}
#'
#' @rdname pbsapply
#'
#' @examples shiny::shinyApp( ui=fluidPage(titlePanel("Progress Bars!!"),
#'                                         fluidRow( column(3, actionButton('go', label="Start")),
#'                                                   column(3, verbatimTextOutput('length')))  ),
#'                            server=shinyServer( function(input, output, session){
#'                                                       output$length = renderText({
#'                                                           if(input$go > 0){
#'                                                              x=pbsapply(c(1:20000), FUN=jitter, session=session, message="My Message", simplify=TRUE, USE.NAMES=FALSE)
#'                                                              paste0('Result length: ', length(x))
#'                                                              }
#'                                                       })
#'                                                 }) )
#' @export
pbsapply <- function (X, FUN, session=getDefaultReactiveDomain(), message=NULL, detail=NULL, ..., simplify = TRUE, USE.NAMES = TRUE) {
    FUN <- match.fun(FUN)
    answer <- pblapply(X, FUN, session,  message, detail, ...)
    if (USE.NAMES && is.character(X) && is.null(names(answer)))
        names(answer) <- X
    if (simplify && length(answer) && length(common.len <- unique(unlist(lapply(answer,
        length)))) == 1L) {
        if (common.len == 1L)
            unlist(answer, recursive = FALSE)
        else if (common.len > 1L)
            array(unlist(answer, recursive = FALSE), dim = c(common.len,
                length(X)), dimnames = if (!(is.null(n1 <- names(answer[[1L]])) &
                is.null(n2 <- names(answer))))
                list(n1, n2))
        else answer
    }
    else answer
}

