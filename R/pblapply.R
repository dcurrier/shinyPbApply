##############################################################
#' Lapply with an auto-generated shiny progress bar.
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
#' @return A list of results from \code{FUN} applied over \code{X}.
#'
#' @seealso \code{\link{}} and \code{\link{}}
#'
#' @rdname pblapply
#'
#' @examples shiny::shinyApp( ui=fluidPage(titlePanel("Progress Bars!!"),
#'                                         fluidRow( column(3, actionButton('go', label="Start")),
#'                                                   column(3, verbatimTextOutput('length')))  ),
#'                            server=shinyServer( function(input, output, session){
#'                                                       output$length = renderText({
#'                                                           if(input$go > 0) {
#'                                                              x=pblapply(c(1:20000), FUN=jitter, session=session, message="My Message")
#'                                                              paste0('Result length: ', length(x))
#'                                                              }
#'                                                       })
#'                                                 }) )
#' @export
pblapply <- function (X, FUN, session=getDefaultReactiveDomain(), message=NULL, detail=NULL, ...) {
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    B <- length(X)
    if (!(interactive() && !is.null(session) && B >= 1))
        return(lapply(X, FUN, ...))
    pb <- shiny::Progress$new(session, min=0, max=1)
    showDetail = if(!is.null(detail) && detail == "percent") paste0("0%") else detail
    pb$set(message=message, detail=showDetail)
    int <- if(B<1000) c(1:B)/100 else seq(0.01, 1, by=0.01)*B
    rval <- vector("list", B)
    for (i in 1:B) {
        rval[[i]] <- FUN(X[[i]], ...)
        showDetail = if(!is.null(detail) && detail == "percent") paste0(i/B*100, "%") else detail
        if(i %in% int) pb$set(value=i/B, detail=showDetail)
    }
    pb$close()
    return(rval)
}
