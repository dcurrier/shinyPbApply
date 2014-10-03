##############################################################
#' Apply with an auto-generated shiny progress bar.
#'
#' When called within a shiny app, this function will auto-
#' generate a progress bar, update the progress bar as evaluation
#' progresses, and close the progress bar upon completion.
#'
#' @param X  An array, including a matrix that will be iterated
#' over.
#'
#' @param MARGIN A vector giving the subscripts which the
#' function will be applied over. 1 indicates rows, 2 indicates
#' columns, c(1,2) indicates rows and columns.
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
#' @return A vector of results from \code{FUN} applied over \code{X}.
#'
#' @seealso \code{\link{}} and \code{\link{}}
#'
#' @rdname pbapply
#'
#' @examples shiny::shinyApp( ui=fluidPage(titlePanel("Progress Bars!!"), actionButton('go', label="Start")),
#'                            server=shinyServer( function(input, output, session){
#'                                                       observe({
#'                                                           if(input$go > 0)
#'                                                              pbapply(matrix(c(1:20000000), ncol=20000), 1, FUN=summary, session=session, message="My Message")
#'                                                       })
#'                                                 }) )
#' @export
pbapply <- function(X, MARGIN, FUN, session=getDefaultReactiveDomain(), message=NULL, detail=NULL, ...) {
    FUN <- match.fun(FUN)
    d <- dim(X)
    dl <- length(d)
    if (dl == 0L)
        stop("dim(X) must have a positive length")
    ds <- 1L:dl
    if (length(oldClass(X)))
        X <- if (dl == 2)
            as.matrix(X)
        else as.array(X)
    d <- dim(X)
    dn <- dimnames(X)
    s.call <- ds[-MARGIN]
    s.ans <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans <- d[MARGIN]
    dn.call <- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    d2 <- prod(d.ans)
    if (d2 == 0L) {
        newX <- array(vector(typeof(X), 1L), dim = c(prod(d.call),
            1L))
        ans <- FUN(if (length(d.call) < 2L)
            newX[, 1]
        else array(newX[, 1L], d.call, dn.call), ...)
        return(if (is.null(ans)) ans else if (length(d.ans) <
            2L) ans[1L][-1L] else array(ans, d.ans, dn.ans))
    }
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)

    pb <- shiny::Progress$new(session, min=0, max=1)
    pb$set(message=message, detail=detail)

    if (length(d.call) < 2L) {
        if (length(dn.call))
            dimnames(newX) <- c(dn.call, list(NULL))
        for (i in 1L:d2) {
            tmp <- FUN(newX[, i], ...)
            if (!is.null(tmp))
                ans[[i]] <- tmp

            pb$set(value=i/d2)

        }
    }
    else for (i in 1L:d2) {
        tmp <- FUN(array(newX[, i], d.call, dn.call), ...)
        if (!is.null(tmp))
            ans[[i]] <- tmp

        pb$set(value=i/d2)

    }

    pb$close()

    ans.list <- is.recursive(ans[[1L]])
    l.ans <- length(ans[[1L]])
    ans.names <- names(ans[[1L]])
    if (!ans.list)
        ans.list <- any(unlist(lapply(ans, length)) != l.ans)
    if (!ans.list && length(ans.names)) {
        all.same <- sapply(ans, function(x) identical(names(x),
            ans.names))
        if (!all(all.same))
            ans.names <- NULL
    }
    len.a <- if (ans.list)
        d2
    else length(ans <- unlist(ans, recursive = FALSE))
    if (length(MARGIN) == 1L && len.a == d2) {
        names(ans) <- if (length(dn.ans[[1L]]))
            dn.ans[[1L]]
        return(ans)
    }
    if (len.a == d2)
        return(array(ans, d.ans, dn.ans))
    if (len.a && len.a%%d2 == 0L) {
        if (is.null(dn.ans))
            dn.ans <- vector(mode = "list", length(d.ans))
        dn.ans <- c(list(ans.names), dn.ans)
        return(array(ans, c(len.a%/%d2, d.ans), if (!all(sapply(dn.ans,
            is.null))) dn.ans))
    }
    return(ans)
}
