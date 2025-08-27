
#' @method knit_print Table
#' @export
#' @importFrom knitr asis_output kable
knit_print.Table <- function(table, ...) {
    col.names <- unname(unlist(
        lapply(
            table$columns,
            function(column) {
                if (column$visible) column$title
            }
        )
    ))
    md <- kable(
        as.data.frame(table),
        col.names = col.names,
        row.names = FALSE,
        caption = table$title
    )
    asis_output(paste(md, collapse = "\n"))
}

#' @method knit_print Image
#' @export
#' @importFrom knitr knit_print
knit_print.Image <- function(image, ...) {
    'Image goes here'
}

#' @method knit_print Group
#' @export
#' @importFrom knitr knit_print asis_output
knit_print.Group <- function(group, ...) {
    md <- unname(unlist(
        lapply(
            group$items,
            function(item) {
                if (item$visible) knit_print(item)
            }
        )
    ))
    asis_output(paste(md, collapse = "\n\n"))
}

#' @method knit_print Array
#' @export
#' @importFrom knitr knit_print
knit_print.Array <- knit_print.Group
