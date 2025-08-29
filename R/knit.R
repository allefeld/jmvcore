
#' @method knit_print Table
#' @export
#' @importFrom knitr asis_output kable
knit_print.Table <- function(table, ...) {
    table <- jmvcore:::fold(table)
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
        caption = paste(table$title, "{.jamovi}")
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
knit_print.Group <- function(group, depth = 1, ...) {
    md <- unname(unlist(
        lapply(
            group$items,
            function(item) {
                if (item$visible) {
                    if (inherits(item, c("Group", "Array"))) {
                        knit_print.Group(item, depth = depth + 1)
                    } else {
                        knit_print(item)
                    }
                }
            }
        )
    ))
    md <- c(paste(strrep("#", depth), group$title, "{.jamovi}"), md)
    asis_output(paste(md, collapse = "\n\n"))
}

#' @method knit_print Array
#' @export
#' @importFrom knitr knit_print asis_output
knit_print.Array <- knit_print.Group
