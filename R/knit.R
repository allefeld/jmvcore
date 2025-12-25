
#' @method knit_print Table
#' @export
#' @importFrom knitr knit_print
knit_print.Table <- function(table, ...) {
    table <- jmvcore:::fold(table)
    col_names <- unname(unlist(
        lapply(
            table$columns,
            function(column) {
                if (column$visible) column$title
            }
        )
    ))
    md <- knitr::kable(
        as.data.frame(table),
        col.names = col_names,
        row.names = FALSE,
        caption = paste(table$title, "{.jamovi}")
    )
    knitr::asis_output(paste(md, collapse = "\n"))
}

#' @method knit_print Image
#' @export
#' @importFrom knitr knit_print
knit_print.Image <- function(image, ...) {
    opts <- knitr::opts_current$get()
    print(opts)
    # Quarto setting `fig-format` -> knitr options `dev` & `fig.retina`
    # null:      "png"  2
    # "retina":  "png"  2
    # "png":     "png"  1
    # "jpeg":    "jpeg" 1
    # "svg":     "svg"  1
    # "pdf":     "pdf"  1
    if (opts$fig.retina == 2) {
        # Should this be high-resolution png instead?
        # But pngs created by `Image$saveAs` are always high-resolution.
        fileext <- ".svg"
    } else {
        # `Image$saveAs` does not support ".jpeg"
        fileext <- switch(opts$dev,
            png    = ".png",
            svg    = ".svg",
            pdf    = ".pdf",
            stop("Unsupported device: ", opts$dev)
        )
    }
    # My guess how knitr creates filenames for plots
    filename <- file.path(
        opts$fig.path,
        paste0(opts$label, "-", knitr:::plot_counter(), fileext)
    )
    image$saveAs(filename)
    knitr::asis_output(paste0(
        "![](", filename, "){.jamovi width=", image$width, "}\n\n")
    )
}

#' @method knit_print Group
#' @export
#' @importFrom knitr knit_print
knit_print.Group <- function(group, depth = 1, ...) {
    md <- unname(unlist(
        lapply(
            group$items,
            function(item) {
                if (item$visible) {
                    if (inherits(item, c("Group", "Array"))) {
                        knit_print.Group(item, depth = depth + 1)
                    } else {
                        knitr::knit_print(item)
                    }
                }
            }
        )
    ))
    md <- c(paste(strrep("#", depth), group$title, "{.jamovi}"), md)
    knitr::asis_output(paste(md, collapse = "\n\n"))
}

#' @method knit_print Array
#' @export
#' @importFrom knitr knit_print asis_output
knit_print.Array <- knit_print.Group
