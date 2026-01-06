.onLoad <- function(libname, pkgname) {
    # avoid loading messages to pop up on use
    if (!requireNamespace("flextable", quietly = TRUE)) return()
    if (!requireNamespace("ftExtra", quietly = TRUE)) return()
    if (!requireNamespace("knitr", quietly = TRUE)) return()
    knitr::opts_chunk$set(
        # Jamovi UI settings as knitr options
        # Number format can be 3–5 significant figures
        # or 2–5 & 16 decimal places
        # Default: 3 significant figures
        jmv.numfmt = list(format = "sf",  digits = 3),
        # Separate number format for p-values
        # Default: 3 decimal places
        jmv.pvalfmt = list(format = "dp", digits = 3),
        # Decimal symbol can be Dot or Comma
        jmv.decmark = ".",
        # Additional: Use nonbreaking spaces in the table?
        jmv.nobreak = FALSE
    )
}


#' @export
#' @importFrom knitr knit_print
emit_css <- function() {
    knitr::asis_output(
        paste(c(
            "```{=html}",
            "<style>",
            readLines(system.file("style.css", package = "jmvcore")),
            "</style>",
            "```"
        ), collapse = "\n")
    )
}


#' @method knit_print Table
#' @export
#' @importFrom knitr knit_print
knit_print.Table <- function(input, ...) {
    if (!requireNamespace("knitr", quietly = TRUE)) {
       stop("knitr is required to knit this object.", call. = FALSE)
    }
    if (!requireNamespace("flextable", quietly = TRUE)) {
        stop("flextable is required to knit this object.", call. = FALSE)
    }
    if (!requireNamespace("ftExtra", quietly = TRUE)) {
        stop("ftExtra is required to knit this object.", call. = FALSE)
    }
    opts <- knitr::opts_current$get()

    # In the input table, each column is uniquely identified by its $name;
    # but some have the same name except for a bracketed part at the end,
    # indicating they belong together.
    # We construct the rowPlan, i.e. a named list
    # where the names are the unique debracketed names in sequence
    # and the values are vectors of indices into the input table columns.

    # determine names and indices of visible columns
    visible <- vapply(
        input$columns,
        function(x) x$visible,
        logical(1),
        USE.NAMES=FALSE
    )
    names <- vapply(
        input$columns[visible],
        function(x) x$name,
        character(1),
        USE.NAMES=FALSE
    )
    indices <- which(visible)
    # create debracketed names
    debracketed <- sub("\\[.*\\]$", "", names)
    # folded names are unique debracketed names
    foldedNames <- unique(debracketed)
    # collect indices for each debracketed name in a list of vectors
    rowPlan <- split(indices, factor(debracketed, foldedNames))
    # create a matrix of empty strings to hold output body
    nFolds <- max(sapply(rowPlan, length))      # num columns of input table
    rowCount <- input$rowCount                  # num rows of input table
    nCols <- length(foldedNames)                # num columns of output body
    nRows <- rowCount * nFolds                  # num rows of output body
    outBody <- matrix("", nrow = nRows, ncol = nCols)

    # fill in outBody and collect information on titles,
    # superTitles, and combined cells
    caption <- input$title
    titles <- c()
    superTitles <- c()
    combineBelowColumns <- c()
    numericColumns <- c()
    footnoteLetters <- strsplit("ᵃᵇᶜᵈᵉᶠᶢʰⁱʲᵏˡᵐⁿᵒᵖ", "")[[1]]
    for (colNo in seq(nCols)) {
        foldedName <- foldedNames[colNo]
        foldedIndices <- rowPlan[[foldedName]]
        # Information about a column should be identical across
        # `foldedIndices`, but that is not always the case. We take it from
        # the last input column because that seems to be what Jamovi does.
        lastColumn <- input$columns[[foldedIndices[length(foldedIndices)]]]
        type <- lastColumn$type
        formats <- strsplit(lastColumn$format, ",", fixed = TRUE)[[1]]
        titles <- c(titles, lastColumn$title)
        superTitles <- c(
            superTitles,
            if (is.null(lastColumn$superTitle)) "" else lastColumn$superTitle
        )
        if (lastColumn$combineBelow) {
            combineBelowColumns <- c(combineBelowColumns, colNo)
        }
        if (type == "number") {
            numericColumns <- c(numericColumns, colNo)
        }
        for (fold in seq_along(foldedIndices)) {
            index <- foldedIndices[fold]
            column <- input$columns[[index]]
            # extract and format values
            if (type == "number") {
                # extract numeric values
                val <- vapply(
                    column$cells,
                    function(cell) cell$value,
                    numeric(1)
                )
                if ("pc" %in% formats) {
                    val <- 100 * val            # correct?
                }
                # determine decimal places
                if ("pvalue" %in% formats) {
                    numfmt <- opts$jmv.pvalfmt
                } else {
                    numfmt <- opts$jmv.numfmt
                }
                if (numfmt$format == "dp") {
                    dp <- numfmt$digits
                } else {
                    sf <- numfmt$digits
                    v <- suppressWarnings(log10(abs(val)))
                    dp <- max(sf - ceiling(min(v[is.finite(v)])), 0)
                }
                vstr <- trimws(formatC(val, format = "f", digits = dp))
                # <.0… notation for p-values
                if ("pvalue" %in% formats && numfmt$format == "dp") {
                    thr <- 10 ^ -numfmt$digits
                    vstr[val < thr] <- paste0(
                        "<",
                        trimws(formatC(thr, format = "f", digits = dp))
                    )
                }
                if ("zto" %in% formats) {
                    # quantity between 0 and 1: remove single 0 before .
                    vstr <- gsub(
                        "(?<![0-9])0\\.",
                        ".",
                        vstr,
                        perl = TRUE
                    )
                }
                # proper minus sign
                vstr <- gsub("-", "−", vstr)
                # correct decimal mark
                vstr <- gsub("\\.", opts$jmv.decmark, vstr)
            } else {
                # extract text values
                vstr <- vapply(
                    column$cells,
                    function(cell) cell$value,
                    character(1)
                )
                # spaces to non-breaking spaces
                if (opts$jmv.nobreak) {
                    vstr <- gsub(" ", "\u00A0", vstr)
                }
            }
            # fill values into output table, adding footnotes
            for (rowNo in seq(rowCount)) {
                outRow <- (rowNo - 1) * nFolds + fold
                cell <- input$columns[[index]]$cells[[rowNo]]
                value <- vstr[rowNo]
                # footnote letters
                if (length(cell$footnotes) > 0) {
                    fnl <- paste(footnoteLetters[
                        sort(match(cell$footnotes, input$footnotes))
                    ], collapse = "")
                    value <- paste0(value, fnl)
                }
                outBody[outRow, colNo] <- value
            }
        }
    }
    if (opts$jmv.nobreak) {
        # spaces to non-breaking spaces
        titles <- gsub(" ", "\u00A0", titles)
        superTitles <- gsub(" ", "\u00A0", superTitles)
    }

    # widths are in pt = 1.333333 px
    px <- 0.75
    # border styles
    borderNone <- officer::fp_border(
        style = "none",
        color = "#333333",
        width = 0 * px
    )
    borderThin <- officer::fp_border(
        style = "solid",
        color = "#333333",
        width = 1 * px
    )
    borderThick <- officer::fp_border(
        style = "solid",
        color = "#333333",
        width = 2 * px
    )
    # padding styles
    # <td>s have a padding of 1 px which needs to be subtracted
    paddCaption <- officer::fp_par(
        padding.top = (4 - 1) * px,
        padding.right = (8 - 1) * px,
        padding.bottom = (4 - 1) * px,
        padding.left = 0 * px
    )
    paddHeader <- officer::fp_par(
        padding.top = (4 - 1) * px,
        padding.right = (8 - 1) * px,
        padding.bottom = (4 - 1) * px,
        padding.left = (8 - 1) * px
    )
    paddNumeric <- officer::fp_par(
        padding.top = (4 - 1) * px,           # should be 8 at first in fold
        padding.right = (20 - 1) * px,
        padding.bottom = (4 - 1) * px,
        padding.left = (8 - 1) * px
    )
    paddText <- officer::fp_par(
        padding.top = (4 - 1) * px,           # should be 8 at first in fold
        padding.right = (8 - 1) * px,
        padding.bottom = (4 - 1) * px,
        padding.left = (8 - 1) * px
    )
    paddFootFirst <- officer::fp_par(
        padding.top = (6 - 1) * px,
        padding.right = (8 - 1) * px,
        padding.bottom = (2 - 1) * px,
        padding.left = (8 - 1) * px
    )
    paddFootLater <- officer::fp_par(
        padding.top = (2 - 1) * px,
        padding.right = (8 - 1) * px,
        padding.bottom = (2 - 1) * px,
        padding.left = (8 - 1) * px
    )

    # construct flextable
    # body
    output <- flextable::flextable(as.data.frame(outBody))
    output <- flextable::style(
        output,
        part = "body",
        pr_p = paddText
    )
    output <- flextable::style(
        output,
        part = "body",
        j = numericColumns,
        pr_p = paddNumeric
    )
    output <- flextable::align(
        output,
        part = "body",
        j = numericColumns,
        align = "right"
    )
    output <- flextable::style(
        output,
        part = "body",
        i = nRows,
        pr_c = officer::fp_cell(
            border.top = borderNone,
            border.bottom = borderThick
        )
    )
    # row headers
    output <- flextable::merge_v(output, j = combineBelowColumns)
    output <- flextable::valign(
        output,
        part = "body",
        j = combineBelowColumns,
        valign = "top"
    )
    # column headers
    output <- flextable::set_header_labels(output, values = titles)
    for (j in seq(nCols)) {
        output <- flextable::style(
            output,
            part="head",
            i = 1,
            j = j,
            pr_c = officer::fp_cell(
                border.top = borderThin,
                border.bottom = borderThin
            )
        )
    }
    # "super" column headers
    if (any(nchar(superTitles) > 0)) {
        output <- flextable::add_header_row(output, values = superTitles)
        output <- flextable::merge_h(output, i = 1, part="head")
        ind <- which(superTitles == "")
        for (j in ind) {
            output <- flextable::style(
                output,
                part="head",
                i = 1,
                j = j,
                pr_c = officer::fp_cell(
                    border.top = borderThin,
                    border.bottom = borderNone
                )
            )
            output <- flextable::style(
                output,
                part="head",
                i = 2,
                j = j,
                pr_c = officer::fp_cell(
                    border.top = borderNone,
                    border.bottom = borderThin
                )
            )
        }
    }
    output <- flextable::style(
        output,
        part = "head",
        pr_p = paddHeader
    )
    output <- flextable::align(
        output,
        part = "head",
        align = "center"
    )
    # caption as header line
    output <- flextable::add_header_lines(output, caption)
    output <- flextable::style(
        output,
        part = "head",
        i = 1,
        j = 1,
        pr_c = officer::fp_cell(
            border.top = borderNone,
            border.bottom = borderThin
        ),
        pr_p = paddCaption
    )
    # notes
    for (key in names(input$notes)) {
        note <- input$notes[[key]]$note
        # proper minus sign
        note <- gsub("-", "−", note)
        output <- flextable::add_footer_lines(
            output,
            values = ftExtra::as_paragraph_md(
                paste("<em>Note.</em>", note),
                .from="html"
            )
        )
    }
    # footnotes
    for (i in seq_along(input$footnotes)) {
        output <- flextable::add_footer_lines(
            output,
            values = paste(footnoteLetters[i], input$footnotes[i])
        )
    }
    output <- flextable::style(
        output,
        part="foot",
        pr_p = paddFootLater
    )
    output <- flextable::style(
        output,
        part="foot",
        i = 1,
        pr_p = paddFootFirst
    )
    # font and font size
    output <- flextable::font(output, fontname = "Arial", part = "all")
    output <- flextable::fontsize(output, size = 9, part = "all")
    # font size is in pt

    # output format-specific table code within div
    knitr::asis_output(paste(
        "::: {.jamovi .jamovi-table}\n",
        knitr::knit_print(output),
        ":::\n",
        sep=""
    ))
}

#' @method knit_print Image
#' @export
#' @importFrom knitr knit_print
knit_print.Image <- function(image, ...) {
    if (!requireNamespace("knitr", quietly = TRUE)) {
        stop("knitr is required to knit this object.", call. = FALSE)
    }
    opts <- knitr::opts_current$get()
    # Quarto setting `fig-format` -> knitr options `dev` & `fig.retina`
    # null:      "png"  2
    # "retina":  "png"  2
    # "png":     "png"  1
    # "jpeg":    "jpeg" 1
    # "svg":     "svg"  1
    # "pdf":     "pdf"  1
    # Not sure how to implement "retina" considering that `Image$saveAs`
    # always creates high-resolution pngs.
    fileext <- switch(opts$dev,
        png    = ".png",
        # `Image$saveAs` does not support ".jpeg"
        svg    = ".svg",
        pdf    = ".pdf",
        stop("Unsupported device: ", opts$dev)
    )
    # My guess how knitr creates filenames for plots
    # Depends on unexported knitr function `plot_counter`!
    filename <- file.path(
        opts$fig.path,
        paste0(opts$label, "-", knitr:::plot_counter(), fileext)
    )
    image$saveAs(filename)
    knitr::asis_output(paste0(
        "![](", filename, "){.jamovi .jamovi-image width=", image$width, "}\n\n")
    )
}

#' @method knit_print Group
#' @export
#' @importFrom knitr knit_print
knit_print.Group <- function(group, depth = 1, class = "jamovi-group", ...) {
    if (!requireNamespace("knitr", quietly = TRUE)) {
        stop("knitr is required to knit this object.", call. = FALSE)
    }
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
    md <- c(
        paste0(
            strrep("#", depth),
            " ",
            group$title,
            " ",
            "{.jamovi .",
            class,
            " .jamovi-h",
            depth,
            "}"
        ),
        md)
    knitr::asis_output(paste(md, collapse = "\n\n"))
}


#' @method knit_print Array
#' @export
#' @importFrom knitr knit_print
knit_print.Array <- function(group, depth = 1, class = "jamovi-array", ...) {
    knit_print.Group(group, depth = depth, class = class)
}
