#' Convert R Markdown to a PDF/\LaTeX or thesis document in German
#'
#' This function serves as wrapper for \code{\link[bookdown]{pdf_book}}, with different
#' default values (e.g., \code{highlight = "default"}), a custom Pandoc \LaTeX template
#' and different knitr default values (e.g., \code{fig.align = "center"}).
#' It is called from the initial R Markdown template file, which should be named `index.Rmd`.
#' The function is based on the `thesis_pdf` function of the
#' \href{https://github.com/ismayc/thesisdown}{thesisdown} package.
#'
#' @param toc logical; \code{TRUE} to include a table of contents in the output.
#' @param toc_depth integer; Depth of headers to include in table of contents. Default set to 5.
#' @param highlight character; syntax highlighting style. Supported styles include "default",
#'        "tango", "pygments", "kate", "monochrome", "espresso", "zenburn", and "haddock".
#'        Pass \code{NULL} to prevent syntax highlighting.
#' @param latex_engine character; \LaTeX engine for producing PDF output. Default is "pdflatex"
#'        as it works well with the German Umlaute.
#' @param pandoc_args Additional command line options to pass to pandoc.
#' @param ... Additional parameters to pass to \code{\link[bookdown]{pdf_book}}.
#'
#' @return A modified \code{\link[rmarkdown]{pdf_document}} based on the UHH LaTeX template.
#'
#' @import bookdown
#' @import knitr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # put in YAML header:
#'  output: UHHthesis::thesis_pdf_de
#' }
thesis_pdf_de <- function(toc = TRUE, toc_depth = 5, highlight = "default",
  latex_engine = "pdflatex", pandoc_args = NULL, ...) {

  base <- bookdown::pdf_book(
    template    = "template.tex",
    toc         = toc,
    toc_depth   = toc_depth,
    highlight   = highlight,
    latex_engine = latex_engine,
    keep_tex    = TRUE,
    pandoc_args = c(pandoc_args, "--top-level-division=section"),
    ...
  )

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment   <- NA

  # To ensure images are in correct place (in line with text)
  base$knitr$opts_chunk$fig.align <- "center"
  # base$knitr$opts_chunk$fig.pos    <- "H"
  # base$knitr$opts_chunk$out.extra  <- ""

  # For tables
  options(knitr.table.format = "latex")
  options(kableExtra.latex.load_packages = FALSE)

  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = fix_envs)
  on.exit(options(bookdown.post.late = old_opt))

  return(base)
}



#' Convert R Markdown to a PDF/\LaTeX or thesis document in English
#'
#' This function serves as wrapper for \code{\link[bookdown]{pdf_book}}, with different
#' default values (e.g., \code{highlight = "default"}), a custom Pandoc \LaTeX template
#' and different knitr default values (e.g., \code{fig.align = "center"}).
#' It is called from the initial R Markdown template file, which should be named `index.Rmd`.
#' The function is based on the `thesis_pdf` function of the
#' \href{https://github.com/ismayc/thesisdown}{thesisdown} package.
#'
#' @param toc logical; \code{TRUE} to include a table of contents in the output.
#' @param toc_depth integer; Depth of headers to include in table of contents. Default set to 5.
#' @param highlight character; syntax highlighting style. Supported styles include "default",
#'        "tango", "pygments", "kate", "monochrome", "espresso", "zenburn", and "haddock".
#'        Pass \code{NULL} to prevent syntax highlighting.
#' @param latex_engine character; \LaTeX engine for producing PDF output. Options are "pdflatex"(default),
#'        "xelatex", "lualatex",  and "tectonic".
#' @param pandoc_args Additional command line options to pass to pandoc.
#' @param ... Additional parameters to pass to \code{\link[bookdown]{pdf_book}}.
#'
#' @return A modified \code{\link[rmarkdown]{pdf_document}} based on the UHH LaTeX template.
#'
#' @import bookdown
#' @import knitr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # put in YAML header:
#'  output: UHHthesis::thesis_pdf_en
#' }
thesis_pdf_en <- function(toc = TRUE, toc_depth = 5, highlight = "default",
  latex_engine = "pdflatex", pandoc_args = NULL, ...) {

  base <- bookdown::pdf_book(
    template    = "template.tex",
    toc         = toc,
    toc_depth   = toc_depth,
    highlight   = highlight,
    latex_engine = latex_engine,
    keep_tex    = TRUE,
    pandoc_args = c(pandoc_args, "--top-level-division=section"),
    ...
  )

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment   <- NA

  # To ensure images are in correct place (in line with text)
  base$knitr$opts_chunk$fig.align <- "center"
  # base$knitr$opts_chunk$fig.pos    <- "H"
  # base$knitr$opts_chunk$out.extra  <- ""

  # For tables
  options(knitr.table.format = "latex")
  options(kableExtra.latex.load_packages = FALSE)

  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = fix_envs)
  on.exit(options(bookdown.post.late = old_opt))

  return(base)
}



#' Convert R Markdown to a Word (docx) thesis document in German
#'
#' This function serves as wrapper for \code{\link[bookdown]{word_document2}}, with different
#' default values (e.g., \code{number_sections = FALSE}), a custom Pandoc Word template
#' and different knitr default values (e.g., \code{fig.align = "center"}).
#' It is called from the initial R Markdown template file, which should be named `index.Rmd`.
#'
#' @param toc logical; \code{TRUE} to include a table of contents in the output.
#' @param toc_depth integer; Depth of headers to include in table of contents. Default set to 5.
#' @param number_sections logical; whether to number section headers: if TRUE, figure/table numbers
#'        will be of the form X.i, where X is the current first-level section number, and i is an
#'        incremental number (the i-th figure/table); if FALSE, figures/tables will be numbered
#'        sequentially in the document from 1, 2, ..., and you cannot cross-reference section
#'        headers in this case.
#' @param highlight character; syntax highlighting style. Supported styles include "default",
#'        "tango", "pygments", "kate", "monochrome", "espresso", "zenburn", and "haddock".
#'        Pass \code{NULL} to prevent syntax highlighting.
#' @param reference_docx character; use the specified file as a style reference in producing a docx file.
#'        The 'uhh-template.docx' template implements most of the standard requirement at the UHH biology
#'        department. If you prefer another template, pass the file name to this argument or simply use
#'        'default' to use your standard Word template.
#' @param pandoc_args Additional command line options to pass to pandoc.
#' @param ... Additional parameters to pass to \code{\link[bookdown]{pdf_book}}.
#'
#' @return A modified \code{\link[rmarkdown]{word_document}} based on the UHH Word
#'  template (soon to come).
#'
#' @import bookdown
#' @import knitr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # put in YAML header:
#'  output: UHHthesis::thesis_word_de
#' }
thesis_word_de <- function(toc = TRUE, toc_depth = 5, number_sections = FALSE,
  highlight = "default", reference_docx = "uhh-template.docx",
  pandoc_args = NULL, ...) {

  base <- bookdown::word_document2(
    toc            = toc,
    toc_depth      = toc_depth,
    highlight      = highlight,
    keep_md        = TRUE,
    reference_docx = reference_docx,
    pandoc_args    = c(pandoc_args, "--top-level-division=section"),
    ...
  )

  # Copied from knitr::render_sweave
  base$knitr$opts_chunk$comment   <- NA

  return(base)
}



#' Convert R Markdown to a Word (docx) thesis document in English
#'
#' This function serves as wrapper for \code{\link[bookdown]{word_document2}}, with different
#' default values (e.g., \code{number_sections = FALSE}), a custom Pandoc Word template
#' and different knitr default values (e.g., \code{fig.align = "center"}).
#' It is called from the initial R Markdown template file, which should be named `index.Rmd`.
#'
#' @param toc logical; \code{TRUE} to include a table of contents in the output.
#' @param toc_depth integer; Depth of headers to include in table of contents. Default set to 5.
#' @param number_sections logical; whether to number section headers: if TRUE, figure/table numbers
#'        will be of the form X.i, where X is the current first-level section number, and i is an
#'        incremental number (the i-th figure/table); if FALSE, figures/tables will be numbered
#'        sequentially in the document from 1, 2, ..., and you cannot cross-reference section
#'        headers in this case.
#' @param highlight character; syntax highlighting style. Supported styles include "default",
#'        "tango", "pygments", "kate", "monochrome", "espresso", "zenburn", and "haddock".
#'        Pass \code{NULL} to prevent syntax highlighting.
#' @param reference_docx character; use the specified file as a style reference in producing a docx file.
#'        The 'uhh-template.docx' template implements most of the standard requirement at the UHH biology
#'        department. If you prefer another template, pass the file name to this argument or simply use
#'        'default' to use your standard Word template.
#' @param pandoc_args Additional command line options to pass to pandoc.
#' @param ... Additional parameters to pass to \code{\link[bookdown]{pdf_book}}.
#'
#' @return A modified \code{\link[rmarkdown]{word_document}} based on the UHH Word
#'  template (soon to come).
#'
#' @import bookdown
#' @import knitr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # put in YAML header:
#'  output: UHHthesis::thesis_word_en
#' }
thesis_word_en <- function(toc = TRUE, toc_depth = 5, number_sections = FALSE,
  highlight = "default", reference_docx = "uhh-template.docx",
  pandoc_args = NULL, ...) {

  base <- bookdown::word_document2(
    toc            = toc,
    toc_depth      = toc_depth,
    highlight      = highlight,
    keep_md        = TRUE,
    reference_docx = reference_docx,
    pandoc_args    = c(pandoc_args, "--top-level-division=section"),
    ...
  )

  # Copied from knitr::render_sweave
  base$knitr$opts_chunk$comment   <- NA

  return(base)
}




### Helper function

fix_envs = function(x) {
  beg_reg <- '^\\s*\\\\begin\\{.*\\}'
  end_reg <- '^\\s*\\\\end\\{.*\\}'
  i3      <- if (length(i1 <- grep(beg_reg, x))) {
    (i1 - 1)[grepl("^\\s*$", x[i1 - 1])]
  }
  i3      <- c(
    i3,
    if (length(i2 <- grep(end_reg, x))) {
      (i2 + 1)[grepl("^\\s*$", x[i2 + 1])]
    }
  )
  if (length(i3)) {
    x <- x[-i3]
  }
  return(x)
}

