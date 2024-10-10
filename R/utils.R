#' Create a Directory
#'
#' This helper function creates a directory if it doesn't already exist.
#'
#' @param folder A filepath for the new directory
#'
#' @return NULL
#'
#' @noRd
create_dir <- function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
}

#' Expand CSAFE Handwriting Database Document Names
#'
#' Documents from the CSAFE Handwriting Database contain the writer, session,
#' prompt, and repetition in the file name:
#' <writer>_<session>_<prompt>_<repetition>. Create writer, session, prompt, and
#' repetition columns by extracting these items from the document file names.
#' The original document name column will be kept.
#'
#' @param df A data frame of containing at least one column of document names
#'   from the CSAFE Handwriting Database.
#' @param docname_col The name of the column containing document names.
#' @param suffix A character string to add to the end of the new columns. Use ""
#'   for no suffix.
#'
#' @return A data frame with new columns: writer, session, prompt, and repetition.
#'
#' @noRd
expand_docnames <- function(df, docname_col = "docname", suffix = "") {
  df <- df %>% tidyr::separate_wider_delim(tidyselect::all_of(docname_col),
    delim = "_",
    names = c(
      paste0("writer", suffix),
      paste0("session", suffix),
      paste0("prompt", suffix),
      paste0("rep", suffix)
    ),
    cols_remove = FALSE
  )
  return(df)
}
