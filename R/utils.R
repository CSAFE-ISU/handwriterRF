# The handwriterRF R package performs writership analysis of handwritten
# documents. Copyright (C) 2024 Iowa State University of Science and Technology
# on behalf of its Center for Statistics and Applications in Forensic Evidence
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.


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
