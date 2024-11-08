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


# External Functions ------------------------------------------------------


#' Plot Histograms
#'
#' Plot histograms of same writer and different writers reference similarity
#' scores from a random forest created with [train_rf()]. Plot a vertical,
#' dashed line at a similarity score calculated with [calculate_slr()] to see
#' whether the score is more typical of the same writer or different writers
#' reference scores.
#'
#' @param rforest A random forest created with [train_rf()]
#' @param score A similarity score calculated with [calculate_slr()]
#'
#' @return A ggplot2 plot of histograms
#' @export
#'
#' @examples
#' plot_histograms(rforest = random_forest)
#'
#' # Add a vertical line 0.1 on the horizontal axis.
#' plot_histograms(rforest = random_forest, score = 0.1)
#'
plot_histograms <- function(rforest, score = NULL) {
  # Prevent note "no visible binding for global variable"
  Score <- Group <- NULL

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  scores <- rforest$scores

  df1 <- data.frame(Score = scores$same_writer, Group = "same writer")
  df2 <- data.frame(Score = scores$diff_writer, Group = "different writers")
  df <- rbind(df1, df2)

  p <- df %>% ggplot2::ggplot(ggplot2::aes(x = Score)) +
    ggplot2::geom_histogram(
      position = "identity",
      ggplot2::aes(fill = Group),
      alpha = 0.4,
      bins = 30
    ) + # Histograms with transparency
    ggplot2::scale_fill_manual(values = c("same writer" = "#6BA4B8", "different writers" = "#F68D2E")) + # Customize colors
    ggplot2::labs(title = "Reference Similarity Scores", x = "Score", y = "Frequency") +
    ggplot2::theme_bw()

  # Optional - add vertical line at score
  if (!is.null(score)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = score,
        color = "black",
        linetype = "dashed"
      ) + # Add vertical line
      ggplot2::annotate("text",
        x = score,
        y = 75, # Dynamically position the label
        label = paste("similarity score", score),
        color = "black",
        size = 3,
        angle = 90,
        vjust = -1,
        hjust = 0.5
      ) # Add text annotation
  }

  return(p)
}
