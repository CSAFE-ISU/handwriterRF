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
#' scores from a validation set. Plot a vertical, dashed line at a similarity
#' score calculated with [calculate_slr()] to see whether the score is more
#' typical of the same writer or different writers reference scores.
#'
#' @param validation A data frame of validation scores calculated with
#'   [get_validation_scores()]
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
plot_histograms <- function(validation, score = NULL, downsample_size = NULL, n_bins = 50) {
  # Prevent note "no visible binding for global variable"
  Score <- Group <- NULL

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  df1 <- data.frame(Score = validation$same_writer, Group = "same writer")
  df2 <- data.frame(Score = validation$diff_writer, Group = "different writers")

  if (!is.null(downsample_size)) {
    df2 <- df2 %>%
      dplyr::slice_sample(n=downsample_size)
  }

  # Instead of frequency of scores, calculate rate of scores by splitting [0, 1] into 50
  # intervals of equal width and calculating the rate of scores in each interval.
  breaks <- seq(0, 100, 2) / 100
  num_same <- nrow(df1)
  df1 <- df1 %>%
    dplyr::mutate(bin = cut(Score, breaks = breaks, labels = seq(0.01, 0.99, 0.02), include.lowest = TRUE)) %>%
    dplyr::group_by(Group, bin) %>%
    dplyr::summarize(rate = dplyr::n() / num_same)
  num_diff <- nrow(df2)
  df2 <- df2 %>%
    dplyr::mutate(bin = cut(Score, breaks = breaks, labels = seq(0.01, 0.99, 0.02), include.lowest = TRUE)) %>%
    dplyr::group_by(Group, bin) %>%
    dplyr::summarize(rate = dplyr::n() / num_diff)
  df <- rbind(df1, df2)
  df$bin <- as.numeric(as.character(df$bin))

  p <- df %>% ggplot2::ggplot(ggplot2::aes(x = bin, y = rate, fill = Group)) +
    ggplot2::geom_bar(stat = "identity",
                      position = "identity",
                      alpha = 0.5) +
    ggplot2::scale_fill_manual(values = c("same writer" = "#6BA4B8", "different writers" = "#F68D2E")) + # Customize colors
    ggplot2::theme_bw()

  # Optional - add vertical line at score
  if (!is.null(score)) {
    ymax <- max(df$rate)
    p <- p +
      ggplot2::geom_vline(
        xintercept = score,
        color = "black",
        linetype = "dashed") +  # add vertical line
      ggplot2::annotate("text",
                        x = score,
                        y = ymax / 2,
                        label = paste("observed score", score),
                        color = "black",
                        size = 3,
                        angle = 90,
                        vjust = -1,
                        hjust = 0.5
                        ) +  # add text
      ggplot2::labs(title = "The observed similarity score compared to reference similarity scores", x = "Score", y = "Rate")
  } else {
    p <- p + ggplot2::labs(title = "Reference similarity scores", x = "Score", y = "Rate")
  }

  return(p)
}
