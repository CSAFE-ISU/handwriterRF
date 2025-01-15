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


#' Plot Scores
#'
#' Plot same writer and different writers reference similarity scores from a
#' validation set. The similarity scores are greater than or equal to zero and
#' less than or equal to one. The interval from 0 to 1 is split into `n_bins`.
#' The proportion of scores in each bin is calculated and plotted. Optionally, a
#' vertical dotted line may be plotted at an observed similarity score.
#'
#' The methods used in this package typically produce many times more different
#' writer scores than same writer scores. For example, `ref_scores` contains
#' 79,600 different writer scores but only 200 same writer scores. Histograms,
#' which show the frequency of scores, don't handle this class imbalance well.
#' Instead, the rate of scores is plotted.
#'
#' @param scores A dataframe of scores calculated with
#'   [get_ref_scores()]
#' @param obs_score Optional. A similarity score calculated with
#'   [calculate_slr()]
#' @param n_bins The number of bins
#'
#' @return A ggplot2 plot of histograms
#' @export
#'
#' @examples
#' plot_scores(scores = ref_scores)
#'
#' plot_scores(scores = ref_scores, n_bins = 70)
#'
#' # Add a vertical line 0.1 on the horizontal axis.
#' plot_scores(scores = ref_scores, obs_score = 0.1)
#'
#' @md
plot_scores <- function(scores, obs_score = NULL, n_bins = 50) {
  get_bin_rates <- function(scores, group, breaks = seq(0.00, 1, 0.02), labels = seq(0.01, 0.99, 0.02)) {
    # prevent note: "no visible binding for global variable"
    Group <- bin <- NULL

    df <- data.frame(Score = scores, Group = group)

    num_same <- nrow(df)

    # split into bins
    df$bin <- cut(df$Score, breaks = breaks, labels = labels, include.lowest = TRUE)

    # calculate rates for bins
    df <- df %>%
      dplyr::group_by(Group, bin) %>%
      dplyr::summarize(rate = dplyr::n() / num_same) %>%
      dplyr::mutate(bin = as.numeric(as.character(bin)))

    return(df)
  }

  # prevent note: "no visible binding for global variable"
  bin <- rate <- Group <- NULL

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Instead of frequency of scores, calculate rate of scores by splitting [0, 1] into 50
  # intervals of equal width and calculating the rate of scores in each interval.
  df1 <- get_bin_rates(scores = scores$same_writer$score, group = "same writer")
  df2 <- get_bin_rates(scores = scores$diff_writer$score, group = "different writers")
  df <- rbind(df1, df2)

  p <- df %>% ggplot2::ggplot(ggplot2::aes(x = bin, y = rate, fill = Group)) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "identity",
      alpha = 0.5
    ) +
    ggplot2::scale_fill_manual(values = c("same writer" = "#6BA4B8", "different writers" = "#F68D2E")) + # Customize colors
    ggplot2::theme_bw()

  # Optional - add vertical line at score
  if (!is.null(obs_score)) {
    ymax <- max(df$rate)
    p <- p +
      ggplot2::geom_vline(
        xintercept = obs_score,
        color = "black",
        linetype = "dashed"
      ) + # add vertical line
      ggplot2::annotate("text",
        x = obs_score,
        y = ymax / 2,
        label = paste("observed score", obs_score),
        color = "black",
        size = 3,
        angle = 90,
        vjust = -1,
        hjust = 0.5
      )
  }

  return(p)
}
