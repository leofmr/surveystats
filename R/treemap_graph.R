#' Make treemap graph
#'
#' Function that takes grouped questions data and make a treemap graph
#'
#' @param grouped_data Tibble.
#' @param with_labels Logical. Default \code{TRUE}
#' @param fill_color Character. Default \code{"Set2"}
#' @param legend_max_size Numeric. Default \code{15}
#' @param base_text_size Numeric. Default \code{9}
#' @param legend_position Character. Default \code{"bottom"}
#'
#' @return A ggplot2 treemap graph
#'
#' @export
#'
#' @author Leonardo Rocha
make_treemap_graph <- function(grouped_data,
                               with_labels = TRUE,
                               fill_color = "Set2",
                               legend_max_size = 15,
                               base_text_size = 9,
                               legend_position = "bottom") {
  grouped_data %>%
    dplyr::mutate(group1 = sapply(group1, break_text, max_size = legend_max_size),
                  group1 = factor(group1),
                  group1 = forcats::fct_reorder(group1, count, sum)) %>%
    add_value_to_labels(with_labels = with_labels) %>%
    ggplot2::ggplot(ggplot2::aes(area = count,
                                 subgroup = group1,
                                 subgroup2 = group2,
                                 fill = group1,
                                 label = group2)) +
    treemapify::geom_treemap(start = "topleft", layout = "srow") +
    treemapify::geom_treemap_text(grow = FALSE,
                                  reflow = TRUE,
                                  min.size = 6,
                                  size = 10,
                                  place = "centre",
                                  start = "topleft",
                                  layout = "srow") +
    treemapify::geom_treemap_subgroup_border(color = "white",
                                             size = 2,
                                             start = "topleft",
                                             layout = "srow") +
    treemapify::geom_treemap_subgroup2_border(color = "white",
                                              size = 0.001,
                                              start = "topleft",
                                              layout = "srow",
                                              linetype = "dotdash") +
    gen_fill_scale(palette_color = fill_color) +
    ggplot2::theme_bw(base_size = base_text_size) +
    ggplot2::theme(legend.position = legend_position,
                   legend.margin = ggplot2::margin(c(5, 5, 5, 5)),
                   legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 15, unit = "pt"))) +
    ggplot2::labs(fill = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1))
}




#' Add value labels for the treemap graph
#'
#' @param data Tibble.
#' @param with_labels Logical.
#'
#' @return tibble with category labels with values added
#'
#' @author Leonardo Rocha
add_value_to_labels <- function(data, with_labels) {
  if (with_labels) {
    data %>%
      dplyr::mutate(percent = count / sum(count),
                    percent_label = scales::percent(percent, accuracy = 1),
                    group2 = paste(group2, "\n(", percent_label, ")", sep = ""))
  } else {
    data
  }
}




#' Make a complementar bar graph
#'
#' Make a complementar bar graph for the treemap combined graph
#'
#' @param grouped_data Tibble.
#' @param with_labels Logical. Default \code{TRUE}
#' @param fill_color Character. Default \code{"Set2"}
#' @param legend_max_size Numeric. Default \code{15}
#' @param base_text_size Numeric. Default \code{9}
#' @param legend_position Character. Default \code{"none"}
#'
#' @return A ggplot2 bar graph
#'
#' @author Leonardo Rocha
make_complementar_bar_graph <- function(grouped_data,
                                        with_labels = TRUE,
                                        fill_color = "Set2",
                                        legend_max_size = 15,
                                        base_text_size = 9,
                                        legend_position = "none") {
  tidy_data <- grouped_data %>%
    dplyr::group_by(group1) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group1 = sapply(group1, break_text, max_size = legend_max_size),
                  percent = count / sum(count),
                  percent_label = scales::percent(percent, accuracy = 1),
                  group1 = forcats::fct_reorder(group1, count),
                  sub_question = NA) %>%
    dplyr::rename("answer_label" = group1)


  tidy_data %>%
    ggplot2::ggplot(ggplot2::aes(x = sub_question, y = percent, fill = answer_label)) +
    ggplot2::geom_col(color = "white") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1)) +
    gen_fill_scale(palette_color = fill_color) +
    gen_bar_label(tidy_data = tidy_data, with_labels = with_labels) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = "none",
          panel.grid = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(limits=c(0, 1), expand = c(0, 0))
}




#' Make a combined treemap graph
#'
#' Similar to \code{make_treemap_graph} this function takes a grouped questions tibble
#' and produces a treemap graph. Along with a treemap it is also genereted a bar graph,
#' with the distribution of the main group. Both graphs are vertically combined into a single figure,
#' with the treemap graph taking \code{treemap_proportion} upper part.
#'
#' @param grouped_data Tibble.
#' @param with_labels Logical. Default \code{TRUE}
#' @param fill_color Character. Default \code{"Set2"}
#' @param legend_max_size Numeric. Default \code{15}
#' @param base_text_size Numeric. Default \code{9}
#' @param legend_position Character. Default \code{"bottom"}
#' @param treemap_proportion Numeric. Default \code{0.9}
#'
#' @return A ggplot2 figure with bar and treemap graphs combined
#'
#' @export
#'
#' @author Leonardo Rocha
make_combined_treemap_graph <- function(grouped_data,
                                        with_labels = TRUE,
                                        fill_color = "Set2",
                                        legend_max_size = 15,
                                        base_text_size = 9,
                                        legend_position = "bottom",
                                        treemap_proportion = 0.9) {

  treemap <- make_treemap_graph(grouped_data = grouped_data,
                                with_labels = with_labels,
                                fill_color = fill_color,
                                legend_max_size = legend_max_size,
                                legend_position = legend_position,
                                base_text_size = base_text_size)

  bar <- make_complementar_bar_graph(grouped_data = grouped_data,
                                with_labels = with_labels,
                                fill_color = fill_color,
                                base_text_size = base_text_size)
  cowplot::ggdraw() +
    cowplot::draw_plot(treemap, x = 0, y = 1 - treemap_proportion, width = 1, height = treemap_proportion) +
    cowplot::draw_plot(bar, x = 0, y = 0, width = 1, height = 1 - treemap_proportion)
}
