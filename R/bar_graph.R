#' Make a bar graph
#'
#' Function to make a likert scale bar graph or ordered by values bar graph
#'
#' Make horizontal bar graph with the \code{question_data} provided.
#' If a \code{negative_col} is provided it will be a likert-scale graph with
#' the \code{negative_col} been the answer with a negative connotation. Else,
#' the answers categories in the bar graph will be ordered according to their
#' prevalence.
#'
#'
#' @param question_data Tibble. Question stats table
#' @param negative_col Character. Name of the category with negative connotation, if \code{FALSE}
#' the answers will be ordered by their prevalence. Default to \code{FALSE}.
#' @param fill_color Character. Color scale name from ggplot2 library. Default \code{'Oranges'}.
#' @param title_max_size Numeric. Max line size for the title text. Default \code{60}.
#' @param legend_max_size Numeric. Max line size for the legend text. Default \code{10}.
#' @param axis_max_size Numeric. Max line size for the x axis text. Default \code{25}.
#' @param legend_position Character. \code{ggplot2} \code{legend_position} character indicator. Default \code{"bottom"}.
#' @param base_text_size Numeric. \code{ggplot2} \code{theme} \code{base_size} for all the text elements. Default \code{9}.
#' @param has_title Logical. Indicative if the graph will have a title. The main question label will be used for the
#' title text. Default \code{FALSE}.
#' @param with_labels Logical.Indicative if the graph will have text indicating the values inside the bar graph.
#' To make the figure cleaner answers with prevalence bellow 5% won't have text. Default \code{TRUE}
#'
#' @return Custom horizontal bar graph: a likert-scale or a ordered by value answers.
#'
#' @export
#'
#' @author Leonardo Rocha
#'
make_bar_graph <- function(question_data,
                           negative_col = FALSE,
                           fill_color = "Oranges",
                           title_max_size = 60,
                           legend_max_size = 10,
                           axis_max_size = 25,
                           legend_position = "bottom",
                           base_text_size = 9,
                           has_title = FALSE,
                           with_labels = TRUE) {


  # Preparação dos dados para a geração do gráfico
  tidy_data <- tidy_data_for_bar_graph(
    data = question_data,
    negative_col = negative_col,
    legend_max_size = legend_max_size,
    axis_max_size = axis_max_size
  )


  graph_title <- gen_title(data = question_data, max_size = title_max_size, has_title = has_title)
  fill_color_scale <- gen_fill_scale(palette_color = fill_color)
  bar_labels <- gen_bar_label(tidy_data = tidy_data, with_labels = with_labels)
  y_axis_labels <- gen_axis_y_scale(data = question_data, negative_col = negative_col)
  bar_theme <- gen_graph_theme(base_text_size = base_text_size, legend_position = legend_position, data = question_data)


  # geração do gráfico de barras a partir dos dados gerados anteriormente
  tidy_data %>%
    ggplot2::ggplot(ggplot2::aes(x = sub_question, y = percent, fill = answer_label)) +
    ggplot2::geom_col(color = "white") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL, title = graph_title) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1)) +
    fill_color_scale +
    bar_labels +
    y_axis_labels +
    bar_theme
}
