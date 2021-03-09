#' Geração de gráfico de barras
#'
#' Função para a geração de gráfico de barras para questões de survey categóricas.
#'
#' Essa função pode ser utilizada para a criação de gráficos em barra,
#' podendo gerar gráficos de barras divergentes ou sequenciais empilhadas.
#' Múltiplas configurações podem ser realizadas através da modificação dos
#' pâmetros. Como por exemplo, adicionar ou remover títulos e rótulos aos
#' dados, modificar o tamanho dos textos, modifcar as cores a serem utilizadas
#' nas escalas divergentes e sequenciais etc.
#'
#' @param question_data Tibble. Dados organizados relativos a uma questão principal
#' @param n_negative Numeric. Número de escalas negativas
#' @param intermediate Logic. Existe escala intermediária
#' @param diverg_scale Logic. Se a escala de cores vai seguir uma lógica divergente
#' @param negative_palette Character. O nome da paleta de cores para a escala negativa
#' @param positive_palette Character. O nome da paleta de cores para a escala positiva
#' @param sequential_palette Character. O nome da paleta de cores para a escala seq
#' @param title_max_size Numeric. Tamanho máximo da linha do título
#' @param with_title Logical. Título deve ser gerado ou não
#' @param legend_max_size Numeric. Tamanho máximo da linha do texto da legenda do gráfico
#' @param axis_max_size Numeric. Tamanho máximo da linha do texto do eixo x do gráfico
#' @param axis_y_interval Numeric. Tamanho do intervalo entre os rótulos do eixo
#' @param axis_y_fixed Logical. Se o eixo vai ser fixo ou ajustado aos dados
#' @param axis_y_gap Numeric. Tamanho da margem para o ajuste aos dados
#' @param legend_position Character. Indicativo da posição da legenda.
#' @param base_text_size Numeric. Tamanho base dos textos para o tema.
#' @param with_labels Logical. Indicador de se o gráfico apresenta rótulo de texto.
#' @param label_thereshold Numeric. Limiar do percentual para que seja atribuido um rotulo à observação.
#' @param label_percent_accuracy Numeric. Precisão das casas decimais do percentual.
#' @param label_text_size Numeric. Tamanho dos rótulos de texto.
#'
#' @return ggplot2::ggplot de gráfico de barras
#'
#' @author Leonardo Rocha
#'
#' @export
#'
make_bar_graph <- function(question_data,
                           n_negative,
                           intermediate,
                           diverg_scale = F,
                           negative_palette = "Reds",
                           positive_palette = "Greens",
                           sequential_palette = "Oranges",
                           title_max_size = 60,
                           with_title = FALSE,
                           legend_max_size = 10, #
                           axis_max_size = 25, #
                           axis_y_interval = 0.25,
                           axis_y_fixed = TRUE,
                           axis_y_gap = 1.1,
                           legend_position = "bottom",
                           base_text_size = 9,
                           with_labels = TRUE,
                           label_thereshold = 0.05,
                           label_percent_accuracy = .2,
                           label_text_size = 2) {

  sub_question_ordered <- class(question_data$sub_question)[1] == "ordered"

  if (sub_question_ordered) {
    question_data$sub_question <- forcats::fct_rev(question_data$sub_question)
  }


  question_data <- question_data %>%
    dplyr::mutate(answer_label = factor(answer_label,
                                        labels = lapply(levels(answer_label),
                                                        break_text, max_size = legend_max_size)),
                  sub_question = factor(sub_question,
                                        labels = lapply(levels(sub_question),
                                                        break_text, max_size = axis_max_size)))

  # tidy_data
  tidy_data <- tidy_data_for_bar_graph(question_data = question_data,
                                       n_negative = n_negative,
                                       intermediate = intermediate,
                                       legend_max_size = legend_max_size,
                                       axis_max_size = axis_max_size)



  # graph_title
  title <- gen_title(question_data = question_data,
                     title_max_size = title_max_size,
                     with_title = with_title)




   # fill_colors
  bar_fill <- gen_fill_scale(tidy_data = tidy_data,
                             n_negative = n_negative,
                             intermediate = intermediate,
                             diverg_scale = diverg_scale,
                             negative_palette = negative_palette,
                             positive_palette = positive_palette,
                             sequential_palette = sequential_palette)



  # bar_labels
  bar_labels <- gen_bar_label(question_data = question_data,
                              tidy_data = tidy_data,
                              n_negative = n_negative,
                              label_thereshold = label_thereshold,
                              label_percent_accuracy = label_percent_accuracy,
                              label_text_size = label_text_size,
                              with_labels = with_labels)



  # y_axis
  y_axis <- gen_axis_y_scale(tidy_data = tidy_data,
                             n_negative = n_negative,
                             axis_y_interval = axis_y_interval,
                             axis_y_fixed = axis_y_fixed,
                             axis_y_gap = axis_y_gap)

  # tema
  theme <- gen_graph_theme(base_text_size = base_text_size,
                           legend_position = legend_position,
                           question_data = question_data)



  #return(tidy_data)

  # gerando o gráfico
  ggplot2::ggplot() +
    ggplot2::geom_col(data = dplyr::filter(tidy_data, percent > 0),
                      ggplot2::aes(x = sub_question, y = percent, fill = answer_label),
                      ggplot2::position_stack(reverse=TRUE)) +
    ggplot2::geom_col(data = dplyr::filter(tidy_data, percent < 0),
                      ggplot2::aes(x = sub_question, y = percent, fill = answer_label),
                      ggplot2::position_stack(reverse=FALSE)) +
    bar_fill + bar_labels + y_axis + theme +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL, title = title) +
    ggplot2::coord_flip()

}
