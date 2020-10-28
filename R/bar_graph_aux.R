#' Arrumar dados para gráfico de barra
#'
#' Função para arrumar os gráficos para o gráfico de barras
#'
#' Preparação dos dados para a geração do gráfico
#' Apenas todos os conhecimentos serão selecionados
#' os valores percentuais das linhas com a escolha de conotação negativa
#' são transformados para o negativo
#' foi feito de uma certa forma que, caso negative_col seja equivalente a FALSE
#' o percent não será alterado
#' como a resposta (answer_label) vai para o legenda do gráfico
#' quebra do answer_label em multiplas linhas com a função break_text com o parâmetro legend_max_size
#' caso a opção de ordenar as respostas por frequência, a resposta será ordenada pela percent
#' como a subquestion vai para o eixo x (que invertido fica como se fosse o eixo y)
#' quebra das subquestions coma função break_text utilizando como parâmetro axis_max_size
#'
#' @param data Tibble. survey dataset completo
#' @param negative_col Character. Nome da questão que possui a conotação negativa
#' @param legend_max_size Numeric. Tamanho máximo da linha do texto legenda do gráfico
#' @param axis_max_size Numeric. Tamanho máximo da linha do texto do eixo x do gráfico
#'
#' @return Tibble. Dados arrumados e prontos para serem utilizados na geração dos gráficos
#'
#' @author Leonardo Rocha
#'
tidy_data_for_bar_graph <- function(data,
                                    negative_col,
                                    legend_max_size,
                                    axis_max_size) {
  tidy_data <- data %>%
    dplyr::filter(knowledge == "Total") %>%
    dplyr::select(sub_question, answer_label, percent) %>%
    dplyr::mutate(percent = ifelse(answer_label == negative_col, -percent, percent),
                  answer_label = factor(answer_label, labels = lapply(levels(answer_label), break_text, max_size = legend_max_size)),
                  sub_question = factor(sub_question, labels = lapply(levels(sub_question), break_text, max_size = axis_max_size)))

  if (negative_col == FALSE) {
    tidy_data$answer_label <- stats::reorder(tidy_data$answer_label, tidy_data$percent)
  }

  return(tidy_data)

}





#' add label
#'
#' Adiciona rótulos ao gráfico
#'
#' @param tidy_data Tibble. Dados da questão principal organizados para a elaboração do gráfico de barras.
#' @param with_labels Logical. Indicador de se o gráfico apresenta rótulo de texto.
#' @param label_thereshold Numeric. Limiar do percentual para que seja atribuido um rotulo à observação.
#' @param percent_accuracy Numeric. Precisão das casas decimais do percentual.
#' @param alignment Numeric. Alinhamento do rótulo de texto.
#' @param text_size Numeric. Tamanho dos rótulos de texto.
#'
gen_bar_label <- function(tidy_data,
                          with_labels,
                          label_thereshold = 0.05,
                          percent_accuracy = 1,
                          alignment = 0.5,
                          text_size = 2) {
  if (with_labels) {
    ggplot2::geom_text(data = tidy_data,
                       ggplot2::aes(x = sub_question,
                           y = percent,
                           group = answer_label,
                           label = ifelse(abs(percent) > label_thereshold,
                                          scales::percent(abs(percent), accuracy = percent_accuracy),
                                          "")),
                       position = ggplot2::position_stack(vjust = alignment),
                       size = text_size)
  } else {
    NULL
  }

}






#' Gerador de título
#'
#' Gerador do título para o gráfico de barras
#'
#'
#' @param data Tibble. Dados organizados relativos a uma questão principal
#' @param max_size Numeric. Tamanho máximo da linha do título
#' @param has_title Logical. Título deve ser gerado ou não
#'
#' @return Título a ser utilizado no gráfico
gen_title <- function(data,
                      max_size,
                      has_title) {
  if (has_title) {
    main_question <- dplyr::first(data$main_question)
    break_text(main_question, max_size = max_size)
  } else {
    NULL
  }
}





#' Gerar escala de cores
#'
#' Gerar escala de cores para o gráfico de barras
#'
#'
#' @param palette_color Character. Nome da paleta de cores utilizadas caso não seja em escala de cinza
#' @param direction_color Numeric. Direção da paleta de cores
#' @param grey_start Numeric. Intensidade de cinza do começo da escala de cinza
#' @param grey_end Numeric. Intensidade de cinza do fim da escala de cinza
#'
#' @return Escala de cor para o gráfico de barras
#'
#' @author Leonardo Rocha
#'
gen_fill_scale <- function(palette_color,
                           direction_color = 1,
                           grey_start = 0.7,
                           grey_end = 0.3) {

  if (palette_color == "Greys" | palette_color == "Grays") {
    ggplot2::scale_fill_grey(start = grey_start, end = grey_end)
  } else {
    ggplot2::scale_fill_brewer(palette = palette_color, direction = direction_color)
  }

}





#' Gerar rótulos do eixo y
#'
#'
#' @param negative_col Character. Nome da coluna que é negativa
#' @param intervals Numeric. Tamanho do intervalo da escala de valores continuos do eixo y
#'
#' @return Escala de valores para o eixo y do gráfico de barras
#'
gen_axis_y_scale <- function(negative_col,
                             intervals = 0.25) {

  is_negative <- typeof(negative_col) == "character"
  lower_limit <- if (is_negative) -1 else 0

  limits <- c(lower_limit, 1)
  breaks <- seq(lower_limit, 1, intervals)
  labels <- scales::percent(abs(breaks))

  ggplot2::scale_y_continuous(limits = limits,
                              breaks = breaks,
                              labels = labels)
}



#' Gerar removedor de rótulos do eixo x
#'
#'
#' @param base_text_size Numeric. Tamanho base dos textos para o tema.
#' @param legend_position Character. Indicativo da posição da legenda.
#' @param data Tibble. Dados com a tabulação da questão principal.
#'
gen_graph_theme <- function(base_text_size,
                            legend_position,
                            data) {
  base_theme <- ggplot2::theme_bw(base_size = base_text_size) +
    ggplot2::theme(legend.position = legend_position,
                   panel.grid = ggplot2::element_blank())

  no_sub <- is.na(dplyr::first(data$sub_question))

  if (no_sub) {
    base_theme +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  } else {
    base_theme
  }

}
