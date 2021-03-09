#' Gerador de título
#'
#' Gerador do título para o gráfico de barras
#'
#'
#' @param question_data Tibble. Dados organizados relativos a uma questão principal
#' @param title_max_size Numeric. Tamanho máximo da linha do título
#' @param with_title Logical. Título deve ser gerado ou não
#'
#' @return Character. Título a ser utilizado no gráfico
#'
#' @author Leonardo Rocha
#'
gen_title <- function(question_data,
                      title_max_size,
                      with_title) {
  if (with_title) {
    main_question <- dplyr::first(question_data$main_question)
    break_text(main_question, max_size = title_max_size)
  } else {
    NULL
  }
}






#' Arrumar dados para gráfico de barra
#'
#' Arrumar os dados para o gráfico de barras
#'
#' Preparação dos dados para a geração dos gráficos
#' Arruma o dados de acordo com a escala das respostas:
#' se existem escalas negativas e se existem escalas intermediárias
#' a regra da escala intermediária só será aplicada caso haja alguma escala negativa
#' pressupondo que as escalas estão em ordem crescente para indicar se há escalas
#' negativas, basta informar a quantidade de respostas com conotação negativa
#' se houver escala intermediária era será equivalente à resposta seguinte às
#' escalas negativas. Caso não haja escala negativa, a escala de respostas será
#' ordenada por suas frequências relativa. Caso haja escala negativa, os de valores das
#' frequências relativa das escalas negativas serão transformados para o valor negativo.
#' Caso haja escala negativa e intermediária, além dos valores negativos serem transformados
#' para o negativo, o valor da escala intermediária será dividido por dois e duplicado
#' sendo um mantendo o mesmo sinal e o outro assumindo o sinal negativo.
#'
#' @param question_data Tibble. Dados organizados relativos a uma questão principal
#' @param n_negative Numeric. Número de escalas negativas
#' @param intermediate Logic. Existe escala intermediária
#' @param legend_max_size Numeric. Tamanho máximo da linha do texto da legenda do gráfico
#' @param axis_max_size Numeric. Tamanho máximo da linha do texto do eixo x do gráfico
#'
#' @return Tibble. Dados arrumados e prontos para serem utilizados na geração dos gráficos
#'
#' @author Leonardo Rocha
#'
tidy_data_for_bar_graph <- function(question_data,
                                    n_negative,
                                    intermediate,
                                    legend_max_size,
                                    axis_max_size) {


  answer_levels <- levels(question_data$answer_label)

  if (n_negative > 0) {

    if (intermediate) {
      intermediate_answer <- answer_levels[n_negative + 1]
      intermediate_select <- question_data$answer_label == intermediate_answer

      intermediate_half <- question_data$percent[intermediate_select] / 2

      intermediate_row_negative <- dplyr::filter(question_data, intermediate_select)
      intermediate_row_negative$percent <- -1 * intermediate_half
      question_data$percent[intermediate_select] <- intermediate_half

      question_data <- rbind(question_data, intermediate_row_negative)
    }

    negative_answers <- answer_levels[0:n_negative]
    negative_select <- question_data$answer_label %in% negative_answers
    question_data$percent[negative_select] <- -1 * question_data$percent[negative_select]

  } else {
    question_data$answer_label <- stats::reorder(question_data$answer_label, question_data$percent)
  }

  return(question_data)
}





#' Rótulo para gráfico de barras
#'
#' Gerado de rótulos para o gráfico de barras
#'
#' @param question_data Tibble. Dados organizados relativos a uma questão principal
#' @param tidy_data Tibble. Dados arrumados e prontos para serem utilizados na geração dos gráficos
#' @param n_negative Numeric. Número de escalas negativas
#' @param label_thereshold Numeric. Limiar do percentual para que seja atribuido um rotulo à observação.
#' @param label_percent_accuracy Numeric. Precisão das casas decimais do percentual.
#' @param label_text_size Numeric. Tamanho dos rótulos de texto.
#' @param with_labels Logical. Indicador de se o gráfico apresenta rótulo de texto.
#'
#' @return ggplot2::geom_text com os rótulos para o gráfico de barras (caso with_labels)
#'
#' @author Leonardo Rocha
#'
gen_bar_label <- function(question_data,
                          tidy_data,
                          n_negative,
                          label_thereshold,
                          label_percent_accuracy,
                          label_text_size,
                          with_labels) {
  if (with_labels) {
    positive_x <- tidy_data %>%
      dplyr::filter(percent > 0) %>%
      dplyr::arrange(sub_question, answer_label) %>%
      dplyr::group_by(sub_question) %>%
      dplyr::mutate(y = cumsum(percent) - percent/2) %>%
      dplyr::ungroup()

    negative_x <- tidy_data %>%
      dplyr::filter(percent < 0) %>%
      dplyr::arrange(sub_question, dplyr::desc(answer_label)) %>%
      dplyr::group_by(sub_question) %>%
      dplyr::mutate(y = cumsum(percent) - percent/2) %>%
      dplyr::ungroup()

    label_df <- rbind(positive_x, negative_x) %>%
      dplyr::group_by(sub_question, answer_label) %>%
      dplyr::summarise(y = sum(y)) %>%
      dplyr::ungroup()

    base_df <- if (n_negative) question_data else tidy_data

    #base_df$sub_question <- as.character(base_df$sub_question)
    #base_df$answer_label <- as.character(base_df$answer_label)
    #label_df$sub_question <- as.character(label_df$sub_question)
    #label_df$answer_label <- as.character(label_df$answer_label)

    #print(base_df)
    #print(label_df)

    label_df <- dplyr::left_join(base_df, label_df, by=c('sub_question', 'answer_label')) %>%
      dplyr::mutate(label = ifelse(abs(percent) > label_thereshold, scales::percent(percent, accuracy = label_percent_accuracy), '')) %>%
      dplyr::select(sub_question, answer_label, y, label)

    ggplot2::geom_text(data = label_df,
                       ggplot2::aes(x = sub_question, y = y, label = label),
                       size = label_text_size)
  } else {
    NULL
  }


}







#' Gera a escala de cores
#'
#' Gera a escala de cores para o gráfico de barras
#'
#' @param tidy_data Tibble. Dados arrumados e prontos para serem utilizados na geração dos gráficos
#' @param n_negative Numeric. Número de escalas negativas
#' @param intermediate Logic. Existe escala intermediária
#' @param diverg_scale Logic. Se a escala de cores vai seguir uma lógica divergente
#' @param negative_palette Character. O nome da paleta de cores para a escala negativa
#' @param positive_palette Character. O nome da paleta de cores para a escala positiva
#' @param sequential_palette Character. O nome da paleta de cores para a escala sequencial
#'
#' @return ggplot2::scale_fill_manual com a escala para preenchimento do gráfico de barras
#'
#' @author Leonardo Rocha
#'
gen_fill_scale <- function(tidy_data,
                           n_negative,
                           intermediate,
                           diverg_scale,
                           negative_palette,
                           positive_palette,
                           sequential_palette) {

  levels <- levels(tidy_data$answer_label)
  len <- length(levels)

  if (n_negative > 0 & diverg_scale) {

    n_color <-max(c(n_negative, 3))
    negative_colors <- RColorBrewer::brewer.pal(n = n_color, name = negative_palette)
    negative_colors <- rev(negative_colors)[0:n_negative]

    if (intermediate) {
      intermediate_color <- 'grey90'
      n_positive <- len - n_negative - 1
    } else {
      intermediate_color <- NULL
      n_positive <- len - n_negative
    }
    n_color <- max(c(n_positive, 3))
    initial <- n_color - n_positive + 1
    positive_colors <- RColorBrewer::brewer.pal(n = n_color, name = positive_palette)
    positive_colors <- positive_colors[initial:n_color]

    palette_values <- c(negative_colors, intermediate_color, positive_colors)
  } else{
    palette_values <- RColorBrewer::brewer.pal(n = len, name = sequential_palette)
  }

  ggplot2::scale_fill_manual(values = palette_values, breaks = levels)
}









#' Gerador do eixo y
#'
#' Gerador da escala de legendas do eixo y para o gráfico de barras
#'
#' @param tidy_data Tibble. Dados arrumados e prontos para serem utilizados na geração dos gráficos
#' @param n_negative Numeric. Número de escalas negativas
#' @param axis_y_interval Numeric. Tamanho do intervalo entre os rótulos do eixo
#' @param axis_y_fixed Logical. Se o eixo vai ser fixo ou ajustado aos dados
#' @param axis_y_gap Numeric. Tamanho da margem para o ajuste aos dados
#'
#' @return ggplot2::scale_y_continuous com a escala e rótulo do eixo y para o gráfico de barras
#'
#' @author Leonardo Rocha
#'
gen_axis_y_scale <- function(tidy_data,
                             n_negative,
                             axis_y_interval,
                             axis_y_fixed,
                             axis_y_gap) {


  limits <- get_limits(tidy_data = tidy_data,
                       n_negative = n_negative,
                       axis_y_interval = axis_y_interval,
                       axis_y_fixed = axis_y_fixed,
                       axis_y_gap = axis_y_gap)

  breaks <- seq(limits[1], limits[2], axis_y_interval)
  labels <- scales::percent(abs(breaks))

  ggplot2::scale_y_continuous(limits = limits, breaks = breaks, labels = labels)

}







#' Gerador do limite para eixo y
#'
#' Gerador do limite inferior e superior dos eixos para o caso da escala do eixo y ser ajustada aos dados.
#'
#' @param tidy_data Tibble. Dados arrumados e prontos para serem utilizados na geração dos gráficos
#' @param n_negative Numeric. Número de escalas negativas
#' @param axis_y_interval Numeric. Tamanho do intervalo entre os rótulos do eixo
#' @param axis_y_fixed Logical. Se o eixo vai ser fixo ou ajustado aos dados
#' @param axis_y_gap Numeric. Tamanho da margem para o ajuste aos dados
#'
#' @return vetor com os limites inferiores e superiores.
#'
#' @author Leonardo Rocha
#'
get_limits <- function(tidy_data,
                       n_negative,
                       axis_y_interval,
                       axis_y_fixed,
                       axis_y_gap) {
  if (n_negative > 0){
    if (axis_y_fixed) {

      return(c(-1, 1))

    } else {

      upper <- dplyr::filter(tidy_data, percent > 0) %>%
        dplyr::group_by(sub_question) %>%
        dplyr::summarise(cum_percent = sum(percent)) %>%
        .$cum_percent %>%
        max() * axis_y_gap

      upper <- ceiling(upper / axis_y_interval) * axis_y_interval
      upper <- min(c(upper, 1))

      lower <- dplyr::filter(tidy_data, percent < 0) %>%
        dplyr::group_by(sub_question) %>%
        dplyr::summarise(cum_percent = sum(percent)) %>%
        .$cum_percent %>%
        min() * axis_y_gap

      lower <- floor(lower / axis_y_interval) * axis_y_interval
      lower <- max(c(lower, -1))

      return(c(lower, upper))
    }
  } else {
    return(c(0, 1))
  }
}









#' Gerador de tema de gráfico em barras
#'
#' Gera o tema para ser utilizado no gráfico de barras.
#'
#' @param base_text_size Numeric. Tamanho base dos textos para o tema.
#' @param legend_position Character. Indicativo da posição da legenda.
#' @param question_data Tibble. Dados com a tabulação da questão principal.
#'
#' @return ggplot2::theme com a configuração do tema para o gráfico de barras
#'
#' @author Leonardo Rocha
#'
gen_graph_theme <- function(base_text_size,
                            legend_position,
                            question_data) {
  base_theme <- ggplot2::theme_bw(base_size = base_text_size) +
    ggplot2::theme(legend.position = legend_position,
                   panel.grid = ggplot2::element_blank())

  no_sub <- is.na(dplyr::first(question_data$sub_question))

  if (no_sub) {
    base_theme +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  } else {
    base_theme
  }

}

