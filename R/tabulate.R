#' Tabulate a survey question
#'
#' \code{tab_survey_question} Create a statistical table for a target question
#'
#' This function create a tibble table, with percent values for each answer and knowledge level for
#' all sub-question (if any) regarding the target main question. It takes a survey data, select the main question,
#' with it's respectives sub-questions, and calculate percent values for each answer, sub-question and knowledge level.
#' Also for each sub-question, it makes a chi-square test for the correlation between responses and knowledge level.
#' Finally, the function applies the question and answers labels.
#'
#' @author Leonardo Rocha
#'
#' @param survey_data Tibble. Survey data
#' @param question_number Numeric. Number of the main question
#' @param question_label Tibble. Question labels data
#' @param answer_label Tibble. Answers labels data
#'
#' @return Tibble. Table with percent values and p-value (chi-squared test between knowledge level) for sub-questions and knowledge levels.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' tab_survey_question(AI_survey$survey_data, 4, AI_survey$questions_labels, AI_survey$answers_labels)
#' tab_survey_question(AI_survey$survey_data, 2, AI_survey$questions_labels, AI_survey$answers_labels)
tab_survey_question <- function(survey_data, question_number, question_label, answer_label) {
  # geração do string que vai ser utilizado para poder selecionar as colunas de dt
  # que serão utilizadas para a tabulação
  question_start <- paste("q", question_number, "_", sep = "")

  # tabulação principal e simples
  # agrupando por subquestão (answer)
  # eliminando as linhas sem respostas
  # transformando as respostas e o conhecimento em fator
  # calculando as contagens por conhecimento e resposta (mas mantendo a questão para adicionar rotulo)
  # por fim completando aquelas respostas que não tiveram frequência para algum grupo com 0
  qtab <- survey_data %>%
    dplyr::select("q1_1", tidyselect::starts_with(question_start)) %>%
    dplyr::rename(knowledge = 1) %>%
    tidyr::gather(question, answer, -1) %>%
    dplyr::filter(answer != "") %>%
    dplyr::mutate(answer = factor(answer),
                  knowledge = factor(knowledge)) %>%
    dplyr::group_by(knowledge, question, answer) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    tidyr::complete(answer, fill = list(count = 0)) %>%
    dplyr::ungroup()

  #calculando o p-valor do teste qui-quadrado por questão.
  # isso é para ver se tem alguma diferença por nível de conhecimento
  # para isso são geradas tabelas de contigência, agrupadas por questão
  # são então cada tabela de contigência é agrupada para questao em uma variável
  # data onde fica localizada a tabela de contigência
  p_values <- qtab %>%
    tidyr::spread(answer, count) %>%
    dplyr::select(-knowledge) %>%
    dplyr::group_by(question) %>%
    tidyr::nest() %>%
    dplyr::mutate(p_value = purrr::as_vector(lapply(data, chi.square_pvalue))) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup()

  # geração de uma tabela com os totais, dos dois níveis de conhecimento
  # é adicionado o valor 3 para conhecimento que será representante de total
  qtab_total <- qtab %>%
    dplyr::group_by(question, answer) %>%
    dplyr::summarise(count = sum(count),
                     knowledge = "3") %>%
    dplyr::ungroup()

  # agrugem dos valores de p_valor por questão
  # nível de conhecimento total
  # também são contabilizados os totais por questão e nível de conhecimento,
  # e contabilizados os valores percentuais de cada resposta
  # ao final são aplicadas as labels às variáveis categóricas representadas por números:
  # questão principal
  # sub questão
  # resposta
  qtab %>%
    dplyr::bind_rows(qtab_total) %>%
    dplyr::group_by(question, knowledge) %>%
    dplyr::mutate(total = sum(count),
                  percent = count / total) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(p_values, by = "question") %>%
    apply_labels(question_label = question_label, answer_label = answer_label)
}

#' Tabulate all survey questions
#'
#'
#' @author Leonardo Rocha
#'
#' @param survey_data Tibble. Survey data
#' @param question_label Tibble. Question labels data
#' @param answer_label Tibble. Answers labels data
#'
#' @return List. A list of Tables. A Table for each question
#' with percent values and p-value (chi-squared test between knowledge level)
#' for sub-questions and knowledge levels.
tab_all_survey_question <- function(survey_data, question_label, answer_label) {
  n_questions <- get_n_questions(survey_data)
  questions_list <- seq(2, n_questions)
  data_list <- purrr::map(questions_list, tab_survey_question,
      survey_data = survey_data,
      question_label = question_label,
      answer_label = answer_label)
  name_list <- sapply(questions_list, function(x) {paste('q', x, sep = '')})
  names(data_list) <- name_list

  return(data_list)
}
