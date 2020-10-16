#' Compute Chi-square p-value
#'
#' \code{chi.square_pvalue} compute the Chi-squared test, with simulated p-values and get its p-value.
#'
#' The function is applied to a contingency table between two categorical variables, and
#' return the p-value of the Chi-square test though the usage of the \code{stats::chisq.test}
#' function.
#'
#' @author Leonardo Rocha
#'
#' @param contingency.table Tibble. With the contingency table between two categorical questions
#'
#' @return Numeric. Chi-square p-value
chi.square_pvalue <- function(contingency.table) {
  stats::chisq.test(contingency.table, simulate.p.value = TRUE)$p.value
}



#' Apply Answers and Questions Labels
#'
#' Apply answers and questions labels to the survey dataset.
#'
#' @author Leonardo Rocha
#'
#' @param survey_data Tibble. Dados já organizados com os percentuais calculados
#' @param question_label Tibble. Dados gerados pelo usuário com os rótulos
#' de questão vinculado aos identificadores de questão
#' @param answer_label Tibble. Dados gerados pelo usuário com os rótulos
#' de respostas vinculadas aos identificadores de questão
#'
#' @return Tibble. The survey_data with
#'
#' @importFrom magrittr %>%
apply_labels <- function(survey_data, question_label, answer_label) {
  survey_data %>%
    dplyr::left_join(question_label, by="question") %>%
    tidyr::separate(question, into=c("main_code", "sub_code")) %>%
    dplyr::mutate(main_code = stringr::str_remove(main_code, "q"),
                  answer = as.character(answer)) %>%
    dplyr::left_join(answer_label, by=c("main_code", "answer")) %>%
    dplyr::mutate(
      main_code = as.integer(main_code),
      sub_code = as.integer(sub_code),
      answer = as.integer(answer),
      main_question = stats::reorder(main_question, main_code),
      sub_question = stats::reorder(sub_question, sub_code),
      answer_label = stats::reorder(answer_label, answer),
      knowledge = factor(knowledge, labels = c("Good knowledge", "Some knowledge", "Total"))
    ) %>%
    dplyr::select(main_question, sub_question, p_value, knowledge, answer_label, count, percent, total) %>%
    dplyr::arrange(sub_question, knowledge, answer_label)
}
