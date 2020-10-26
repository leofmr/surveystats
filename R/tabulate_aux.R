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
#' @param survey_data Tibble. Question table with numbers as categories
#' @param question_label Tibble. Table of question labels
#' @param answer_label Tibble. Table of answer labels
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


#' Get number of questions
#'
#' Get the number of questions from the survey data.
#'
#' @author Leonardo Rocha
#'
#' @param survey_data Tibble. Question table with numbers as categories
#'
#' @return Numeric. The number of questions containing in the survey data
get_n_questions <- function(survey_data) {
  columns <- colnames(survey_data)
  last_col <- columns[ncol(survey_data)]
  main_question <- stringr::str_split(last_col, pattern = "_")[[1]][1]
  as.numeric(stringr::str_replace(main_question, pattern = "q", replacement = ""))
}
