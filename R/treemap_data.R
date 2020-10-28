#' Tabulate grouped questions
#'
#' Generate a table from questions that have some dependency relationship
#'
#' The \code{tab_grouped_questions} takes a \code{surveydata}, a main independent question (\code{group1_col}) and a list of
#' main questions (\code{group2_col}) that are dependent on the main independent question and generate a table that relates
#' answers in both group of questions.
#'
#' @param surveydata List.
#' @param group1_col Character.
#' @param group2_col Vector.
#' @param rm.other Logical.
#' @param unite.sep Character.
#'
#' @return Tibble.
#'
#' @export
#'
#' @author Leonardo Rocha
tab_grouped_questions <- function(surveydata, group1_col, group2_col, rm.other=TRUE, unite.sep = '__') {
  surveydata$data %>%
    dplyr::select(c(group1_col, group2_col)) %>%
    apply_labels_answer(answer = surveydata$answers) %>%
    tidyr::unite("group2", group2_col, na.rm = TRUE, sep = unite.sep) %>%
    tidyr::drop_na() %>%
    dplyr::rename("group1" = group1_col) %>%
    dplyr::group_by(group1, group2) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!stringr::str_detect(group2, unite.sep)) %>%
    filter_data(rm.other = rm.other)
}


#' Filter other and missing data
#'
#' @param data Tibble.
#' @param rm.other Logical.
#' @param other_label Character.
#'
#' @return Tibble.
#'
#' @author Leonardo Rocha
filter_data <- function(data, rm.other, other_label = "Other") {
  if (rm.other){
    data %>%
      dplyr::filter(group1 != other_label) %>%
      dplyr::filter(group2 != "")
  } else {
    data %>%
      dplyr::filter(group1 == other_label | group2 != "") %>%
      dplyr::filter(!(group1 == other_label & group2 != ""))
  }
}


#' Apply answers label to survey data
#'
#' @param data Tibble.
#' @param answer Tibble.
#'
#' @return Tibble.
#'
#' @author Leonardo Rocha
apply_labels_answer <- function(data, answer) {
  q_code <- colnames(data)
  q_number <- stringr::str_match(q_code, "q(\\d{1,2})_")[,2]

  for (i in 1:length(q_code)) {
    labeller <- answer %>%
      dplyr::filter(main_code == q_number[i]) %>%
      dplyr::mutate(answer = as.numeric(answer))

    data[[q_code[i]]] <- factor(data[[q_code[i]]], levels = labeller$answer, labels = labeller$answer_label)
  }
  data %>% dplyr::as_tibble()
}
