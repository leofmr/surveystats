#' Generate sub_question data
#'
#' Helper function that generates sub_questions answers stats
#' for the Generate Random Question function. The answers count is
#' generated through a uniform probability distribution.
#'
#' @param sub Numeric. The index of the sub_question that is being generated
#' @param n_answer Numeric. The number of answer options
#' @param min_count Numeric. Minimum count value that an answer will receive
#' @param max_count Numeric. Maximum count value that an answer will receive
#'
#' @return Sub-question answer stats data
#'
#' @author Leonardo Rocha
#'
gen_sub_question_data <- function(sub, n_answer, min_count, max_count) {
  answer_label <- factor(seq(1,n_answer), ordered = T)
  counts <- stats::runif(n_answer, min=min_count, max=max_count)
  total_counts <- sum(counts)
  percent <- counts / total_counts

  tibble::tibble(
    sub_question = sub,
    answer_label = answer_label,
    count = counts,
    percent = percent,
    total = total_counts
  )
}


#' Generate question stats data
#'
#' A functions that helps in the development in new functionalities by
#' generating a random question stats data. The answers count is generated
#' through a uniform probability distribution. Labels can be employed in all
#' categorical variables.
#'
#' @param n_sub Numeric. The number of sub_questions that is being generated
#' @param n_answer Numeric. The number of answer options
#' @param label_answer Vector.
#' @param label_sub_question Vector.
#' @param label_question Character.
#' @param min_count Numeric. Minimum count value that an answer will receive
#' @param max_count Numeric. Maximum count value that an answer will receive
#'
#' @return Sub-question answer stats data
#'
#' @author Leonardo Rocha
#'
#' @export
#'
#' @importFrom magrittr %>%
gen_random_question <- function(n_sub, n_answer, label_answer=NULL, label_sub_question=NULL,  label_question=NULL, min_count=1, max_count=500){


  subquestion_data_list <- lapply(seq(1,n_sub),
                                  gen_sub_question_data,
                                  n_answer=n_answer,
                                  min_count=min_count,
                                  max_count=max_count)

  main_question <- paste('Test question with', n_sub, 'sub-questions and', n_answer, 'possible answers')
  question_data <- do.call('rbind', subquestion_data_list) %>%
    dplyr::mutate(sub_question = factor(sub_question, ordered = T),
           main_question =factor(main_question)) %>%
    dplyr::relocate(main_question)

  if (!is.null(label_answer)) {question_data$answer_label <- factor(question_data$answer_label, labels=label_answer)}

  if (!is.null(label_sub_question)) {question_data$sub_question <- factor(question_data$sub_question, labels=label_sub_question)}

  if (!is.null(label_question)) {question_data$main_question <- label_question}
  if (n_sub == 1) {question_data$sub_question <- NA}

  return(question_data)
}

