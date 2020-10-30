#' Make a Wilcoxon rank sum test
#'
#' Make a Wilcoxon rank sum test between answer and knowledge
#'
#' @param surveydata Tibble.
#' @param qnumber Numeric.
#'
#' @return Tibble with wicolxon stats
#'
#' @export
#'
#' @author Leonardo Rocha
make_wilcox_test <- function(surveydata, qnumber) {
  question_start <- paste("q", qnumber, "_", sep = "")
  surveydata %>%
    dplyr::select("q1_1", tidyselect::starts_with(question_start)) %>%
    dplyr::rename(knowledge = 1) %>%
    tidyr::gather(question, answer, -1) %>%
    dplyr::group_by(question) %>%
    tidyr::nest() %>%
    dplyr::mutate("wilcox_stats" = (lapply(data, wilcox_ind_sample))) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = c("wilcox_stats")) #%>%
    #dplyr::select(question, statistic, p)
}




#'  Utility function for the Wicoxon rank sum test
#'
#'  @param nested_data Tibble.
#'
#'  @return row with wilcoxcon rank sum test for two idependent
#'
#'  @author Leonardo Rocha
wilcox_ind_sample <- function(nested_data) {
  nested_data %>%
    dplyr::mutate(answer = as.integer(answer)) %>%
    rstatix::wilcox_test(answer~ knowledge) %>%
    rstatix::add_significance() %>%
    dplyr::select(-".y.") %>%
    dplyr::rename("g1_answer" = "group1",
                  "g2_knowledge" = "group2")
}

