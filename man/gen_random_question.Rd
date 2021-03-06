% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_data.R
\name{gen_random_question}
\alias{gen_random_question}
\title{Generate question stats data}
\usage{
gen_random_question(
  n_sub,
  n_answer,
  label_answer = NULL,
  label_sub_question = NULL,
  label_question = NULL,
  min_count = 1,
  max_count = 500
)
}
\arguments{
\item{n_sub}{Numeric. The number of sub_questions that is being generated}

\item{n_answer}{Numeric. The number of answer options}

\item{label_answer}{Vector.}

\item{label_sub_question}{Vector.}

\item{label_question}{Character.}

\item{min_count}{Numeric. Minimum count value that an answer will receive}

\item{max_count}{Numeric. Maximum count value that an answer will receive}
}
\value{
Sub-question answer stats data
}
\description{
A functions that helps in the development in new functionalities by
generating a random question stats data. The answers count is generated
through a uniform probability distribution. Labels can be employed in all
categorical variables.
}
\author{
Leonardo Rocha
}
