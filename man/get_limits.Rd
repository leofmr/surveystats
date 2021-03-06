% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bar_graph_aux.R
\name{get_limits}
\alias{get_limits}
\title{Gerador do limite para eixo y}
\usage{
get_limits(tidy_data, n_negative, axis_y_interval, axis_y_fixed, axis_y_gap)
}
\arguments{
\item{tidy_data}{Tibble. Dados arrumados e prontos para serem utilizados na geração dos gráficos}

\item{n_negative}{Numeric. Número de escalas negativas}

\item{axis_y_interval}{Numeric. Tamanho do intervalo entre os rótulos do eixo}

\item{axis_y_fixed}{Logical. Se o eixo vai ser fixo ou ajustado aos dados}

\item{axis_y_gap}{Numeric. Tamanho da margem para o ajuste aos dados}
}
\value{
vetor com os limites inferiores e superiores.
}
\description{
Gerador do limite inferior e superior dos eixos para o caso da escala do eixo y ser ajustada aos dados.
}
\author{
Leonardo Rocha
}
