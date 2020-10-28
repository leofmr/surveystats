#' Break text
#'
#' Break a text into multiple lines
#'
#'
#'
#' @param string Character. Text to break
#' @param max_size Numeric. Max size line parameter
#'
#' @return Character. Text containing one or more lines
#'
#' @author Leonardo Rocha
#'
break_text <- function(string, max_size) {
  # quebra o texto em uma lista de strings, separado por ' '.
  text_list <- as.list(stringr::str_split(string, " ")[[1]])

  # resultado final vai ser uma lista de strings que será juntada por '\n'
  final <- list()
  final_index <- 1

  # pegando a lista por linhas - iniciando para fazer o loop para compor a lista de strings
  current <- list()
  current_index <- 1
  char_counter <- 0

  # para cada texto
  for (x in text_list) {

    # conto quantos caracteres tem e adiciono mais a palavra atual na lista de current
    # a lista de current é como se fosse a linha atual
    # eu vou adicionando palavras nela até que ela ultrapasse o limite definido por max size
    char_counter <- char_counter + nchar(x)
    current[[current_index]] <- x

    # se o limite ultrapassar eu tenho que juntar todas as palavras na lista corrente
    # e inserir o string gerado por essa junção, na lista final (lista de linhas)
    if (char_counter >= max_size) {
      # adicionar a linha na lista de linhas na posição atual
      final[[final_index]] <- paste(current, collapse = " ")
      # incrementa em um a posição
      final_index <- final_index + 1
      # zera a linha e a contagem de caracteres
      current <- list()
      current_index <- 0
      char_counter <- 0
    }
    # ao final adiciona em um o index da linha corrente, para ele adicionar a palavra na próxima posição
    current_index <- current_index + 1
  }

  # caso o tamanho na corrente seja maior que zero, ou seja,
  # exista um resto que não foi adicionado porque ele não ultrapassou o limite
  # adicionar esse resto à lista final
  if (length(current) > 0) {
    final[[final_index]] <- paste(current, collapse = " ")
  }

  # retorna junção da lista final, separado por "\n"
  paste(final, collapse = "\n")
}
