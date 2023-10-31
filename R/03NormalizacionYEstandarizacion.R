
#' @title Estandarizar un vector
#' 
#' @description Dado un vector de tipo numerico estandariza dicho vector
#' @param x El vector numerico a estandarizar
#' @return El vector estandarizado
#' @examples 
#' x <- sample(1:100, 50)
#' xest <- estandarizar(x)
estandarizar <- function(x){
  # Se garantiza que el vector es numerico
  if (!is.numeric(x)){
    stop("El vector x debe ser numerico. Clase del vector x proporcionado: ", class(x))
  }
  # Se computa la formula de estandarizacion
  return((x - mean(x))/sd(x))
}

#' @title Normalizar un vector
#' 
#' @description Dado un vector de tipo numerico normaliza dicho vector
#' @param x El vector numerico a normalizar
#' @return El vector normalizado
#' @examples 
#' x <- sample(1:100, 50)
#' xnorm <- normalizar(x)
normalizar <- function(x){
  # Se garantiza que el vector es numerico
  if (!is.numeric(x)){
    stop("El vector x debe ser numerico. Clase del vector x proporcionado: ", class(x))
  }
  # Se computa la formula de normalizacion
  return((x - min(x))/(max(x) - min(x)))
}

#' @title Estandarizar un dataFrame
#' 
#' @description Dado un dataFrame con columnas numericas estandariza dicho dataFrame
#' @param df El dataFrame a estandarizar
#' @return El dataFrame estandarizado
#' @examples 
#' df <- data.frame( x = sample(1:100, 50),
#' y = sample(1:100, 50),
#' z = sample(1:100, 50))
#' dfest <- estandarizarDF(df)
estandarizarDF <- function(df){
  # Se garantiza que todas las columnas son numericas
  if(!all(apply(df, MARGIN = 2, is.numeric))){
    stop("Todas las variables deben ser numericas")
  }
  # Se estandariza cada columna
  return(apply(df, MARGIN = 2, FUN = estandarizar))
}

#' @title Normalizar un dataFrame
#' 
#' @description Dado un dataFrame con columnas numericas normaliza dicho dataFrame
#' @param df El dataFrame a normalizar
#' @return El dataFrame normalizado
#' @examples 
#' df <- data.frame( x = sample(1:100, 50),
#' y = sample(1:100, 50),
#' z = sample(1:100, 50))
#' dfnorm <- normalizarDF(df)
normalizarDF <- function(df){
  # Se garantiza que todas las columnas son numericas
  if(!all(apply(df, MARGIN = 2, is.numeric))){
    stop("Todas las variables deben ser numericas")
  }
  # Se normaliza cada columna
  return(apply(df, MARGIN = 2, FUN = normalizar))
}




