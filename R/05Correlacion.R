
#' @title Calcular la correlacion
#' 
#' @description Dado dos vectores calcula la correlacion si los dos vectores son numericos, y la informacion mutua si uno de los dos vectores es categorico
#' @param x El primer vector
#' @param y El segundo vector
#' @return La correlacion o la informacion mutua
#' @examples 
#' n <- 100
#' x <- sample(1:200, n, replace = TRUE)
#' y <- sample(1:200, n, replace = TRUE)
#' correlacion(x,y)
correlacion <- function(x, y){
  # Se garantiza que los dos vectores tienen la misma longitud
  if (length(x) != length(y)){
    stop("Los dos vectores deben tener la misma longitud")
  }
  # Se garantiza que ningun vector es logico
  if (is.logical(x) | is.logical(y)){
    stop("Las variables deben ser categoricas o numericas")
  }
  # Si los dos son numericos
  if (is.numeric(x) & is.numeric(y)){
    # Se computa la formula de la correlacion
    res <- sum((x - mean(x)) * (y - mean(y))) / sqrt( sum((x - mean(x))**2) * sum((y - mean(y))**2))
  } else{
    # Se computa la formula de la entropia
    res <- entropy(x) + entropy(y) - entropy(paste(x,y))
  }
  return(res)
}

#' @title Calcular la correlacion de una variable
#' 
#' @description Dado un vector x y un dataFrame, calcula las correlaciones que mantiene x con todas las columnas del dataFrame
#' @param x El vector
#' @param df El dataFrame
#' @return Las correlaciones que mantiene x con todas las columnas del dataFrame
correlacionUnaVariable <- function(x, df){
  # Se garantiza que ninguna columna del dataFrame ni x es logica
  if (any(apply(df, MARGIN = 2, FUN = is.logical)) | is.logical(x)){
    stop("Todas las variables deben ser numericas o categoricas")
  }
  # Para cada columna del df se calcula la correlacion con x
  return( sapply(df, FUN = correlacion, x) )
}

#' @title Calcular la correlacion de un dataFrame
#' 
#' @description Dado un dataFrame calcula la correlacion por pares entre variables
#' @param df El dataFrame
#' @return Las matriz de correlaciones
#' @examples 
#' x <- sample(1:500, 100, replace = TRUE)
#' df <- data.frame(a = sample(1:500, 100, replace = TRUE),
#' b = 1:100,
#' c = sample(1:500, 100, replace = TRUE),
#' d = seq(1, 1000, 10))
#' correlacionDF(df)
correlacionDF <- function(df){
  # Se garantiza que ninguna columna del dataFrame es logica
  if (any(apply(df, MARGIN = 2, FUN = is.logical))){
    stop("Todas las variables deben ser numericas o categoricas")
  }
  # Por cada columna del dataFrame, se calcula la correlacion que mantiene con todo el dataFrame
  res <- sapply(df, FUN = correlacionUnaVariable, df)
  # El resultado se convierte en una matriz
  res <- matrix(res, nrow = length(df), byrow = TRUE)
  # Se actualizan los nombres de las filas y columnas de la matriz
  row.names(res) <- names(df)
  colnames(res) <- names(df)
  return(res)
}

