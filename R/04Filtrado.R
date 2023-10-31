#' @title Filtrar un dataFrame
#' 
#' @description Dado un dataFrame filtra las variables segun la entropia, el AUC o la varianza
#' @param df El dataFrame a filtrar
#' @param varianza Un valor logico que indica si se quiere filtrar por la varianza
#' @param AUC Un valor logico que indica si se quiere filtrar por el AUC
#' @param ent Un valor logico que indica si se quiere filtrar por la entropia
#' @param varUmbr El umbral de la varianza
#' @param AUCUmbr El umbral del AUC
#' @param entUmbr El umbral de la entropia
#' @param varOP El operador que se utilizara al comparar la varianza
#' @param AUCOP El operador que se utilizara al comparar el AUC
#' @param entOP El operador que se utilizara al comparar la entropia
#' @return El dataFrame filtrado
#' @examples 
#' n <- 20
#' df <- data.frame(a.x = sample(1:100, n, replace = TRUE),
#' a.y = sample(c(TRUE, FALSE), n, replace = TRUE),
#' b = sample(c("a", "b", "c", "d"), n, replace = TRUE),
#' c.x = sample(1:50, n, replace = TRUE),
#' c.y = sample(c(TRUE, FALSE), n, replace = TRUE),
#' d = sample(c("a", "b", "c"), n, replace = TRUE),
#' e = sample(c("a", "b"), n, replace = TRUE),
#' f = sample(50:100, n, replace = TRUE))
#' filtrar(df, ent = TRUE, entUmbr = 1.6, entOP = "LO")
filtrar <- function(df, varianza = FALSE, AUC = FALSE, ent = FALSE,
                    varUmbr = 5, AUCUmbr = 0.5, entUmbr = 0.5,
                    varOP = "EQ", AUCOP = "EQ", entOP = "EQ"){
  # Se garantiza que df es un dataFrame
  if(!is.data.frame(df)){
    stop("El df de entrada debe ser un data.frame")
  }
  # Se garantiza que las variables logicas solo van por detras de las variables numericas
  if(!all(sapply(1:length(df), function(i){ if (!is.logical(df[,i])){
    # Si no es logica, no hay problema
    return(TRUE)
  } else{
    if (i == 1){
      # Si la primera columna es logica no se cumple la condicion
      return(FALSE)
    } else {
      # Si la anterior columna es numerica se cumple la condicion, si no, no
      return(is.numeric(df[,i-1]))
    }}}))){
    stop("Las variables logicas solo pueden ir despues de una variable numerica")
  }
  # En un principio, todas las variables pasan el filtro
  resultados <- rep(TRUE, length(df))
  # Si se quiere filtrar por la entropia
  if (ent){
    # Se consiguen los indices de las columnas que no sean numericas ni logicas
    indices <- sapply(df, FUN = function(x) { return(!is.numeric(x) & !is.logical(x)) })
    # Las columnas numericas y logicas pasan el filtro, los otros por ahora no
    resultadosEnt <- !indices
    # Se calculan las entropias de las columnas categoricas
    entropias <- rep(NaN, length(df))
    entropias[indices] <- unlist(lapply(df[,indices], entropy))
    # Se ensenan las entropias
    cat("Entropias: ", entropias, "\n")
    # Se hace la comparacion. Si cumplen la condicion, pasan el filtro
    switch (entOP, 
            LO={
              comparacion <- entropias < entUmbr
            },
            HI={
              comparacion <- entropias > entUmbr
            },
            {
              comparacion <- entropias == entUmbr
            })
    # Se actualizan los resultados generales
    resultados <- resultados & (resultadosEnt | comparacion)
  }
  # Si se quiere filtrar por el AUC
  if (AUC){
    # Se calcula el valor AUC de las columnas numericas que van seguidas por una logica
    valoresAUC <- sapply(1:length(df), FUN = function(i) {
      # Si es logico, se calcula el AUC con la columna anterior
      if (is.logical(df[,i])) {
        return(calculoAUC(df[,(i-1):i]))
        # Si es la ultima columna, no se devuelve nada
      } else if (i == length(df)){ 
        return(NaN)
      }
      # Si es numerico y va seguido de un logico, se calcula el AUC con la columna siguiente
      else if (is.numeric(df[,i]) & is.logical(df[,i+1])){
        return(calculoAUC(df[,i:(i+1)]))
        # Si no, no se devuelve nada
      } else {
        return(NaN)
      }})
    # Se muestran los valores AUC
    cat("Valores AUC: ", valoresAUC, "\n")
    # Las columnas categoricas pasan el filtro, los otros por ahora no
    resultadosAUC <- is.nan(valoresAUC)
    # Se hace la comparacion. Si cumplen la condicion, pasan el filtro
    switch (AUCOP, 
            LO={
              comparacion <- valoresAUC < AUCUmbr
            },
            HI={
              comparacion <- valoresAUC > AUCUmbr
            },
            {
              comparacion <- valoresAUC == AUCUmbr
            })
    # Se actualizan los resultados generales
    resultados <- resultados & (resultadosAUC | comparacion)
  }
  if (varianza){
    # Se calcula la varianza de las columnas numericas
    varianzas <- sapply(1:length(df), FUN = function(i) {
      # Si es logica, se calcula la varianza de la columna anterior
      if (is.logical(df[,i])) {
        return(calculoVarianza(df[,i-1]))
        # Si es numerica se calcula la varianza de la columna
      } else if (is.numeric(df[,i])){ 
        return(calculoVarianza(df[,i]))
        # Si no, no se devuelve nada
      } else {
        return(NaN)
      }})
    # Se muestran las varianzas
    cat("Varianzas: ", varianzas, "\n")
    # Las columnas categoricas pasan el filtro, los otros por ahora no
    resultadosVar <- is.nan(varianzas)
    # Se hace la comparacion. Si cumplen la condicion, pasan el filtro
    switch (varOP, 
            LO={
              comparacion <- varianzas < varUmbr
            },
            HI={
              comparacion <- varianzas > varUmbr
            },
            {
              comparacion <- varianzas == varUmbr
            })
    # Se actualizan los resultados generales
    resultados <- resultados & (resultadosVar | comparacion)
  }
  # Se devuelven solo las columnas que han pasado el filtro
  return(df[,resultados])
}


