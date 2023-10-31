#' @title Calcular la entropia de un vector
#' 
#' @description Dado un vector de tipo numerico o categorico calcula la entropia
#' @param x.discretized El vector de entrada
#' @return La entropia del vector
#' @examples 
#' x.discretized <- factor(c("a", "c", "c", "a", "c"), levels = c("a", "b", "c"))
#' entropy(x.discretized)
entropy <- function(x.discretized){
  # Se garantiza que el vector de entrada no es logico
  if(is.logical(x.discretized)){
    stop("El vector de entrada debe ser categorico o numerico")
  }
  # Se calculan el numero de ocurrencias de cada elemento y se dividen por la longitud de la lista. 
  # Es decir, se calculan las probabilidades
  p <- table(x.discretized)/length(x.discretized)
  # Con cada probabilidad se calcula -pi*log2(pi)
  entr <- -p * log2(p)
  # En el caso de que pi sea cero, -pi*log2(pi) devuelve NaN. Para no dar errores, estos valores se convierten a 0
  entr[entr == "NaN"] = 0
  # Se suman todos los valores
  entr <- sum(entr)
  return(entr)
}

#' @title Calcular TPR y FPR
#' 
#' @description Dado un dataFrame con una columna numerica y una boolear y un valor de corte, calula el TPR y FPR
#' @param valor.corte El valor de corte
#' @param df Un dataFrame con una columna numerica y una boolear
#' @return La entropia del vector
TPRyFPR <- function(valor.corte, df){
  # Se garantiza que el dataFrame solo tiene dos columnas
  if (length(df) != 2){
    stop("El dataFrame debe tener dos columnas")
  }
  # Se garantiza que la primera columna es numerica y la segunda logica
  if (!(is.numeric(df[,1]) & is.logical(df[,2]))){
    stop("La primera columna debe ser numerica y la segunda logica")
  }
  # Se garantiza que valor.corte es un unico numero
  if (length(valor.corte) != 1 | !is.numeric(valor.corte)){
    stop("El valor de corte debe ser un unico numero.", )
  }
  # Se calcula la prediccion del modelo. Cuando el valor es menor o igual que el valor de corte devuelve TRUE, si no FALSE
  prediccion <- df[,1] <= valor.corte
  # Se calculan TP, FP, FN y FP contando cuantas veces ocurre cada convinacion
  sumas <- sapply(list(c(TRUE,TRUE), c(FALSE,TRUE), c(TRUE,FALSE), c(FALSE,FALSE)), FUN = function(x){ return(sum(x[1]==df[,2] & x[2]==prediccion)) })
  # Se ponen los nombres en las sumas
  names(sumas) <- c("TP", "FP", "FN", "TN")
  # Se calculan TPR y FPR
  TPR <- unname(sumas["TP"] / (sumas["TP"] + sumas["FN"]))
  FPR <- unname(sumas["FP"] / (sumas["FP"] + sumas["TN"]))
  return(list(FPR=FPR, TPR=TPR))
}

#' @title Calcular la curva ROC
#' 
#' @description Dado un dataFrame con una columna numerica y una boolear calcula los puntos de su curva ROC
#' @param df Un dataFrame con una columna numerica y una boolear
#' @return Los puntos de la curva ROC
curvaROC <- function(df){
  # Se garantiza que el dataFrame solo tiene dos columnas
  if (length(df) != 2){
    stop("El dataFrame debe tener dos columnas")
  }
  # Se garantiza que la primera columna es numerica y la segunda logica
  if (!(is.numeric(df[,1]) & is.logical(df[,2]))){
    stop("La primera columna debe ser numerica y la segunda logica")
  }
  # Por cada numero del dataFrame, se calculan el TPR Y FPR
  # Solo se toman estos valores ya que son los unicos que cambian los valores.
  puntos <- sapply(sort(df[,1]) , FUN = TPRyFPR, df = df)
  return(t(puntos))
}

#' @title Calcular la area AUC
#' 
#' @description Dado un dataFrame con una columna numerica y una boolear calcula su correspondiente area AUC
#' @param df Un dataFrame con una columna numerica y una boolear
#' @return El area AUC
#' @examples 
#' df <- data.frame(x = sample(1:100, 20),
#' y = sample(c(TRUE, FALSE), 20, replace = TRUE))
#' calculoAUC(df)
calculoAUC <- function(df){
  # Se garantiza que el dataFrame solo tiene dos columnas
  if (length(df) != 2){
    stop("El dataFrame debe tener dos columnas")
  }
  # Se garantiza que la primera columna es numerica y la segunda logica
  if (!(is.numeric(df[,1]) & is.logical(df[,2]))){
    stop("La primera columna debe ser numerica y la segunda logica")
  }
  # Se calcular los puntos de la curva ROC
  puntos <- curvaROC(df)
  # Se calcula el area de la curva ROC
  x <- as.numeric(puntos[,"FPR"])
  y <- as.numeric(puntos[,"TPR"])
  delta.x <- diff(x)
  mean.y  <- rowMeans(cbind(y[-1], y[-length(y)])) 
  return(sum(delta.x * mean.y))
}


#' @title Calcular la varianza
#' 
#' @description Dado un vector numerico, calcula su varianza
#' @param x Un vector numerico
#' @return Los puntos de la curva ROC
#' @examples 
#' n <- 20
#' x <- round(runif(n)*100)
#' calculoVarianza(x)
calculoVarianza <- function(x){
  # Se garantiza que el vector es numerico
  if (!is.numeric(x)){
    stop("El vector x debe ser numerico")
  }
  # Se computa la formula de la varianza
  return(sum((x-mean(x))**2)/(length(x) - 1))
}

#' @title Calcular la varianza, AUC y entropia
#' 
#' @description Dado un dataFrame con variables continuas y discretas, calcula la entropia de las discretas y AUC y varianza de las continuas 
#' @param df Un dataFrame con columnas categoricas y numericas. Las numericas deben estar seguidas por una logica
#' @return Un vector con las varianzas, AUC y entropias
#' @examples 
#' df <- data.frame(x = sample(1:100, 20),
#' y = sample(c(TRUE, FALSE), 20, replace = TRUE))
#' df <- data.frame( a = as.factor(sample(c("a", "b", "c"), 15, replace = TRUE)),
#' b = sample(1:100, 15),
#' c = sample(c(TRUE, FALSE), 15, replace = TRUE),
#' d = as.factor(sample(c("a", "b", "c"), 15, replace = TRUE)),
#' e = as.factor(sample(c("a", "b", "c"), 15, replace = TRUE)),
#' f = sample(1:100, 15),
#' g = sample(c(TRUE, FALSE), 15, replace = TRUE))
#' calculoMetricas(df)
calculoMetricas <- function(df){
  # Con cada indice de columna se le llama a una funcion
  resultados <- sapply(1:ncol(df), FUN = function(i){
                                            # Si la columna i es categorica se calcula la entropia de la columna
                                            if (is.factor(df[,i])){
                                              return(c(entr = entropy(df[,i])))
                                            # Si la columna i es numerica 
                                            } else if (is.numeric(df[,i]) | is.integer(df[,i])){
                                              # Si la columna i+1 no es logica salta un error
                                              if (i == ncol(df) | !is.logical(df[,i+1])){
                                                stop("La variable de la columna ", i, " no contiene su respectiva variable de clase binaria")
                                              }
                                              # Se calcula la varianza 
                                              varianza = calculoVarianza(df[,i])
                                              # Se calcula el AUC
                                              AUC = calculoAUC(df[,i:(i+1)])
                                              return(c(var = varianza, AUC = AUC))
                                            # Si la columna i es logica
                                            } else if (is.logical(df[,i])){
                                              # Si no tiene una columna numerica por delante, salta un error
                                              if (i == 1 | !is.numeric(df[,i-1])){
                                                stop("La variable binaria de la columna ", i, " no contiene su respectiva variable numerica")
                                              }
                                              #En este caso no se devuelve nada
                                              return()
                                            } else{
                                              stop("Solo se admiten variables discretas o numericas con su respectiva variable de clase binaria. Columna erronea: ", i)
                                            }
                                  } )
  # El resultado se convierte en un vector
  return(unlist(resultados))
}



