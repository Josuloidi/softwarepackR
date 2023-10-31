#' @title Discretizar atributo a corde a unos puntos de corte
#' 
#' @description Dado un vector de tipo numerico y un vector de puntos de corte, discretiza el vector numerico utilizando los puntos de corte
#' @param x El vector numerico a discretizar
#' @param cut.points El vector de puntos de corte
#' @return Un vector de valores categoricos resultado de aplicar la discretizacion
discretize <- function (x, cut.points) {
  # Se garantiza que el vector x es numerico
  if (!is.numeric(x)){
    stop("El vector x debe ser numerico")
  }
  # Se garantiza que el vector de puntos de corte es numerico
  if (!is.numeric(cut.points)){
    stop("El vector cut.points debe ser numerico")
  }
  # Gracias a la funcion cut se consigue el vector numerico discretizado. En concreto, se consigue el indice (1,2,...) del intervalo al que pertenece cada elemento.
  x.discretized <- cut(x, c(-Inf, cut.points, Inf), labels = FALSE, include.lowest = TRUE)
  # Se consiguen los nombres de los intervalos pegando dos listas que contienen los cut points con un desplazamiento
  levels <- paste(c(-Inf, round(cut.points,2)), c(round(cut.points,2), Inf), sep=",")
  # La discretizacion se convierte en un factor. Como levels se utilizan los nombres conseguidos. Asi en cada posicion se guarda el nombre del intervalo al que pertenece.
  x.discretized <- factor(levels[x.discretized], levels = levels)
  return(x.discretized)
}


#' @title Discretizar atributo mediante Equal Width
#' 
#' @description Dado un vector de tipo numerico y un numero de intervalos esta funciun discretiza el vector utilizando la estrategia Equal Width
#' @param x El vector numerico a discretizar
#' @param num.bins Numero de intervalos que se desean crear
#' @return Un vector de valores categoricos resultado de aplicar el Equal Width y un vector numerico que guarda los puntos de corte
#' @examples 
#' x <- sample(1:100, 50)
#' num.bins <- 5
#' discretizeEW(x, num.bins)
discretizeEW <- function (x, num.bins) {
  # Se garantiza que el vector X es numerico
  if (!is.numeric(x)){
    stop("El vector x debe ser numerico.")
  } 
  # Se garantiza que num.bins es un unico numero entero
  if (length(num.bins) != 1 | round(num.bins) - num.bins != 0){
    stop("El numero de intervalos debe ser un unico numero entero.", )
  }
  # Mediante la funcion seq se consiguen los cut points. La distancia entre los cut points es max(x) - min(x)) / num.bins 
  cut.points <- seq(min(x), max(x), (max(x) - min(x)) / num.bins)[2:num.bins]
  # Teniendo x y los cut points se consigue el vector x discretizado
  x.discretized <- discretize(x, cut.points)
  return(list(x.discretized = x.discretized, cut.points = cut.points))
}

#' @title Discretizar atributo mediante Equal Frecuency
#' 
#' @description Dado un vector de tipo numerico y un numero de intervalos esta funciun discretiza el vector utilizando la estrategia Equal Frecuency
#' @param x El vector numerico a discretizar
#' @param num.bins Numero de intervalos que se desean crear
#' @return Un vector de valores categoricos resultado de aplicar el Equal Frecuency y un vector numerico que guarda los puntos de corte
#' @examples 
#' x <- sample(1:100, 50)
#' num.bins <- 5
#' discretizeEF(x, num.bins)
discretizeEF <- function (x, num.bins) {
  # Se garantiza que x es un vector numerico
  if (!is.numeric(x)){
    stop("El vector x debe ser numerico.")
  } 
  # Se garantiza que num.bins es un unico numero entero
  if (length(num.bins) != 1 | round(num.bins) - num.bins != 0){
    stop("El numero de intervalos debe ser un unico numero entero.", )
  }
  # La frecuencia minima de todos los intervalos
  frecuency <- length(x) %/% num.bins
  # La frecuencia de los primeros intervalos en el caso de que length(x) no sea multiplo del numero de intervalos
  frecuency1 <- ceiling(length(x)/num.bins)
  # Si length(x) es multiplo del numero de intervalos
  if (frecuency1 == frecuency){
    # En cada rango habra la misma cantidad de numeros, por lo que los indices
    # de estos numeros mantendran la misma frecuencia 
    cutInd <- seq(frecuency, length(x), frecuency)
  } else{
    # En los primeros rangos habra mas numeros, por lo que los indices seguiran
    # esta frecuencia (frecuency1) y los siguientes una distinta (frecuency).
    centro <- length(x)%%num.bins * frecuency1
    cutInd <- c(seq(frecuency1, centro, frecuency1), seq(centro + frecuency, length(x), frecuency))
  }
  # Se ordenan todos los numeros y se toman los cutPoints que indican los indices
  cut.points <- sort(x)[cutInd][1:num.bins-1]
  # Se discretizan los numeros
  x.discretized <- discretize(x, cut.points)
  return(list(x.discretized=x.discretized, cut.points=cut.points))
}


#' @title Discretizar atributo mediante Jenks Natural Breaks
#' 
#' @description Dado un vector de tipo numerico y un numero de intervalos esta funciun discretiza el vector utilizando la estrategia Jenks Natural Breaks
#' @param x El vector numerico a discretizar
#' @param num.bins Numero de intervalos que se desean crear
#' @return Un vector de valores categoricos resultado de aplicar el Jenks Natural Breaks y un vector numerico que guarda los puntos de corte
#' @examples 
#' x <- sample(1:100, 50)
#' num.bins <- 5
#' discretizeJNB(x, num.bins)
discretizeJNB <- function (x, num.bins) {
  # Se garantiza que x es un vector numerico
  if (!is.numeric(x)){
    stop("El vector x debe ser numerico.")
  } 
  # Se garantiza que num.bins es un unico numero entero
  if (length(num.bins) != 1 | round(num.bins) - num.bins != 0){
    stop("El numero de intervalos debe ser un unico numero entero.", )
  }
  # Se ordena el vector x
  ordenado <- sort(x)
  # Se calculan las diferencias entre numeros que estan al lado
  diferencias <- diff(ordenado)
  # Se nombran los elementos de la lista de diferencias para saber cual es el indice del primer numero en cada diferencia
  names(diferencias) <- 1:(length(x)-1)
  # Se ordena la lista de diferencias y se toman las num.bins-1 distancias maximas
  diferenciasMaximas <- sort(diferencias, decreasing = TRUE)[1:num.bins-1]
  # De la lista ordenada x, se toman los numeros que pertenecen a las distancias maximas
  cut.points <- sort(ordenado[as.numeric(names(diferenciasMaximas))])
  # Se discretizan los numeros
  x.discretized <- discretize(x, cut.points)
  return(list(x.discretized=x.discretized, cut.points=cut.points))
}



#' @title Discretizar dataFrame mediante Jenks Natural Breaks
#' 
#' @description Dado un dataFrame con columnas numericas y un numero de intervalos esta funciun discretiza todas las columnas utilizando la estrategia indicada
#' @param df El dataFrame a discretizar
#' @param num.bins Numero de intervalos que se desean crear
#' @param discretizacion El metodo de discretizacion
#' @return Un dataFrame con columnas categoricas resultado de aplicar la discretizacion aplicada a ellas
#' @examples 
#' n <- 50
#' df <- data.frame(x = round(runif(n)*100),
#' y = round(runif(n)*100),
#' z = round(runif(n)*100))
#' num.bins <- 4
#' discretizarColumnas(df,num.bins,"EW")
discretizarColumnas <- function(df, num.bins, discretizacion = "TODAS"){
  # Se garantiza que el dataFrame solo contiene columnas numericas
  if(!all(apply(df, MARGIN = 2, FUN = is.numeric))){
    stop("Todo el data.frame debe ser numerico")
  }
  # Se garantiza que num.bins es un unico numero entero
  if (!is.numeric(num.bins) | length(num.bins) != 1 | round(num.bins) - num.bins != 0){
    stop("El numero de intervalos debe ser un unico numero entero.", )
  }
  # Por cada columna se ejecuta una funcion
  discretizados <- apply(df, MARGIN = 2, FUN = function(x) {
                                                # Segun el valor que tiene la variable discretizacion se calcula un distinto resultado 
                                                switch (discretizacion, 
                                                        EW={
                                                          # Equal Width
                                                          res <- data.frame(EW = discretizeEW(x,num.bins)$x.discretized)
                                                        },
                                                        EF={
                                                          # Equal Frecuency
                                                          res <- data.frame(EF = discretizeEF(x,num.bins)$x.discretized)
                                                        },
                                                        JNB={
                                                          # Jenks Natural Breaks
                                                          res <- data.frame(EF = discretizeJNB(x,num.bins)$x.discretized)
                                                        },
                                                        {
                                                          # Todas
                                                          discretizadosEW <- discretizeEW(x,num.bins)$x.discretized
                                                          discretizadosEF <- discretizeEF(x,num.bins)$x.discretized
                                                          discretizadosJNB <- discretizeJNB(x,num.bins)$x.discretized
                                                          # En el resultado se anaden la columna inicial y los tres resultados
                                                          res <- data.frame(x=x,EW=discretizadosEW,EF=discretizadosEF,JNB=discretizadosJNB)
                                                        })
                                                        return(res)
                                                    })
  # El resultado se convierte en un dataFrame
  return(do.call(cbind, discretizados))
}



