#' @title Visualizar curva ROC
#' 
#' @description Dado un dataFrame con una columna numerica y una boolear visualiza su curva ROC
#' @param df El dataFrame
#' @examples 
#' df <- data.frame(a = sample(1:500, 100, replace = TRUE),
#' b = sample(c(FALSE, TRUE), 100, replace = TRUE))
#' plotROC(df)
plotROC <- function(df){
  # Se garantiza que df es un dataFrame
  if (!is.data.frame(df)){
    stop("El df de entrada debe ser un data.frame")
  }
  # Se garantiza que el dataFrame tiene una columna numerica y una logica
  if (length(df) != 2 || !is.numeric(df[,1]) || !is.logical(df[,2])){
    stop("El data.frame de entrada debe tener dos columnas, la primera de tipo numeric y la segunda de tipo logical")
  }
  # Se calcula la curva ROC
  curva <- curvaROC(df)
  # Se calcula el AUC
  AUC <- calculoAUC(df)
  # Se visualiza la curva ROC
  plot(curva[,2], curva[,1], type='l', col='red', main=paste("Curva ROC (", round(AUC, 2), ")", sep= ""), xlab="FPR", ylab="TPR", bty='n')
}

#' @title Visualizar correlaciones
#' 
#' @description Dado un dataFrame calcula la correlacion por pares entre variables
#' @param df El dataFrame
#' @examples 
#' library(ggplot2)
#' library(reshape2)
#' df <- data.frame(a = sample(1:500, 100, replace = TRUE),
#' b = 1:100,
#' c = sample(1:500, 100, replace = TRUE),
#' d = seq(1, 1000, 10))
#' plotCorrelaciones(df)
plotCorrelaciones <- function(df){
  if(!requireNamespace("ggplot2") | !requireNamespace("reshape2")){
    stop("La funcion 'plotCorrelaciones' requiere la instalacion del
         paquete ggplot2 y reshape2. Instala estos paquetes y
         vuelve a ejecutar la funcion")
  }
  # Se garantiza que df es un dataFrame
  if (!is.data.frame(df)){
    stop("El df de entrada debe ser un data.frame")
  }
  # Se garantiza que en el dataFrame no hay ninguna columna logica
  if (any(apply(df, MARGIN = 2, FUN = is.logical))){
    stop("Todas las variables deben ser numericas o categoricas")
  }
  # Se calcula la matriz de correlaciones
  correlaciones <- correlacionDF(df)
  # Se visualiza un heatmap utilizando la matriz de correlaciones
  correlaciones <- melt(correlaciones)
  names(correlaciones) <- c("Variable1", "Variable2", "Correlacion")
  ggplot(correlaciones, aes(x=Variable2, y=Variable1, fill=Correlacion)) + geom_tile() + 
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid = element_blank(),
                       axis.ticks = element_blank())
}


#' @title Visualizar boxPlots
#' 
#' @description Dado un dataFrame con variables numericas visualiza todos los boxplots de las variables
#' @param df El dataFrame
#' @examples 
#' df <- data.frame(a = sample(1:500, 100, replace = TRUE),
#' b = sample(1:200, 100, replace = TRUE),
#' c = sample(100:800, 100, replace = TRUE),
#' d = sample(1:1000, 100, replace = TRUE))
#' plotBoxPlots(df)
plotBoxPlots <- function(df){
  # Se garantiza que df es un dataFrame
  if (!is.data.frame(df)){
    stop("El df de entrada debe ser un data.frame")
  }
  # Se garantiza que todas las variables son numericas
  if (!all(apply(df, MARGIN = 2, is.numeric))){
    stop("Todas las variables deben ser numericas")
  }
  # Se visualizan los boxplots
  data <- data.frame(
    name = rep(colnames(df), each = nrow(df)),
    value = unlist(df)
  )
  boxplot(data$value~data$name, xlab = "Variables", ylab = "")
}


#' @title Visualizar barras discretizadas
#' 
#' @description Dado un vector numerico, un numero de intervalos y un metodo de discretizacion, discretiza el vector y muestra la frecuancia de todos los intervalos
#' @param x El vector numerico
#' @param num.bins El numero de intervalos
#' @param metodo El metodo de discretizacion (EW, EB o JNB)
#' @examples 
#' x<-sample(1:1000, 100)
#' num.bins <- 5
#' plotBarrasDiscretizadas(x, num.bins, "EF")
plotBarrasDiscretizadas <- function(x, num.bins, metodo = "EW"){
  # Se garantiza que el vector x es numerico
  if (!is.numeric(x)){
    stop("El vector x debe ser numerico")
  } 
  # Se garantiza que el numero de intervalos es un unico numero entero
  if (length(num.bins) != 1 | round(num.bins) - num.bins != 0){
    stop("El numero de intervalos debe ser un unico numero entero.", )
  }
  # Se discretiza el vector numerico segun el metodo indicado
  switch (metodo, 
          EF={
            x.discretized <- discretizeEF(x,num.bins)$x.discretized
          },
          JNB={
            x.discretized <- discretizeJNB(x,num.bins)$x.discretized
          },
          {
            x.discretized <- discretizeEW(x,num.bins)$x.discretized
          })
  # Se calculan las frecuencias
  frecuencias <- table(x.discretized)
  # Se visualiza el diagrama de barras
  barplot(frecuencias, main="Vector X discretizado", xlab="Intervalo", ylab="Frecuencia")
}