
#' Una clase S4 para representar dataFrames
#'
#' @slot colNames Los nombres de las columnas
#' @slot rowNames Los nombres de las filas
#' @slot colNum Numero de columnas
#' @slot rowNum Numero de filas
#' @slot data La lista que guarda todos los datos
setClass(Class="DataFrame",
         slots=c("colNames"="character", "rowNames"="character", "colNum"="numeric", "rowNum"="numeric", "data"="list"))

checkValidityDataFrame <- function(object) {
  if(!all(sapply(object@data, length) == object@rowNum)){
    stop("Todas las variables deben tener el mismo numero de datos")
  }
  if(length(object@colNames) != object@colNum){
    stop("Debe haber tantos nombres de columnas como columnas")
  }
  if(length(object@rowNames) != object@rowNum){
    stop("Debe haber tantos nombres de filas como filas")
  }
  return(TRUE)
}

setValidity(Class="DataFrame", method=checkValidityDataFrame)

#' Constructor de la clase DataFrame
#'
#' @description Esta funcion crea una instancia de \code{\linkS4class{dataFrame}}
#'
#' @param data Lista que guarda toda la informacion
#' @param colNames Los nombres de las columnas
#' @param rowNames Los nombres de las filas
#' @return Un objeto de la clase \code{\linkS4class{DataFrame}}
dataFrame <- function(data, colNames = NULL, rowNames = NULL){
  # Se garantiza que data es una lista
  if (!is.list(data)){
    stop("El parametro data debe ser una lista")
  }
  # Se consigue el numero de columnas
  colNum <- length(data)
  # Se consigue el numero de filas
  if (length(data) == 0){
    rowNum <- 0
  } else{
    rowNum <- length(data[[1]])
  }
  # Si no se han dado los nombres de la columnas
  if (is.null(colNames)){
    # Si la lista tiene nombres
    if (!is.null(names(data))) {
      # Se mantendran los nombres de la lista
      colNames <- names(data)
    } else {
      # Se crean los nombres de las columnas: x1, x2, ...
      colNames <- paste("x", 1:colNum, sep="")
    }
  }
  # Si no se han dado los nombres de las filas
  if (is.null(rowNames)) {
    # Se crean los nombres de las filas: y1, y2, ...
    rowNames <- paste("y", 1:rowNum, sep="")
  }
  # Se crea el objeto DataFrame
  object <- new("DataFrame", colNames=colNames, rowNames=rowNames, colNum=colNum, rowNum=rowNum, data=data)
  return(object)
}

setGeneric(name="addRow", def=function(x, line, name = NULL) standardGeneric("addRow"))

#' A?adir fila al dataFrame
#'
#' @description Esta funcion a?ade una fila a un objeto de tipo \code{\linkS4class{dataFrame}}
#'
#' @param x El dataFrame
#' @param line La nueva linea que se quiere a?adir
#' @param name El nombre de la fila
#' @return Un DataFrame con la fila a?adida
#' @examples 
#' DF <- dataFrame(list(a = c(1,2,3,3,5,7,8),
#' b = c("x", "y", "z", "x", "x", "y", "x"),
#' c = c(4,5,6,6,1,2,3),
#' d = c(7,8,9,7,6,5,3),
#' e = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)))
#' DF = addRow(DF, list(9, "x", 3, 3, FALSE))
setMethod(f="addRow",
          signature="DataFrame",
          definition=function(x,line,name = NULL){
            # Se garantiza que la nueva linea tiene la longitud correcta
            if (length(line) != x@colNum){
              stop("La nueva linea debe tener el mismo numero de columnas que las otras")
            }
            # Se a?ade la nueva fila
            x@data <- mapply(c, x@data, line, SIMPLIFY = FALSE)
            # Se actualiza el numero de filas
            x@rowNum <- x@rowNum + 1
            # Si no se ha dado un nombre, se genera uno
            if (is.null(name)){
              name <- paste("y", x@rowNum, sep="")
            }
            # Se a?ade el nombre
            x@rowNames <- c(x@rowNames, name)
            return(x)
          })

setGeneric(name="addCol", def=function(x, column, name = NULL) standardGeneric("addCol"))

#' A?adir columna al dataFrame
#'
#' @description Esta funcion a?ade una columna a un objeto de tipo \code{\linkS4class{dataFrame}}
#'
#' @param x El dataFrame
#' @param column La nueva columna que se quiere a?adir
#' @param name El nombre de la columna
#' @return Un DataFrame con la columna a?adida
#' @examples 
#' DF <- dataFrame(list(a = c(1,2,3,3,5,7,8),
#' b = c("x", "y", "z", "x", "x", "y", "x"),
#' c = c(4,5,6,6,1,2,3),
#' d = c(7,8,9,7,6,5,3),
#' e = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)))
#' DF = addCol(DF, rep(TRUE, 7))
setMethod(f="addCol",
          signature="DataFrame",
          definition=function(x,column,name = NULL){
            # Se garantiza que la columna tiene la longitud correcta
            if (length(column) != x@rowNum){
              stop("La nueva columna debe tener el mismo numero de lineas que las otras")
            }
            # Se a?ade la columna
            x@data = append(x@data, list(column))
            # Se actualiza el numero de columnas
            x@colNum <- x@colNum + 1
            # Si no se ha dado un nombre se genera uno
            if (is.null(name)){
              name <- paste("x", x@colNum, sep="")
            }
            # Se a?ade el nombre
            x@colNames <- c(x@colNames, name)
            return(x)
          })

setGeneric(name = "print", def = function(x) standardGeneric("print"))

#' Imprimir dataFrame
#'
#' @description Esta función imprime un objeto de tipo \code{\linkS4class{dataFrame}}
#'
#' @param x El dataFrame
#' @examples
#' DF <- dataFrame(list(a = c(1,2,3,3,5,7,8),
#' b = c("x", "y", "z", "x", "x", "y", "x"),
#' c = c(4,5,6,6,1,2,3),
#' d = c(7,8,9,7,6,5,3),
#' e = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)))
setMethod(f="print",
          signature="DataFrame",
          definition=function(x){
            # Se imprime el nombre de la clase
            cat("DataFrame\n")
            # Se imprimen los nombres de las columnas
            cat("   ", paste(x@colNames ,collapse = " "))
            # Se consiguen todas las líneas para imprimir
            lineas <- lapply(1:x@rowNum, function(i){
              return(paste(c(x@rowNames[i], unlist(lapply(x@data, function(j){return(j[i])}))), collapse = " "))
            })
            # Se imprimen todas las líneas
            cat("\n", paste(unlist(lineas), collapse = "\n"))
          })


setGeneric(name="getCols", def=function(x, names) standardGeneric("getCols"))

#' Conseguir columnas de dataFrame
#'
#' @description Esta funcion consigue las columnas indicadas de un objeto de tipo \code{\linkS4class{dataFrame}}
#'
#' @param x El dataFrame
#' @param names Los nombres de las columnas
#' @return Un DataFrame que contiene solo las columnas indicadas
#' @examples 
#' DF <- dataFrame(list(a = c(1,2,3,3,5,7,8),
#' b = c("x", "y", "z", "x", "x", "y", "x"),
#' c = c(4,5,6,6,1,2,3),
#' d = c(7,8,9,7,6,5,3),
#' e = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)))
#' DFaux <- getCols(DF, c("a", "c"))
setMethod(f="getCols",
          signature="DataFrame",
          definition=function(x,names){
            # Se consiguen los indices de los nombres
            indices <- which(x@colNames %in% names)
            # Se garantiza que se han encontrado todos los nombres
            if (length(indices) != length(names)){
              stop("Algun indice no existe")
            }
            # Se mantienen las columnas indicadas por los indices
            x@data <- x@data[indices]
            # Los nombres seran los indicados
            x@colNames <- x@colNames[indices]
            # El numero de columnas sera el mismo que de nombres
            x@colNum <- length(names)
            return(x)
          })

setGeneric(name="getRows", def=function(x, names) standardGeneric("getRows"))

#' Conseguir filas del dataFrame
#'
#' @description Esta funcion consigue las filas indicadas de un objeto de tipo \code{\linkS4class{dataFrame}}
#'
#' @param x El dataFrame
#' @param names Los nombres de las filas
#' @return Un DataFrame que contiene solo las filas indicadas
#' @examples 
#' DF <- dataFrame(list(a = c(1,2,3,3,5,7,8),
#' b = c("x", "y", "z", "x", "x", "y", "x"),
#' c = c(4,5,6,6,1,2,3),
#' d = c(7,8,9,7,6,5,3),
#' e = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)))
#' DFaux <- getRows(DF, c("y3", "y1"))
setMethod(f="getRows",
          signature="DataFrame",
          definition=function(x,names){
            # Se consiguen los indices de las filas
            indices <- which(x@rowNames %in% names)
            # Se garantiza que se han encontrado todas las filas
            if (length(indices) != length(names)){
              stop("Algun indice no existe")
            }
            # Se consigue la lista solo con las filas necesarias
            x@data <- lapply(x@data, FUN = function(x, ind) {return(x[ind])}, indices)
            # Los nombres seran los indicados
            x@rowNames <- x@rowNames[indices]
            # El numero de firas sera el mismo que de nombres
            x@rowNum <- length(names)
            return(x)
          })