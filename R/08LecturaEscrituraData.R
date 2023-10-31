#setwd("/home/josu/Documentos/ESKOLA/Master/Software/Proyecto")
file.path = "TiemposActividades.csv"

#' @title Leer un dataFrame de un fichero csv
#' 
#' @description Dado un directorio de un fichero lee la informacion y crea un dataFrame
#' @param file.path Directorio del fichero
#' @param header Indicativo de si el fichero guarda los nombres de las columnas
#' @param rownames Indicativo de si el fichero guarda los nombres de las filas
#' @param sep Separador que se ha utilizado en el fichero csv
#' @return El dataFrame con toda la informacion
#' @examples 
#' n <- 10
#' df <- data.frame( a = round(runif(n) * 100 ,2),
#' b = as.character(letters[1:n]),
#' c = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
#' d = sample(1:100, n),
#' e = round(runif(n) * 100 ,2),
#' f = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)))
#' writeDataFrame(df, "tabla.csv", header = FALSE, rownames = FALSE, sep = "#")
#' df <- readDataFrame("tabla.csv", header = FALSE, rownames = FALSE, sep = "#")
readDataFrame <- function(file.path, header = FALSE, rownames = FALSE, sep = "\t") {
  # Se crea la conexion
  con <- file(description=file.path, open="r")
  # Se leen todas las lineas
  lines <- readLines(con=con)
  # Se cierra la conexion
  close(con)
  # Si se ha indicado que aparecen los nombres de las columnas
  if (header){
    # Se consiguen los nombres de la primera linea
    nombresColumnas = unlist(strsplit(lines[1], sep))
    # Se elimina la primera linea
    lines <- lines[-1]
  }
  # Se dividen todas las lineas 
  dataFrame <- lapply(lines, FUN = function(row){
                                    return(unlist(strsplit(row, sep)))
                                   })
  # Se crea el dataFrame
  dataFrame <- as.data.frame(do.call(rbind, dataFrame))
  # Ha cada columna se le asigna su type adecuado
  dataFrame <- lapply(dataFrame, function(x) {
                                    # Si contiene puntos y se puede convertir a numeric, sera numerico
                                    if (any(grepl("[.]", x))){
                                      if (!any(is.na(suppressWarnings(as.numeric(as.character(x)))))){
                                        return(as.numeric(as.character(x)))
                                      }
                                    }
                                    # Si se puede convertir a integer, sera integer
                                    if (!any(is.na(suppressWarnings(as.integer(as.character(x)))))){
                                      return(as.integer(as.character(x)))
                                    # Si contiene menos que 6 valores distintos, sera factor
                                    } else if (length(table(x)) <= 5){
                                      return(as.factor(x))
                                    # Si no, sera character
                                    } else {
                                      return(as.character(as.character(x)))
                                    }
  })
  dataFrame <- data.frame(dataFrame, stringsAsFactors = FALSE)
  # Si se ha indicado que aparecen los nombres de las columnas se anaden los nombres de las columnas
  if (header){
    colnames(dataFrame) <- nombresColumnas
  }
  # Si se ha indicado que aparecen los nombres de las filas, se convierte la primera columna en nombres del dataFrame
  if (rownames){
    row.names(dataFrame) <- dataFrame[,1]
    dataFrame <- dataFrame[,-1]
  }
  return(dataFrame)
}

#' @title Escribir un dataFrame en un fichero csv
#' 
#' @description Dado un directorio de un fichero y un dataFrame, escribe el dataFrame en el fichero
#' @param dataFrame El dataFrame
#' @param file.path Directorio del fichero
#' @param header Indicativo de si se quieren guardar los nombres de las columnas
#' @param rownames Indicativo de si se quieren guardar los nombres de las filas
#' @param sep Separador que se quiere utilizar en el fichero csv
#' @examples 
#' n <- 10
#' df <- data.frame( a = round(runif(n) * 100 ,2),
#' b = as.character(letters[1:n]),
#' c = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
#' d = sample(1:100, n),
#' e = round(runif(n) * 100 ,2),
#' f = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)))
#' writeDataFrame(df, "tabla.csv", header = FALSE, rownames = FALSE, sep = "#")
writeDataFrame <- function(dataFrame, file.path, header = FALSE, rownames = FALSE, sep = "\t"){
  # Se garantiza que se ha dado un dataFrame como entrada
  if (!is.data.frame(dataFrame)){
    stop("Debes dar un data.frame como entrada")
  } 
  # Se habre la conexion
  con <- file(description = file.path, open="w")
  # Si se deben escribir los nombres de las columnas
  if (header){
    # Se consiguen los nombres
    nombresColumnas <- colnames(dataFrame)
    # Si se deben anadir los nombres de las filas se anade un nombre mas
    if (rownames){
      nombresColumnas <- c("ROWID", nombresColumnas)
    }
    # Se escribe la linea con los nombres
    writeLines(paste(nombresColumnas, collapse = sep), con=con)
  }
  # Se consiguen todas las lineas
  lines <- apply(dataFrame, MARGIN=1,
                 FUN=function(row) {
                   return(paste(row, collapse=sep))
                 })
  # Si se deben anadir los nombres de las filas, se anaden en las lineas
  if (rownames) {
    lines <- paste(rownames(dataFrame), lines, sep = sep)
  }
  # Se escriben las lineas
  writeLines(lines, con=con)
  # Se cierra la conexion
  close(con)
}









