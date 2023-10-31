## -----------------------------------------------------------------------------
library("softwarepack")

## -----------------------------------------------------------------------------
x <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4, 4.4)
num.bins <- 4

## -----------------------------------------------------------------------------
res <- discretizeEW(x,num.bins)
res

## -----------------------------------------------------------------------------
table(res$x.discretized)

## -----------------------------------------------------------------------------
y <- c(9.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4, 4.4)
res <- discretizeEW(y,num.bins)
table(res$x.discretized)

## -----------------------------------------------------------------------------
res <- discretizeEF(x,num.bins)
res

## -----------------------------------------------------------------------------
table(res$x.discretized)

## -----------------------------------------------------------------------------
y <- c(0.5, 1.2, 4.4, 5.3, 8.4, 10.2, 11.5, 20.5, 23.1, 80.1)
res <- discretizeEF(y,num.bins)
table(res$x.discretized)

## -----------------------------------------------------------------------------
res <- discretizeJNB(x,num.bins)
res

## -----------------------------------------------------------------------------
table(res$x.discretized)

## -----------------------------------------------------------------------------
n <- 10
df <- data.frame(x = round(runif(n)*100),
                y = round(runif(n)*100),
                z = round(runif(n)*100))
num.bins <- 4
df

## -----------------------------------------------------------------------------
discretizarColumnas(df,num.bins,"EW")

## -----------------------------------------------------------------------------
discretizarColumnas(df,num.bins,"EF")

## -----------------------------------------------------------------------------
discretizarColumnas(df,num.bins,"EF")

## -----------------------------------------------------------------------------
discretizarColumnas(df,num.bins)

## -----------------------------------------------------------------------------
x <- round(runif(n)*100)
calculoVarianza(x)

## -----------------------------------------------------------------------------
x <- round(runif(n)*100)
var(x)

## -----------------------------------------------------------------------------
n <- 20
df <- data.frame(x = sample(1:100, 20),
                 y = sample(c(TRUE, FALSE), 20, replace = TRUE))
df

## -----------------------------------------------------------------------------
calculoAUC(df)

## -----------------------------------------------------------------------------
x.discretized <- factor(c("a", "c", "c", "a", "c"), levels = c("a", "b", "c"))
entropy(x.discretized)

## -----------------------------------------------------------------------------
n <- 15
df <- data.frame( a = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  b = sample(1:100, n),
                  c = sample(c(TRUE, FALSE), n, replace = TRUE),
                  d = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  e = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  f = sample(1:100, n),
                  g = sample(c(TRUE, FALSE), n, replace = TRUE))
df

## -----------------------------------------------------------------------------
calculoMetricas(df)

## -----------------------------------------------------------------------------
df <- data.frame( a = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  b = sample(1:100, n),
                  d = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  e = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  f = sample(1:100, n),
                  g = sample(c(TRUE, FALSE), n, replace = TRUE))
tryCatch({
  calculoMetricas(df)
}, error = function(e) {
  print(conditionMessage(e))
})

## -----------------------------------------------------------------------------
df <- data.frame( a = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  c = sample(c(TRUE, FALSE), n, replace = TRUE),
                  d = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  e = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  f = sample(1:100, n),
                  g = sample(c(TRUE, FALSE), n, replace = TRUE))
tryCatch({
  calculoMetricas(df)
}, error = function(e) {
  print(conditionMessage(e))
})

## -----------------------------------------------------------------------------
n <- 20
x <- sample(1:100, n)
x_norm <- normalizar(x)
x_norm

## -----------------------------------------------------------------------------
cat("Mínimo: ", min(x_norm))
cat("Máximo: ", max(x_norm))

## -----------------------------------------------------------------------------
x_est <- estandarizar(x)
x_est

## -----------------------------------------------------------------------------
cat("Media: ", round(mean(x_est),2))
cat("Desviación estandar: ", round(sd(x_est),2))

## -----------------------------------------------------------------------------
n <- 10
df <- data.frame( x = sample(1:100, n),
                  y = sample(1:100, n),
                  z = sample(1:100, n),
                  t = sample(1:100, n))
df

## -----------------------------------------------------------------------------
dfnorm <- normalizarDF(df)
dfnorm

## -----------------------------------------------------------------------------
cat("Mínimos: \n", apply(dfnorm, MARGIN = 2, min),2)
cat("Máximos: \n", apply(dfnorm, MARGIN = 2, max),2)

## -----------------------------------------------------------------------------
dfest <- estandarizarDF(df)
dfest

## -----------------------------------------------------------------------------
cat("Medias: \n", round(apply(dfest, MARGIN = 2, mean),2))
cat("Desviaciones estandar: \n", round(apply(dfest, MARGIN = 2, sd),2))

## -----------------------------------------------------------------------------
set.seed(50)
n <- 20
df <- data.frame(a.x = sample(1:100, n, replace = TRUE),
                 a.y = sample(c(TRUE, FALSE), n, replace = TRUE),
                 b = sample(c("a", "b", "c", "d"), n, replace = TRUE),
                 c.x = sample(1:50, n, replace = TRUE),
                 c.y = sample(c(TRUE, FALSE), n, replace = TRUE),
                 d = sample(c("a", "b", "c"), n, replace = TRUE),
                 e = sample(c("a", "b"), n, replace = TRUE),
                 f = sample(50:100, n, replace = TRUE))
df

## -----------------------------------------------------------------------------
filtrar(df, ent = TRUE, entUmbr = 1.6, entOP = "LO")

## -----------------------------------------------------------------------------
filtrar(df, AUC = TRUE, AUCUmbr = 0.4, AUCOP = "HI")

## -----------------------------------------------------------------------------
filtrar(df, varianza = TRUE, varUmbr = 210, varOP = "LO")

## -----------------------------------------------------------------------------
filtrar(df, ent = TRUE, entUmbr = 1.6, entOP = "LO",
        AUC = TRUE, AUCUmbr = 0.4, AUCOP = "HI",
        varianza = TRUE, varUmbr = 210, varOP = "LO")

## -----------------------------------------------------------------------------
n <- 20
x <- sample(1:200, n, replace = TRUE)
y <- sample(1:200, n, replace = TRUE)
correlacion(x,y)

## -----------------------------------------------------------------------------
cor(x,y)

## -----------------------------------------------------------------------------
n <- 20
x <- sample(c("a", "b", "c"), n, replace = TRUE)
y <- sample(c("a", "b", "c"), n, replace = TRUE)
correlacion(x,y)

## -----------------------------------------------------------------------------
n <- 100
df <- data.frame(a = sample(1:500, n, replace = TRUE),
                 b = 1:n,
                 c = sample(1:500, n, replace = TRUE),
                 d = seq(1, 1000, 1000/n))
correlacionDF(df)

## -----------------------------------------------------------------------------
cor(df)

## -----------------------------------------------------------------------------
df <- data.frame(a= sample(c("a", "b", "c", "d"), n, replace = TRUE),
                 b = sample(c("e", "f", "g", "h"), n, replace = TRUE),
                 c = sample(c("a", "b", "c", "d"), n, replace = TRUE),
                 d = sample(c("e", "f", "g", "h"), n, replace = TRUE))
correlacionDF(df)

## -----------------------------------------------------------------------------
df <- data.frame(a = sample(1:50, n, replace = TRUE),
                 b = sample(c("a", "b", "c", "d"), 100, replace = TRUE),
                 c = sample(1:50, n, replace = TRUE),
                 d = sample(c("e", "f", "g", "h"), 100, replace = TRUE))
correlacionDF(df)

## -----------------------------------------------------------------------------
library("ggplot2")
library("reshape2")

## -----------------------------------------------------------------------------
df <- data.frame(a = sample(1:500, 100, replace = TRUE),
                 b = sample(c(FALSE, TRUE), 100, replace = TRUE))
plotROC(df)

## -----------------------------------------------------------------------------
n <- 100
df <- data.frame(a = sample(1:500, n, replace = TRUE),
                 b = 1:n,
                 c = sample(1:500, n, replace = TRUE),
                 d = seq(1, 1000, 1000/n))
plotCorrelaciones(df)

## -----------------------------------------------------------------------------
n <- 20
df <- data.frame(a= sample(c("a", "b", "c"), n, replace = TRUE),
                 b = sample(c("b", "c"), n, replace = TRUE),
                 c = sample(c("a", "b", "c"), n, replace = TRUE),
                 d = sample(c("b", "c"), n, replace = TRUE))
plotCorrelaciones(df)

## -----------------------------------------------------------------------------
df <- data.frame(a = sample(1:50, n, replace = TRUE),
                 b = sample(c("a", "b", "c", "d"), n, replace = TRUE),
                 c = sample(1:50, n, replace = TRUE),
                 d = sample(c("e", "f", "g", "h"), n, replace = TRUE))
plotCorrelaciones(df)

## -----------------------------------------------------------------------------
n <- 100
df <- data.frame(a = sample(1:100, n, replace = TRUE),
                 b = sample(1:500, n, replace = TRUE),
                 c = sample(50:150, n, replace = TRUE),
                 d = sample(100:500, n, replace = TRUE),
                 e = sample(50:300, n, replace = TRUE))
plotBoxPlots(df)

## -----------------------------------------------------------------------------
n <- 500
x <- sample(1:2000, n)
num.bins <- 4

## -----------------------------------------------------------------------------
cat(num.bins)
plotBarrasDiscretizadas(x, num.bins, "EW")

## -----------------------------------------------------------------------------
cat(num.bins)
plotBarrasDiscretizadas(x, num.bins, "EF")

## -----------------------------------------------------------------------------
cat(num.bins)
plotBarrasDiscretizadas(x, num.bins, "JNB")

## -----------------------------------------------------------------------------
n <- 10
df <- data.frame( a = round(runif(n) * 100 ,2),
                  b = as.character(letters[1:n]),
                  c = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)),
                  d = sample(1:100, n),
                  e = round(runif(n) * 100 ,2),
                  f = as.factor(sample(c("a", "b", "c"), n, replace = TRUE)))
row.names(df) <- letters[1:n]
df

## -----------------------------------------------------------------------------
sapply(df, class)

## -----------------------------------------------------------------------------
writeDataFrame(df, "tabla.csv", header = FALSE, rownames = FALSE, sep = "#")

## -----------------------------------------------------------------------------
dfAux <- readDataFrame("tabla.csv", header = FALSE, rownames = FALSE, sep = "#")
dfAux

## -----------------------------------------------------------------------------
all(dfAux == df)

## -----------------------------------------------------------------------------
sapply(dfAux, class)

## -----------------------------------------------------------------------------
writeDataFrame(df, "tabla.csv", header = TRUE, rownames = TRUE, sep = "@")

## -----------------------------------------------------------------------------
dfAux <- readDataFrame("tabla.csv", header = TRUE, rownames = TRUE, sep = "@")
dfAux

## -----------------------------------------------------------------------------
all(dfAux == df)

## -----------------------------------------------------------------------------
sapply(dfAux, class)

## -----------------------------------------------------------------------------
dfAux <- readDataFrame("tabla.csv", header = TRUE, rownames = FALSE, sep = "@")
dfAux

