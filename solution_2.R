options(scipen = 999, digits = 2)
set.seed(8)
mes <- c('Enero', 'Enero', 'Febrero', 'Febrero', 'Marzo', 'Marzo')
no <- c(2, 4, 6, 8, 10, 12)
valor <- c(10, 20, 30, 40, 50, 60)

df <- data.frame(mes, no, valor)




total_mes <- function(mes){
    med <- c('no', 'valor')
    sapply(med, function(x) sum(df[df$mes == mes, x]))
}

foo <- as.data.frame(t(sapply(df$mes, function(x) total_mes(x))), row.names = FALSE)
names(foo) <- c('tsr', 'monto')

df <- cbind(df, df[2:3]/foo)
names(df)[4:5] <- c('proporcion_trs', 'proporcion_total')
df
