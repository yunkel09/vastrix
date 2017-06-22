options(scipen = 999, digits = 2)
set.seed(8)
library(magrittr)
library(dplyr)

mes <- c('2017/01/01', '2017/01/01', '2017/02/01', '2017/02/01', '2017/03/01', '2017/03/01')

df <- data.frame(mydate = month(mes, label = T, abb = FALSE),
                 qty = c(2, 4, 6, 8, 10, 15),
                 amount = c(10, 20, 30, 40, 50, 60),
                 stringsAsFactors = FALSE)


total_mes <- function(mes){
    med <- c('qty', 'amount')
    sapply(med, function(x) sum(df[df$mydate == mes, x]))
}

foo <- as.data.frame(t(sapply(df$mydate, function(x) total_mes(x))), row.names = FALSE)
names(foo) <- c('tsr', 'monto')

df <- cbind(df, df[2:3]/foo)
names(df)[4:5] <- c('proporcion_trs', 'proporcion_total')
df



## 2

x <- with(df, tapply(amount, mydate, sum))
str(x)
