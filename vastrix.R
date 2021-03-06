#   ____________________________________________________________________________
#   vastrix    -  William Chavarria                                         ####


#' Se utilizó dplyr version 0.7.0
#' devtools::install_github('tidyverse/dplyr')


    options(scipen = 999)           # avoid scientific notation
    Sys.setlocale("LC_ALL", "C")    


#  Required Packages                                                       

    library(tidyverse)      # data manipulation (filter, summarize, mutate)
    library(lubridate)      # function 'month' and 'year'
    library(magrittr)       # compound assignment pipe operator: %<>%
    

# 1. Cargar el data frame                                                           

    url_vastrix <- 'http://talent.com.gt/IntroR/Trx_Vastrix.rda'
    download.file(url = url_vastrix, destfile = 'vastrix.rda')
    load('vastrix.rda')

    # cambiar nombre del objeto
    vastrix <- as_tibble(Trx_Vastrix)
    rm(Trx_Vastrix)

    # cambiar a minuscula los nombres
    names(vastrix) %<>% tolower

 
# 2. determinar cantidad de registros

    n <- nrow(vastrix)

# 3. resumen diario

    resumen_diario  <-  vastrix %>%
                            group_by(fct_dt) %>%
                            summarise(
                                no_transacciones = n(),
                                valor_total = sum(billd_amnt),
                                valor_promedio = mean(billd_amnt),
                                no_usarios_unicos = n_distinct(ar_key),
                                no_transacciones_electronicas = sum(vstrx_clss == 'EPIN'),
                                no_transacciones_fisicas = sum(vstrx_clss == 'PIN'))

# 4. resumen mensual

    resumen_mensual <-  vastrix %>%
                            mutate(
                                month = month(fct_dt, label = T, abb = FALSE),
                                year = year(fct_dt)) %>%
                            group_by(year, month) %>%
                            summarise(
                                valor_total = sum(billd_amnt),
                                valor_promedio = mean(billd_amnt),
                                no_usuarios_unicos = n_distinct(ar_key),
                                no_transacciones_electronicas = sum(vstrx_clss == 'EPIN'),
                                no_transacciones_fisicas = sum(vstrx_clss == 'PIN'))

# 5. resumen_mensual_grupos
     
    resumen_mg      <-  vastrix %>%
                            mutate(
                                ano = year(fct_dt),
                                mes = month(fct_dt, label = T, abb = FALSE)) %>%
                            group_by(ano, mes, vstrx_grp) %>%
                            summarise(
                                no_transacciones = n(),
                                valor_total = sum(billd_amnt))
                   
# 6. agregar a resumen mensual dos columnas: porcentaje de transaccioens y porcentaje de monto: rmg(Resumen Mensual Grupos)
# se cambia el nombre del df para hacerlo mas corto
    
    rmg         <-  vastrix %>%
                        mutate(
                            ano = year(fct_dt),
                            mes = month(fct_dt, label = T, abb = FALSE)) %>%
                        group_by(ano, mes, vstrx_grp) %>%
                        summarise(
                            no_transacciones = n(),
                            valor_total = sum(billd_amnt)) %>%
                        ungroup %>%
                        mutate(fecha = paste(mes, ano, sep = '_')) %>%
                        select(-ano, -mes) %>%
                        do(.[c(4,1:3)])

    
    # definir funcion para calcular total en función del argumento 'mes'
    total_mes <- function(mi_mes){
        med <- c('no_transacciones', 'valor_total')
        sapply(med, function(x) sum(rmg[rmg$fecha == mi_mes, x]))
    }
   
    # calcular df de totales por mes
    foo         <- as.data.frame(t(sapply(rmg$fecha, function(x) total_mes(x))), row.names = FALSE)
    
    
    # calcular porcentajes
    rmg                 <- cbind(rmg, rmg[3:4]/foo)
    names(rmg)[5:6]     <- c('porcentaje_transacciones', 'porcentaje_monto_total')


    
    # 7. borrar df vastrix
    
    rm(vastrix)
   