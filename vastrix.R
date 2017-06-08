#   ____________________________________________________________________________
#   vastrix                                                                ####

    options(scipen = 999)
    Sys.setlocale("LC_ALL", "C")


#  Required Packages                                                       

    library(tidyverse)      # data manipulation (filter, summarize, mutate)
    library(lubridate)      # function 'month'
    library(magrittr)       # compound assignment pipe operator: %<>%
    

# 1. Cargar el data frame                                                           

    url_vastrix <- 'http://talent.com.gt/IntroR/Trx_Vastrix.rda'
    download.file(url = url_vastrix, destfile = 'vastrix.rda')
    load('vastrix.rda')

    vastrix <- as_tibble(Trx_Vastrix)
    rm(Trx_Vastrix)

    names(vastrix) %<>% tolower

 
# # #2. determinar cantidad de registros

    n <- nrow(vastrix)

# # # 3. resumen diario

    resumen_diario  <-  vastrix %>%
                            group_by(fct_dt) %>%
                            summarise(
                                no_transacciones = n(),
                                valor_total = sum(billd_amnt),
                                valor_promedio = mean(billd_amnt),
                                no_usarios_unicos = n_distinct(ar_key),
                                no_transacciones_electronicas = sum(vstrx_clss == 'EPIN'),
                                no_transacciones_fisicas = sum(vstrx_clss == 'PIN')
                            )

# # 4. resumen mensual

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

# # 5. resumen_mensual_grupos
     
    resumen_mg  <-  vastrix %>%
                            mutate(
                                ano = year(fct_dt),
                                mes = month(fct_dt, label = T, abb = FALSE)) %>%
                            group_by(ano, mes, vstrx_grp) %>%
                            summarise(
                                no_transacciones = n(),
                                valor_total = sum(billd_amnt))
                   
    
    resumen_mg_plus <-      vastrix %>%
                            mutate(
                                ano = year(fct_dt),
                                mes = month(fct_dt, label = T, abb = FALSE)) %>%
                            group_by(ano, mes, vstrx_grp) %>%
                            summarise(
                                no_transacciones = n(),
                                valor_total = sum(billd_amnt)) %>%
                            ungroup %>%
                            mutate(myfecha = paste(mes, ano, sep = '_')) %>%
                            select(-ano, -mes) %>%
                            do(.[c(4,1:3)])

                                    
    
    # funcion para extraer la suma total por mes de las transacciones
    total_transact_fun <- function(mifecha){
                trs <- sum(resumen_mg_plus[resumen_mg_plus$myfecha == mifecha, 'no_transacciones'])
                return(trs)
    }
    
    # funcion para extraer la suma total por mes de los montos
    total_monto_fun <- function(mifecha){
                mto <- sum(resumen_mg_plus[resumen_mg_plus$myfecha == mifecha, 'valor_total'])
                return(mto)
    }
    
    # vector de comparacion
    mymes <- as.character(unique(resumen_mg_plus$myfecha))

    # incializacion de vectores para eficiencia del bucle
    resumen_mg_plus$porcentaje_transacciones  <- numeric(length = length(resumen_mg_plus$no_transacciones))
    resumen_mg_plus$porcentaje_monto          <- numeric(length = length(resumen_mg_plus$no_transacciones))
   
    i <- 1
    j <- 1
    # calculo de la columna de transacciones por mes
    for(i in seq_along(resumen_mg_plus$myfecha)){
        for(j in seq_along(mymes))
            if(resumen_mg_plus$myfecha[i] == mymes[j]){
                resumen_mg_plus$porcentaje_transacciones[i] = resumen_mg_plus$no_transacciones[i]/total_transact_fun(resumen_mg_plus$myfecha[i])
                resumen_mg_plus$porcentaje_monto[i] = resumen_mg_plus$valor_total[i]/total_monto_fun(resumen_mg_plus$myfecha[i])
                break()
            }
    }
    
   