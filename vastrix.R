#   ____________________________________________________________________________
#   vastrix                                                                ####


#  Required Packages                                                       

    library(dplyr)
    library(tibble)

# 1. Cargar el data frame                                                           

    url_vastrix <- 'http://talent.com.gt/IntroR/Trx_Vastrix.rda'
    download.file(url = url_vastrix, destfile = 'vastrix.rda')
    load('vastrix.rda')
    
    vastrix <- as_tibble(Trx_Vastrix)
    rm(Trx_Vastrix)
    

# 2. Determinar cantidad de registros

    n <- nrow(vastrix)
    
# 3. Resumen Diario
    
    Resumen_Diario <- vastrix %>% group_by(FCT_DT) %>% summarise(
                                                            no_transacciones = n(),
                                                            valor_total = sum(BILLD_AMNT),
                                                            valor_promedio = mean(BILLD_AMNT),
                                                            no_usarios_unicos = n_distinct(AR_KEY),
                                                            no_transacciones_electronicas = sum(VSTRX_CLSS == 'EPIN'),
                                                            no_transacciones_fisicas = sum(VSTRX_CLSS == 'PIN')
                                                        )
    
# 4.   
    
