#verificando se a primeira data é um bom proxy do início da obra

aux <- simec_gastos

aux$Data.de.Assinatura.do.Contrato <- as.Date(aux$Data.de.Assinatura.do.Contrato , 
                                                                       "%Y-%m-%d")
aux <- aux %>%
  filter(Situação == "Execução") %>%
  mutate(dif = primeira_data -
           Data.de.Assinatura.do.Contrato) #quantos dias se passaram entre a assinatura do contrato e o primeiro repasse?
  
  
aux$dif <- as.numeric(aux$dif)

aux <- aux %>%
  mutate(dif_negativa = ifelse(dif < 0, "sim","não"))

aux %>%
  group_by(dif_negativa) %>%
  summarise(obras = n())


quantile(aux$dif, na.rm=TRUE)
# > quantile(aux$dif, na.rm=TRUE)
#    0%   25%   50%   75%  100% 
# -2470  -922  -514  -104   754 

# dif_negativa obras
# <chr> <int>
# 1          não   227 casos a data do primeiro repasse foi posterior a data de assinatura do contrato
# 2          sim  1799 casos a data do repasse aconteceu antes da assinatura do contrato
# 3         <NA>    11

# Vendo se o mesmo se repete para a data de assinatura dos convênios

aux_con <- simec_gastos %>%
  filter(Situação == "Execução") %>%
  mutate(ano_convenio = str_sub(Termo.Convênio, start= -4),
         dia_convenio = "31",
         mes_convenio = "12",
         data_est_convenio = paste(ano_convenio, mes_convenio, dia_convenio, sep="-"),
         data_est_convenio = as.Date(data_est_convenio, "%Y-%m-%d"),
         dif = primeira_data - data_est_convenio)

aux_con$dif <- as.numeric(aux_con$dif)

aux_con <- aux_con %>%
  mutate(dif_negativa = ifelse(dif < 0, "sim","não"))

aux_con %>%
  group_by(dif_negativa) %>%
  summarise(obras = n())

# 
# 31/12
# dif_negativa obras
# <chr> <int>
# 1          não   568   
# 2          sim  1468    em todos esses casos a data de assinatura do convênio foi depois da primeira parcela
# 3         <NA>     1

quantile(aux_con$dif , na.rm=TRUE)

head(simec_gastos$primeira_data) 


## agora em 01/01

aux_con2 <- simec_gastos %>%
  filter(Situação == "Execução") %>%
  mutate(ano_convenio = str_sub(Termo.Convênio, start= -4),
         dia_convenio = "01",
         mes_convenio = "01",
         data_est_convenio = paste(ano_convenio, mes_convenio, dia_convenio, sep="-"),
         data_est_convenio = as.Date(data_est_convenio, "%Y-%m-%d"),
         dif = primeira_data - data_est_convenio)

aux_con2$dif <- as.numeric(aux_con2$dif)

aux_con2 <- aux_con2 %>%
  mutate(dif_negativa = ifelse(dif < 0, "sim","não"))

aux_con2 %>%
  group_by(dif_negativa) %>%
  summarise(obras = n())
# 
# # A tibble: 3 x 2
# dif_negativa obras
# <chr> <int>
# 1          não  1770
# 2          sim   266 quando muda para primeiro de janeiro, as observações com a data de repasse anterior ao contrato diminuem muito
# 3         <NA>     1