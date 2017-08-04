## Análise dos dados pós-raspagem

library(tidyverse)
library(stringr)
library(tidyr)

#Endereço para achar as obras: http://simec.mec.gov.br/painelObras/dadosobra.php?obra=
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\planilhas\\tadepe")
load("arquivos_simec_fin_v2.RData")

simec_fin1 <- lista_objetos[[3]]
simec_fin2 <- lista_objetos[[4]]

simec <- read.table(file="obras28072017.csv", sep=";",  
                    header=T, encoding="utf-8", comment.char = "", quote = "\"", as.is = TRUE,
                    na.strings="")

## números rápidos
# fonte dos dados do ipca http://dadosabertos.bcb.gov.br/dataset/4447-indice-nacional-de-precos-ao-consumidor-amplo-ipca---comercializaveis

# criando indice para atualizar para junho de 2017
ipca <- read_csv2("bcdata.sgs.4447.csv")
ipca <- bind_rows(ipca, data.frame(data="01/07/2017", valor=0))
ipca <- ipca %>%
  mutate(data= as.Date(data, "%d/%m/%Y"),
         mes_ano = format(data, "%m/%Y")) %>%
  filter(data > as.Date("2007-01-01")) %>%
  mutate(indice = cumprod(1+valor/100),
         indice_max = last(indice),
         indice = indice/indice_max)

simec <- simec %>%
  mutate(Data.de.Assinatura.do.Contrato = as.Date(Data.de.Assinatura.do.Contrato),
         mes_ano_assinatura_contrato = format(Data.de.Assinatura.do.Contrato, "%m/%Y"))


head(simec)


# ajustando formato dos dados (gasto e data)
pagamentos_simec1 <- simec_fin1 %>%
  mutate(Valor.do.Pagamento = str_trim(Valor.do.Pagamento),
         Valor.do.Pagamento = gsub("R\\$ ", "", Valor.do.Pagamento),
         Valor.do.Pagamento = gsub("\\.", "", Valor.do.Pagamento),
         Valor.do.Pagamento = as.numeric(gsub(",", "\\.", Valor.do.Pagamento)),
         Data.de.Pagamento = as.Date(Data.de.Pagamento, "%d/%m/%Y")) %>%
  group_by(id) %>%
  summarise(pagamento = sum(Valor.do.Pagamento),
            primeira_data = min(Data.de.Pagamento),
            segunda_data = rev(sort(Data.de.Pagamento)[2]),
            ultima_data = max(Data.de.Pagamento))

pagamentos_simec2 <- simec_fin2 %>%
  mutate(Valor.Repassado = str_trim(Valor.Repassado),
         Valor.Repassado = gsub("R\\$ ", "", Valor.Repassado),
         Valor.Repassado = gsub("\\.", "", Valor.Repassado),
         Valor.Repassado = as.numeric(gsub(",", "\\.", Valor.Repassado)),
         Data.do.Repasse = as.Date(Data.do.Repasse, "%d/%m/%Y")) %>%
  group_by(id) %>%
  summarise(pagamento = sum(Valor.Repassado),
            primeira_data = min(Data.do.Repasse),      #primeira data registrada do repasse
            ultima_data = max(Data.do.Repasse))        #ultima data registrada do repasse

# juntando as duas tabelas
pagamento_simec <- bind_rows(pagamentos_simec1, pagamentos_simec2)

pagamento_simec <- pagamento_simec %>%
  mutate(mes_ano = format(primeira_data, "%m/%Y"))

# descontando inflação
pagamento_simec_inflacao <- pagamento_simec %>%
  left_join(ipca, by="mes_ano") %>%
  mutate(pagamento_cte_jun17 = pagamento/indice)


pagamento_simec_inflacao %>%
  summarise(gasto_jun17 = sum(pagamento_cte_jun17),
            gasto_burro = sum(pagamento))

# total gasto por ano (valores correntes)
pagamento_simec_inflacao %>%
  mutate(ano = format(primeira_data, "%Y")) %>%
  group_by(ano) %>%
  summarise(x=sum(pagamento_cte_jun17),
            y=sum(pagamento)) %>%
  summarise(sum(x), sum(y))

# juntando com tabela original do simec
simec_gastos <- simec %>%
  inner_join(pagamento_simec_inflacao, by = c("ID" = "id")) %>%
  mutate(mes_ano_assinatura_contrato = ifelse(is.na(mes_ano_assinatura_contrato), 
                                              primeira_data, mes_ano_assinatura_contrato)) %>%
  full_join(ipca, by=c("mes_ano_assinatura_contrato" = "mes_ano")) %>%
  mutate(valor_pactuado_fnde_cte_jun17 = Valor.Pactuado.com.o.FNDE/indice.y)

# total de gasto por situação, com soma burra (sem considerar inflação)
simec_gastos %>%
  group_by(Situação) %>%
  summarise(sum(pagamento_cte_jun17))
######################################################################################################
#Jessica começou a partir daqui 

#1 Tabela de obras do proinfância e situação de cada uma das obras

simec_gastos_tb <- simec_gastos %>%
  mutate(num=1) %>%
  group_by(Situação) %>%
  summarise(pagto = sum(pagamento_cte_jun17),
            obras = sum(num)) %>%
  rename(situacao = Situação)

pagto <- sum(simec_gastos_tb$pagto, na.rm = T)
obras <- sum(simec_gastos_tb$obras)

linha_final <- data.frame(situacao = "total", pagto, obras)

simec_gastos_tb1 <- bind_rows(simec_gastos_tb, linha_final) %>%
  mutate(perc_pagto = round(pagto/9656262359 ,2) ,
         pecr_obras = round( obras/9375 ,2))    #1
simec_gastos_tb1

#2. Calculando tempo de duração das obras

Tipo.do.Projeto <- c("Escola de Educação Infantil Tipo B",
                     "Escola de Educação Infantil Tipo C",
                     "MI - Escola de Educação Infantil Tipo B",
                     "MI - Escola de Educação Infantil Tipo C",
                     "Espaço Educativo - 12 Salas",
                     "Espaço Educativo - 01 Sala",
                     "Espaço Educativo - 02 Salas",
                     "Espaço Educativo - 04 Salas",
                     "Espaço Educativo - 06 Salas",
                     "Projeto 1 Convencional",
                     "Projeto 2 Convencional")

tempo_exe_meses <- c(9,6,6,4,13,5,5,7,7,11,9)

execucao <- data.frame(Tipo.do.Projeto, tempo_exe_meses)

simec_atraso <- simec_gastos %>%   #2
  left_join(execucao) %>%
  mutate(tempo_exe_dias = tempo_exe_meses*30)

simec_atraso$Data.Prevista.de.Conclusão.da.Obra <- as.Date(simec_atraso$Data.Prevista.de.Conclusão.da.Obra , "%d/%m/%Y")
simec_atraso$Data.da.Última.Vistoria.do.Estado.ou.Município <- as.Date(simec_atraso$Data.da.Última.Vistoria.do.Estado.ou.Município , "%Y-%m-%d")
simec_atraso$primeira_data <- as.Date(simec_atraso$primeira_data ,  "%Y-%m-%d")
simec_atraso$Data.de.Assinatura.do.Contrato <- as.Date(simec_atraso$Data.de.Assinatura.do.Contrato, "%Y-%m-%d")

#teste se podemos usar tanto Data.Prevista.de.Conclusão.da.Obra quanto Data.da.Última.Vistoria.do.Estado.ou.Município

j <- simec_atraso %>%
  filter(Situação == "Concluída",
         !is.na(Data.Prevista.de.Conclusão.da.Obra)) %>%
  mutate(dif_finais = Data.Prevista.de.Conclusão.da.Obra - Data.da.Última.Vistoria.do.Estado.ou.Município, na.rm=T)

j %>%
  ggplot(aes(dif_finais)) + geom_histogram() #são muito próximas

x <- simec_atraso %>%
  filter(Situação == "Concluída") %>%  #4333 obras
  filter(!is.na(Data.Prevista.de.Conclusão.da.Obra) | !is.na(Data.da.Última.Vistoria.do.Estado.ou.Município))   #4322

#Observação : teremos como data de término oficial da obra Data.Prevista.de.Conclusão.da.Obra |
# Data.da.Última.Vistoria.do.Estado.ou.Município

#3. Atraso das obras concluidas

simec_atraso$Data.Prevista.de.Conclusão.da.Obra <- as.Date(simec_atraso$Data.Prevista.de.Conclusão.da.Obra)

simec_atraso_concluidas <- simec_atraso %>%
  filter(Situação == "Concluída") %>%
  mutate(data_ideal = Data.de.Assinatura.do.Contrato + tempo_exe_dias,
         data_final_gov = Data.Prevista.de.Conclusão.da.Obra,
         data_final_gov = case_when(
           is.na(Data.Prevista.de.Conclusão.da.Obra) ~ Data.da.Última.Vistoria.do.Estado.ou.Município,
           TRUE ~ Data.Prevista.de.Conclusão.da.Obra))

simec_atraso_concluidas$data_ideal <- as.Date(simec_atraso_concluidas$data_ideal , "%Y-%m-%d")
simec_atraso_concluidas$data_final_gov <- as.Date(simec_atraso_concluidas$data_final_gov , "%Y-%m-%d")

simec_atraso_concluidas <- simec_atraso_concluidas %>%
  mutate(atraso = data_final_gov - data_ideal,
         entregue_atrasada = ifelse(atraso > 0, "sim", "não"))

simec_atraso_concluidas %>%
  group_by(entregue_atrasada) %>%
  summarise(atraso = mean(atraso))

##4. Total gasto até hoje (nas concluídas) e total pactuado até hoje

simec_atraso_concluidas_pagto <- simec_atraso_concluidas %>%
  group_by(Tipo.do.Projeto) %>%
  summarise(valor_total_pactuado = sum(valor_pactuado_fnde_cte_jun17, na.rm=T),
            valor_total_gasto = sum(pagamento_cte_jun17, na.rn=T),
            obras = n()) %>%
  mutate(dif_per = (valor_total_gasto - valor_total_pactuado)/valor_total_pactuado)

simec_atraso_concluidas_pagto

total_pagto_concluidas <- sum(simec_atraso_concluidas_pagto$valor_total_gasto) 
total_pactuado_concluidas <- sum(simec_atraso_concluidas_pagto$valor_total_pactuado) 
total_pagto_concluidas/total_pactuado_concluidas - 1 #foi gasto 4% a mais do que o pactuado
total_pagto_concluidas - total_pactuado_concluidas   #259.056.016 ou cerca de 259 mil reais

# 5. Gasto por ano (efeito da eleição)

library(scales)
locale("pt", decimal_mark = ",")

pagamentos_ano_simec1 <- simec_fin1 %>%
  mutate(Valor.do.Pagamento = str_trim(Valor.do.Pagamento),
         Valor.do.Pagamento = gsub("R\\$ ", "", Valor.do.Pagamento),
         Valor.do.Pagamento = gsub("\\.", "", Valor.do.Pagamento),
         Valor.do.Pagamento = as.numeric(gsub(",", "\\.", Valor.do.Pagamento)),
         Data.de.Pagamento = as.Date(Data.de.Pagamento, "%d/%m/%Y")) %>%
  rename(data_pagamento_ou_repasse = Data.de.Pagamento,
         valor_pagamento_ou_repasse = Valor.do.Pagamento) %>%
  select(id, data_pagamento_ou_repasse, valor_pagamento_ou_repasse)

pagamentos_ano_simec2 <- simec_fin2 %>%
  mutate(Valor.Repassado = str_trim(Valor.Repassado),
         Valor.Repassado = gsub("R\\$ ", "", Valor.Repassado),
         Valor.Repassado = gsub("\\.", "", Valor.Repassado),
         Valor.Repassado = as.numeric(gsub(",", "\\.", Valor.Repassado)),
         Data.do.Repasse = as.Date(Data.do.Repasse, "%d/%m/%Y")) %>%
  rename(data_pagamento_ou_repasse = Data.do.Repasse,
         valor_pagamento_ou_repasse = Valor.Repassado) %>%
  select(id, data_pagamento_ou_repasse, valor_pagamento_ou_repasse)

pagamento_ano_simec <- bind_rows(pagamentos_ano_simec1, pagamentos_ano_simec2) %>%
  mutate(mes_ano = format(data_pagamento_ou_repasse, "%m/%Y")) %>%
  left_join(ipca, by="mes_ano") %>%
  mutate(pagto_repasse_cte_jun17 = valor_pagamento_ou_repasse/indice,
         ano = format(data_pagamento_ou_repasse, "%Y",
                      ano_eleitoral = ifelse(ano == 2002 |
                                               ano == 2006 | 
                                               ano == 2010 | 
                                               ano == 2014 , 
                                             "sim", "não"))) 
graf_pagto_ano <- pagamento_ano_simec %>%
  group_by(ano) %>%
  summarise(total_pagto_repasse_cte_jun17 = sum(pagto_repasse_cte_jun17)) %>%
  mutate(ano = as.numeric(ano),
         total_pagto = total_pagto_repasse_cte_jun17 / 100000000)

graf_pagto_ano %>%
  ggplot(aes(x=ano, y=total_pagto)) +
  labs(title="Repasses Proinfância", 
       subtitle="Repasses efetuados pelo Governo Federal às prefeituras", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil", 
       y="Repasse") +
  geom_line() + xlab("") + ylab("") + scale_y_continuous(labels = dollar_format(suffix = " bi", prefix = "R$ ",
                                                                                                  decimal.mark = ",",
                                                                                big.mark = " ")) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw() +
  theme(panel.grid.minor = element_blank())




#6. obras em execução e iniciadas
#todas as obras que têm Data de assinatura do contrato foram consideradas como iniciadas

obras_iniciadas <- simec_atraso %>%
  filter(!is.na(Data.de.Assinatura.do.Contrato),
         Situação != "Concluída") %>%
  group_by(Situação) %>%
  summarise(Obras = n(), Custo = sum(pagamento_cte_jun17)) %>%
  mutate(Custo = round(Custo/1000000, 2)) %>%
  arrange(desc(Custo)) %>%
  mutate(Custo = as.character(Custo))
  

obras_iniciadas$Custo <- paste(obras_iniciadas$Custo, "mi")
obras_iniciadas$Custo <- gsub("[.]", ",", obras_iniciadas$Custo)

obras_iniciadas         


sum(obras_iniciadas$obras) - 120 #obras canceladas #numero de obras iniciadas exceto canceladas e concluidas

write.table(obras_iniciadas, file="obras_iniciadas.csv", row.names = F, sep=";")

custo_paralisadas <- obras_iniciadas %>%
  filter(Situação != "Obra Cancelada",
         Situação != "Execução")

custo_paralisadas
sum(custo_paralisadas$Obras)

write.table(custo_paralisadas, file="custo_paralisadas.csv", row.names = F, sep=";")

custo_paradas_abandonadas <- simec_atraso %>%
  filter(!is.na(Data.de.Assinatura.do.Contrato),
         Situação != "Concluída",
         Situação != "Execução") %>%
  group_by(Situação) %>%
  summarise(Obras = n(), Custo = sum(pagamento_cte_jun17)) %>%
  mutate(Custo = round(Custo/1000000, 2))

sum(custo_paradas_abandonadas$Custo)

#quantas obras já deveriam ter sido concluídas de fato foram?

dia_final <- as.Date("2017-07-27")

#7. Obras iniciadas e atrasadas:

#para obras que ainda não foram concluidas
#e que não foram canceladas
#e que possuem Data de assinatura de contrato ou constam como "em execução
#e cujos projetos tem tempo de execução conhecida

#OBS: A data da última vistoria como procy da entrega vale só para as obras já concluídas

execucao_e_atrasos <- simec_atraso %>%
  filter(Situação != "Concluída",           
         Situação!= "Obra Cancelada",
         !is.na(Data.de.Assinatura.do.Contrato),    
         !is.na(tempo_exe_dias) | !is.na(Data.Prevista.de.Conclusão.da.Obra)) %>%  
  mutate(data_estimada_de_entrega = Data.Prevista.de.Conclusão.da.Obra,
         data_estimada_de_entrega = case_when(!is.na(Data.Prevista.de.Conclusão.da.Obra) ~ data_estimada_de_entrega,
                                              TRUE ~ Data.de.Assinatura.do.Contrato + tempo_exe_dias),
         dia_final = dia_final,
         ja_devia_estar_concluida = ifelse(data_estimada_de_entrega <= dia_final ,
                                           "sim", "não"),
         tempo_de_atraso = dia_final,
         tempo_de_atraso = dia_final - data_estimada_de_entrega)

# 7 Quantidade de obras atrasadas -

obras_atrasadas <- execucao_e_atrasos %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(obras = n())

obras_atrasadas

#Quantas obras já deviam estar concluídas e qual é a situação de cada uma delas:

obras_atrasadas_sit <- execucao_e_atrasos %>%
  filter(ja_devia_estar_concluida == "sim") %>%
  group_by(Situação) %>%
  summarise(Obras = n(), Custo = sum(pagamento_cte_jun17)) %>%
  mutate(Custo = round(Custo/1000000, 2)) %>%
  arrange(desc(Custo)) %>%
  mutate(Custo = as.character(Custo))


obras_atrasadas_sit$Custo <- paste(obras_atrasadas_sit$Custo, "mi")
obras_atrasadas_sit$Custo <- gsub("[.]", ",", obras_atrasadas_sit$Custo)

obras_atrasadas_sit
write.table(obras_atrasadas_sit, file="obras_atrasadas_sit.csv", row.names = F, sep=";")

### Qual é o atraso médio das obras iniciadas?

atraso_medio_execucao <- execucao_e_atrasos %>%
  filter(ja_devia_estar_concluida == "sim",
         Situação == "Execução") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(tempo_medio_atraso = mean(tempo_de_atraso)) 
atraso_medio_execucao 

atraso_medio_iniciadas <- execucao_e_atrasos %>%
  filter(ja_devia_estar_concluida == "sim") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(tempo_medio_atraso = mean(tempo_de_atraso)) 

atraso_medio_iniciadas

atraso_medio_paralisadas <- execucao_e_atrasos %>%
  filter(ja_devia_estar_concluida == "sim",
         Situação != "Execução") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(tempo_medio_atraso = mean(tempo_de_atraso)) 

atraso_medio_paralisadas

# 9. Obras entregues por ano

ano_conclusao <- simec_atraso%>%
  filter(Situação == "Concluída") %>%
  mutate(ano_prev_concl = format(Data.Prevista.de.Conclusão.da.Obra, "%Y"),
         ano_data.vist = format(Data.da.Última.Vistoria.do.Estado.ou.Município, "%Y"),
         ano_concluida = ano_prev_concl , 
         ano_concluida = ifelse(is.na(ano_concluida), ano_data.vist, ano_concluida))

graf_ano_conclusao <- ano_conclusao %>%
  group_by(ano_concluida) %>%
  summarise(obras = n()) %>%
  filter(ano_concluida != "<NA>",
         ano_concluida != "2018") %>%
  mutate(obras = as.numeric(obras),
         ano_concluida = as.numeric(ano_concluida))

graf_ano_conclusao %>%
  ggplot(aes(x=ano_concluida, y=obras, group=1)) +
  labs(title="Obras entregues", 
       subtitle="Obras concluídas por ano", 
       caption="Fonte: SIMEC. Elaborado por Transparência Brasil") +
  geom_line() + xlab("") + ylab("") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + theme_bw()

#Dados inconsistentes e inexistentes

names(simec_atraso)

inex <- simec_atraso %>%
  filter(is.na(Município)|
         is.na(UF)|
         is.na(CEP)|
         is.na(Situação)|
         is.na(Logradouro)|
         is.na(Termo.Convênio)|
         is.na(Fim.da.Vigência.Termo.Convênio)|
         is.na(Tipo.do.Projeto)|
         is.na(Valor.Pactuado.pelo.FNDE)|
         is.na(Email)|
         is.na(Total.Pago)) %>%
  mutate(Município = ifelse(is.na(Município), 1 ,0),
         UF = ifelse(is.na(UF), 1 ,0),
         CEP = ifelse(is.na(CEP), 1 ,0),
         Logradouro = ifelse(is.na(Logradouro), 1 ,0),
         Termo.Convênio = ifelse(is.na(Termo.Convênio), 1 ,0),
         Fim.da.Vigência.Termo.Convênio = ifelse(is.na(Fim.da.Vigência.Termo.Convênio), 1 ,0),
         Tipo.do.Projeto = ifelse(is.na(Tipo.do.Projeto), 1 ,0),
         Valor.Pactuado.pelo.FNDE = ifelse(is.na(Valor.Pactuado.pelo.FNDE), 1 ,0),
         Email = ifelse(is.na(Email), 1 ,0),
         Total.Pago = ifelse(is.na(Total.Pago), 1 ,0),
         Situação = ifelse(is.na(Situação),1,0)) %>%
  select(Município, UF, CEP, Logradouro, Termo.Convênio, Fim.da.Vigência.Termo.Convênio,
         Tipo.do.Projeto, Valor.Pactuado.pelo.FNDE, Email, Total.Pago, Situação)

Município <- sum(inex$Município)
UF <- sum(inex$UF) 
CEP <- sum(inex$CEP)
Logradouro <- sum(inex$Logradouro)
Termo.Convênio <- sum(inex$Termo.Convênio)
Fim.da.Vigência.Termo.Convênio <- sum(inex$Fim.da.Vigência.Termo.Convênio)
Tipo.do.Projeto <- sum(inex$Tipo.do.Projeto)
Valor.Pactuado.pelo.FNDE <- sum(inex$Valor.Pactuado.pelo.FNDE)
Email <- sum(inex$Email)
Total.Pago <- sum(inex$Total.Pago)
Situação <- sum(inex$Situação)

inexistentes <- data.frame(Município, UF, CEP, Logradouro, Termo.Convênio,
                           Fim.da.Vigência.Termo.Convênio, Tipo.do.Projeto,
                           Valor.Pactuado.pelo.FNDE, Email, 
                           Total.Pago, Situação)
inexistentes

write.table(inexistentes, file="inexistentes.csv", row.names = F, sep=";")

#inconsistências:
#a. Obras em execução sem data de assinatura de contrato: #16

simec %>%
  filter(Situação == "Execução" & is.na(Data.de.Assinatura.do.Contrato)) %>%
  summarise(n())   #16

#b. Obras em execução que não possuem data prevista de entrega 279

simec %>%
  filter(Situação == "Execução" & is.na(Data.Prevista.de.Conclusão.da.Obra)) %>%
  summarise(n()) #279

#c. Obras concluídas que constam que a última data de vistoria do município foi em 2018: 5

simec %>%
  mutate(dia_final = dia_final) %>%
  mutate(Data.da.Última.Vistoria.do.Estado.ou.Município = 
           as.Date(Data.da.Última.Vistoria.do.Estado.ou.Município)) %>%
  filter(Situação == "Concluída",
         Data.da.Última.Vistoria.do.Estado.ou.Município > dia_final) %>%
  summarise(n()) #5

#d. Obras em licitação que tem percentual de execução da obra > 0 #227

simec %>%
  filter(Situação == "Licitação" & !is.na(Percentual.de.Execução) & Percentual.de.Execução >= 0.01) %>%
  summarise( casos = n()) #227

#Easter Egg
