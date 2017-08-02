## Análise dos dados pós-raspagem

library(tidyverse)
library(stringr)


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

#Quantas escolas foram pactuadas no âmbito do proinfância e qual é a situação de cada
#uma delas?

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
         pecr_obras = round( obras/9375 ,2))


#Calculando obras atrasadas

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

simec_atraso <- simec_gastos %>%
  left_join(execucao) %>%
  mutate(tempo_exe_dias = tempo_exe_meses*30)

simec_atraso$Data.Prevista.de.Conclusão.da.Obra <- as.Date(simec_atraso$Data.Prevista.de.Conclusão.da.Obra ,
                                                           "%d/%m/%Y")
simec_atraso$Data.da.Última.Vistoria.do.Estado.ou.Município <- as.Date(simec_atraso$Data.da.Última.Vistoria.do.Estado.ou.Município , 
                                                                       "%Y-%m-%d")
simec_atraso$primeira_data <- as.Date(simec_atraso$primeira_data ,  "%Y-%m-%d")

simec_atraso_concluidas <- simec_atraso %>%
  filter(Situação == "Concluída") %>%
  mutate(atraso_datas_finais = ifelse(Data.Prevista.de.Conclusão.da.Obra < Data.da.Última.Vistoria.do.Estado.ou.Município, "atraso", "sem atraso"),
         data_prevista_cronograma = primeira_data + tempo_exe_dias ,
         duracao = Data.Prevista.de.Conclusão.da.Obra - primeira_data ) 

simec_atraso_concluidas
##

simec_atraso_concluidas_pagto <- simec_atraso_concluidas %>%
  group_by(Tipo.do.Projeto) %>%
  summarise(valor_total_pactuado = sum(valor_pactuado_fnde_cte_jun17, na.rm=T),
            valor_total_gasto = sum(pagamento_cte_jun17, na.rn=T),
            obras = n()) %>%
  mutate(dif_per = (valor_total_gasto - valor_total_pactuado)/valor_total_pactuado)

simec_atraso_concluidas_pagto

x <- sum(simec_atraso_concluidas_pagto$valor_total_gasto) 
y <- sum(simec_atraso_concluidas_pagto$valor_total_pactuado) 
x/y - 1 #foi gasto 4% a mais do que o pactuado
x - y   #259.056.016 ou cerca de 259 mil reais


#quantas obras já deveriam ter sido concluídas de fato foram?

dia_final <- as.Date("2017-07-27")

#das obras que deveriam estar prontas até 27/27/2017 , quantas foram entregues e quantas estão atrasadas?

#obras concluidas:

simec_gastos_tb1   #4333 obras concluídas

simec_atraso %>%
  filter(Situação == "Execução", 
         is.na(Data.Prevista.de.Conclusão.da.Obra)) %>%
  mutate(num = 1) %>%
  group_by(Data.Prevista.de.Conclusão.da.Obra) %>%
  summarise(obras_sem_data = sum(num))   #95 obras em execução não têm data prevista de entrega

simec_atraso %>%
  filter(is.na(tempo_exe_dias),
         is.na(Data.Prevista.de.Conclusão.da.Obra),
         Situação != "Concluída",
         !is.na(primeira_data)) %>%
  group_by(Situação) %>%
  summarise(obras_projetos_sem_prazo = n())
#14+2+27+7+19 = 69 # Em 69 casos as obras já registraram ao menos um repasse mas não contam com 
#data de entrega ou projeto com execução conhecida de modo que seja possível estipular uma data de entrega

execucao_e_atrasos <- simec_atraso %>%
  filter(Situação != "Concluída",                           #para obras que ainda não foram concluidas
         !is.na(primeira_data) | Situação == "Execução",    #e que já receberam ao menos um repasse ou constam como "em execução
         !is.na(tempo_exe_dias) | !is.na(Data.Prevista.de.Conclusão.da.Obra),         #e cujos projetos tem tempo de execução conhecida  
         Situação!= "Cancelada") %>%    #e que não foram canceladas
  mutate(data_estimada_de_entrega = Data.Prevista.de.Conclusão.da.Obra,
         data_estimada_de_entrega = case_when(!is.na(Data.Prevista.de.Conclusão.da.Obra) ~ data_estimada_de_entrega,
                                              TRUE ~ primeira_data + tempo_exe_dias),
         data_extracao_planilha = dia_final,
         ja_devia_estar_concluida = ifelse(data_estimada_de_entrega <= data_extracao_planilha ,
                                           "sim", "não"),
         tempo_de_atraso = dia_final,
         tempo_de_atraso = dia_final - data_estimada_de_entrega)


sum(is.na(execucao_e_atrasos$data_estimada_de_entrega)) #deu conta de todos os casos
sum(is.na(execucao_e_atrasos$ja_devia_estar_concluida)) 
sum(is.na(execucao_e_atrasos$primeira_data)) 

execucao_e_atrasos %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(obras = n())

# não  1701
# sim  3258 66%
# total 4959

execucao_e_atrasos %>%
  filter(Situação == "Execução") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(obras = n())

# ja_devia_estar_concluida obras
# <chr> <int>
# 1                      não  1327
# 2                      sim   708 35%


execucao_e_atrasos %>%
  group_by(Situação, ja_devia_estar_concluida ) %>%
  summarise(obras = n()) %>%
  spread(ja_devia_estar_concluida, obras) %>%
  rename(iniciada_sem_atraso = não,
         iniciada_com_atraso = sim,
         situacao = Situação) %>%
  mutate(total_iniciadas = iniciada_sem_atraso + iniciada_com_atraso) %>%
  left_join(simec_gastos_tb1) %>%
  rename(obras_totais = obras) %>%
  select(situacao, obras_totais, total_iniciadas, iniciada_sem_atraso, iniciada_com_atraso)

# # Groups:   situacao [8]
#                      situacao       obras_totais   total_iniciadas   iniciada_sem_atraso    iniciada_com_atraso
# <chr>        <dbl>           <int>               <int>               <int>
# 1                  Contratação          155             141                  10                 131
# 2              Em Reformulação          315             315                   3                 312
# 3                     Execução         2037            2035                1327                 708
# 4                    Inacabada          566             566                  19                 547
# 5                    Licitação          330             303                   3                 300
# 6               Obra Cancelada          314             307                   1                 306
# 7                   Paralisada          584             584                 292                 292
# 8 Planejamento pelo proponente          727             708                  46                 662
# 


simec_atraso  %>%
  filter(is.na(tempo_exe_meses),
         is.na(Data.Prevista.de.Conclusão.da.Obra),
         Situação != "Concluída",
         !is.na(primeira_data)) %>%
  group_by(Tipo.do.Projeto) %>%
  summarise(obras = n())


# Qual é o atraso médio das obras iniciadas?

execucao_e_atrasos %>%
  filter(ja_devia_estar_concluida == "sim",
         Situação == "Execução") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(tempo_medio_atraso = mean(tempo_de_atraso)) #331 days

execucao_e_atrasos %>%
  filter(ja_devia_estar_concluida == "sim",
         Situação != "Execução") %>%
  group_by(ja_devia_estar_concluida) %>%
  summarise(tempo_medio_atraso = mean(tempo_de_atraso)) #1088 days ou 2.9 anos de atraso entre as 
#obras que estão paralizadas


obras_iniciadas <-  execucao_e_atrasos %>%
  group_by(Situação) %>%
  summarise(obras = n()) %>%
  arrange(desc(obras))

total <- c("Total", sum(obras_iniciadas$obras))

obras_iniciadas  <- obras_iniciadas  %>%
  rbind(total)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\tadepe\\fantastico")
write.table(obras_iniciadas, file="obras_iniciadas.csv", row.names = FALSE, sep=";", dec=",")

#data de conclusão da obra com Data prevista

ano_entrega <- simec_atraso %>%
  filter(Situação == "Concluída") %>%
  select(Data.Prevista.de.Conclusão.da.Obra)

count(ano_entrega$Data.Prevista.de.Conclusão.da.Obra)
##
programas <- simec_gastos %>%
  mutate(programa = ifelse(Tipo.do.Projeto == "Espaço Educativo - 08 Salas" |
                             Tipo.do.Projeto == "Espaço Educativo - 10 Salas", 
                           "Fundaescola", "Proinfância"),
         num = 1) %>%
  filter(Situação != "Concluída") %>%   #obras que não foram concluídas
  group_by(programa, Situação) %>%
  summarise(obras = sum(num))

programas

