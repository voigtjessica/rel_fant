#Script corrigido:

#inserir aqui a parte do scraping do Manoel

simec_gastos_tb <- simec_gastos %>%
  group_by(Situação) %>%
  summarise(pagto = sum(pagamento_cte_jun17),
            obras = n()) %>%
  rename(situacao = Situação)

pagto <- sum(simec_gastos_tb$pagto, na.rm = T)
obras <- sum(simec_gastos_tb$obras)
linha_final <- data.frame(situacao = "total", pagto, obras)

simec_gastos_tb1 <- bind_rows(simec_gastos_tb, linha_final) %>%
  mutate(perc_pagto = round(pagto/XtotalpagamentoX ,2) ,
         pecr_obras = round( obras/XnumerototalobrasX ,2))    #1
simec_gastos_tb1


