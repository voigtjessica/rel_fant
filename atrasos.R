#Atrasos

load(obras_situacao_tb.Rdata)

x <- obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim")

sum(!is.na(x$ja_devia_estar_concluida))
#5596 obras a serem entregues que conseguimos estimar a data de entrega

5596/7453 #75% das obras

sum(is.na(x$Data.Prevista.de.Conclusão.da.Obra))

2778/5596 #50% de obras para serem entregues não havia informação sobre a data prevista

y <- obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim",
         atrasada == "sim")

sum(is.na(y$Data.Prevista.de.Conclusão.da.Obra))

898/5596 # 16% 
 