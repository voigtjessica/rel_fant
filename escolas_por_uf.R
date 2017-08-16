setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\tadepe\\fantastico\\rel_fant")

load("obras_situacao_tb.Rdata")

## Escolas Rio de Janeiro UF
## Nome , endereço, % de execução, Calssificação (atrasada , paralisada) 

escolas_br_atraso <- obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim",
         atrasada == "sim",
         paralisada_tb == "não-paralisada") %>%
  mutate(classificação_tb = "atrasada",
         total_repassado = round(pagamento_cte_jun17, 0)) %>%
  select(ID, Nome, Logradouro, Município, UF, Percentual.de.Execução, classificação_tb,
         total_repassado, Empresa.Contratada)

escolas_br_paralisadas <- obras_situacao_tb %>%
  filter(obra_a_ser_entregue == "sim",
         paralisada_tb == "paralisada") %>%
  mutate(classificação_tb = "paralisada",
         total_repassado = round(pagamento_cte_jun17, 0))%>%
  select(ID, Nome, Logradouro, Município, UF, Percentual.de.Execução, classificação_tb,
         total_repassado, Empresa.Contratada)

escolas_br <- obras_situacao_tb  %>%
  filter(obra_a_ser_entregue == "sim",
         atrasada == "não",
         paralisada_tb == "não-paralisada") %>%
  mutate(classificação_tb = Situação,
         total_repassado = round(pagamento_cte_jun17, 0))%>%
  select(ID, Nome, Logradouro, Município, UF, Percentual.de.Execução, classificação_tb,
         total_repassado, Empresa.Contratada) %>%
    bind_rows(escolas_br_paralisadas) %>% 
    bind_rows(escolas_br_atraso) 
    
    
escolas_br[is.na(escolas_br)] <- "Sem Informação"
View(escolas_br)


escolas_sc <- escolas_br%>%
  filter(UF == "SC")

escolas_es <- escolas_br%>%
  filter(UF == "ES")

escolas_rn <- escolas_br%>%
  filter(UF == "RN")

escolas_sp <- escolas_br%>%
  filter(UF == "SP")

escolas_rs <- escolas_br%>%
  filter(UF == "RS")

escolas_pr <- escolas_br%>%
  filter(UF == "PR")

escolas_ms <- escolas_br%>%
  filter(UF == "MS")

escolas_pb <- escolas_br%>%
  filter(UF == "PB")

escolas_mt <- escolas_br%>%
  filter(UF == "MT")

escolas_pe <- escolas_br%>%
  filter(UF == "PE")

escolas_to <- escolas_br%>%
  filter(UF == "TO")

escolas_rr <- escolas_br%>%
  filter(UF == "RR")

escolas_go <- escolas_br%>%
  filter(UF == "GO")

escolas_ma <- escolas_br%>%
  filter(UF == "MA")

escolas_pi <- escolas_br%>%
  filter(UF == "PI")

escolas_se <- escolas_br%>%
  filter(UF == "SE")

escolas_mg <- escolas_br%>%
  filter(UF == "MG")

escolas_ro <- escolas_br%>%
  filter(UF == "RO")

escolas_ba <- escolas_br%>%
  filter(UF == "BA")

escolas_ce <- escolas_br%>%
  filter(UF == "CE")

escolas_al <- escolas_br%>%
  filter(UF == "AL")

escolas_pa <- escolas_br%>%
  filter(UF == "PA")

escolas_rj <- escolas_br%>%
  filter(UF == "RJ")

escolas_am <- escolas_br%>%
  filter(UF == "AM")

escolas_ac <- escolas_br%>%
  filter(UF == "AC")

escolas_df <- escolas_br%>%
  filter(UF == "DF")

escolas_ap <- escolas_br%>%
  filter(UF == "AP")

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\tadepe\\fantastico\\obras_por_uf")

write.table(escolas_sc, file="escolas_sc.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_es, file="escolas_es.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_rn, file="escolas_rn.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_sp, file="escolas_sp.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_rs, file="escolas_rs.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_pr, file="escolas_pr.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_ms, file="escolas_ms.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_pb, file="escolas_pb.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_mt, file="escolas_mt.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_pe, file="escolas_pe.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_to, file="escolas_to.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_rr, file="escolas_rr.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_go, file="escolas_go.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_ma, file="escolas_ma.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_pi, file="escolas_pi.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_se, file="escolas_se.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_mg, file="escolas_mg.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_ro, file="escolas_ro.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_ba, file="escolas_ba.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_ce, file="escolas_ce.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_al, file="escolas_al.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_pa, file="escolas_pa.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_rj, file="escolas_rj.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_am, file="escolas_am.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_ac, file="escolas_ac.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_df, file="escolas_df.csv", sep=";", dec=",", row.names = FALSE)
write.table(escolas_ap, file="escolas_ap.csv", sep=";", dec=",", row.names = FALSE)