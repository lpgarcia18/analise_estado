library(readxl)
library(tidyverse)
library(reshape2)

municip_regiao_pop <- read_excel("base/municip_regiao_pop.xls")

curva_obitos_SRAG <- read_excel("base/parametro SRAG.xlsx", 
    sheet = "todos")

canal_endemico <- merge(municip_regiao_pop, curva_obitos_SRAG, by.x = "COD_IBGE",by.y = "Município", all = T)




canal_endemico <- canal_endemico %>% dplyr::select(REGIAO_DE_SAUDE, `data do obito`)
canal_endemico <- na.omit(canal_endemico)
names(canal_endemico) <- c("REGIAO", "DATA")
canal_endemico$OBITOS <- 1
canal_endemico$DATA <- substr(canal_endemico$DATA, 0, 4) 
canal_endemico <- canal_endemico %>% 
	group_by(REGIAO, DATA) %>%
	summarise(OBITOS = sum(OBITOS, na.rm = T))

analise <- canal_endemico %>% 
	group_by(DATA) %>%
	summarise(OBITOS = sum(OBITOS, na.rm = T))

canal_endemico$OBITOS <- canal_endemico$OBITOS/12 # Fazendo uma média por semana

canal_endemico <- subset(canal_endemico, canal_endemico$DATA %in% c("2015","2017","2018")) #retirando 2016 e 2019, pois são outliers
canal_endemico <- dcast(REGIAO ~ DATA ,data = canal_endemico)

canal_quartis <- lapply(canal_endemico[,-1], as.numeric ) %>% as.data.frame()
canal_quartis <- apply(canal_quartis,1, quantile ) %>% 
	t() %>%
	as.data.frame()
canal_endemico <- cbind(canal_endemico[,1], canal_quartis) %>% as.data.frame()
canal_endemico$`0%` <- NULL

names(canal_endemico)[1] <- "REGIAO"

write_xlsx(canal_endemico,"base/canal_endemico.xlsx")

