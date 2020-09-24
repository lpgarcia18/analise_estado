library(readxl)
library(tidyverse)

municip_regiao_pop <- read_excel("base/municip_regiao_pop.xls")

curva_obitos_SRAG <- read_excel("base/obitos_srag_2015_2019.xlsx", 
    sheet = "COMPILADO", col_types = c("text", 
        "date"))

canal_endemico <- merge(municip_regiao_pop, curva_obitos_SRAG, by.x = "COD_IBGE", all = T)


canal_endemico <- canal_endemico %>% dplyr::select(REGIAO_DE_SAUDE, DT_OBITO)
canal_endemico <- na.omit(canal_endemico)
names(canal_endemico) <- c("REGIAO", "DATA")
canal_endemico$OBITOS <- 1
canal_endemico <- canal_endemico %>% 
	group_by(REGIAO, DATA) %>%
	summarise(OBITOS = sum(OBITOS, na.rm = T))
canal_endemico$ANO <- substr(canal_endemico$DATA,0,4)
canal_endemico$DIA <- substr(canal_endemico$DATA,9,10)
canal_endemico$MES <- substr(canal_endemico$DATA,6,7)
canal_endemico$MES_DIA <- paste0(canal_endemico$MES, "-", canal_endemico$DIA)
canal_endemico$MES_DIA <- as.factor(canal_endemico$MES_DIA)
ggplot(canal_endemico, aes(MES_DIA, OBITOS, color = factor(ANO), group = factor(ANO)))+
	geom_smooth()

canal_endemico <- subset(canal_endemico, canal_endemico$ANO != 2019) #2019 foi tirado, pois foi a transição do info grip para o sivep e está muito atípico
MES_DIA <- c((Sys.Date()-365):Sys.Date())
MES_DIA <- as.Date(MES_DIA, origin = "1970-01-01")
MES_DIA <- substr(MES_DIA, 6,10) 
MES_DIA <- sort(MES_DIA)
MES_DIA <- rep(MES_DIA, 16)%>% as.data.frame()
MES_DIA <- MES_DIA[MES_DIA$. != "02-29", ] %>% as.data.frame()
names(MES_DIA) <- "MES_DIA"


REGIAO <- unique(canal_endemico$REGIAO) 
REGIAO <- rep(REGIAO, 365)%>% as.data.frame()
names(REGIAO) <- "REGIAO"
REGIAO <- sort(REGIAO)

MES_DIA <- cbind(MES_DIA, REGIAO) %>% as.data.frame()

canal_endemico <- merge(canal_endemico, MES_DIA, by = c("MES_DIA", "REGIAO"), all = T)

canal_endemico <- canal_endemico %>% dplyr::select(REGIAO, MES_DIA, OBITOS)
canal_endemico$OBITOS <- ifelse(is.na(canal_endemico$OBITOS),0, canal_endemico$OBITOS)
canal_endemico$DATA <- paste0("2020-", canal_endemico$MES_DIA) %>% as.Date()
ggplot(canal_endemico, aes(DATA, OBITOS, color = as.factor(REGIAO)))+
	geom_smooth()

canal_endemico$PRIM_QUARTIL <- canal_endemico$OBITOS/4
canal_endemico$SEGUND_QUARTIL <- canal_endemico$OBITOS/2
canal_endemico$TERC_QUARTIL <- 3*canal_endemico$OBITOS/4
canal_endemico$QUARTO_QUARTIL <- canal_endemico$OBITOS
canal_endemico$MES_DIA <- NULL
canal_endemico$OBITOS <- NULL

canal_endemico <- subset(canal_endemico, canal_endemico$DATA > Sys.Date() &
			 	canal_endemico$DATA <= (Sys.Date()+14))

canal_endemico <- canal_endemico %>% 
	group_by(REGIAO) %>%
	summarise(PRIM_QUARTIL = sum(PRIM_QUARTIL, na.rm = T),
		  SEGUND_QUARTIL = sum(SEGUND_QUARTIL, na.rm = T),
		  TERC_QUARTIL = sum(TERC_QUARTIL, na.rm = T),
		  QUARTO_QUARTIL = sum(QUARTO_QUARTIL, na.rm = T))

write_xlsx(canal_endemico,"base/canal_endemico.xlsx")

