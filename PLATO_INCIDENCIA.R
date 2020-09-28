source("nowcasting_Rt_SEIRD.R")
casos_novos <- base_nowcasted
casos_novos_list <- list()
for(i in 1:16){ # são 16 regiões
	casos_novos_cort <- subset(casos_novos, as.numeric(casos_novos$REGIAO) == i)
	casos_novos_cort <- subset(casos_novos_cort, casos_novos_cort$DATA < Sys.Date() &
				   	casos_novos_cort$DATA >= (Sys.Date()-15))
	casos_novos_cort <- casos_novos_cort %>% dplyr::select(REGIAO, CASOS)
	casos_novos_cort <- casos_novos_cort %>% 
		group_by(REGIAO) %>%
		summarise(CASOS = sum(CASOS, na.rm = T))
	casos_novos_list[[i]] <- casos_novos_cort
}
	
casos_novos <- do.call(rbind, casos_novos_list)	
casos_novos <- merge(casos_novos, municip_regiao_pop, by = "REGIAO", all = T)
casos_novos$PLATO_INCIDENCIA <- casos_novos$CASOS/casos_novos$POP*100000
	
casos_novos$GRAVIDADE <- ifelse(casos_novos$PLATO_INCIDENCIA <= 10, "Moderado",
					ifelse(casos_novos$PLATO_INCIDENCIA <= 20, "Alto",
					       ifelse(casos_novos$PLATO_INCIDENCIA <= 30, "Grave", "Gravíssimo")))

write_xlsx(casos_novos,"base/PLATO_INCIDENCIA.xlsx")
