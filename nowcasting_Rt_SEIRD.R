# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(forecast)
library(EpiEstim)
library(EpiModel)
library(foreign)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(reshape2)
library(RcppRoll)
library(writexl)


# Nowcasting --------------------------------------------------------------------
#Utiliza-se os dados de casos confirmados do estado de SC 
#Disponível em: http://dados.sc.gov.br/dataset/covid-19-dados-anonimizados-de-casos-confirmados
base_inicial <- read_delim("base/boavista_covid_dados_abertos.csv", 
					   ";", escape_double = FALSE, trim_ws = TRUE)

casos <- base_inicial
casos <- data.frame(INICIO_SINTOMAS = casos$data_inicio_sintomas, 
		    REGIAO = casos$regional_saude,
		    CASOS = rep.int(1,nrow(casos)))

casos <- subset(casos, casos$REGIAO != "NULL") #retirando os casos que não possuem informação sobre a região de saúde

casos <- casos %>%
	group_by(INICIO_SINTOMAS, REGIAO) %>%
	summarise(CASOS = sum(CASOS, na.rm = T))
casos$INICIO_SINTOMAS <- as.Date(casos$INICIO_SINTOMAS, format = "%d/%m/%Y")


# Truncando atrasos -------------------------------------------------------
#A contaminação ocorre, em média, 5 dias antes do início dos sintomas, trabalhar-se-á a data provável de contaminação
casos$DT_CONTAMINACAO <- casos$INICIO_SINTOMAS - 5 
casos$INICIO_SINTOMAS <- NULL




ggplot(casos, aes(DT_CONTAMINACAO, CASOS, color = REGIAO))+
	geom_line()

#Truncando datas  com atraso em que o paciente foi contaminado, mas que ainda não tem resultado de exame
contaminacao_ao_inicio_sintomas <- 6 #95% dos pacientes que desenvolvem sintomas o fazem em até 6 dias (Prem K et al, 2020)
inicio_sintomas_resultado_exames <- 8 #95% dos exames que vão para o gal tem seu resultado liberado após 8 dias do início dos sintomas

recorte <- (contaminacao_ao_inicio_sintomas + 
	    	inicio_sintomas_resultado_exames
	    )

casos <- casos[order(casos$DT_CONTAMINACAO),]
casos <- subset(casos, casos$DT_CONTAMINACAO <
			       	(tail(casos$DT_CONTAMINACAO,1) - recorte))

ggplot(casos, aes(DT_CONTAMINACAO, CASOS, color = REGIAO))+
	geom_line()


#realizando as projeções do período excluído para ajustar os artefatos
casos$REGIAO <- factor(casos$REGIAO, levels = c("ALTO URUGUAI CATARINENSE",
						"ALTO VALE DO ITAJAI",
						"ALTO VALE DO RIO DO PEIXE",
						"CARBONIFERA",
						"EXTREMO OESTE",
						"EXTREMO SUL CATARINENSE",
						"FOZ DO RIO ITAJAI",
						"GRANDE FLORIANOPOLIS",
						"LAGUNA",
						"MEDIO VALE DO ITAJAI",
						"MEIO OESTE",
						"NORDESTE",
						"OESTE",
						"PLANALTO NORTE",
						"SERRA CATARINENSE",
						"XANXERE"))

casos_list <- list()
for(i in 1:16){ # são 16 regiões
	casos_cort <- subset(casos, as.numeric(casos$REGIAO) == i)
	casos_proj <- forecast(auto.arima(casos_cort$CASOS,lambda = "auto", biasadj = T, stepwise=FALSE, approximation=FALSE),
			       h=((Sys.Date()-1)-max(as.Date(casos_cort$DT_CONTAMINACAO))))$mean[1:((Sys.Date()-1)-max(as.Date(casos_cort$DT_CONTAMINACAO)))] %>%
		as.data.frame()
	casos_proj <- data.frame(MEDIANA_CASOS = casos_proj,
				 DT_CONTAMINACAO = c(max(as.Date(casos_cort$DT_CONTAMINACAO)+1):(Sys.Date()-1)))
	
	names(casos_proj) <- c("CASOS", "DT_CONTAMINACAO")
	casos_proj$DT_CONTAMINACAO <- as.Date(casos_proj$DT_CONTAMINACAO, origin = "1970-01-01")
	casos_proj$REGIAO <- unique(casos_cort$REGIAO)
	casos_list[[i]] <- rbind(casos_cort, casos_proj) %>% as.data.frame()
}

casos_nowcasted <- do.call(rbind, casos_list)
casos_nowcasted$REGIAO <- factor(casos_nowcasted$REGIAO, levels = c("ALTO URUGUAI CATARINENSE",
						"ALTO VALE DO ITAJAI",
						"ALTO VALE DO RIO DO PEIXE",
						"CARBONIFERA",
						"EXTREMO OESTE",
						"EXTREMO SUL CATARINENSE",
						"FOZ DO RIO ITAJAI",
						"GRANDE FLORIANOPOLIS",
						"LAGUNA",
						"MEDIO VALE DO ITAJAI",
						"MEIO OESTE",
						"NORDESTE",
						"OESTE",
						"PLANALTO NORTE",
						"SERRA CATARINENSE",
						"XANXERE"))

ggplot(casos_nowcasted, aes(DT_CONTAMINACAO, CASOS, color = REGIAO))+
	geom_line()


#################################################
#Estimativa do número de óbitos
#################################################
obitos <- base_inicial

obitos$DATA <- as.Date(obitos$data_obito, format = "%Y-%m-%d")
obitos$INICIO_SINTOMAS <- as.Date(obitos$data_inicio_sintomas, format = "%Y-%m-%d")

obitos <- data.frame(DATA = obitos$DATA, 
		     REGIAO = obitos$regional_saude,
		     INICIO_SINTOMAS = obitos$INICIO_SINTOMAS,
		     OBITOS = ifelse(obitos$obito == 'SIM', 1, 0))
obitos <- subset(obitos, obitos$OBITOS == 1)

obitos$inicio_sintomas_obito <- obitos$DATA - obitos$INICIO_SINTOMAS 
inicio_sintomas_obito <- quantile(obitos$inicio_sintomas_obito, 0.5)
contaminacao_obito <- contaminacao_ao_inicio_sintomas + inicio_sintomas_obito

obitos <- obitos %>% 
	  group_by(DATA, REGIAO) %>%
	  summarise(OBITOS = sum(OBITOS, na.rm = T))
names(obitos) <- c("DATA", "REGIAO", "OBITOS")

ggplot(obitos, aes(DATA, OBITOS, color = REGIAO))+
	geom_line()

#################################################
#Estimativa do número de expostos
#################################################
#O perído de exposição pode ser definido como tempo entre o contato com o vírus (início da incubação) ao início da infectividade.
#Os pacientes expostos iniciam o contágio, em média, 2 a 3 dias antes do início dos sintomas. (Wölfel R et all, 2020).
#Adotou-se, então, como período de exposição aquele entrea 5 dias e 3 dias antes do início dos sintomas.
#Para se analisar a quantidade de pacientes expostos por dia, subtraiu-se 5 da data de início de sintomas e utilizou-se
#soma móvel de 2 dias (5 dia ao 3 dia antes do início dos sintomas).Para corrigir o truncados à direita,
#utilizou-se o modelo de suavização esponencial ou ARIMA, com o menor erro quadrado.
expostos <- casos_nowcasted

expostos_list <- list()
for(i in 1:16){ # são 16 regiões
	expostos_cort <- subset(expostos, as.numeric(expostos$REGIAO) == i)
	expostos_cort$EXPOSTOS <- roll_sum(expostos_cort$CASOS,2, fill = 0, align = "right") #menos de 3 dias do início dos sintomas
	expostos_cort$CASOS <- NULL
	
	expostos_list[[i]] <- expostos_cort
}

expostos_nowcasted <- do.call(rbind, expostos_list)

ggplot(expostos_nowcasted, aes(DT_CONTAMINACAO, EXPOSTOS, color = REGIAO))+
	geom_line()

#################################################
#Estimativa do número de infectantes
#################################################
#De acordo com estudos recentes(Zou L et al, 2020; To KKW et al, 2020), a carga viral diminui monotonicamente
#após o início dos sintomas. Outro estudo de Wuhan detectou o vírums em pacientes 20 dias (mediana)
#após o início dos sintomas (Zhou F et al, 2020). Contudo, após 8 didas do início dos sintomas,
#o vírus vivo não pode mais ser cultivado, o que pode indicar o fim do perído de infectividade. (Wölfel R et al, 2020)
#Esta pesquisa adotou, então, como período infectante aquele entre dois dias antes e 8 dias após o início dos sintomas.
#Para a estimativa dos casos truncados, utilizou-se o modelo de suavização esponencial
#ou ARIMA, com o menor erro quadrado
infectantes <- casos_nowcasted

infectantes_list <- list()
for(i in 1:16){ # são 16 regiões
	infectantes_cort <- subset(infectantes, as.numeric(infectantes$REGIAO) == i)
	infectantes_cort$INFECTANTES <- roll_sum(infectantes_cort$CASOS,14, fill = 0, align = "right") #menos de 3 dias do início dos sintomas
	infectantes_cort$CASOS <- NULL
	
	infectantes_list[[i]] <- infectantes_cort
}

infectantes_nowcasted <- do.call(rbind, infectantes_list)

ggplot(infectantes_nowcasted, aes(DT_CONTAMINACAO, INFECTANTES, color = REGIAO))+
	geom_line()


#################################################
#Estimativa do número de recuperados
#################################################
#Considerou-se recuperado o indivíduo com mais de 16 dias após a contaminação (6 da contaminação ao início dos sintomas +
#10 de transmissibilidade após o início dos sintomas, segundo documento da SVS) e que não foi a óbito. Óbitos e recuperados
#são cumulativos
recuperados <- casos_nowcasted
recuperados$DATA <- (recuperados$DT_CONTAMINACAO+16) %>% as.character()
recuperados <- subset(recuperados, recuperados$DATA < Sys.Date())
recuperados$DT_CONTAMINACAO <- NULL
recuperados$RECUPERADOS <- recuperados$CASOS
recuperados$CASOS <- NULL
recuperados$DATA <- as.Date(recuperados$DATA)


recuperados <- merge(recuperados, obitos, by = c("DATA", "REGIAO"), all = T)
recuperados[is.na(recuperados)] <- 0
recuperados$RECUPERADOS <- recuperados$RECUPERADOS - recuperados$OBITOS
#Base SEIRD
casos_nowcasted$DATA <- as.Date(casos_nowcasted$DT_CONTAMINACAO)
casos_nowcasted$DT_CONTAMINACAO <- NULL
expostos_nowcasted$DATA <- as.Date(expostos_nowcasted$DT_CONTAMINACAO)
expostos_nowcasted$DT_CONTAMINACAO <- NULL
infectantes_nowcasted$DATA <- as.Date(infectantes_nowcasted$DT_CONTAMINACAO)
infectantes_nowcasted$DT_CONTAMINACAO <- NULL


base <- merge(recuperados, expostos_nowcasted, by = c("DATA", "REGIAO"), all = T)
base <- merge(base, infectantes_nowcasted, by = c("DATA", "REGIAO"), all = T)
base <- merge(base, casos_nowcasted, by = c("DATA", "REGIAO"), all = T)

##Cumulativos e suceptíveis
base$DATA <- as.Date(base$DATA, "%Y-%m-%d")

base[is.na(base$RECUPERADOS), names(base) == "RECUPERADOS"] <- 0
base[is.na(base$OBITOS), names(base) == "OBITOS"] <- 0

base$REGIAO <- factor(base$REGIAO, levels = c("ALTO URUGUAI CATARINENSE",
						"ALTO VALE DO ITAJAI",
						"ALTO VALE DO RIO DO PEIXE",
						"CARBONIFERA",
						"EXTREMO OESTE",
						"EXTREMO SUL CATARINENSE",
						"FOZ DO RIO ITAJAI",
						"GRANDE FLORIANOPOLIS",
						"LAGUNA",
						"MEDIO VALE DO ITAJAI",
						"MEIO OESTE",
						"NORDESTE",
						"OESTE",
						"PLANALTO NORTE",
						"SERRA CATARINENSE",
						"XANXERE"))


base_list <- list()
for(i in 1:16){ # são 16 regiões
	base_cort <- subset(base, as.numeric(base$REGIAO) == i)
	base_cort <- base_cort[order(base_cort$DATA),]
	base_cort$CUM_RECUPERADOS <- cumsum(base_cort$RECUPERADOS)
	base_cort$CUM_OBITOS <- cumsum(base_cort$OBITOS)
	base_list[[i]] <- base_cort
}

base_nowcasted <- do.call(rbind, base_list)


ggplot(base_nowcasted, aes(DATA, CUM_RECUPERADOS, color = REGIAO))+
	geom_line()

ggplot(base_nowcasted, aes(DATA, CUM_OBITOS, color = REGIAO))+
	geom_line()


#################################################
#Estimativa do número de suscetíveis
#################################################
POP <- 7164788
base_nowcasted$SUSCETIVEIS <- POP - base_nowcasted$CUM_RECUPERADOS - base_nowcasted$CUM_OBITOS - base_nowcasted$EXPOSTOS - base_nowcasted$INFECTANTES

#################################################
#merge
#################################################
#merge dos dados de ocupação de leitos e número de intenação com os outros dados
base_nowcasted$DATA <- as.Date(base_nowcasted$DATA)
base_nowcasted <- unique(base_nowcasted)
base_nowcasted <- na.omit(base_nowcasted)

# Estimando o Rt ----------------------------------------------------------
source("apeEstim.R")
source("apePredPost.R")


res_base_list <- list()
for(i in 1:16){ # são 16 regiões
	incidencia <- subset(base_nowcasted, as.numeric(base_nowcasted$REGIAO) == i)
	incidencia <- incidencia %>% dplyr::select("CASOS", "DATA", "REGIAO")
	incidencia <- subset(incidencia, incidencia$DATA > c(Sys.Date()-92)) #Utilizando dados dos últimos trës meses
	
	#Left trunc
	trunc <- 0
	
	Icovid <- incidencia$CASOS #Incidência
	gencovid <- EpiEstim::discr_si(c(0:max(incidencia$CASOS)), mu = 4.8, sigma = 2.3) #distribuição gama
	Lcovid <- overall_infectivity(Icovid, gencovid)
	
	#Priors and settings
	Rprior = c(1, 5); a = 0.025 #Confidence interval level
	
	#Clean Lam vectors of NAs
	Lcovid[is.na(Lcovid)] = 0# <------ important
	
	#Best estimates and prediction
	Rmodcovid <- apeEstim(Icovid, gencovid, Lcovid, Rprior, a, trunc, "covid")
	Rcovid <- Rmodcovid[[2]][[4]]
	RcovidCI_025 <- Rmodcovid[[2]][[5]][1,]
	RcovidCI_975 <- Rmodcovid[[2]][[5]][2,]
	DATA <- tail(incidencia$DATA,-1)
	REGIAO <- unique(incidencia$REGIAO)
	res_base <- data.frame(DATA = DATA, MEDIA = Rcovid, IC025 = RcovidCI_025, IC975 = RcovidCI_975, REGIAO = REGIAO)
	res_base_list[[i]] <- subset(res_base, res_base$DATA >= (Sys.Date() -61))
}	

res_base <- do.call(rbind, res_base_list)	



# Forecast do número de casos e dos óbitos --------------------------------
projecoes_list <- list()
for(i in 1:16){ # são 16 regiões
	projecoes <- subset(base_nowcasted, as.numeric(base_nowcasted$REGIAO) == i)
	res_proj <- subset(res_base, as.numeric(res_base$REGIAO) == i)
	#Estados Iniciais
	S <- tail(projecoes$SUSCETIVEIS,1)[1]
	E <- tail(projecoes$EXPOSTOS,1)[1]
	I <- tail(projecoes$INFECTANTES,1)[1]
	R <- tail(projecoes$CUM_RECUPERADOS,1)[1]
	D <- tail(projecoes$CUM_OBITOS,1)[1]
	N <- S + E + I +  R + D
	id.dur <- as.numeric(contaminacao_obito)
	ir.dur <- 14 
	ei.dur <- 2
	etha <- 1/ir.dur
	betha <- 1/ei.dur
	delta <- 1/id.dur
	
	#Estimativa das probabilidade para o modelo
	projecoes$TOTAL <- projecoes$EXPOSTOS + projecoes$INFECTANTES + projecoes$CUM_RECUPERADOS + projecoes$CUM_OBITOS
	projecoes$CUM_CASOS <- cumsum(projecoes$CASOS)
	prob1 <- tail(projecoes$CUM_OBITOS,1)/tail(projecoes$TOTAL,1) #Taxa de hospitalizados recuperados de UTI - denominador usando a defasagem
	
	init <- init.dcm(S = S,
			 E = E,
			 I = I,
			 R = R,
			 D = D,
			 se.flow = 0,
			 ei.flow = 0,
			 ir.flow = 0,
			 id.flow = 0
	)
	
	
	
	param <- param.dcm(Rt = c(tail(res_proj$IC025,1),
				  tail(res_proj$MEDIA,1),
				  tail(res_proj$IC975,1)),
			   etha = 1/ir.dur,
			   betha = 1/ei.dur,
			   delta = 1/id.dur,
			   prob1 = prob1
	)
	
	
	#Função SEIRD
	SEIRD <- function(t, t0, parms) {
		with(as.list(c(t0, parms)), {
			
			N <- S + E + I +  R + D
			
			alpha <- etha * Rt * I/N
			
			#Equações diferenciais
			dS <- -alpha*S
			dE <- alpha*S - betha*E
			dI <- betha*E - prob1*delta*I - (1-prob1)*etha*I
			dR <- (1 - prob1)*etha*I 
			dD <- prob1*delta*I
			
			#Outputs
			list(c(dS, dE, dI, dR, dD,
			       se.flow = alpha * S,
			       ei.flow = betha * E,
			       ir.flow = (1 - prob1)*etha*I,
			       id.flow = prob1*delta*I),
			     num = N,
			     s.prev = S / N,
			     e.prev = E / N,
			     i.prev = I / N,
			     ei.prev = (E + I)/N,
			     r.prev = R / N,
			     d.prev = D / N)
		})
	}
	
	
	#Resolvendo as equações diferenciais
	projecao <- 60
	control <- control.dcm(nsteps = projecao, new.mod = SEIRD)
	mod <- dcm(param, init, control)
	
	
	######################################
	#Cenário Rt 1 - IC2.5
	######################################
	resultados_cenario_1 <- data.frame(SUSCETIVEIS = mod$epi$S$run1,
					   EXPOSTOS = mod$epi$E$run1,
					   INFECTANTES = mod$epi$I$run1,
					   CUM_RECUPERADOS = mod$epi$R$run1,
					   CUM_OBITOS = mod$epi$D$run1,
					   REGIAO = unique(projecoes$REGIAO))
	
	resultados_cenario_1 <- resultados_cenario_1[-1,]
	resultados_cenario_1$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
	resultados_cenario_1$DATA  <- as.Date(resultados_cenario_1$DATA , origin = "1970-01-01")
	base_select <- projecoes %>% dplyr::select(REGIAO, DATA, SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS)
	resultados_cenario_1 <- rbind(base_select, resultados_cenario_1)
	names(resultados_cenario_1) <-c("REGIAO", "DATA", "SUSCETIVEIS_CENARIO_1", "CUM_RECUPERADOS_CENARIO_1", "EXPOSTOS_CENARIO_1", "INFECTANTES_CENARIO_1", "CUM_OBITOS_CENARIO_1")
	
	######################################
	#Cenário 2 - Rt Mediana
	######################################
	resultados_cenario_2 <- data.frame(SUSCETIVEIS = mod$epi$S$run2,
					   EXPOSTOS = mod$epi$E$run2,
					   INFECTANTES = mod$epi$I$run2,
					   CUM_RECUPERADOS = mod$epi$R$run2,
					   CUM_OBITOS = mod$epi$D$run2,
					   REGIAO = unique(projecoes$REGIAO))
	
	resultados_cenario_2 <- resultados_cenario_2[-1,]
	resultados_cenario_2$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
	resultados_cenario_2$DATA  <- as.Date(resultados_cenario_2$DATA , origin = "1970-01-01")
	base_select <- projecoes %>% dplyr::select(REGIAO, DATA, SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS)
	resultados_cenario_2 <- rbind(base_select, resultados_cenario_2)
	names(resultados_cenario_2) <-c("REGIAO", "DATA", "SUSCETIVEIS_CENARIO_2", "CUM_RECUPERADOS_CENARIO_2", "EXPOSTOS_CENARIO_2", "INFECTANTES_CENARIO_2", "CUM_OBITOS_CENARIO_2")
	
	######################################
	#Cenário 3 - Rt IC975
	######################################
	resultados_cenario_3 <- data.frame(SUSCETIVEIS = mod$epi$S$run3,
					   EXPOSTOS = mod$epi$E$run3,
					   INFECTANTES = mod$epi$I$run3,
					   CUM_RECUPERADOS = mod$epi$R$run3,
					   CUM_OBITOS = mod$epi$D$run3,
					   REGIAO = unique(projecoes$REGIAO))
	
	resultados_cenario_3 <- resultados_cenario_3[-1,]
	resultados_cenario_3$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
	resultados_cenario_3$DATA  <- as.Date(resultados_cenario_3$DATA , origin = "1970-01-01")
	base_select <- projecoes %>% dplyr::select(REGIAO, DATA, SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS)
	resultados_cenario_3 <- rbind(base_select, resultados_cenario_3)
	names(resultados_cenario_3) <-c("REGIAO", "DATA", "SUSCETIVEIS_CENARIO_3", "CUM_RECUPERADOS_CENARIO_3", "EXPOSTOS_CENARIO_3", "INFECTANTES_CENARIO_3", "CUM_OBITOS_CENARIO_3")
	
	#Unindo as bases de resultados
	resultados <- merge(resultados_cenario_1, resultados_cenario_2, by= "DATA", all = T)
	resultados <- merge(resultados_cenario_3, resultados, by= "DATA", all = T)
	projecoes_list[[i]] <- resultados
}

projecoes_final <- do.call(rbind, projecoes_list)


ggplot(projecoes_final, aes(DATA, CUM_OBITOS_CENARIO_3, color = REGIAO))+
	geom_line()+
	theme_bw()




# Cálculo dos indicadores -------------------------------------------------
#Óbitos por SRAG
obitos_proj <- projecoes_final %>% select(REGIAO, DATA, CUM_OBITOS_CENARIO_1, CUM_OBITOS_CENARIO_2, CUM_OBITOS_CENARIO_3)
obitos_proj <- subset(obitos_proj, obitos_proj$DATA >= Sys.Date() &
			 	obitos_proj$DATA < (Sys.Date()+31))
obitos_proj_list <- list()
for(i in 1:16){ # são 16 regiões
	obitos_cort <- subset(obitos_proj, as.numeric(obitos_proj$REGIAO) == i)
	obitos_cort$OBITOS_CENARIO_3 <- obitos_cort$CUM_OBITOS_CENARIO_3 - lag(obitos_cort$CUM_OBITOS_CENARIO_3,1)
	obitos_cort$CUM_OBITOS_CENARIO_1 <- NULL
	obitos_cort$CUM_OBITOS_CENARIO_2 <- NULL
	obitos_cort$CUM_OBITOS_CENARIO_3 <- NULL
	obitos_proj_list[[i]] <- obitos_cort
}
obitos_proj <- do.call(rbind, obitos_proj_list)

obitos_proj <- obitos_proj %>% 
	group_by(REGIAO) %>%
	summarise(OBITOS_CENARIO_3 = sum(OBITOS_CENARIO_3, na.rm = T))

canal_endemico <- read_excel("base/canal_endemico.xlsx")

obitos_canal_endemico <- merge(obitos_proj, canal_endemico, by = "REGIAO", all = T)

obitos_canal_endemico$GRAVIDADE <- ifelse(obitos_canal_endemico$OBITOS_CENARIO_3 <= obitos_canal_endemico$PRIM_QUARTIL, "Moderado",
					ifelse(obitos_canal_endemico$OBITOS_CENARIO_3 <= obitos_canal_endemico$SEGUND_QUARTIL, "Alto",
					       ifelse(obitos_canal_endemico$OBITOS_CENARIO_3 <= obitos_canal_endemico$TERC_QUARTIL, "Grave", "Gravíssimo")))

write_xlsx(obitos_canal_endemico,"base/GRAVIDADE.xlsx")


#Rt dos últimos 14 dias, limite superior
res_base_14_dias <- subset(res_base, res_base$DATA >= (Sys.Date()-14))
res_base_14_dias <- res_base_14_dias %>% dplyr::select(REGIAO, DATA, IC975)
names(res_base_14_dias)[3] <- "LIMITE_SUPERIOR_Rt"
ggplot(res_base_14_dias, aes(DATA, LIMITE_SUPERIOR_Rt, color = REGIAO))+
	geom_line(size = 1.5)

res_base_14_dias$RT_CONTROLADO <- ifelse(res_base_14_dias$LIMITE_SUPERIOR_Rt < 1, "Sim", "Não")

res_base_14_dias_list <- list()
for(i in 1:16){ # são 16 regiões
	res_base_14_dias_cort <- subset(res_base_14_dias, as.numeric(res_base_14_dias$REGIAO) == i)
	res_base_14_dias_cort$CRESCIMENTO <- ifelse(sum(tail(res_base_14_dias_cort$RT_CONTROLADO,14)=="Sim") == 14, "Moderado",
					   ifelse(sum(tail(res_base_14_dias_cort$RT_CONTROLADO,7)=="Sim") == 7, "Alto",
					          ifelse(sum(tail(res_base_14_dias_cort$RT_CONTROLADO,3)=="Sim") == 3, "Grave", "Gravíssimo")))
	res_base_14_dias_cort <- tail(res_base_14_dias_cort,1)
	res_base_14_dias_list[[i]] <- res_base_14_dias_cort
}

res_base_14_dias <- do.call(rbind, res_base_14_dias_list) %>% as.data.frame()
write_xlsx(res_base_14_dias,"base/CRESCIMENTO.xlsx")


#Infectantes/ população
municip_regiao_pop <- read_excel("base/municip_regiao_pop.xls")
municip_regiao_pop <- municip_regiao_pop %>%
	group_by(REGIAO_DE_SAUDE) %>%
	summarise(POP = sum(POP_2020, na.rm = T))

names(municip_regiao_pop)[1] <- "REGIAO" 

casos_ativos_populacao <- merge(base_nowcasted, municip_regiao_pop, by = "REGIAO", all= T)
casos_ativos_populacao$ATIVOS_POP <- casos_ativos_populacao$INFECTANTES/casos_ativos_populacao$POP*100000
casos_ativos_populacao <- casos_ativos_populacao %>% select(REGIAO, DATA, ATIVOS_POP)

ggplot(casos_ativos_populacao, aes(DATA, ATIVOS_POP, color = REGIAO))+
	geom_line(size = 1.5)

casos_ativos_populacao$INFECTIVIDADE <- ifelse(casos_ativos_populacao$ATIVOS_POP <= 50, "Moderado",
					ifelse(casos_ativos_populacao$ATIVOS_POP <= 100, "Alto",
					       ifelse(casos_ativos_populacao$ATIVOS_POP <= 150, "Grave", "Gravíssimo")))

casos_ativos_populacao_list <- list()
for(i in 1:16){ # são 16 regiões
	casos_ativos_populacao_cort <- subset(casos_ativos_populacao, as.numeric(casos_ativos_populacao$REGIAO) == i)
	casos_ativos_populacao_cort <- tail(casos_ativos_populacao_cort,1)
	casos_ativos_populacao_list[[i]] <- casos_ativos_populacao_cort
}

casos_ativos_populacao <- do.call(rbind, casos_ativos_populacao_list) %>% as.data.frame()


write_xlsx(casos_ativos_populacao,"base/INFECTIVIDADE.xlsx")


#cofirmados/suspeitos
serie_agrupada_regioes <- read_excel("base/serie_agrupada_regioes.xlsx")
serie_agrupada_regioes <- serie_agrupada_regioes %>% dplyr::select(dia, regiao, suspeitos, confirmados)
serie_agrupada_regioes <- subset(serie_agrupada_regioes, serie_agrupada_regioes$regiao != "OUTROS ESTADOS")
MEIO <- subset(serie_agrupada_regioes, serie_agrupada_regioes$regiao == "HERVAL D'OESTE" | #Herval D'Oestes é um município do meio oeste, mas está fora da região na base de dados
                       serie_agrupada_regioes$regiao == "MEIO OESTE") #Por isso, somaram-se os dados para se ter o total do meio oeste
MEIO <- MEIO %>%
	group_by(dia) %>%
	summarise(confirmados = sum(confirmados, na.rm = T),
		  suspeitos = sum(suspeitos, na.rm = T))
MEIO$regiao <- "MEIO OESTE" 

serie_agrupada_regioes <- subset(serie_agrupada_regioes, serie_agrupada_regioes$regiao != "HERVAL D'OESTE" &
                       serie_agrupada_regioes$regiao != "MEIO OESTE") 

serie_agrupada_regioes <- rbind(serie_agrupada_regioes, MEIO) %>% as.data.frame()

serie_agrupada_regioes$TX_CONFIRMADOS_SUSPEITOS <- serie_agrupada_regioes$confirmados/serie_agrupada_regioes$suspeitos*100

names(serie_agrupada_regioes) <- c("DATA", "REGIAO", "SUSPEITOS", "CONFIRMADOS", "TX_CONFIRMADOS_SUSPEITOS")
serie_agrupada_regioes$DATA <- as.Date(serie_agrupada_regioes$DATA)

ggplot(serie_agrupada_regioes, aes(DATA, SUSPEITOS, color = REGIAO))+
	geom_line(size = 1.5)

ggplot(serie_agrupada_regioes, aes(DATA, CONFIRMADOS, color = REGIAO))+
	geom_line(size = 1.5)


ggplot(serie_agrupada_regioes, aes(DATA, TX_CONFIRMADOS_SUSPEITOS, color = REGIAO))+
	geom_line(size = 1.5)

serie_agrupada_regioes$SENSIBILIDADE <- ifelse(serie_agrupada_regioes$TX_CONFIRMADOS_SUSPEITOS <= 6, "Moderado",
					ifelse(serie_agrupada_regioes$TX_CONFIRMADOS_SUSPEITOS <= 9, "Alto",
					       ifelse(serie_agrupada_regioes$TX_CONFIRMADOS_SUSPEITOS <= 12, "Grave", "Gravíssimo")))

serie_agrupada_regioes_list <- list()
for(i in 1:16){ # são 16 regiões
	serie_agrupada_regioes_cort <- subset(serie_agrupada_regioes, as.numeric(as.factor(serie_agrupada_regioes$REGIAO)) == i)
	serie_agrupada_regioes_cort <- tail(serie_agrupada_regioes_cort,1)
	serie_agrupada_regioes_list[[i]] <- serie_agrupada_regioes_cort
}

serie_agrupada_regioes <- do.call(rbind, serie_agrupada_regioes_list) %>% as.data.frame()

write_xlsx(serie_agrupada_regioes,"base/SENSIBILIDADE.xlsx")


#Efeito de desenho do inquérito de síndrome gripal
efeito_desenho <- read_excel("base/efeito_desenho.xlsx")

efeito_desenho$VIGILANCIA_ATIVA <- ifelse(efeito_desenho$EFEITO_DESENHO <= 1, "Moderado",
					ifelse(efeito_desenho$EFEITO_DESENHO <= 1.5, "Alto",
					       ifelse(efeito_desenho$EFEITO_DESENHO <= 2, "Grave", 
					              ifelse(efeito_desenho$EFEITO_DESENHO > 2 |
					                     efeito_desenho$EFEITO_DESENHO == "NÃO REALIZADO", "Gravíssimo", NA))))


write_xlsx(efeito_desenho,"base/VIGILANCIA_ATIVA.xlsx")


#Taxa de ocupação de leitos
ocupacao_uti <- read_csv("base/ocupacao_uti.csv")
ocupacao_uti <- subset(ocupacao_uti, ocupacao_uti$classificacao == "uti")
ocupacao_uti$macrorregiao <- NULL
ocupacao_uti$classificacao <- NULL
names(ocupacao_uti) <- c("REGIAO", "TX_OCUPACAO_UTI")

ocupacao_uti$CAPACIDADE_UTI <- ifelse(ocupacao_uti$TX_OCUPACAO_UTI <= 50, "Moderado",
					ifelse(ocupacao_uti$TX_OCUPACAO_UTI <= 70, "Alto",
					       ifelse(ocupacao_uti$TX_OCUPACAO_UTI <= 90, "Grave", "Gravíssimo")))




# Análise dos indicadores ---------------------------------------------------
situacao_indicadores <- obitos_canal_endemico
situacao_indicadores <- merge(situacao_indicadores, res_base_14_dias, by = c("REGIAO"), all = T)
situacao_indicadores <- merge(situacao_indicadores, casos_ativos_populacao, by = c("REGIAO"), all = T)
situacao_indicadores <- merge(situacao_indicadores, serie_agrupada_regioes, by = c("REGIAO"), all = T)
situacao_indicadores <- merge(situacao_indicadores, efeito_desenho, by = c("REGIAO"), all = T)
situacao_indicadores <- merge(situacao_indicadores, ocupacao_uti, by = c("REGIAO"), all = T)



situacao_indicadores <- situacao_indicadores %>% select(REGIAO, GRAVIDADE, CRESCIMENTO, INFECTIVIDADE, SENSIBILIDADE, VIGILANCIA_ATIVA, CAPACIDADE_UTI)
write_xlsx(situacao_indicadores,"base/situacao_indicadores.xlsx")

pontuacao_indicadores <- situacao_indicadores

pontuacao_indicadores[pontuacao_indicadores$GRAVIDADE == "Moderado", names(pontuacao_indicadores) == "GRAVIDADE"] <- 5 
pontuacao_indicadores[pontuacao_indicadores$GRAVIDADE == "Alto", names(pontuacao_indicadores) == "GRAVIDADE"] <- 15 
pontuacao_indicadores[pontuacao_indicadores$GRAVIDADE == "Grave", names(pontuacao_indicadores) == "GRAVIDADE"] <- 30 
pontuacao_indicadores[pontuacao_indicadores$GRAVIDADE == "Gravíssimo", names(pontuacao_indicadores) == "GRAVIDADE"] <- 50 

pontuacao_indicadores[pontuacao_indicadores$CRESCIMENTO == "Moderado", names(pontuacao_indicadores) == "CRESCIMENTO"] <- 5 
pontuacao_indicadores[pontuacao_indicadores$CRESCIMENTO == "Alto", names(pontuacao_indicadores) == "CRESCIMENTO"] <- 15
pontuacao_indicadores[pontuacao_indicadores$CRESCIMENTO == "Grave", names(pontuacao_indicadores) == "CRESCIMENTO"] <- 30 
pontuacao_indicadores[pontuacao_indicadores$CRESCIMENTO == "Gravíssimo", names(pontuacao_indicadores) == "CRESCIMENTO"] <- 50 

pontuacao_indicadores[pontuacao_indicadores$INFECTIVIDADE == "Moderado", names(pontuacao_indicadores) == "INFECTIVIDADE"] <- 5 
pontuacao_indicadores[pontuacao_indicadores$INFECTIVIDADE == "Alto", names(pontuacao_indicadores) == "INFECTIVIDADE"] <- 15 
pontuacao_indicadores[pontuacao_indicadores$INFECTIVIDADE == "Grave", names(pontuacao_indicadores) == "INFECTIVIDADE"] <- 30 
pontuacao_indicadores[pontuacao_indicadores$INFECTIVIDADE == "Gravíssimo", names(pontuacao_indicadores) == "INFECTIVIDADE"] <- 50 

pontuacao_indicadores[pontuacao_indicadores$SENSIBILIDADE == "Moderado", names(pontuacao_indicadores) == "SENSIBILIDADE"] <- 5 
pontuacao_indicadores[pontuacao_indicadores$SENSIBILIDADE == "Alto", names(pontuacao_indicadores) == "SENSIBILIDADE"] <- 15 
pontuacao_indicadores[pontuacao_indicadores$SENSIBILIDADE == "Grave", names(pontuacao_indicadores) == "SENSIBILIDADE"] <- 30 
pontuacao_indicadores[pontuacao_indicadores$SENSIBILIDADE == "Gravíssimo", names(pontuacao_indicadores) == "SENSIBILIDADE"] <- 50 

pontuacao_indicadores[pontuacao_indicadores$VIGILANCIA_ATIVA == "Moderado", names(pontuacao_indicadores) == "VIGILANCIA_ATIVA"] <- 5 
pontuacao_indicadores[pontuacao_indicadores$VIGILANCIA_ATIVA == "Alto", names(pontuacao_indicadores) == "VIGILANCIA_ATIVA"] <- 15 
pontuacao_indicadores[pontuacao_indicadores$VIGILANCIA_ATIVA == "Grave", names(pontuacao_indicadores) == "VIGILANCIA_ATIVA"] <- 30 
pontuacao_indicadores[pontuacao_indicadores$VIGILANCIA_ATIVA == "Gravíssimo", names(pontuacao_indicadores) == "VIGILANCIA_ATIVA"] <- 50

pontuacao_indicadores[pontuacao_indicadores$CAPACIDADE_UTI == "Moderado", names(pontuacao_indicadores) == "CAPACIDADE_UTI"] <- 5
pontuacao_indicadores[pontuacao_indicadores$CAPACIDADE_UTI == "Alto", names(pontuacao_indicadores) == "CAPACIDADE_UTI"] <- 15 
pontuacao_indicadores[pontuacao_indicadores$CAPACIDADE_UTI == "Grave", names(pontuacao_indicadores) == "CAPACIDADE_UTI"] <- 30 
pontuacao_indicadores[pontuacao_indicadores$CAPACIDADE_UTI == "Gravíssimo", names(pontuacao_indicadores) == "CAPACIDADE_UTI"] <- 50 


# Análise das dimensões ---------------------------------------------------
pontuacao_dimensoes <- pontuacao_indicadores
pontuacao_dimensoes[,-1] <- lapply(pontuacao_dimensoes[,-1], as.numeric) %>% as.data.frame() 
pontuacao_dimensoes$EVENTO_SENTINELA <- pontuacao_dimensoes$GRAVIDADE
pontuacao_dimensoes$TRANSMISSIBILIDADE <- (pontuacao_dimensoes$CRESCIMENTO + pontuacao_dimensoes$INFECTIVIDADE)/2
pontuacao_dimensoes$MONITORAMENTO <- (pontuacao_dimensoes$SENSIBILIDADE + pontuacao_dimensoes$VIGILANCIA_ATIVA)/2
pontuacao_dimensoes$CAPACIDADE_DE_ATENCAO <- pontuacao_dimensoes$CAPACIDADE_UTI
pontuacao_dimensoes <- pontuacao_dimensoes %>% dplyr::select(REGIAO, EVENTO_SENTINELA, TRANSMISSIBILIDADE,
							     MONITORAMENTO, CAPACIDADE_DE_ATENCAO)


situacao_dimensoes <- pontuacao_dimensoes
situacao_dimensoes[situacao_dimensoes > 30] <- "Gravíssimo"
situacao_dimensoes[situacao_dimensoes <= 30 &
		   	situacao_dimensoes > 15] <- "Grave"
situacao_dimensoes[situacao_dimensoes <= 15 &
		   	situacao_dimensoes > 5] <- "Alto"
situacao_dimensoes[situacao_dimensoes <= 5] <- "Moderado"

# Análise do indicador sintético ---------------------------------------------------
pontuacao_sintetica <- pontuacao_dimensoes 
pontuacao_sintetica$INDICADOR_SINTETICO <- (pontuacao_sintetica$EVENTO_SENTINELA +
	pontuacao_sintetica$TRANSMISSIBILIDADE +
	pontuacao_sintetica$MONITORAMENTO +
	pontuacao_sintetica$CAPACIDADE_DE_ATENCAO)/4

pontuacao_sintetica <- pontuacao_sintetica %>% dplyr::select(REGIAO, INDICADOR_SINTETICO)

situacao_sintetica <- pontuacao_sintetica
situacao_sintetica[situacao_sintetica > 30] <- "Gravíssimo"
situacao_sintetica[situacao_sintetica <= 30 &
		   	situacao_sintetica > 15] <- "Grave"
situacao_sintetica[situacao_sintetica <= 15 &
		   	situacao_sintetica > 5] <- "Alto"
situacao_sintetica[situacao_sintetica <= 5] <- "Moderado"

write_xlsx(situacao_indicadores,"base/situacao_indicadores.xlsx")
write_xlsx(pontuacao_indicadores,"base/pontuacao_indicadores.xlsx")
write_xlsx(situacao_dimensoes,"base/situacao_dimensoes.xlsx")
write_xlsx(pontuacao_dimensoes,"base/pontuacao_dimensoes.xlsx")
write_xlsx(situacao_sintetica,"base/situacao_sintetica.xlsx")
write_xlsx(pontuacao_sintetica,"base/pontuacao_sintetica.xlsx")