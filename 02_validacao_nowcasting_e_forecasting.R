rm(list = ls())
gc()

if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('scales')) {install.packages('scales')};library('scales')
if(!require ('Metrics')) {install.packages('Metrics')};library('Metrics')
if(!require ('scoringutils')) {install.packages('scoringutils')};library('scoringutils')
if(!require ('readxl')) {install.packages('readxl')};library('readxl')
if(!require ('lubridate')) {install.packages('lubridate')};library('lubridate')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')


###############################################
################### Dados #####################
###############################################
###baixando a base consolidada
dir<-setwd("X:/CCD/CVE/RESPIRATORIAS")

##Base cir
cir<-read_excel("05_SRAG_SISTEMA_DE_ALERTA/00_dados_auxiliares/DRS_GVS_CEREST_RRAS_CIR_GVE_MUNICIPIO_MATRIZ.xls")
cir<- cir |> select(Cod6, RS_CIR, COD_CIR,ID_MUNICIP)|>
  rename(CO_MUN_RES=Cod6,
         cod_cir=COD_CIR)

#### dados de população para calcular a incidência
oname <- load("X:/CCD/CVE/RESPIRATORIAS/13_PROJ_SARAMPO/4_bases/pop_sarampo_novo.RData")
dfpop <- get(oname);rm(list=oname);gc()
dfpop <- dfpop %>%
    filter(Ano==2023) %>%
    group_by(ID_MUN) %>%
    summarise(pop = sum(POP)) %>%
    inner_join(.,cir,by=c("ID_MUN"="ID_MUNICIP")) %>%
    group_by(RS_CIR) %>%
    summarise(pop = sum(pop)) %>%
    rename(rs_cir=RS_CIR)

##Base bruta do dia: consolidado em relação às bases usadas nas analises
load("X:/CCD/CVE/RESPIRATORIAS/01_SRAG/4_bases/boletim.RData")
#dados<-vroom("C:/Users/tzenker/Documents/EXTRACAO_ROBO_SIVEP_FULL_12_08_2024_00_05_1149479.csv")
#names(boletim)<- tolower(names(boletim))
conso<- boletim %>% select(CO_MUN_RES, DT_SIN_PRI) %>%
                   left_join(cir, by="CO_MUN_RES")
names(conso)<- tolower(names(conso))
# rm(dado)

###############################################
##########  VALIDACAO #########################
###############################################

###Lendo as bases de forecasting
#dir<-setwd("G:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA/08_manuscritos/Artigo_ForScan/2_output/forecasting/sem_trim/")

#@ no código disponível em 01_forecasting_bases_antigas_cir.R não esá claro como esses objetos são construídos.
dir<-setwd("X:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA/08_manuscritos/Artigo_ForScan/2_output/forecasting")
temp_cir = list.files(pattern="ForScan")

##Pegando as bases que serão avaliadas
file.name<-as.list(temp_cir)

#setwd("G:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA/08_manuscritos/Artigo_ForScan/dados_teste/")
#i<-file.name[[1]]

###Baixando e tratando as bases
datalist_cir<-list()
#i<-file.name[[1]]
#rm(i)
for (i in file.name) {
  df_cir<-read.csv(i)
  df_cir$dt_event<-ymd(df_cir$date_event)
  #df_cir$base<-dmy(substr(i,13,22)) ##bases retrospectivas
  df_cir$base<-ymd(substr(i,11,18)) ##bases propsectivas
  datalist_cir[[i]]<-df_cir
}

##juntando tudo
fore_cir<- bind_rows(datalist_cir)

#fore_cir<-big_data_cir

##Calculando casos observados

##POR CIR
conso_total_cir <- conso %>% #rename(DRS="17DRS") %>%
  filter(dt_sin_pri>"2021-01-01") %>%
  mutate(cod_cir=as.integer(cod_cir)) %>%
  group_by(rs_cir, cod_cir, dt_sin_pri) %>%
  summarise(casos=n())

#####Calculando número de casos observados para cada semana de forecasting

conso_cir<- fore_cir %>%
  group_by(base, cod_cir) %>%
  summarise(data_max=max(dt_event)+6,
            data_min=min(dt_event)) %>%
  mutate(week_min=as.integer(format(data_min, "%w"))) %>%
  mutate(week_min=replace(week_min, week_min==0,7)) %>%
  full_join(conso_total_cir, by="cod_cir", relationship = "many-to-many") |>
  group_by(base,cod_cir, dt_sin_pri) |>   ##era DT_SIM_PRI
  filter(dt_sin_pri >= data_min & dt_sin_pri <= data_max)|>
  as.data.frame()

##definindo a semana epidemiológica de cada caso consolidado

dt_event <- Date()

for(j in 1:nrow(conso_cir)){
    dt_event[j] <- floor_date(conso_cir$dt_sin_pri[j],
                              'week',
                              week_start = conso_cir$week_min[j])
}

conso_cir2 <- conso_cir |>
  #ungroup() |>
  mutate(dt_event = dt_event) |>
  group_by(rs_cir, cod_cir, base, dt_event) |>
  summarise(casos=sum(casos, na.rm = TRUE))|>
  as.data.frame()|>
  select(base, dt_event, rs_cir, cod_cir, casos)

join_cir <- left_join(fore_cir,conso_cir2, by=c("base", "cod_cir", "dt_event")) %>%
  select(-rs_cir)

cod_cir<- cir %>% select(RS_CIR, cod_cir) %>%
  mutate(rs_cir=RS_CIR)%>%
  distinct(rs_cir, cod_cir)

join_cir2<- inner_join(join_cir, cod_cir, by="cod_cir")

##coloando 0 nas semanas que não tem casos registrados
join_cir2$casos[is.na(join_cir2$casos)]<-0

#######Fazendo tabela no modelo que ele calcula todos os scores
modelo_cir<-  join_cir2 %>% select(rs_cir, base, week, dt_event, Median, LI, LS, LIb, LSb, casos) %>%
  rename(true_value=casos,
         "0.5"=Median,
         "0.025"=LI,
         "0.975"=LS,
         "0.25"=LIb,
         "0.75"=LSb)

modelo_cir$sample<-rep(1:length(modelo_cir$rs_cir))

modelo_full_cir<-pivot_longer(data=modelo_cir, names_to="quantile", values_to = "prediction", cols = c(5:9))

modelo_full_cir$quantile<-as.numeric(modelo_full_cir$quantile)
modelo_full_cir$prediction<-as.numeric(modelo_full_cir$prediction)
# calculo da incidência para fazer com que os calculos sejam padronizadas
modelo_full_cir <- inner_join(modelo_full_cir,dfpop) %>%
    rename(observed = true_value,
           predicted = prediction) %>%
    mutate(observed = 100000 * observed/pop,
           predicted = 100000 * predicted/pop) %>%
    select(-pop)
modelo_full_cir <- as_forecast_quantile(
    modelo_full_cir,
    observed = "observed",
    predicted = "predicted",
    quantile = "quantile"
)

#str(modelo_full_cir)
# arrumando para o formato novo do pacote
lmetrics <- get_metrics(modelo_full_cir)
lmetrics[["interval_coverage_90"]] <- NULL
lmetrics[["interval_coverage_95"]] <- purrr::partial( interval_coverage, interval_range = 95 )
lmetrics <- lmetrics[names(lmetrics)[c(1:6,8,7)]]

teste_cir<-score(modelo_full_cir,metrics = lmetrics) %>%
  summarise_scores(by=c("sample"),  na.rm = TRUE)

#dados$sample<-rep(1:length(dados$DRS))
modelo_cir <- inner_join(modelo_cir,dfpop) %>%
    mutate(across(`0.5`:true_value,~.x*100000/pop)) %>%
    select(-pop)


valida_join_cir<-full_join(teste_cir,modelo_cir, by="sample")

################ CALCULO DAS MÉTRICAS ##################
###Pegando cobertura de 50% e 95%

valida_join_cir<- valida_join_cir %>%
  rename(casos=true_value,
         Median="0.5",
         LI="0.025",
         LS="0.975",
         LIb="0.25",
         LSb="0.75") %>%
  mutate(cov_50= case_when(casos>=LIb & casos<=LSb ~ 1,
                           TRUE ~ 0),
         cov_95= case_when(casos>=LI & casos<=LS ~ 1,
                           TRUE ~ 0))

#write.csv(valida_join_cir, "bases_antigas_forecasting_observados_indicadores_pop.csv")

setwd("X:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA/08_manuscritos/Artigo_ForScan")

# write.csv(valida_join_cir, "2_output/validacao/validaco_ForSCan_base_consolidada_02_10_2024_b.csv")
write.csv(valida_join_cir, "2_output/validacao/validaco_ForSCan_base_consolidada_02_10_2024.csv")


####Bases salvas no formato para o satscan

join_cir3<-join_cir2[,-1]

###Substituindo na por 0 nas semanas 8 à 10

join_cir3$observed[is.na(join_cir3$observed) & join_cir3$week<11] <- 0


nome.bases<-as.list(unique(valida_join_cir$base))


for(i in nome.bases){

sub_base<-join_cir3 %>%
          filter(base==i)

data<-i

write.csv(sub_base, paste0("2_output/forecasting/forecasting_com_dados_consolidados_",data,".csv"), row.names = FALSE)

}
