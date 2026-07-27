######## Análise auxiliar para o artigo ForScan ########
#@ Objetivo: avaliação de classificação baseado em diferentes metodos
library(tidyverse)
df_ad <- read_csv2("G:/CCD/CVE/RESPIRATORIAS/_EQUIPE/DaniloPereiraMori/artigos/artigo_tati_NFcastingSatScan/tab_tudo_RR.csv")
df_ad <- mutate(df_ad,classRR = ifelse(is.na(RR),0,1)) %>% #classRR==1: RR > 1; otherwise: RR <=1
    filter(id_key %in% c("RR_casos","RR_Mdn","RR_Mdn_mv","RR_obs")) %>%
    select(COD_CIR,data,id,classRR) %>%
    pivot_wider(names_from=id,values_from=classRR) %>%
    mutate(
        Mdn_c = case_when(
            casos == Mdn & casos == 1 ~ "VP",
            casos == Mdn & casos == 0 ~ "VN",
            casos != Mdn & casos == 1 ~ "FP",
            casos != Mdn & casos == 0 ~ "FN"),
        Mdn_mv_c = case_when(
            casos == Mdn_mv & casos == 1 ~ "VP",
            casos == Mdn_mv & casos == 0 ~ "VN",
            casos != Mdn_mv & casos == 1 ~ "FP",
            casos != Mdn_mv & casos == 0 ~ "FN"),
        obs_c = case_when(
            casos == obs & casos == 1 ~ "VP",
            casos == obs & casos == 0 ~ "VN",
            casos != obs & casos == 1 ~ "FP",
            casos != obs & casos == 0 ~ "FN")
        )
dfsumm <- df_ad %>%
    select(data,ends_with("_c")) %>%
    pivot_longer(ends_with("_c"),
                 names_to = "metodo",
                 values_to = "classe") %>%
    group_by(data) %>%
    count(metodo, classe) %>%
    pivot_wider(names_from = classe,
                values_from = n,
                values_fill = 0) %>%
    mutate(
        Sensibilidade = VP/(VP+FN),
        Especificidade = VN/(VN+FP),
        Precisao = VP/(VP+FP),
        VPN = VN/(VN+FN),
        Acuracia = (VP+VN)/(VP+VN+FP+FN),
        F1 = 2*VP/(2*VP+FP+FN),
        BalancedAccuracy = (Sensibilidade + Especificidade)/2,
        Youden = Sensibilidade + Especificidade - 1
    )


dfsumm %>%
    filter(if_any(c("Sensibilidade","Especificidade","Precisao",
                    "VPN","Acuracia","F1","BalancedAccuracy","Youden"), is.na)) %>%
    pull(metodo) %>% unique


