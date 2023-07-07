# Author: Cayan e Kimura
# Link drive: https://drive.google.com/file/d/1_zBca4oj2Yj4WYVCpnZDqDc_xrNywR6a/view?usp=drive_link

# Carregando pacotes (script original)
library('hpiR')
library('readr')
library('tidyverse')
library('forcats')
library('janitor')
library('rpart')
library('rpart.plot')
library('dplyr')
library('magrittr')
library('gridExtra')
library('knitr')
library('kableExtra')
library('metan')
library('corrplot')
library('highcharter')
library('ggthemes')
library('zoo')
library('gridExtra')
library('reshape2')
library('scales')



# Pacotes (script ajuste) ------------------------------------------------------
library(stringr)
library(dplyr)
library(readr)
library(janitor)
library(purrr)
library(tidyr)

# Data frame regiao ------------------------------------------------------------
setor_fisc <- c(99, 91, 82, 81, 80, 78, 75, 74, 73, 72, 71, 69, 68, 67, 66, 65,
                60, 59, 58, 57, 56,	55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45,
                44, 43, 42, 41, 40, 36, 35, 34, 33, 32, 31, 30, 29, 27, 26, 25,
                24, 23, 22, 21, 20, 16, 14, 13, 12, 11, 10, 0)
regiao <- c("Oeste", "Leste", "Leste", "Sudeste", "Leste", "Leste", "Leste",
            "Sul", "Leste", "Sul", "Sudeste", "Sul", "Leste", "Sul", "Sul",
            "Centro", "Sul", "Leste", "Sul", "Sul", "Leste", "Oeste", "Leste",
            "Leste", "Leste", "Leste", "Leste", "Oeste", "Sul", "Sul", "Sul",
            "Sul", "Centro", "Centro", "Centro", "Centro", "Oeste", "Sudeste",
            "Sudeste", "Sudeste", "Leste", "Centro", "Centro", "Centro", "Oeste",
            "Norte", "Norte", "Norte", "Norte", "Norte", "Norte", "Norte", "Norte",
            "Norte", "Centro", "Centro", "Centro", "Centro", "Centro", "Sudeste")

regioes <- data.frame(
    setor_fisc = as.character(setor_fisc),
    regiao = regiao
    )


# Pasta local com dados (alterar quando necessario)
pasta <- "/home/cayan/Documentos/Trabalhos/mercado_imob/dados"
arquivos <- list.files(pasta)
arquivos_s <- arquivos[str_detect(arquivos, "^s([0-9])")]

# Lista para cada arquivo carregado (s1 ... s99)
list_df <- list()
for (i in 1:length(arquivos_s)) {

list_df[[i]] <- read_delim(
    paste0(pasta, "/", arquivos_s[i]),
    delim = ";", escape_double = FALSE,
    col_types = cols(
            Inscrição = col_character(),
            `Área Total Construída` = col_number()
            ),
    locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE
    ) %>%
    janitor::clean_names() %>%
    select(inscricao, cep, area_total_construida, area_terreno, tipo_edificacao)
}

# Data frame empilhado (mesma dimensao do df2 original)
df2 <- do.call(rbind, list_df)

# Cadastro Imobiliario ---------------------------------------------------------
cadastro_imobiliario <- df2 %>%
    mutate(
        inscri = substring(inscricao, 1, nchar(inscricao)-5),
        cep5 = substr(cep, 1, 5),
        setor_fisc = substring(inscricao, 1, 2),
        tipo_imove = tipo_edificacao
    ) %>%
    left_join(regioes, by = c("setor_fisc"))


cadastro_imobiliario_planilha <- cadastro_imobiliario %>%
    filter(tipo_imove %in% c('10 - Casa', '20 - Aparatamento'))


# Dados de vendas --------------------------------------------------------------
unico_2022 <- read_delim(
    paste0(pasta, "/2022_reduzido.csv"),
    delim = ";", escape_double = FALSE,
    col_types = cols(DATA_TRANSACAO = col_date(format = "%d/%m/%Y"),
    VALOR_INSTRUMENTO = col_number(),
    INSCRICAO = col_character()),
    locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"),
    trim_ws = TRUE
    ) %>%
    janitor::clean_names()

# Camadas de lotes -------------------------------------------------------------
csv_lotes <- read_csv(paste0(pasta, "/csv_lotes_1.csv")) %>%
    janitor::clean_names() %>%
    mutate(inscri=substring(rotulo,1,nchar(rotulo)-5)) 

# Excluindo as incrições com mais de um endereço
csv_lotes2 <- csv_lotes[!duplicated(csv_lotes$inscri),]


# verificar objetivo em cada um
strings_interesse <- c(
    "FRA", "\\*GU", "1\\) F", "1\\)F", "GUIA COMP",
    "1/3", "1/2", "2/3", "1/96", "50", "68", "90",
    "1\\) PR", "COMPRA DE 5", "CONTRIBUINTE NÃO", 
    "CORRESPONDE AO IM", "CORRESPONDENDO A FRA",
    "DIFERENÇA DE ITBI", "DIV",
    "ESTA GUIA CORRESPONDE AO USUFR",
    "ESTA GUIA DE ITBI CORRESPONDE A PARTE IDEAL DE 5",
    "GUIA DE ITBI CORRESPONDENTE A 2",
    "GUIA DE ITBI CORRESPONDENTE A C",
    "GUIA DE ITBI DE PARTE IDEAL DE 5",
    "GUIA DE ITBI PARCIAL 5",
    "GUIA DE ITBI REF. 5", "GUIA DE ITBI REF. 1", "GUIA DE ITBI REF. 2")

detectar <- paste(strings_interesse, collapse = "|")

base2 <- unico_2022 %>%
    select(-fracao_ideal) %>%
    filter( is.na(obs) | str_detect(obs, detectar, negate = TRUE) )

# verificar objetivo dos filtros, pois sao muitas strings
strings_interesse_02 <- c(
    "BASE DE CÁLCULO AR", "BASE DE CÁLCULO = 2", "CESSÃO D")

#Base1 <- Base2 %>%
#    filter(is.na(obs)|substring(obs,1,18)!='BASE DE CÁLCULO AR')
#     %>% filter(is.na(obs)|substring(obs,1,19)!='BASE DE CÁLCULO = 2')
#      %>% filter(is.na(obs)|substring(obs,1,7)!='CESSÃO D')  %>%
#      filter(is.na(obs)|substring(obs,1,25)!='A TRANSACAO SE REFERE A 5')  %>%
#      filter(is.na(obs)|substring(obs,1,17)!='GUIA DE ITBI COMP')  %>%
#      filter(is.na(obs)|substring(obs,1,21)!='GUIA REFERENTE A CESS')  %>%
#      filter(is.na(obs)|substring(obs,1,5)!='TESTE') %>%
#      filter(is.na(obs)|substring(obs,1,11)!='PARTE IDEAL') %>%
#      filter(is.na(obs)|substring(obs,1,20)!='VENDA DE PARTE IDEAL') %>%
#      filter(is.na(obs)|substring(obs,1,11)!='VALOR DA CE') %>%
#      filter(is.na(obs)|substring(obs,1,4)!='1)FR') %>%
#      filter(is.na(obs)|substring(obs,1,12)!='RELATIVO A 1') %>%
#      filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 1') %>%
#      filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 2') %>%
#      filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 5') %>%
#      filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISICAO DE 5') %>%
#      filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE A CESS')%>%
#      filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE A DIFE')%>%
#      filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE A META') %>%
#      filter(is.na(obs)|substring(obs,1,9)!='ITBI CESS') %>%
#      filter(is.na(obs)|substring(obs,1,11)!='ADJUDICAÇÃO')
#      %>% filter(is.na(obs)|substring(obs,1,11)!='ADJUDICACAO') %>%
#      filter(is.na(obs)|substring(obs,1,11)!='AUTO DE ADJ') %>%
#      filter(is.na(obs)|substring(obs,1,12)!='CARTA DE ADJ')  %>%
#      filter(is.na(obs)|substring(obs,1,12)!='CESSÃO DE DI')  %>%
#      filter(is.na(obs)|substring(obs,1,12)!='CESSAO DE DI') 
#
#Base01 <- Base1 %>%
#    filter(is.na(obs)|substring(obs,1,12)!='ITBI DE CESS') %>%
#    filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE À AQUISIÇÃO DE 5') %>%
#    filter(is.na(obs)|substring(obs,1,23)!='LANÇAMENTO RELATIVO À 5') %>%
#    filter(is.na(obs)|substring(obs,1,26)!='REFERENTE AO EXCESSO DE ME') %>%
#    filter(is.na(obs)|substring(obs,1,12)!='RELATIVO A 1')  %>%
#    filter(is.na(obs)|substring(obs,1,11)!='REFERENTE 3') %>%
#    filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 9') %>%
#    filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 1')%>%
#    filter(is.na(obs)|substring(obs,1,10)!='""GUIA COM') %>%
#    filter(is.na(obs)|substring(obs,1,20)!='""GUIA REFERENTE A C') %>%
#    filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 2')%>%
#    filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 4')%>%
#    filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 5')%>%
#    filter(is.na(obs)|substring(obs,1,17)!='CARTA ADJUDICACAO')%>%
#    filter(is.na(obs)|substring(obs,1,11)!='CESSAO DE 5')%>%
#    filter(is.na(obs)|substring(obs,1,11)!='CESSÃO DE 5')%>% filter(is.na(obs)|substring(obs,1,11)!='CESSÃO DE 1')%>% filter(is.na(obs)|substring(obs,1,11)!='CESSÃO DE 4')%>% filter(is.na(obs)|substring(obs,1,11)!='CESSÃO DE 2')%>% filter(is.na(obs)|substring(obs,1,15)!='CESSÃO DE PARTE')%>% filter(is.na(obs)|substring(obs,1,15)!='CESSAO DE PARTE') %>% filter(is.na(obs)|substring(obs,1,19)!='VENDA REFERENTE A 5') %>% filter(is.na(obs)|substring(obs,1,19)!='VENDA REFERENTE A 6') %>% filter(is.na(obs)|substring(obs,1,18)!='VENDA DE SOMENTE 5') %>% filter(is.na(obs)|substring(obs,1,12)!='VENDA DE PAR') %>% filter(is.na(obs)|substring(obs,1,12)!='VENDA DE MET') %>% filter(is.na(obs)|substring(obs,1,11)!='VENDA DE 50') %>% filter(is.na(obs)|substring(obs,1,10)!='VENDA DE 2')  %>% filter(is.na(obs)|substring(obs,1,12)!='VENDA DA MET')
#
#Base02 <- Base01 %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A PA')%>% filter(is.na(obs)|substring(obs,1,26)!='GUIA DE ITBI REFERENTE A M')%>% filter(is.na(obs)|substring(obs,1,11)!='GUIA REF. 5')%>% filter(is.na(obs)|substring(obs,1,18)!='GUIA REFERENTE A 5') %>% filter(is.na(obs)|substring(obs,1,14)!='HOUVE DESDOBRO') %>% filter(is.na(obs)|substring(obs,1,14)!='INTEGRALIZAÇÃO COR') %>% filter(is.na(obs)|substring(obs,1,19)!='INTEGRALIZAÇÃO DE 1') %>% filter(is.na(obs)|substring(obs,1,19)!='INTEGRALIZAÇÃO DE 3') %>% filter(is.na(obs)|substring(obs,1,19)!='INTEGRALIZAÇÃO DE 4') %>% filter(is.na(obs)|substring(obs,1,19)!='INTEGRALIZAÇÃO DE 5') %>% filter(is.na(obs)|substring(obs,1,9)!='ITBI COMP') %>% filter(is.na(obs)|substring(obs,1,38)!='ITBI CORRESPONDENTE A PARTE IDEAL DE 5') %>% filter(is.na(obs)|substring(obs,1,24)!='ITBI CORRESPONDENTE A ME') %>% filter(is.na(obs)|substring(obs,1,12)!='ITBI REF.A 5') %>% filter(is.na(obs)|substring(obs,1,13)!='ITBI REF.A (1') %>% filter(is.na(obs)|substring(obs,1,13)!='ITBI REF.A ME') %>% filter(is.na(obs)|substring(obs,1,13)!='ITBI REF.A PA')%>% filter(is.na(obs)|substring(obs,1,13)!='ITBI REF.A FR') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE  A PAR') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE  A MET') %>% filter(is.na(obs)|substring(obs,1,25)!='ITBI REFERENTE  A UMA PAR') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE  A FRA')  %>% filter(is.na(obs)|substring(obs,1,25)!='VALOR VENAL REFERENTE A 5')
#
#Base03 <- Base02 %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 3') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 4') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 5') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 6') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 7') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 8') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 2') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 3') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 4') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 5') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 7') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 8') %>% filter(is.na(obs)|substring(obs,1,30)!='ITBI REFERENTE A CARTA DE ADJU') %>% filter(is.na(obs)|substring(obs,1,28)!='ITBI REFERENTE A COMPRA DE 1') %>% filter(is.na(obs)|substring(obs,1,28)!='ITBI REFERENTE A COMPRA DE 4') %>% filter(is.na(obs)|substring(obs,1,28)!='ITBI REFERENTE A COMPRA DE 5') %>% filter(is.na(obs)|substring(obs,1,20)!='ITBI REFERENTE A FRA') %>% filter(is.na(obs)|substring(obs,1,25)!='ITBI REFERENTE A GUIA COM') %>% filter(is.na(obs)|substring(obs,1,20)!='ITBI REFERENTE A INT') %>% filter(is.na(obs)|substring(obs,1,20)!='ITBI REFERENTE A PAR') %>% filter(is.na(obs)|substring(obs,1,29)!='ITBI REFERENTE A PERMUTA DE 1') %>% filter(is.na(obs)|substring(obs,1,29)!='ITBI REFERENTE A PERMUTA DE 2') %>% filter(is.na(obs)|substring(obs,1,29)!='ITBI REFERENTE A PERMUTA DE 5')%>% filter(is.na(obs)|substring(obs,1,35)!='ITBI REFERENTE A PERMUTA DE UMA PAR') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE A TERÇ') %>% filter(is.na(obs)|substring(obs,1,21)!='ITIBI REFERENTE A PAR') %>% filter(is.na(obs)|substring(obs,1,11)!='VENDA DE 1/')
#
#Base04 <- Base03  %>% filter(is.na(obs)|substring(obs,1,31)!='VALOR DO INSTRUMENTO ATUALIZADO')  %>% filter(is.na(obs)|substring(obs,1,31)!='Valor do instrumento atualizado')  %>% filter(is.na(obs)|substring(obs,1,29)!='VALOR DA TRANSACAO ATUALIZADO')  %>% filter(is.na(obs)|substring(obs,1,29)!='VALOR DA TRANSAÇÃO ATUALIZADO')  %>% filter(is.na(obs)|substring(obs,1,15)!='UMA PARTE IDEAL')  %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AUISIÇÃO DE 50')  %>% filter(is.na(obs)|substring(obs,1,33)!='- Valor do instrumento atualizado')  %>% filter(is.na(obs)|substring(obs,1,33)!='- VALOR DO INSTRUMENTO ATUALIZADO') %>% filter(is.na(obs)|substring(obs,1,32)!='* CESSAO DE COMPROMISSO DE PARTE') %>% filter(is.na(obs)|substring(obs,1,33)!='1) O VALOR VENAL CORRETO DA PARTE') %>% filter(is.na(obs)|substring(obs,1,43)!='VENDEDORES: LUIZ ALVES RAMOS E OUTROS DE PA') %>% filter(is.na(obs)|substring(obs,1,17)!='REFERENTE A PARTE') %>% filter(is.na(obs)|substring(obs,1,25)!='GUIA DE ITBI REF. A PARTE') %>% filter(is.na(obs)|substring(obs,1,34)!='GUIA DE ITBI REFERENTE A UMA PARTE') %>% filter(is.na(obs)|substring(obs,1,39)!='GUIA DE ITBI REFERENTE A VENDA DE PARTE')
#
#Base05 <- Base04 %>% filter(is.na(obs)|substring(obs,1,20)!='CESSÃO REFERENTE A 5')%>% filter(is.na(obs)|substring(obs,1,20)!='CESSAO REFERENTE A 5')%>% filter(is.na(obs)|substring(obs,1,24)!='CESSÃO REFERENTE A PARTE')%>% filter(is.na(obs)|substring(obs,1,24)!='CESSAO REFERENTE À PARTE')%>% filter(is.na(obs)|substring(obs,1,9)!='COMPLENTO')%>% filter(is.na(obs)|substring(obs,1,5)!='COMPL') %>% filter(is.na(obs)|substring(obs,1,16)!='REFERENTE A CESS')%>% filter(is.na(obs)|substring(obs,1,6)!='METADE')%>% filter(is.na(obs)|substring(obs,1,46)!='ITBI REFERENTE PARTE IDEAL CORRESPONDENTE A 50') %>% filter(is.na(obs)|substring(obs,1,19)!='GUIA DE ITBI REF. P')%>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE À 1/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE a 1/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE À 2/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE a 2/') %>% filter(is.na(obs)|substring(obs,1,26)!='GUIA DE ITBI REFERENTE A 3') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A 4/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A 50') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A 6/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A 75') %>% filter(is.na(obs)|substring(obs,1,36)!='GUIA DE ITBI REFERENTE A CESSAO DE 5') %>% filter(substring(situacao,1,4)!='Subs') %>% filter(substring(situacao,1,4)!='Canc')  %>% filter(is.na(obs)|substring(obs,1,11) !='COMPLEMENTO') %>% filter(valor_instrumento > 1000)

