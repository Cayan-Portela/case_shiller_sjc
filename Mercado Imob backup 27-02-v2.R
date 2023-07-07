### Este é um  estudo de mercado imobiliário, e, quando falamos de mercados, estamos falando de trocas (compras e vendas) balizadas por preços, ou seja, há alguém demandando e alguém ofertando um produto. A quantidade que cada um estará disposto a ofertar ou demandar depende do preço. Grande parte do do esforço da "ciência econômica" se dá em se debruçar sobre o ponto em que as curvas de oferta e demanda se cruzam, pois elas definem as quantidades transacionadas e os preços praticados. Desta forma, percebe-se a necessidade de um índice de preços correto para o estudo da economia, pois ele indica o preço em que o equilíbrio do mercado foi se movendo ao longo do tempo. Há índices gerais de inflação (IPCA), bem como outros mais específicos, como da construção civil (INCC), mas este só indica a variação dos preços para se construir algo, ou seja, variação do custo do tijolo, mão de obra, etc. Estabelecer um índice de preços sobre os imóveis (já constituídos ou em construção) é mais complicado, principalmente por dois motivos: i) os bens transacionados não são os mesmos (os imóveis são muito diferentes entre si), ii) há um estoque constituído e nem todos são transacionados todos os anos. Para contornar estes problemas, Karl Case e Robert Shiller criaram um índice, que levou o nome deles, neste artigo: https://www.nber.org/system/files/working_papers/w2393/w2393.pdf 

### O índice trabalha com pares de vendas dos mesmos imóveis ao longo do tempo, ou seja, precisa que um imóvel tenha sido vendido ao menos duas vezes na base. Pegando os pares de vendas e a valorização de cada um deles entre os períodos, o índice é gerado, conformne detalhamento do artigo do link anterior.

### Neste script há todos os gráficos gerados para a apresentação, bem como as informações que foram utilizadas para a geração dos mapas. Estes, por sua vez, foram desenvolvidos no QGis, de forma mais manual, não há o envio automático das informações do R para o QGis. De qualquer forma, é bem tranquilo unir as tabelas com as informações espaciais e gerar os mapas, o maior trabalho é o script do R que está a seguir. Nos baseamos no pacote hpiR para gerar o índice Case-Shiller, https://cran.r-project.org/web/packages/hpiR/vignettes/introduction.html . Sugiro que abram o link, o autor constrói passo a passo o índice, explicando cada função, dá um panorama bem legal. Ele também possui um link que mostra a parte das equações utilizadas para gerar os índices.

### Enquanto economista, acredito que não consigo contribuir para o aprofundamento da teoria, com o desenvolvimento de novas equações, testes de confiabilidade, etc. A contribuição deste estudo está em desenvolver uma metodologia para captar a evolução dos preços no mercado imobiliário ao longo do tempo a partir da base de dados do ITBI. No Brasil, o índice Case-Shiller havia sido desenvolvido apenas uma vez, em parceria do IPEA com a UnB, segue o link do estudo: https://www.scielo.br/j/neco/a/Ft9vTrn5SMbBV55Fbj5tMLR/?format=pdf&lang=pt

### Até pela dificuldade em se gerar um Case-Shiller, bem como a dificuldade em se obter dados, utiliza-se muito o FIPE-ZAP, mas ele é péssimo, apenas faz médias daquilo que é anunciado no site. Aqui criamos uma metodologia para utilizar o índice mais aceito internacionalmente. Vou detalhando conforme as fases do script. Novamente, sugiro acompanhar com o link do pacote hpiR aberto.


### Carregando pacotes

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

### Regionalização por região. Aqui apenas está sendo carregada a base de dados que confronta o setor fiscal com a região geográfica da cidade, parar gerarmos (no futuro) o índice por regiões.

Regioes <- read.csv(text="
setor_fisc	regiao
99	Oeste
91	Leste
82	Leste
81	Sudeste
80	Leste
78	Leste
75	Leste
74	Sul
73	Leste
72	Sul
71	Sudeste
69	Sul
68	Leste
67	Sul
66	Sul
65	Centro
60	Sul
59	Leste
58	Sul
57	Sul
56	Leste
55	Oeste
54	Leste
53	Leste
52	Leste
51	Leste
50	Leste
49	Oeste
48	Sul
47	Sul
46	Sul
45	Sul
44	Centro
43	Centro
42	Centro
41	Centro
40	Oeste
36	Sudeste
35	Sudeste
34	Sudeste
33	Leste
32	Centro
31	Centro
30	Centro
29	Oeste
27	Norte
26	Norte
25	Norte
24	Norte
23	Norte
22	Norte
21	Norte
20	Norte
16	Norte
14	Centro
13	Centro
12	Centro
11	Centro
10	Centro
0	Sudeste
",sep="\t",colClasses = c("character","character"))

### Cadastro Imobiliário. O cadastro imobiliário da prefeitura é muito grande e não conseguimos baixar ele de uma vez. Precisamos baixar setor fiscal por setor fiscal e juntar aqui no R, pois o excel não tem capacidade para isso... Não irei encaminhar estas bases, pois acabamos utilizando elas mais para gerar os mapas, não as estatísticas. Há também questões de LGPD envolvidas com esta base, mas ela não é exatamente importante para o estudo.

s10 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s10.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)

s11 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s11.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)

s12 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s12.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s13 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s13.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s14 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s14.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s15 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s15.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s16 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s16.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s20 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s20.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s21 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s21.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s22 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s22.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s23 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s23.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s24 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s24.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s25 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s25.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s26 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s26.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE) %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s27 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s27.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s28 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s28.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s29 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s29.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)

s30 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s30.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s31 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s31.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s32 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s32.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s33 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s33.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s34 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s34.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s35 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s35.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s36 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s36.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s38 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s38.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s40 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s40.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s41 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s41.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s42 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s42.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s43 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s43.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s44 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s44.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s45 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s45.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s46 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s46.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s47 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s47.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s48 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s48.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s49 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s49.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s50 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s50.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s51 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s51.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s52 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s52.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s53 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s53.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s54 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s54.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s55 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s55.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s56 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s56.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s57 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s57.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s58 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s58.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s59 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s59.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s60 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s60.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s65 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s65.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s65 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s65.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s66 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s66.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s67 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s67.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s68 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s68.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE) %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s69 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s69.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s70 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s70.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s71 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s71.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s72 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s72.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s73 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s73.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE) %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s74 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s74.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s75 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s75.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s76 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s76.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE) %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s77 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s77.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s78 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s78.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE) %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s79 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s79.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s80 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s80.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s81 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s81.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s82 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s82.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)

s83 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s83.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s84 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s84.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s88 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s88.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s90 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s90.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s91 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s91.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)


s99 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/Cadastro_imobiliario/s99.csv", delim = ";", escape_double = FALSE, col_types = cols(Inscrição = col_character(), `Área Total Construída` = col_number()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)  %>% mutate (inscricao = `Inscrição`) %>% select(inscricao, CEP, `Área Total Construída`, `Área Terreno`, `Tipo Edificação`)

list_df = list(s10,s11,s12,s13,s14,s15,s16,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s38,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s65,s66,s67,s68,s69,s70,s71,s72,s72,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s88,s90,s91,s99)

df2 <- list_df %>% reduce(full_join, by= c("inscricao","CEP","Área Total Construída", "Área Terreno", "Tipo Edificação"))

Cadastro_imobiliario <- df2 %>% mutate(inscri=substring(inscricao,1,nchar(inscricao)-5)) %>% mutate(CEP5=substring(CEP,1,5))  %>% mutate(CEP5=substring(CEP,1,5)) %>% mutate(setor_fisc=substring(inscricao,1,2)) %>% mutate(TIPO_IMOVE = `Tipo Edificação` )

Cadastro_imobiliario <- left_join(Cadastro_imobiliario, Regioes, match = 'first', by = c('setor_fisc'))

Cadastro_imobiliario%>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

Cadastro_imobiliario_planilha <- Cadastro_imobiliario%>% filter((TIPO_IMOVE == '10 - Casa') | (TIPO_IMOVE == '20 - Apartamento'))

Cadastro_imobiliario_planilha %>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

### Carregando dados de vendas. Esta é a base do ITBI, está com o caminho da pasta no nosso computador. Aqui temos a inscrição imobiliária única de cada imóvel, a situação da guia (algumas são canceladas por ter valores errados, outras não foram pagas, em outras está tudo certo, etc.), observações (importante para retirarmos guias que não representam compras e vendas), valor pago de ITBI (uma fração do valor pago no imóvel, dependendo da quantidade que foi financiado, data, etc), valor venal do imóvel (que é abaixo do valor de mercado, e, justamente por isso, indica possíveis subnotificações caso o valor informado da transação pelo comprador seja menor que ele. No índice que usamos, não fizemos nenhuma restrição sobre o valor da venda ter que ser maior que o valor venal, mas em outros testes que fizemos, restringimos), valor do instrumento (que é o valor informado), valor financiado, e a data da transação (não é a data da geração da guia, pois há muita gente que demora alguns anos para registrar a compra/venda, esta é realmente a data da transação).

Unico_2022 <- read_delim("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/2022_reduzido.csv", delim = ";", escape_double = FALSE, col_types = cols(DATA_TRANSACAO = col_date(format = "%d/%m/%Y"), VALOR_INSTRUMENTO = col_number(), INSCRICAO = col_character()), locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)

### Carregando camada de lotes do nosso QGis, para complementar informações sobre os imóveis (se é apartamento, casa, etc, para gerarmos índices posteriores)

csv_lotes <- read_csv("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados/csv_lotes_1.csv") %>% mutate(inscri=substring(rotulo,1,nchar(rotulo)-5)) 

### Excluindo as incrições com mais de um endereço

csv_lotes2 <- csv_lotes[!duplicated(csv_lotes$inscri),]

### Excluindo guias que foram substituídas, canceladas, duplicadas, complementadas, adjudicações, partes ideais, frações ideais, cessõesm etc. Como a maior parte destas informações está na coluna de observação, há um técnico preenchendo livremente as informações, por isso tentei excluir por aqui todas as formas que encontrei destes tipos de guias que deveriam ser excluídas. Quando se colocavam muitos filtros na base, pelo menos aqui, travava o computador. Por isso tive que criar algumas bases e filtrá-las.

Base2 <- clean_names(Unico_2022[,c(-6)]) %>% mutate(ID=as.character(paste0(data_transacao,"...",inscricao))) %>% mutate(inscri=substring(inscricao,1,nchar(inscricao)-5)) %>% mutate(inscri2=substring(inscricao,14,17)) %>% filter(is.na(obs)|substring(obs,1,3)!='FRA') %>% filter(is.na(obs)|substring(obs,1,3)!='*GU') %>% filter(is.na(obs)|substring(obs,1,4)!='1) F') %>% filter(is.na(obs)|substring(obs,1,4)!='1)FR') %>% filter(is.na(obs)|substring(obs,1,9)!='GUIA COMP') %>% filter(is.na(obs)|substring(obs,1,3)!='1/3') %>% filter(is.na(obs)|substring(obs,1,3)!='1/2') %>% filter(is.na(obs)|substring(obs,1,3)!='2/3') %>% filter(is.na(obs)|substring(obs,1,4)!='1/96') %>% filter(is.na(obs)|substring(obs,1,2)!='50') %>% filter(is.na(obs)|substring(obs,1,2)!='68') %>% filter(is.na(obs)|substring(obs,1,2)!='90') %>% filter(is.na(obs)|substring(obs,1,4)!='1) PR') %>% filter(is.na(obs)|substring(obs,1,11)!='COMPRA DE 5') %>% filter(is.na(obs)|substring(obs,1,16)!='CONTRIBUINTE NÃO') %>% filter(is.na(obs)|substring(obs,1,18)!='CORRESPONDE AO IM') %>% filter(is.na(obs)|substring(obs,1,20)!='CORRESPONDENDO A FRA') %>% filter(is.na(obs)|substring(obs,1,17)!='DIFERENÇA DE ITBI') %>% filter(is.na(obs)|substring(obs,1,3)!='DIV') %>% filter(is.na(obs)|substring(obs,1,30)!='ESTA GUIA CORRESPONDE AO USUFR')%>% filter(is.na(obs)|substring(obs,1,46)!='ESTA GUIA DE ITBI CORRESPONDE A PARTE IDEAL DE 5') %>% filter(is.na(obs)|substring(obs,1,31)!='GUIA DE ITBI CORRESPONDENTE A 2') %>% filter(is.na(obs)|substring(obs,1,31)!='GUIA DE ITBI CORRESPONDENTE A C')%>% filter(is.na(obs)|substring(obs,1,32)!='GUIA DE ITBI DE PARTE IDEAL DE 5')%>% filter(is.na(obs)|substring(obs,1,22)!='GUIA DE ITBI PARCIAL 5')%>% filter(is.na(obs)|substring(obs,1,19)!='GUIA DE ITBI REF. 5')%>% filter(is.na(obs)|substring(obs,1,19)!='GUIA DE ITBI REF. 1')%>% filter(is.na(obs)|substring(obs,1,19)!='GUIA DE ITBI REF. 2')

Base1 <- Base2 %>% filter(is.na(obs)|substring(obs,1,18)!='BASE DE CÁLCULO AR')  %>% filter(is.na(obs)|substring(obs,1,19)!='BASE DE CÁLCULO = 2')  %>% filter(is.na(obs)|substring(obs,1,7)!='CESSÃO D')  %>% filter(is.na(obs)|substring(obs,1,25)!='A TRANSACAO SE REFERE A 5')  %>% filter(is.na(obs)|substring(obs,1,17)!='GUIA DE ITBI COMP')  %>% filter(is.na(obs)|substring(obs,1,21)!='GUIA REFERENTE A CESS')  %>% filter(is.na(obs)|substring(obs,1,5)!='TESTE') %>% filter(is.na(obs)|substring(obs,1,11)!='PARTE IDEAL') %>% filter(is.na(obs)|substring(obs,1,20)!='VENDA DE PARTE IDEAL') %>% filter(is.na(obs)|substring(obs,1,11)!='VALOR DA CE') %>% filter(is.na(obs)|substring(obs,1,4)!='1)FR') %>% filter(is.na(obs)|substring(obs,1,12)!='RELATIVO A 1') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 1') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 2') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 5') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISICAO DE 5') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE A CESS')%>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE A DIFE')%>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE A META') %>% filter(is.na(obs)|substring(obs,1,9)!='ITBI CESS')  %>% filter(is.na(obs)|substring(obs,1,11)!='ADJUDICAÇÃO')   %>% filter(is.na(obs)|substring(obs,1,11)!='ADJUDICACAO') %>% filter(is.na(obs)|substring(obs,1,11)!='AUTO DE ADJ') %>% filter(is.na(obs)|substring(obs,1,12)!='CARTA DE ADJ')  %>% filter(is.na(obs)|substring(obs,1,12)!='CESSÃO DE DI')  %>% filter(is.na(obs)|substring(obs,1,12)!='CESSAO DE DI') 

Base01 <- Base1 %>% filter(is.na(obs)|substring(obs,1,12)!='ITBI DE CESS') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE À AQUISIÇÃO DE 5') %>% filter(is.na(obs)|substring(obs,1,23)!='LANÇAMENTO RELATIVO À 5') %>% filter(is.na(obs)|substring(obs,1,26)!='REFERENTE AO EXCESSO DE ME') %>% filter(is.na(obs)|substring(obs,1,12)!='RELATIVO A 1')  %>% filter(is.na(obs)|substring(obs,1,11)!='REFERENTE 3') %>% filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 9') %>% filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 1')%>% filter(is.na(obs)|substring(obs,1,10)!='""GUIA COM')%>% filter(is.na(obs)|substring(obs,1,20)!='""GUIA REFERENTE A C')%>% filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 2')%>% filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 4')%>% filter(is.na(obs)|substring(obs,1,13)!='REFERENTE A 5')%>% filter(is.na(obs)|substring(obs,1,17)!='CARTA ADJUDICACAO')%>% filter(is.na(obs)|substring(obs,1,11)!='CESSAO DE 5')%>% filter(is.na(obs)|substring(obs,1,11)!='CESSÃO DE 5')%>% filter(is.na(obs)|substring(obs,1,11)!='CESSÃO DE 1')%>% filter(is.na(obs)|substring(obs,1,11)!='CESSÃO DE 4')%>% filter(is.na(obs)|substring(obs,1,11)!='CESSÃO DE 2')%>% filter(is.na(obs)|substring(obs,1,15)!='CESSÃO DE PARTE')%>% filter(is.na(obs)|substring(obs,1,15)!='CESSAO DE PARTE') %>% filter(is.na(obs)|substring(obs,1,19)!='VENDA REFERENTE A 5') %>% filter(is.na(obs)|substring(obs,1,19)!='VENDA REFERENTE A 6') %>% filter(is.na(obs)|substring(obs,1,18)!='VENDA DE SOMENTE 5') %>% filter(is.na(obs)|substring(obs,1,12)!='VENDA DE PAR') %>% filter(is.na(obs)|substring(obs,1,12)!='VENDA DE MET') %>% filter(is.na(obs)|substring(obs,1,11)!='VENDA DE 50') %>% filter(is.na(obs)|substring(obs,1,10)!='VENDA DE 2')  %>% filter(is.na(obs)|substring(obs,1,12)!='VENDA DA MET')

Base02 <- Base01 %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A PA')%>% filter(is.na(obs)|substring(obs,1,26)!='GUIA DE ITBI REFERENTE A M')%>% filter(is.na(obs)|substring(obs,1,11)!='GUIA REF. 5')%>% filter(is.na(obs)|substring(obs,1,18)!='GUIA REFERENTE A 5') %>% filter(is.na(obs)|substring(obs,1,14)!='HOUVE DESDOBRO') %>% filter(is.na(obs)|substring(obs,1,14)!='INTEGRALIZAÇÃO COR') %>% filter(is.na(obs)|substring(obs,1,19)!='INTEGRALIZAÇÃO DE 1') %>% filter(is.na(obs)|substring(obs,1,19)!='INTEGRALIZAÇÃO DE 3') %>% filter(is.na(obs)|substring(obs,1,19)!='INTEGRALIZAÇÃO DE 4') %>% filter(is.na(obs)|substring(obs,1,19)!='INTEGRALIZAÇÃO DE 5') %>% filter(is.na(obs)|substring(obs,1,9)!='ITBI COMP') %>% filter(is.na(obs)|substring(obs,1,38)!='ITBI CORRESPONDENTE A PARTE IDEAL DE 5') %>% filter(is.na(obs)|substring(obs,1,24)!='ITBI CORRESPONDENTE A ME') %>% filter(is.na(obs)|substring(obs,1,12)!='ITBI REF.A 5') %>% filter(is.na(obs)|substring(obs,1,13)!='ITBI REF.A (1') %>% filter(is.na(obs)|substring(obs,1,13)!='ITBI REF.A ME') %>% filter(is.na(obs)|substring(obs,1,13)!='ITBI REF.A PA')%>% filter(is.na(obs)|substring(obs,1,13)!='ITBI REF.A FR') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE  A PAR') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE  A MET') %>% filter(is.na(obs)|substring(obs,1,25)!='ITBI REFERENTE  A UMA PAR') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE  A FRA')  %>% filter(is.na(obs)|substring(obs,1,25)!='VALOR VENAL REFERENTE A 5')

Base03 <- Base02 %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 3') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 4') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 5') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 6') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 7') %>% filter(is.na(obs)|substring(obs,1,18)!='ITBI REFERENTE A 8') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 2') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 3') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 4') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 5') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 7') %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AQUISIÇÃO DE 8') %>% filter(is.na(obs)|substring(obs,1,30)!='ITBI REFERENTE A CARTA DE ADJU') %>% filter(is.na(obs)|substring(obs,1,28)!='ITBI REFERENTE A COMPRA DE 1') %>% filter(is.na(obs)|substring(obs,1,28)!='ITBI REFERENTE A COMPRA DE 4') %>% filter(is.na(obs)|substring(obs,1,28)!='ITBI REFERENTE A COMPRA DE 5') %>% filter(is.na(obs)|substring(obs,1,20)!='ITBI REFERENTE A FRA') %>% filter(is.na(obs)|substring(obs,1,25)!='ITBI REFERENTE A GUIA COM') %>% filter(is.na(obs)|substring(obs,1,20)!='ITBI REFERENTE A INT') %>% filter(is.na(obs)|substring(obs,1,20)!='ITBI REFERENTE A PAR') %>% filter(is.na(obs)|substring(obs,1,29)!='ITBI REFERENTE A PERMUTA DE 1') %>% filter(is.na(obs)|substring(obs,1,29)!='ITBI REFERENTE A PERMUTA DE 2') %>% filter(is.na(obs)|substring(obs,1,29)!='ITBI REFERENTE A PERMUTA DE 5')%>% filter(is.na(obs)|substring(obs,1,35)!='ITBI REFERENTE A PERMUTA DE UMA PAR') %>% filter(is.na(obs)|substring(obs,1,21)!='ITBI REFERENTE A TERÇ') %>% filter(is.na(obs)|substring(obs,1,21)!='ITIBI REFERENTE A PAR') %>% filter(is.na(obs)|substring(obs,1,11)!='VENDA DE 1/')

Base04 <- Base03  %>% filter(is.na(obs)|substring(obs,1,31)!='VALOR DO INSTRUMENTO ATUALIZADO')  %>% filter(is.na(obs)|substring(obs,1,31)!='Valor do instrumento atualizado')  %>% filter(is.na(obs)|substring(obs,1,29)!='VALOR DA TRANSACAO ATUALIZADO')  %>% filter(is.na(obs)|substring(obs,1,29)!='VALOR DA TRANSAÇÃO ATUALIZADO')  %>% filter(is.na(obs)|substring(obs,1,15)!='UMA PARTE IDEAL')  %>% filter(is.na(obs)|substring(obs,1,31)!='ITBI REFERENTE A AUISIÇÃO DE 50')  %>% filter(is.na(obs)|substring(obs,1,33)!='- Valor do instrumento atualizado')  %>% filter(is.na(obs)|substring(obs,1,33)!='- VALOR DO INSTRUMENTO ATUALIZADO') %>% filter(is.na(obs)|substring(obs,1,32)!='* CESSAO DE COMPROMISSO DE PARTE') %>% filter(is.na(obs)|substring(obs,1,33)!='1) O VALOR VENAL CORRETO DA PARTE') %>% filter(is.na(obs)|substring(obs,1,43)!='VENDEDORES: LUIZ ALVES RAMOS E OUTROS DE PA') %>% filter(is.na(obs)|substring(obs,1,17)!='REFERENTE A PARTE') %>% filter(is.na(obs)|substring(obs,1,25)!='GUIA DE ITBI REF. A PARTE') %>% filter(is.na(obs)|substring(obs,1,34)!='GUIA DE ITBI REFERENTE A UMA PARTE') %>% filter(is.na(obs)|substring(obs,1,39)!='GUIA DE ITBI REFERENTE A VENDA DE PARTE')

Base05 <- Base04 %>% filter(is.na(obs)|substring(obs,1,20)!='CESSÃO REFERENTE A 5')%>% filter(is.na(obs)|substring(obs,1,20)!='CESSAO REFERENTE A 5')%>% filter(is.na(obs)|substring(obs,1,24)!='CESSÃO REFERENTE A PARTE')%>% filter(is.na(obs)|substring(obs,1,24)!='CESSAO REFERENTE À PARTE')%>% filter(is.na(obs)|substring(obs,1,9)!='COMPLENTO')%>% filter(is.na(obs)|substring(obs,1,5)!='COMPL') %>% filter(is.na(obs)|substring(obs,1,16)!='REFERENTE A CESS')%>% filter(is.na(obs)|substring(obs,1,6)!='METADE')%>% filter(is.na(obs)|substring(obs,1,46)!='ITBI REFERENTE PARTE IDEAL CORRESPONDENTE A 50') %>% filter(is.na(obs)|substring(obs,1,19)!='GUIA DE ITBI REF. P')%>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE À 1/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE a 1/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE À 2/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE a 2/') %>% filter(is.na(obs)|substring(obs,1,26)!='GUIA DE ITBI REFERENTE A 3') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A 4/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A 50') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A 6/') %>% filter(is.na(obs)|substring(obs,1,27)!='GUIA DE ITBI REFERENTE A 75') %>% filter(is.na(obs)|substring(obs,1,36)!='GUIA DE ITBI REFERENTE A CESSAO DE 5') %>% filter(substring(situacao,1,4)!='Subs') %>% filter(substring(situacao,1,4)!='Canc')  %>% filter(is.na(obs)|substring(obs,1,11) !='COMPLEMENTO') %>% filter(valor_instrumento > 1000)

Baseteste <- Base05[!grepl('PARTE ',Base05$obs),]
Baseteste1 <- Baseteste[!grepl('PARTES I',Baseteste$obs),]
Baseteste2 <- Baseteste1[!grepl('FRACAO',Baseteste1$obs),]
Baseteste3 <- Baseteste2[!grepl('FRAÇÃO',Baseteste2$obs),]
Baseteste4 <- Baseteste3[!grepl('ATUALIZADO',Baseteste3$obs),]
Baseteste5 <- Baseteste4[!grepl('ATUALIZADA',Baseteste4$obs),]
Baseteste6 <- Baseteste5[!grepl('ADJUDICA',Baseteste5$obs),]
Base <- Baseteste6[!grepl('COMPLEM',Baseteste6$obs),]

### Juntando a base de lotes com a base de vendas pela coluna de inscrição sem os últimos dígitos (para unir também os apartamentos). Cada imóvel possui sua prórpia inscrição imobiliária (sua forma é: SETOR (2 dígitos) . QUADRA (4 dígitos) . LOTE (4 dígitos) . UNIDADE (4 dígitos) . Desta forma, cada unidade (apartamento) de um prédio possui inscrição única, mas todas estas inscrições estão em um mesmo lote da camada espacial. Por isso, para unir as informações, criamos uma coluna com as inscrições sem os últimos 4 dígitos, parando nos lotes, não nas unidades, assim, passando as informações dos lotes dos prédios para os apartamentos)). 

base_cs <- left_join(Base, csv_lotes2, match = 'first', by = c('inscri'))

### Removendo registros duplicados gerados com a união

base_cs <- base_cs[!duplicated(base_cs$ID),]

### Selecionando apenas apartamentos com inscrição única, excluindo apartamentos vendidos antes da separação das inscrições, não comparáveis entre si. Os apartamentos só ganham inscrição imobiliária individual despois do Habite-se, ou seja, enquanto estão na planta ou sendo vendidos durante a construção, eles ficam com a mesma incrição imobiliária durante um tempo. Ou seja, a cobertura e o apartamento do térreo ficam com a mesma inscrição imobiliária até o momento do habite-se, quando se separam. Para não termos a comparação de preços de venda de imóveis diferentes, restringimos isso, deixando somente as vendas de apartamentos únicos e comparáveis entre si ao longo do tempo, excluindo a possibilidade de ter imóveis diferentes na mesma inscrição.

### O primeiro índice que vamos gerar é o de apartamentos. Para gerar a base utilizada em sua construção, selecionamos os apartamentos vendidos por ao menos 10 mil reais cujo valor informado era maior que o valor venal (para impedir subnotificações, afinal o imposto é cobrado proporcionalmente ao valor do imóvel, então as pessoas estariam tentadas a subnotificar os valores, e realmente acontece). Aqui vale a pena acompanhar com o hpiR aberto, pois sigo mais ou menos o mesmo caminho.

base_cs <- base_cs %>% filter(TIPO_CONST == '20 - Apartamento') %>% filter(inscri2 != '0000') %>% filter(valor_instrumento > 10000) %>% filter(valor_instrumento > valor_venal_total) 

### Geração de tabela por região

base_cs%>%group_by(regiao_max)%>%tally()%>%arrange(-n) %>% kable()

### Criação do modelo ignorado, que limita as valorizações

modelocev <- dateToPeriod(trans_df = base_cs,
                          date = 'data_transacao',
                          periodicity = 'quarterly',
                          min_date = as.Date('2007-01-01'),
                          max_date = as.Date('2022-12-30'),
                          adj_type = 'clip')

head(attr(modelocev, 'period_table'))
modelocev2 <- rtCreateTrans(trans_df = modelocev,
                            date = 'data_transacao',
                            prop_id = 'inscricao',
                            trans_id = 'ID',
                            price = 'valor_instrumento',
                            min_period_dist = 2)

### Aqui, diferentemente do que está no pacote hpiR, a variação (positiva ou negativa) foi limitada, para tentar evitar as subnotificações. Como é visto na apresentação, este índice não ficou crível nem com esta restrição. De qualquer forma, foi tentado.

modelocev2 <- modelocev2[which(modelocev2$price_2/modelocev2$price_1 < 1.25 * 1.125 ^ ((modelocev2$period_2 - modelocev2$period_1) / 4)),]
modelocev2 <- modelocev2[which(modelocev2$price_2/modelocev2$price_1 > 0.90 * 1.03 ^ ((modelocev2$period_2 - modelocev2$period_1) / 4)),]
modelocev2 <- modelocev2[which(modelocev2$price_1 > 0),]
modelocevR <- hpiModel(model_type = 'rt',
                       hpi_df = modelocev2,
                       estimator = 'robust',
                       log_dep = TRUE)
modelocevB <- hpiModel(model_type = 'rt',
                       hpi_df = modelocev2,
                       estimator = 'base',
                       log_dep = TRUE)
modelocevW <- hpiModel(model_type = 'rt',
                       hpi_df = modelocev2,
                       estimator = 'weighted',
                       log_dep = TRUE)
indicecevR <- modelToIndex(model_obj = modelocevR)
indicecevB <- modelToIndex(model_obj = modelocevB)
indicecevW <- modelToIndex(model_obj = modelocevW)
plot(indicecevR)

cevsmooth <- smoothIndex(index_obj = indicecevW,
                         order = 5)
indicecevW <- smoothIndex(index_obj = indicecevW,
                         order = 7,
                         in_place = TRUE)
plot(indicecevW, smooth=TRUE)

tabela_indice <- data.frame(cbind(indicecevW$name,indicecevW$value,indicecevW$smooth))

indicecevW_vol <- calcVolatility(index = indicecevW$value,
                                window = 3)
plot(indicecevW_vol)

### Criação do modelo sem todas essas fases, conforme o hpiR. Trimestral, a partir de 2009, utilizando o valor informado, com o estimador weighted, o mesmo utilizado por Case e Shiller em 1987. Índice suavizado anualmente, ou seja, por 4 períodos trimestrais. Houve a necessidade de interstício de 1 ano entre as vendas de um mesmo imóvel para serem consideradas válidas.

rt_1 <- rtIndex(trans_df = base_cs,
                periodicity = 'quarterly',
                min_date = '2009-01-01',
                max_date = '2022-09-01',
                adj_type = 'clip',
                date = 'data_transacao',
                price = 'valor_instrumento',
                min_period_dist = 4,
                trans_id = 'ID',
                prop_id = 'inscricao',
                seq_only = TRUE,
                estimator = 'weighted',
                log_dep = TRUE,
                trim_model = TRUE,
                smooth = TRUE,
                smooth_order = 4) 

### Plotagem do índice suavizado

plot(rt_1, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indiceapto <- data.frame(cbind(rt_1$index$name,rt_1$index$value,rt_1$index$smooth))

tabela_indiceaptosgrande <- data.frame(cbind(rt_1$data)) %>% mutate(setor_fisc=substring(prop_id,1,2))

base_indiceaptogrande <- left_join(tabela_indiceaptosgrande, Regioes, match = 'first', by = c('setor_fisc'))

base_indiceaptogrande%>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

### Primeiro teste de acurácia 

rt_1_teste <- calcAccuracy(hpi_obj = rt_1,
                        test_type = 'rt',
                        test_method = 'kfold',
                        k = 10,
                        seed = 123,
                        in_place = TRUE,
                        in_place_name = 'smooth_is_accuracy',
                        smooth=TRUE)

### Plotagem do teste

plot(rt_1_teste$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_series <- createSeries(hpi_obj = rt_1,
                          train_period = 24)

### Plotagem

plot(rt_series)

### Plotagem e novos testes de acurácia

rt_series <- smoothSeries(series_obj = rt_series,
                          order = 4)

rt_seriesVol <- calcSeriesVolatility(series_obj = rt_series,
                                  window = 3,
                                  smooth = TRUE,
                                  order = 4)

rt_seriesVol_ok <- calcVolatility(index = rt_1$index$smooth,
                                 window = 3)

plot(rt_seriesVol_ok)

rt_sacc <- calcSeriesAccuracy(series_obj = rt_series,
                              test_method = 'insample',
                              test_type = 'rt')

rt_series <- calcSeriesAccuracy(series_obj = rt_series,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = TRUE,
                                in_place = TRUE)

plot(rt_series$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_rev <- calcRevision(series_obj = rt_series)

rt_series <- calcRevision(series_obj = rt_series,
                          in_place = TRUE,
                          smooth = TRUE)

plot(rt_rev, measure='median')

tabela_indiceACC <- data.frame(cbind(rt_series$accuracy_smooth$error,rt_series$accuracy_smooth$rt_price,rt_series$accuracy_smooth$pred_price,rt_series$accuracy_smooth$log_error))

dx4 <- density(tabela_indiceACC$X4)

plot(dx4, main = "Densidade dos erros")

dx1 <- density(tabela_indiceACC$X1)

### Plotagem da densidade dos erros

plot(dx1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indiceACC, aes(x=X2, y = X3)) + geom_point() + ylim(0,1500000) + xlim(0,1500000)


##### Índice para financiamentos, este realmente utilizado

### Construindo o ID para o índice, excluindo frações, excluindo guias substituídas, canceladas e complementos e selecionando as vendas com valor mínimo de 20 mil reais 

Base_financiamentos <- Base %>% mutate(ID=as.character(paste0(data_transacao,"...",inscricao))) %>% mutate(inscri=substring(inscricao,1,nchar(inscricao)-5)) %>% filter(is.na(obs)|substring(obs,1,3)!='FRA') %>% filter(substring(situacao,1,4)!='Subs') %>% filter(substring(situacao,1,4)!='Canc')  %>% filter(is.na(obs)|substring(obs,1,11) !='COMPLEMENTO') %>% filter(valor_instrumento > 20000)  %>% filter(valor_financiado > 0)

### Juntando a base de lotes com a base de vendas pela coluna de inscrição sem os últimos dígitos (para unir também os apartamentos)

base_cs_financiamento <- left_join(Base_financiamentos, csv_lotes2, match = 'first', by = c('inscri'))

### Geração de tabela por região

base_cs_financiamento%>%group_by(regiao_max)%>%tally()%>%arrange(-n) %>% kable()

### Índice. Foi utilizado o estimador weighted, o mesmo do Case-Shiller de 1987, suavizado anualmente e com 1 ano de interstício entre vendas.

rt_financiamento <- rtIndex(trans_df = base_cs_financiamento,
                periodicity = 'quarterly',
                min_date = '2009-01-01',
                max_date = '2022-09-01',
                adj_type = 'clip',
                date = 'data_transacao',
                price = 'valor_instrumento',
                min_period_dist = 4,
                trans_id = 'ID',
                prop_id = 'inscricao',
                seq_only = TRUE,
                estimator = 'weighted',
                log_dep = TRUE,
                trim_model = TRUE,
                smooth = TRUE,
                smooth_order = 4) 

### Plotagem do índice suavizado

plot(rt_financiamento, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento <- data.frame(cbind(rt_financiamento$index$name,rt_financiamento$index$value,rt_financiamento$index$smooth))

tabela_indicefinanciamentosgrande <- data.frame(cbind(rt_financiamento$data)) %>% mutate(setor_fisc=substring(prop_id,1,2))

base_indicefinanciamentogrande <- left_join(tabela_indicefinanciamentosgrande, Regioes, match = 'first', by = c('setor_fisc'))

base_indicefinanciamentogrande%>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

### Volatilidade do índice

rt_financiamentoVol_ok <- calcVolatility(index = rt_financiamento$index$smooth,
                                  window = 3)

plot(rt_financiamentoVol_ok)

### Primeiro teste de acurácia 

rt_financiamento_2 <- calcAccuracy(hpi_obj = rt_financiamento,
                           test_type = 'rt',
                           test_method = 'kfold',
                           k = 10,
                           seed = 123,
                           in_place = TRUE,
                           in_place_name = 'smooth_is_accuracy',
                           smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes <- createSeries(hpi_obj = rt_financiamento,
                          train_period = 24)

### Plotagem

plot(rt_financiamento_testes)

### Plotagem e novos testes de acurácia

rt_financiamento_testes <- smoothSeries(series_obj = rt_financiamento_testes,
                          order = 4)

rt_financiamento_testesVol <- calcSeriesVolatility(series_obj = rt_financiamento_testes,
                                     window = 3,
                                     smooth = TRUE,
                                     order = 4)

rt_aaa <- calcSeriesAccuracy(series_obj = rt_financiamento_testes,
                              test_method = 'insample',
                              test_type = 'rt')

rt_financiamento_testes <- calcSeriesAccuracy(series_obj = rt_financiamento_testes,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = TRUE,
                                in_place = TRUE)

plot(rt_financiamento_testes$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado <- calcRevision(series_obj = rt_financiamento_testes)

rt_financiamento_testes <- calcRevision(series_obj = rt_financiamento_testes,
                          in_place = TRUE,
                          smooth = TRUE)

plot(rt_financiamento_revisado, measure='median')

tabela_dadosfinanciamentos <- data.frame(cbind(rt_financiamento$data))

tabela_indice_financiamento <- data.frame(cbind(rt_financiamento_testes$accuracy_smooth$error,rt_financiamento_testes$accuracy_smooth$rt_price,rt_financiamento_testes$accuracy_smooth$pred_price,rt_financiamento_testes$accuracy_smooth$log_error))

dx44 <- density(tabela_indice_financiamento$X4)

plot(dx44, main = "Densidade dos erros")

dx11 <- density(tabela_indice_financiamento$X1)

### Plotagem da densidade dos erros

plot(dx11, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000) + geom_smooth(methor='lm',colour='blue',se=FALSE) 

##### Índice com todas as vendas, que foi o que fizemos primeiro, mas que fomos apagando e escrevendo outros em cima, pois claramente o resultado era ruim. Depois, refizemos ele para comparar com os anteriores.

### Construindo o ID para o índice, coluna de inscrição sem os últimos números, excluindo frações, excluindo guias substituídas, canceladas e complementos e selecionando as vendas com valor mínimo de 10 mil reais 

Base_tudo <- Base %>% mutate(ID=as.character(paste0(data_transacao,"...",inscricao))) %>% mutate(inscri=substring(inscricao,1,nchar(inscricao)-5)) %>% filter(is.na(obs)|substring(obs,1,3)!='FRA') %>% filter(substring(situacao,1,4)!='Subs') %>% filter(substring(situacao,1,4)!='Canc')  %>% filter(is.na(obs)|substring(obs,1,11) !='COMPLEMENTO') %>% filter(valor_instrumento > 10000) 

###%>% filter(valor_instrumento > 20000) %>% filter(valor_instrumento > valor_venal_total), utilizamos com este final também, para ver se mudava algo, mas continua ruim.

### Juntando a base de lotes com a base de vendas pela coluna de inscrição sem os últimos dígitos (para unir também os apartamentos)

base_cs_tudo <- left_join(Base_tudo, csv_lotes2, match = 'first', by = c('inscri'))

### Geração de tabela por região

base_cs_tudo %>%group_by(regiao_max)%>%tally()%>%arrange(-n) %>% kable()

### Índice

rt_tudo <- rtIndex(trans_df = base_cs_tudo,
                            periodicity = 'quarterly',
                            min_date = '2009-01-01',
                            max_date = '2022-09-01',
                            adj_type = 'clip',
                            date = 'data_transacao',
                            price = 'valor_instrumento',
                            min_period_dist = 4,
                            trans_id = 'ID',
                            prop_id = 'inscricao',
                            seq_only = TRUE,
                            estimator = 'weighted',
                            log_dep = TRUE,
                            trim_model = TRUE,
                            smooth = TRUE,
                            smooth_order = 4) 

tabela_indicetudobase <- data.frame(cbind(rt_tudo$data)) %>% mutate(setor_fisc=substring(prop_id,1,2))

base_indicetudo <- left_join(tabela_indicetudobase, Regioes, match = 'first', by = c('setor_fisc'))

base_indicetudo%>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

### Plotagem do índice suavizado

plot(rt_tudo, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicetudo <- data.frame(cbind(rt_tudo$index$name,rt_tudo$index$value,rt_tudo$index$smooth))

### Volatilidade do índice

rt_tudoVol_ok <- calcVolatility(index = rt_tudo$index$smooth,
                                         window = 3)

plot(rt_tudoVol_ok)

### Primeiro teste de acurácia 

rt_tudo_2 <- calcAccuracy(hpi_obj = rt_tudo,
                                   test_type = 'rt',
                                   test_method = 'kfold',
                                   k = 10,
                                   seed = 123,
                                   in_place = TRUE,
                                   in_place_name = 'smooth_is_accuracy',
                                   smooth=TRUE)

### Plotagem do teste

plot(rt_tudo_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_tudo_testes <- createSeries(hpi_obj = rt_tudo,
                                        train_period = 24)

### Plotagem

plot(rt_tudo_testes)

### Plotagem e novos testes de acurácia

rt_tudo_testes <- smoothSeries(series_obj = rt_tudo_testes,
                                        order = 4)

rt_tudo_testesVol <- calcSeriesVolatility(series_obj = rt_tudo_testes,
                                                   window = 3,
                                                   smooth = TRUE,
                                                   order = 4)

rt_ttt <- calcSeriesAccuracy(series_obj = rt_tudo_testes,
                             test_method = 'insample',
                             test_type = 'rt')

rt_tudo_testes <- calcSeriesAccuracy(series_obj = rt_tudo_testes,
                                              test_method = 'forecast',
                                              test_type = 'rt',
                                              smooth = TRUE,
                                              in_place = TRUE)

plot(rt_tudo_testes$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_tudo_revisado <- calcRevision(series_obj = rt_tudo_testes)

rt_tudo_testes <- calcRevision(series_obj = rt_tudo_testes,
                                        in_place = TRUE,
                                        smooth = TRUE)

plot(rt_tudo_revisado, measure='median')

tabela_indice_tudo <- data.frame(cbind(rt_tudo_testes$accuracy_smooth$error,rt_tudo_testes$accuracy_smooth$rt_price,rt_tudo_testes$accuracy_smooth$pred_price,rt_tudo_testes$accuracy_smooth$log_error))

dx444 <- density(tabela_indice_tudo$X4)

plot(dx444, main = "Densidade dos erros")

dx111 <- density(tabela_indice_tudo$X1)

### Plotagem da densidade dos erros

plot(dx111, xlim=c(-1,5), main = "Densidade dos erros")

plot(tabela_indice_tudo$X1,tabela_indice_tudo$X2, xlim= c(-1,5), ylim = c(000000,1100000))

plot(tabela_indice_financiamento$X1,tabela_indice_financiamento$X2, xlim= c(-1,5), ylim = c(000000,1100000) ) 


plot(tabela_indiceACC$X1,tabela_indiceACC$X2, xlim= c(-1,5), ylim = c(000000,1100000) )

ggplot(data = tabela_indice_tudo, aes(x=X2, y = X3)) + geom_point() + ylim(0,1500000) + xlim(0,1500000)  



#### Criando base para gerar os índices regionais. Aqui, como entendemos que o índice gerado pelas vendas financiadas foi o estatísticamente mais correto, passamos a utilizar somente ele.

Base_financiamentos <- Base %>% mutate(ID=as.character(paste0(data_transacao,"...",inscricao))) %>% mutate(inscri=substring(inscricao,1,nchar(inscricao)-5)) %>% filter(is.na(obs)|substring(obs,1,3)!='FRA') %>% filter(substring(situacao,1,4)!='Subs') %>% filter(substring(situacao,1,4)!='Canc')  %>% filter(is.na(obs)|substring(obs,1,11) !='COMPLEMENTO') %>% filter(valor_instrumento > 20000)  %>% filter(valor_financiado > 0) %>% mutate (setor_fisc = substring(inscricao,1,2))

### Juntando a base de lotes com a base de vendas pela coluna de inscrição sem os últimos dígitos (para unir também os apartamentos)

base_financiamento_regiao <- left_join(Base_financiamentos, Regioes, match = 'first', by = c('setor_fisc'))

### Resumo por região

base_financiamento_regiao%>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

### Criando base de cada região

base_financiamento_sul <- base_financiamento_regiao %>% filter(regiao == 'Sul') 
base_financiamento_norte <- base_financiamento_regiao %>% filter(regiao == 'Norte') 
base_financiamento_sudeste <- base_financiamento_regiao %>% filter(regiao == 'Sudeste') 
base_financiamento_leste <- base_financiamento_regiao %>% filter(regiao == ('Leste') | regiao == ('Sudeste')) 
base_financiamento_centro <- base_financiamento_regiao %>% filter(regiao == ('Centro') | regiao == ('Norte')) 
base_financiamento_oeste <- base_financiamento_regiao %>% filter(regiao == 'Oeste') 

### Índice Sul

rt_financiamento_sul <- rtIndex(trans_df = base_financiamento_sul,
                            periodicity = 'quarterly',
                            min_date = '2009-01-01',
                            max_date = '2022-09-01',
                            adj_type = 'clip',
                            date = 'data_transacao',
                            price = 'valor_instrumento',
                            min_period_dist = 4,
                            trans_id = 'ID',
                            prop_id = 'inscricao',
                            seq_only = TRUE,
                            estimator = 'weighted',
                            log_dep = TRUE,
                            trim_model = TRUE,
                            smooth = TRUE,
                            smooth_order = 6) 

### Plotagem do índice suavizado

plot(rt_financiamento_sul, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento_sul <- data.frame(cbind(rt_financiamento_sul$index$name,rt_financiamento_sul$index$value,rt_financiamento_sul$index$smooth))

### Volatilidade do índice

rt_financiamentoVol_sul <- calcVolatility(index = rt_financiamento_sul$index$smooth,
                                         window = 3)

plot(rt_financiamentoVol_sul)

### Primeiro teste de acurácia 

rt_financiamento_sul_2 <- calcAccuracy(hpi_obj = rt_financiamento_sul,
                                   test_type = 'rt',
                                   test_method = 'kfold',
                                   k = 10,
                                   seed = 123,
                                   in_place = TRUE,
                                   in_place_name = 'smooth_is_accuracy',
                                   smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_sul_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes_sul <- createSeries(hpi_obj = rt_financiamento_sul,
                                        train_period = 24)

### Plotagem

plot(rt_financiamento_testes_sul)

### Plotagem e novos testes de acurácia

rt_financiamento_testes_sul_2 <- smoothSeries(series_obj = rt_financiamento_testes_sul,
                                        order = 4)

rt_financiamento_testesVol_sul <- calcSeriesVolatility(series_obj = rt_financiamento_testes_sul_2,
                                                   window = 3,
                                                   smooth = TRUE,
                                                   order = 4)

rt_sul <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_sul,
                             test_method = 'insample',
                             test_type = 'rt')

rt_financiamento_testes_sul <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_sul_2,
                                              test_method = 'forecast',
                                              test_type = 'rt',
                                              smooth = TRUE,
                                              in_place = TRUE)

plot(rt_financiamento_testes_sul$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado_sul <- calcRevision(series_obj = rt_financiamento_testes_sul)

rt_financiamento_testes_sul <- calcRevision(series_obj = rt_financiamento_testes_sul,
                                        in_place = TRUE,
                                        smooth = TRUE)

plot(rt_financiamento_revisado_sul, measure='median')

tabela_dadosfinanciamentos_sul <- data.frame(cbind(rt_financiamento_sul$data))

tabela_indice_financiamento_sul <- data.frame(cbind(rt_financiamento_testes_sul$accuracy_smooth$error,rt_financiamento_testes_sul$accuracy_smooth$rt_price,rt_financiamento_testes_sul$accuracy_smooth$pred_price,rt_financiamento_testes_sul$accuracy_smooth$log_error))

dxsul4 <- density(tabela_indice_financiamento_sul$X4)

plot(dx44, main = "Densidade dos erros")

dxsul1 <- density(tabela_indice_financiamento_sul$X1)

### Plotagem da densidade dos erros

plot(dxsul1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento_sul, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)


### Índice norte

rt_financiamento_norte <- rtIndex(trans_df = base_financiamento_norte,
                                  periodicity = 'quarterly',
                                  min_date = '2009-01-01',
                                  max_date = '2022-09-01',
                                  adj_type = 'clip',
                                  date = 'data_transacao',
                                  price = 'valor_instrumento',
                                  min_period_dist = 4,
                                  trans_id = 'ID',
                                  prop_id = 'inscricao',
                                  seq_only = TRUE,
                                  estimator = 'weighted',
                                  log_dep = TRUE,
                                  trim_model = TRUE,
                                  smooth = TRUE,
                                  smooth_order = 6) 

### Plotagem do índice suavizado

plot(rt_financiamento_norte, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento_norte <- data.frame(cbind(rt_financiamento_norte$index$name,rt_financiamento_norte$index$value,rt_financiamento_norte$index$smooth))

### Volatilidade do índice

rt_financiamentoVol_norte <- calcVolatility(index = rt_financiamento_norte$index$smooth,
                                            window = 3)

plot(rt_financiamentoVol_norte)

### Primeiro teste de acurácia 

rt_financiamento_norte_2 <- calcAccuracy(hpi_obj = rt_financiamento_norte,
                                         test_type = 'rt',
                                         test_method = 'kfold',
                                         k = 10,
                                         seed = 123,
                                         in_place = TRUE,
                                         in_place_name = 'smooth_is_accuracy',
                                         smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_norte_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes_norte <- createSeries(hpi_obj = rt_financiamento_norte,
                                              train_period = 24)

### Plotagem

plot(rt_financiamento_testes_norte)

### Plotagem e novos testes de acurácia

rt_financiamento_testes_norte_2 <- smoothSeries(series_obj = rt_financiamento_testes_norte,
                                                order = 4)

rt_financiamento_testesVol_norte <- calcSeriesVolatility(series_obj = rt_financiamento_testes_norte_2,
                                                         window = 3,
                                                         smooth = TRUE,
                                                         order = 4)

rt_norte <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_norte,
                               test_method = 'insample',
                               test_type = 'rt')

rt_financiamento_testes_norte <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_norte_2,
                                                    test_method = 'forecast',
                                                    test_type = 'rt',
                                                    smooth = TRUE,
                                                    in_place = TRUE)

plot(rt_financiamento_testes_norte$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado_norte <- calcRevision(series_obj = rt_financiamento_testes_norte)

rt_financiamento_testes_norte <- calcRevision(series_obj = rt_financiamento_testes_norte,
                                              in_place = TRUE,
                                              smooth = TRUE)

plot(rt_financiamento_revisado_norte, measure='median')

tabela_dadosfinanciamentos_norte <- data.frame(cbind(rt_financiamento_norte$data))

tabela_indice_financiamento_norte <- data.frame(cbind(rt_financiamento_testes_norte$accuracy_smooth$error,rt_financiamento_testes_norte$accuracy_smooth$rt_price,rt_financiamento_testes_norte$accuracy_smooth$pred_price,rt_financiamento_testes_norte$accuracy_smooth$log_error))

dxnorte4 <- density(tabela_indice_financiamento_norte$X4)

plot(dx44, main = "Densidade dos erros")

dxnorte1 <- density(tabela_indice_financiamento_norte$X1)

### Plotagem da densidade dos erros

plot(dxnorte1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento_norte, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)

### Índice sudeste. Neste e no índice da norte, dando spoiler, acreditamos que estatísticamente não estava muito bom, embora não saibamos se é correto afirmar isso. Optamos, posteriormente, por juntar sudeste com leste e norte com centro, por conta das similaridades entre as regiões.

rt_financiamento_sudeste <- rtIndex(trans_df = base_financiamento_sudeste,
                                    periodicity = 'quarterly',
                                    min_date = '2009-01-01',
                                    max_date = '2022-09-01',
                                    adj_type = 'clip',
                                    date = 'data_transacao',
                                    price = 'valor_instrumento',
                                    min_period_dist = 4,
                                    trans_id = 'ID',
                                    prop_id = 'inscricao',
                                    seq_only = TRUE,
                                    estimator = 'weighted',
                                    log_dep = TRUE,
                                    trim_model = TRUE,
                                    smooth = TRUE,
                                    smooth_order = 6) 

### Plotagem do índice suavizado

plot(rt_financiamento_sudeste, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento_sudeste <- data.frame(cbind(rt_financiamento_sudeste$index$name,rt_financiamento_sudeste$index$value,rt_financiamento_sudeste$index$smooth))

### Volatilidade do índice

rt_financiamentoVol_sudeste <- calcVolatility(index = rt_financiamento_sudeste$index$smooth,
                                              window = 3)

plot(rt_financiamentoVol_sudeste)

### Primeiro teste de acurácia 

rt_financiamento_sudeste_2 <- calcAccuracy(hpi_obj = rt_financiamento_sudeste,
                                           test_type = 'rt',
                                           test_method = 'kfold',
                                           k = 10,
                                           seed = 123,
                                           in_place = TRUE,
                                           in_place_name = 'smooth_is_accuracy',
                                           smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_sudeste_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes_sudeste <- createSeries(hpi_obj = rt_financiamento_sudeste,
                                                train_period = 24)

### Plotagem

plot(rt_financiamento_testes_sudeste)

### Plotagem e novos testes de acurácia

rt_financiamento_testes_sudeste_2 <- smoothSeries(series_obj = rt_financiamento_testes_sudeste,
                                                  order = 4)

rt_financiamento_testesVol_sudeste <- calcSeriesVolatility(series_obj = rt_financiamento_testes_sudeste_2,
                                                           window = 3,
                                                           smooth = TRUE,
                                                           order = 4)

rt_sudeste <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_sudeste,
                                 test_method = 'insample',
                                 test_type = 'rt')

rt_financiamento_testes_sudeste <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_sudeste_2,
                                                      test_method = 'forecast',
                                                      test_type = 'rt',
                                                      smooth = TRUE,
                                                      in_place = TRUE)

plot(rt_financiamento_testes_sudeste$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado_sudeste <- calcRevision(series_obj = rt_financiamento_testes_sudeste)

rt_financiamento_testes_sudeste <- calcRevision(series_obj = rt_financiamento_testes_sudeste,
                                                in_place = TRUE,
                                                smooth = TRUE)

plot(rt_financiamento_revisado_sudeste, measure='median')

tabela_dadosfinanciamentos_sudeste <- data.frame(cbind(rt_financiamento_sudeste$data))

tabela_indice_financiamento_sudeste <- data.frame(cbind(rt_financiamento_testes_sudeste$accuracy_smooth$error,rt_financiamento_testes_sudeste$accuracy_smooth$rt_price,rt_financiamento_testes_sudeste$accuracy_smooth$pred_price,rt_financiamento_testes_sudeste$accuracy_smooth$log_error))

dxsudeste4 <- density(tabela_indice_financiamento_sudeste$X4)

plot(dx44, main = "Densidade dos erros")

dxsudeste1 <- density(tabela_indice_financiamento_sudeste$X1)

### Plotagem da densidade dos erros

plot(dxsudeste1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento_sudeste, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)


### Índice oeste

rt_financiamento_oeste <- rtIndex(trans_df = base_financiamento_oeste,
                                  periodicity = 'quarterly',
                                  min_date = '2009-01-01',
                                  max_date = '2022-09-01',
                                  adj_type = 'clip',
                                  date = 'data_transacao',
                                  price = 'valor_instrumento',
                                  min_period_dist = 4,
                                  trans_id = 'ID',
                                  prop_id = 'inscricao',
                                  seq_only = TRUE,
                                  estimator = 'weighted',
                                  log_dep = TRUE,
                                  trim_model = TRUE,
                                  smooth = TRUE,
                                  smooth_order = 6) 

### Plotagem do índice suavizado

plot(rt_financiamento_oeste, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento_oeste <- data.frame(cbind(rt_financiamento_oeste$index$name,rt_financiamento_oeste$index$value,rt_financiamento_oeste$index$smooth))

### Volatilidade do índice

rt_financiamentoVol_oeste <- calcVolatility(index = rt_financiamento_oeste$index$smooth,
                                            window = 3)

plot(rt_financiamentoVol_oeste)

### Primeiro teste de acurácia 

rt_financiamento_oeste_2 <- calcAccuracy(hpi_obj = rt_financiamento_oeste,
                                         test_type = 'rt',
                                         test_method = 'kfold',
                                         k = 10,
                                         seed = 123,
                                         in_place = TRUE,
                                         in_place_name = 'smooth_is_accuracy',
                                         smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_oeste_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes_oeste <- createSeries(hpi_obj = rt_financiamento_oeste,
                                              train_period = 24)

### Plotagem

plot(rt_financiamento_testes_oeste)

### Plotagem e novos testes de acurácia

rt_financiamento_testes_oeste_2 <- smoothSeries(series_obj = rt_financiamento_testes_oeste,
                                                order = 4)

rt_financiamento_testesVol_oeste <- calcSeriesVolatility(series_obj = rt_financiamento_testes_oeste_2,
                                                         window = 3,
                                                         smooth = TRUE,
                                                         order = 4)

rt_oeste <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_oeste,
                               test_method = 'insample',
                               test_type = 'rt')

rt_financiamento_testes_oeste <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_oeste_2,
                                                    test_method = 'forecast',
                                                    test_type = 'rt',
                                                    smooth = TRUE,
                                                    in_place = TRUE)

plot(rt_financiamento_testes_oeste$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado_oeste <- calcRevision(series_obj = rt_financiamento_testes_oeste)

rt_financiamento_testes_oeste <- calcRevision(series_obj = rt_financiamento_testes_oeste,
                                              in_place = TRUE,
                                              smooth = TRUE)

plot(rt_financiamento_revisado_oeste, measure='median')

tabela_dadosfinanciamentos_oeste <- data.frame(cbind(rt_financiamento_oeste$data))

tabela_indice_financiamento_oeste <- data.frame(cbind(rt_financiamento_testes_oeste$accuracy_smooth$error,rt_financiamento_testes_oeste$accuracy_smooth$rt_price,rt_financiamento_testes_oeste$accuracy_smooth$pred_price,rt_financiamento_testes_oeste$accuracy_smooth$log_error))

dxoeste4 <- density(tabela_indice_financiamento_oeste$X4)

plot(dx44, main = "Densidade dos erros")

dxoeste1 <- density(tabela_indice_financiamento_oeste$X1)

### Plotagem da densidade dos erros

plot(dxoeste1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento_oeste, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)


### Índice leste

rt_financiamento_leste <- rtIndex(trans_df = base_financiamento_leste,
                                  periodicity = 'quarterly',
                                  min_date = '2009-01-01',
                                  max_date = '2022-09-01',
                                  adj_type = 'clip',
                                  date = 'data_transacao',
                                  price = 'valor_instrumento',
                                  min_period_dist = 4,
                                  trans_id = 'ID',
                                  prop_id = 'inscricao',
                                  seq_only = TRUE,
                                  estimator = 'weighted',
                                  log_dep = TRUE,
                                  trim_model = TRUE,
                                  smooth = TRUE,
                                  smooth_order = 6) 

### Plotagem do índice suavizado

plot(rt_financiamento_leste, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento_leste <- data.frame(cbind(rt_financiamento_leste$index$name,rt_financiamento_leste$index$value,rt_financiamento_leste$index$smooth))

### Volatilidade do índice

rt_financiamentoVol_leste <- calcVolatility(index = rt_financiamento_leste$index$smooth,
                                            window = 3)

plot(rt_financiamentoVol_leste)

### Primeiro teste de acurácia 

rt_financiamento_leste_2 <- calcAccuracy(hpi_obj = rt_financiamento_leste,
                                         test_type = 'rt',
                                         test_method = 'kfold',
                                         k = 10,
                                         seed = 123,
                                         in_place = TRUE,
                                         in_place_name = 'smooth_is_accuracy',
                                         smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_leste_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes_leste <- createSeries(hpi_obj = rt_financiamento_leste,
                                              train_period = 24)

### Plotagem

plot(rt_financiamento_testes_leste)

### Plotagem e novos testes de acurácia

rt_financiamento_testes_leste_2 <- smoothSeries(series_obj = rt_financiamento_testes_leste,
                                                order = 4)

rt_financiamento_testesVol_leste <- calcSeriesVolatility(series_obj = rt_financiamento_testes_leste_2,
                                                         window = 3,
                                                         smooth = TRUE,
                                                         order = 4)

rt_leste <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_leste,
                               test_method = 'insample',
                               test_type = 'rt')

rt_financiamento_testes_leste <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_leste_2,
                                                    test_method = 'forecast',
                                                    test_type = 'rt',
                                                    smooth = TRUE,
                                                    in_place = TRUE)

plot(rt_financiamento_testes_leste$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado_leste <- calcRevision(series_obj = rt_financiamento_testes_leste)

rt_financiamento_testes_leste <- calcRevision(series_obj = rt_financiamento_testes_leste,
                                              in_place = TRUE,
                                              smooth = TRUE)

plot(rt_financiamento_revisado_leste, measure='median')

tabela_dadosfinanciamentos_leste <- data.frame(cbind(rt_financiamento_leste$data))

tabela_indice_financiamento_leste <- data.frame(cbind(rt_financiamento_testes_leste$accuracy_smooth$error,rt_financiamento_testes_leste$accuracy_smooth$rt_price,rt_financiamento_testes_leste$accuracy_smooth$pred_price,rt_financiamento_testes_leste$accuracy_smooth$log_error))

dxleste4 <- density(tabela_indice_financiamento_leste$X4)

plot(dx44, main = "Densidade dos erros")

dxleste1 <- density(tabela_indice_financiamento_leste$X1)

### Plotagem da densidade dos erros

plot(dxleste1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento_leste, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)

### Índice centro

rt_financiamento_centro <- rtIndex(trans_df = base_financiamento_centro,
                                   periodicity = 'quarterly',
                                   min_date = '2009-01-01',
                                   max_date = '2022-09-01',
                                   adj_type = 'clip',
                                   date = 'data_transacao',
                                   price = 'valor_instrumento',
                                   min_period_dist = 4,
                                   trans_id = 'ID',
                                   prop_id = 'inscricao',
                                   seq_only = TRUE,
                                   estimator = 'weighted',
                                   log_dep = TRUE,
                                   trim_model = TRUE,
                                   smooth = TRUE,
                                   smooth_order = 6) 

### Plotagem do índice suavizado

plot(rt_financiamento_centro, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento_centro <- data.frame(cbind(rt_financiamento_centro$index$name,rt_financiamento_centro$index$value,rt_financiamento_centro$index$smooth))

### Volatilidade do índice

rt_financiamentoVol_centro <- calcVolatility(index = rt_financiamento_centro$index$smooth,
                                             window = 3)

plot(rt_financiamentoVol_centro)

### Primeiro teste de acurácia 

rt_financiamento_centro_2 <- calcAccuracy(hpi_obj = rt_financiamento_centro,
                                          test_type = 'rt',
                                          test_method = 'kfold',
                                          k = 10,
                                          seed = 123,
                                          in_place = TRUE,
                                          in_place_name = 'smooth_is_accuracy',
                                          smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_centro_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes_centro <- createSeries(hpi_obj = rt_financiamento_centro,
                                               train_period = 24)

### Plotagem

plot(rt_financiamento_testes_centro)

### Plotagem e novos testes de acurácia

rt_financiamento_testes_centro_2 <- smoothSeries(series_obj = rt_financiamento_testes_centro,
                                                 order = 4)

rt_financiamento_testesVol_centro <- calcSeriesVolatility(series_obj = rt_financiamento_testes_centro_2,
                                                          window = 3,
                                                          smooth = TRUE,
                                                          order = 4)

rt_centro <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_centro,
                                test_method = 'insample',
                                test_type = 'rt')

rt_financiamento_testes_centro <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_centro_2,
                                                     test_method = 'forecast',
                                                     test_type = 'rt',
                                                     smooth = TRUE,
                                                     in_place = TRUE)

plot(rt_financiamento_testes_centro$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado_centro <- calcRevision(series_obj = rt_financiamento_testes_centro)

rt_financiamento_testes_centro <- calcRevision(series_obj = rt_financiamento_testes_centro,
                                               in_place = TRUE,
                                               smooth = TRUE)

plot(rt_financiamento_revisado_centro, measure='median')

tabela_dadosfinanciamentos_centro <- data.frame(cbind(rt_financiamento_centro$data))

tabela_indice_financiamento_centro <- data.frame(cbind(rt_financiamento_testes_centro$accuracy_smooth$error,rt_financiamento_testes_centro$accuracy_smooth$rt_price,rt_financiamento_testes_centro$accuracy_smooth$pred_price,rt_financiamento_testes_centro$accuracy_smooth$log_error))

dxcentro4 <- density(tabela_indice_financiamento_centro$X4)

plot(dx44, main = "Densidade dos erros")

dxcentro1 <- density(tabela_indice_financiamento_centro$X1)

### Plotagem da densidade dos erros

plot(dxcentro1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento_centro, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)

### Índice de terrenos. Acabou ficando com poucas observações, acredito que estatísticamente não ficou tão bom. Até pela pouca quantidade de vendas, acabamos não utilizando a base de financiamento. Soma-se a isso, o fato de poucos terrenos serem financiados, especialmente os que construtoras compram para fazerem edifícios.

base_terrenos <- left_join(Base, csv_lotes2, match = 'first', by = c('inscri'))

base_terrenos <- base_terrenos %>% filter(TIPO_IMOVE == 'TERRENO')  %>% filter(valor_instrumento > 50000) %>% filter(valor_instrumento > valor_venal_total) 

rterrenos <- rtIndex(trans_df = base_terrenos,
                     periodicity = 'quarterly',
                     min_date = '2009-01-01',
                     max_date = '2022-09-01',
                     adj_type = 'clip',
                     date = 'data_transacao',
                     price = 'valor_instrumento',
                     min_period_dist = 4,
                     trans_id = 'ID',
                     prop_id = 'inscricao',
                     seq_only = TRUE,
                     estimator = 'weighted',
                     log_dep = TRUE,
                     trim_model = TRUE,
                     smooth = TRUE,
                     smooth_order = 4) 

### Plotagem do índice suavizado

plot(rterrenos, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_terrenos <- data.frame(cbind(rterrenos$index$name,rterrenos$index$value,rterrenos$index$smooth))

tabela_terrenao <- data.frame(cbind(rterrenos$data)) %>% mutate(setor_fisc=substring(prop_id,1,2))

base_terrenao <- left_join(tabela_terrenao, Regioes, match = 'first', by = c('setor_fisc'))

base_terrenao%>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

### Primeiro teste de acurácia 

rterrenos_1 <- calcAccuracy(hpi_obj = rterrenos,
                            test_type = 'rt',
                            test_method = 'kfold',
                            k = 10,
                            seed = 123,
                            in_place = TRUE,
                            in_place_name = 'smooth_is_accuracy',
                            smooth=TRUE)

### Plotagem do teste

plot(rterrenos_1$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_terrenos2 <- createSeries(hpi_obj = rterrenos,
                             train_period = 24)

### Plotagem

plot(rt_terrenos2)

### Plotagem e novos testes de acurácia

rt_terrenos2 <- smoothSeries(series_obj = rt_terrenos2,
                             order = 4)


rt_terrenos2acc <- calcSeriesAccuracy(series_obj = rt_terrenos2,
                                      test_method = 'insample',
                                      test_type = 'rt')

rt_terrenos2 <- calcSeriesAccuracy(series_obj = rt_terrenos2,
                                   test_method = 'forecast',
                                   test_type = 'rt',
                                   smooth = TRUE,
                                   in_place = TRUE)

plot(rt_terrenos2$accuracy_smooth)



tabela_indiceterrenos <- data.frame(cbind(rt_terrenos2$accuracy_smooth$error,rt_terrenos2 $accuracy_smooth$rt_price,rt_terrenos2 $accuracy_smooth$pred_price,rt_terrenos2 $accuracy_smooth$log_error))

ggplot(data = tabela_indiceterrenos, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)



### Índice consolidacao. Aqui na cidade temos três macrozonas criadas pelo Plano Diretor (consolidação, estruturação e ocupação controlada). Geramos os índice para essas espacializações, bem como para as centralidades definididas no referido plano.

base_consolidacao <- left_join(Base, csv_lotes2, match = 'first', by = c('inscri'))

base_consolidacao <- base_consolidacao %>% filter(a_max == 'Consolidacao')  %>% filter(valor_instrumento > 20000) %>% filter(valor_financiado > 0)

base_consolidacao <- base_consolidacao[!duplicated(base_consolidacao$ID),]

rconsolidacao <- rtIndex(trans_df = base_consolidacao,
                         periodicity = 'quarterly',
                         min_date = '2009-01-01',
                         max_date = '2022-09-01',
                         adj_type = 'clip',
                         date = 'data_transacao',
                         price = 'valor_instrumento',
                         min_period_dist = 4,
                         trans_id = 'ID',
                         prop_id = 'inscricao',
                         seq_only = TRUE,
                         estimator = 'weighted',
                         log_dep = TRUE,
                         trim_model = TRUE,
                         smooth = TRUE,
                         smooth_order = 4) 

### Plotagem do índice suavizado

plot(rconsolidacao, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_consolidacao <- data.frame(cbind(rconsolidacao$index$name,rconsolidacao$index$value,rconsolidacao$index$smooth))

tabela_terrenao <- data.frame(cbind(rconsolidacao$data)) %>% mutate(setor_fisc=substring(prop_id,1,2))

base_terrenao <- left_join(tabela_terrenao, Regioes, match = 'first', by = c('setor_fisc'))

base_terrenao%>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

### Primeiro teste de acurácia 

rconsolidacao_1 <- calcAccuracy(hpi_obj = rconsolidacao,
                                test_type = 'rt',
                                test_method = 'kfold',
                                k = 10,
                                seed = 123,
                                in_place = TRUE,
                                in_place_name = 'smooth_is_accuracy',
                                smooth=TRUE)

### Plotagem do teste

plot(rconsolidacao_1$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_consolidacao2 <- createSeries(hpi_obj = rconsolidacao,
                                 train_period = 24)

### Plotagem

plot(rt_consolidacao2)

### Plotagem e novos testes de acurácia

rt_consolidacao2 <- smoothSeries(series_obj = rt_consolidacao2,
                                 order = 4)


rt_consolidacao2acc <- calcSeriesAccuracy(series_obj = rt_consolidacao2,
                                          test_method = 'insample',
                                          test_type = 'rt')

rt_consolidacao2 <- calcSeriesAccuracy(series_obj = rt_consolidacao2,
                                       test_method = 'forecast',
                                       test_type = 'rt',
                                       smooth = TRUE,
                                       in_place = TRUE)

plot(rt_consolidacao2$accuracy_smooth)



tabela_indiceconsolidacao <- data.frame(cbind(rt_consolidacao2$accuracy_smooth$error,rt_consolidacao2 $accuracy_smooth$rt_price,rt_consolidacao2 $accuracy_smooth$pred_price,rt_consolidacao2 $accuracy_smooth$log_error))

ggplot(data = tabela_indiceconsolidacao, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)

### Índice centr

base_centr <- left_join(Base, csv_lotes2, match = 'first', by = c('inscri'))

base_centr <- base_centr %>% filter(valor_instrumento > 20000) %>% filter(valor_financiado > 0)

base_centr <- base_centr[!(is.na(base_centr$Categoria_max)), ]

base_centr <- base_centr[!duplicated(base_centr$ID),]

rcentr <- rtIndex(trans_df = base_centr,
                  periodicity = 'quarterly',
                  min_date = '2009-01-01',
                  max_date = '2022-09-01',
                  adj_type = 'clip',
                  date = 'data_transacao',
                  price = 'valor_instrumento',
                  min_period_dist = 4,
                  trans_id = 'ID',
                  prop_id = 'inscricao',
                  seq_only = TRUE,
                  estimator = 'weighted',
                  log_dep = TRUE,
                  trim_model = TRUE,
                  smooth = TRUE,
                  smooth_order = 4) 

### Plotagem do índice suavizado

plot(rcentr, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_centr <- data.frame(cbind(rcentr$index$name,rcentr$index$value,rcentr$index$smooth))

### Primeiro teste de acurácia 

rcentr_1 <- calcAccuracy(hpi_obj = rcentr,
                         test_type = 'rt',
                         test_method = 'kfold',
                         k = 10,
                         seed = 123,
                         in_place = TRUE,
                         in_place_name = 'smooth_is_accuracy',
                         smooth=TRUE)

### Plotagem do teste

plot(rcentr_1$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_centr2 <- createSeries(hpi_obj = rcentr,
                          train_period = 24)

### Plotagem

plot(rt_centr2)

### Plotagem e novos testes de acurácia

rt_centr2 <- smoothSeries(series_obj = rt_centr2,
                          order = 4)


rt_centr2acc <- calcSeriesAccuracy(series_obj = rt_centr2,
                                   test_method = 'insample',
                                   test_type = 'rt')

rt_centr2 <- calcSeriesAccuracy(series_obj = rt_centr2,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = TRUE,
                                in_place = TRUE)

plot(rt_centr2$accuracy_smooth)



tabela_indicecentr <- data.frame(cbind(rt_centr2$accuracy_smooth$error,rt_centr2 $accuracy_smooth$rt_price,rt_centr2 $accuracy_smooth$pred_price,rt_centr2 $accuracy_smooth$log_error))

ggplot(data = tabela_indicecentr, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)

### Índice naocentr

base_naocentr <- left_join(Base, csv_lotes2, match = 'first', by = c('inscri'))

base_naocentr <- base_naocentr %>% filter(valor_instrumento > 20000) %>% filter(valor_financiado > 0)

base_naocentr <- base_naocentr %>% filter(is.na(base_naocentr$Categoria_max) )

base_naocentr <- base_naocentr[!duplicated(base_naocentr$ID),]

rnaocentr <- rtIndex(trans_df = base_naocentr,
                     periodicity = 'quarterly',
                     min_date = '2009-01-01',
                     max_date = '2022-09-01',
                     adj_type = 'clip',
                     date = 'data_transacao',
                     price = 'valor_instrumento',
                     min_period_dist = 4,
                     trans_id = 'ID',
                     prop_id = 'inscricao',
                     seq_only = TRUE,
                     estimator = 'weighted',
                     log_dep = TRUE,
                     trim_model = TRUE,
                     smooth = TRUE,
                     smooth_order = 4) 

### Plotagem do índice suavizado

plot(rnaocentr, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_naocentr <- data.frame(cbind(rnaocentr$index$name,rnaocentr$index$value,rnaocentr$index$smooth))

### Primeiro teste de acurácia 

rnaocentr_1 <- calcAccuracy(hpi_obj = rnaocentr,
                            test_type = 'rt',
                            test_method = 'kfold',
                            k = 10,
                            seed = 123,
                            in_place = TRUE,
                            in_place_name = 'smooth_is_accuracy',
                            smooth=TRUE)

### Plotagem do teste

plot(rnaocentr_1$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_naocentr2 <- createSeries(hpi_obj = rnaocentr,
                             train_period = 24)

### Plotagem

plot(rt_naocentr2)

### Plotagem e novos testes de acurácia

rt_naocentr2 <- smoothSeries(series_obj = rt_naocentr2,
                             order = 4)


rt_naocentr2acc <- calcSeriesAccuracy(series_obj = rt_naocentr2,
                                      test_method = 'insample',
                                      test_type = 'rt')

rt_naocentr2 <- calcSeriesAccuracy(series_obj = rt_naocentr2,
                                   test_method = 'forecast',
                                   test_type = 'rt',
                                   smooth = TRUE,
                                   in_place = TRUE)

plot(rt_naocentr2$accuracy_smooth)



tabela_indicenaocentr <- data.frame(cbind(rt_naocentr2$accuracy_smooth$error,rt_naocentr2 $accuracy_smooth$rt_price,rt_naocentr2 $accuracy_smooth$pred_price,rt_naocentr2 $accuracy_smooth$log_error))

ggplot(data = tabela_indicenaocentr, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)


### Índice municipal

base_municipal <- left_join(Base, csv_lotes2, match = 'first', by = c('inscri'))

base_municipal <- base_municipal %>% filter(Categoria_max == 'Municipal')  %>% filter(valor_instrumento > 20000) %>% filter(valor_financiado > 0)

base_municipal <- base_municipal[!duplicated(base_municipal$ID),]

rmunicipal <- rtIndex(trans_df = base_municipal,
                      periodicity = 'quarterly',
                      min_date = '2009-01-01',
                      max_date = '2022-09-01',
                      adj_type = 'clip',
                      date = 'data_transacao',
                      price = 'valor_instrumento',
                      min_period_dist = 4,
                      trans_id = 'ID',
                      prop_id = 'inscricao',
                      seq_only = TRUE,
                      estimator = 'weighted',
                      log_dep = TRUE,
                      trim_model = TRUE,
                      smooth = TRUE,
                      smooth_order = 4) 

### Plotagem do índice suavizado

plot(rmunicipal, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_municipal <- data.frame(cbind(rmunicipal$index$name,rmunicipal$index$value,rmunicipal$index$smooth))

### Primeiro teste de acurácia 

rmunicipal_1 <- calcAccuracy(hpi_obj = rmunicipal,
                             test_type = 'rt',
                             test_method = 'kfold',
                             k = 10,
                             seed = 123,
                             in_place = TRUE,
                             in_place_name = 'smooth_is_accuracy',
                             smooth=TRUE)

### Plotagem do teste

plot(rmunicipal_1$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_municipal2 <- createSeries(hpi_obj = rmunicipal,
                              train_period = 24)

### Plotagem

plot(rt_municipal2)

### Plotagem e novos testes de acurácia

rt_municipal2 <- smoothSeries(series_obj = rt_municipal2,
                              order = 4)


rt_municipal2acc <- calcSeriesAccuracy(series_obj = rt_municipal2,
                                       test_method = 'insample',
                                       test_type = 'rt')

rt_municipal2 <- calcSeriesAccuracy(series_obj = rt_municipal2,
                                    test_method = 'forecast',
                                    test_type = 'rt',
                                    smooth = TRUE,
                                    in_place = TRUE)

plot(rt_municipal2$accuracy_smooth)



tabela_indicemunicipal <- data.frame(cbind(rt_municipal2$accuracy_smooth$error,rt_municipal2 $accuracy_smooth$rt_price,rt_municipal2 $accuracy_smooth$pred_price,rt_municipal2 $accuracy_smooth$log_error))

ggplot(data = tabela_indicemunicipal, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)



### Juntando a base de lotes com a base de vendas pela coluna de inscrição sem os últimos dígitos (para unir também os apartamentos)

base_financiamento_tipos <- left_join(Base_financiamentos, Cadastro_imobiliario, match = 'first', by = c('inscricao'))

### Resumo por região

base_financiamento_tipos%>%group_by(TIPO_IMOVE)%>%tally()%>%arrange(-n) %>% kable()
Cadastro_imobiliario%>%group_by(TIPO_IMOVE)%>%tally()%>%arrange(-n) %>% kable()

### Criando base de cada região

base_financiamento_apto <- base_financiamento_tipos %>% filter(TIPO_IMOVE == '20 - Apartamento') 
base_financiamento_casas <- base_financiamento_tipos %>% filter(TIPO_IMOVE == '10 - Casa') 

### Índice apto

rt_financiamento_apto <- rtIndex(trans_df = base_financiamento_apto,
                                 periodicity = 'quarterly',
                                 min_date = '2009-01-01',
                                 max_date = '2022-09-01',
                                 adj_type = 'clip',
                                 date = 'data_transacao',
                                 price = 'valor_instrumento',
                                 min_period_dist = 4,
                                 trans_id = 'ID',
                                 prop_id = 'inscricao',
                                 seq_only = TRUE,
                                 estimator = 'weighted',
                                 log_dep = TRUE,
                                 trim_model = TRUE,
                                 smooth = TRUE,
                                 smooth_order = 6) 

### Plotagem do índice suavizado

plot(rt_financiamento_apto, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento_apto <- data.frame(cbind(rt_financiamento_apto$index$name,rt_financiamento_apto$index$value,rt_financiamento_apto$index$smooth))

### Volatilidade do índice

rt_financiamentoVol_apto <- calcVolatility(index = rt_financiamento_apto$index$smooth,
                                           window = 3)

plot(rt_financiamentoVol_apto)

### Primeiro teste de acurácia 

rt_financiamento_apto_2 <- calcAccuracy(hpi_obj = rt_financiamento_apto,
                                        test_type = 'rt',
                                        test_method = 'kfold',
                                        k = 10,
                                        seed = 123,
                                        in_place = TRUE,
                                        in_place_name = 'smooth_is_accuracy',
                                        smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_apto_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes_apto <- createSeries(hpi_obj = rt_financiamento_apto,
                                             train_period = 24)

### Plotagem

plot(rt_financiamento_testes_apto)

### Plotagem e novos testes de acurácia

rt_financiamento_testes_apto_2 <- smoothSeries(series_obj = rt_financiamento_testes_apto,
                                               order = 4)

rt_financiamento_testesVol_apto <- calcSeriesVolatility(series_obj = rt_financiamento_testes_apto_2,
                                                        window = 3,
                                                        smooth = TRUE,
                                                        order = 4)

rt_apto <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_apto,
                              test_method = 'insample',
                              test_type = 'rt')

rt_financiamento_testes_apto <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_apto_2,
                                                   test_method = 'forecast',
                                                   test_type = 'rt',
                                                   smooth = TRUE,
                                                   in_place = TRUE)

plot(rt_financiamento_testes_apto$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado_apto <- calcRevision(series_obj = rt_financiamento_testes_apto)

rt_financiamento_testes_apto <- calcRevision(series_obj = rt_financiamento_testes_apto,
                                             in_place = TRUE,
                                             smooth = TRUE)

plot(rt_financiamento_revisado_apto, measure='median')

tabela_dadosfinanciamentos_apto <- data.frame(cbind(rt_financiamento_apto$data))

tabela_indice_financiamento_apto <- data.frame(cbind(rt_financiamento_testes_apto$accuracy_smooth$error,rt_financiamento_testes_apto$accuracy_smooth$rt_price,rt_financiamento_testes_apto$accuracy_smooth$pred_price,rt_financiamento_testes_apto$accuracy_smooth$log_error))

dxapto4 <- density(tabela_indice_financiamento_apto$X4)

plot(dx44, main = "Densidade dos erros")

dxapto1 <- density(tabela_indice_financiamento_apto$X1)

### Plotagem da densidade dos erros

plot(dxapto1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento_apto, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)


### Índice casas, em oposição ao índice de apartamentos.

rt_financiamento_casas <- rtIndex(trans_df = base_financiamento_casas,
                                  periodicity = 'quarterly',
                                  min_date = '2009-01-01',
                                  max_date = '2022-09-01',
                                  adj_type = 'clip',
                                  date = 'data_transacao',
                                  price = 'valor_instrumento',
                                  min_period_dist = 4,
                                  trans_id = 'ID',
                                  prop_id = 'inscricao',
                                  seq_only = TRUE,
                                  estimator = 'weighted',
                                  log_dep = TRUE,
                                  trim_model = TRUE,
                                  smooth = TRUE,
                                  smooth_order = 6) 

### Plotagem do índice suavizado

plot(rt_financiamento_casas, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_indicefinanciamento_casas <- data.frame(cbind(rt_financiamento_casas$index$name,rt_financiamento_casas$index$value,rt_financiamento_casas$index$smooth))

### Volatilidade do índice

rt_financiamentoVol_casas <- calcVolatility(index = rt_financiamento_casas$index$smooth,
                                            window = 3)

plot(rt_financiamentoVol_casas)

### Primeiro teste de acurácia 

rt_financiamento_casas_2 <- calcAccuracy(hpi_obj = rt_financiamento_casas,
                                         test_type = 'rt',
                                         test_method = 'kfold',
                                         k = 10,
                                         seed = 123,
                                         in_place = TRUE,
                                         in_place_name = 'smooth_is_accuracy',
                                         smooth=TRUE)

### Plotagem do teste

plot(rt_financiamento_casas_2$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_financiamento_testes_casas <- createSeries(hpi_obj = rt_financiamento_casas,
                                              train_period = 24)

### Plotagem

plot(rt_financiamento_testes_casas)

### Plotagem e novos testes de acurácia

rt_financiamento_testes_casas_2 <- smoothSeries(series_obj = rt_financiamento_testes_casas,
                                                order = 4)

rt_financiamento_testesVol_casas <- calcSeriesVolatility(series_obj = rt_financiamento_testes_casas_2,
                                                         window = 3,
                                                         smooth = TRUE,
                                                         order = 4)

rt_casas <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_casas,
                               test_method = 'insample',
                               test_type = 'rt')

rt_financiamento_testes_casas <- calcSeriesAccuracy(series_obj = rt_financiamento_testes_casas_2,
                                                    test_method = 'forecast',
                                                    test_type = 'rt',
                                                    smooth = TRUE,
                                                    in_place = TRUE)

plot(rt_financiamento_testes_casas$accuracy_smooth)

### Cálculo da revisão do índice a partir de informações mais novas

rt_financiamento_revisado_casas <- calcRevision(series_obj = rt_financiamento_testes_casas)

rt_financiamento_testes_casas <- calcRevision(series_obj = rt_financiamento_testes_casas,
                                              in_place = TRUE,
                                              smooth = TRUE)

plot(rt_financiamento_revisado_casas, measure='median')

tabela_dadosfinanciamentos_casas <- data.frame(cbind(rt_financiamento_casas$data))

tabela_indice_financiamento_casas <- data.frame(cbind(rt_financiamento_testes_casas$accuracy_smooth$error,rt_financiamento_testes_casas$accuracy_smooth$rt_price,rt_financiamento_testes_casas$accuracy_smooth$pred_price,rt_financiamento_testes_casas$accuracy_smooth$log_error))

dxcasas4 <- density(tabela_indice_financiamento_casas$X4)

plot(dx44, main = "Densidade dos erros")

dxcasas1 <- density(tabela_indice_financiamento_casas$X1)

### Plotagem da densidade dos erros

plot(dxcasas1, xlim=c(-1,5), main = "Densidade dos erros")

ggplot(data = tabela_indice_financiamento_casas, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)

### Índice estruturacao

base_estruturacao <- left_join(Base, csv_lotes2, match = 'first', by = c('inscri'))

base_estruturacao <- base_estruturacao %>% filter(a_max == 'Estruturacao')  %>% filter(valor_instrumento > 20000)  %>% filter(valor_financiado > 0) 

restruturacao <- rtIndex(trans_df = base_estruturacao,
                         periodicity = 'quarterly',
                         min_date = '2009-01-01',
                         max_date = '2022-09-01',
                         adj_type = 'clip',
                         date = 'data_transacao',
                         price = 'valor_instrumento',
                         min_period_dist = 4,
                         trans_id = 'ID',
                         prop_id = 'inscricao',
                         seq_only = TRUE,
                         estimator = 'weighted',
                         log_dep = TRUE,
                         trim_model = TRUE,
                         smooth = TRUE,
                         smooth_order = 4) 

### Plotagem do índice suavizado

plot(restruturacao, smooth=TRUE)

### Criação de uma tabela para gerar melhores gráficos

tabela_estruturacao <- data.frame(cbind(restruturacao$index$name,restruturacao$index$value,restruturacao$index$smooth))

tabela_terrenao <- data.frame(cbind(restruturacao$data)) %>% mutate(setor_fisc=substring(prop_id,1,2))

base_terrenao <- left_join(tabela_terrenao, Regioes, match = 'first', by = c('setor_fisc'))

base_terrenao%>%group_by(regiao)%>%tally()%>%arrange(-n) %>% kable()

### Primeiro teste de acurácia 

restruturacao_1 <- calcAccuracy(hpi_obj = restruturacao,
                                test_type = 'rt',
                                test_method = 'kfold',
                                k = 10,
                                seed = 123,
                                in_place = TRUE,
                                in_place_name = 'smooth_is_accuracy',
                                smooth=TRUE)

### Plotagem do teste

plot(restruturacao_1$index$smooth_is_accuracy)

### Treinamento do modelo para gerar previsões

rt_estruturacao2 <- createSeries(hpi_obj = restruturacao,
                                 train_period = 24)

### Plotagem

plot(rt_estruturacao2)

### Plotagem e novos testes de acurácia

rt_estruturacao2 <- smoothSeries(series_obj = rt_estruturacao2,
                                 order = 4)


rt_estruturacao2acc <- calcSeriesAccuracy(series_obj = rt_estruturacao2,
                                          test_method = 'insample',
                                          test_type = 'rt')

rt_estruturacao2 <- calcSeriesAccuracy(series_obj = rt_estruturacao2,
                                       test_method = 'forecast',
                                       test_type = 'rt',
                                       smooth = TRUE,
                                       in_place = TRUE)

plot(rt_estruturacao2$accuracy_smooth)


### Gerando muitas tabelas para gerar muitos gráficos

tabela_indiceestruturacao <- data.frame(cbind(rt_estruturacao2$accuracy_smooth$error,rt_estruturacao2 $accuracy_smooth$rt_price,rt_estruturacao2 $accuracy_smooth$pred_price,rt_estruturacao2 $accuracy_smooth$log_error))

ggplot(data = tabela_indiceestruturacao, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)

tabela_indices_todos <- data.frame(cbind(tabela_indicetudo$rt_tudo.index.name,tabela_indicetudo$rt_tudo.index.smooth,tabela_indicefinanciamento$rt_financiamento.index.smooth,tabela_indiceapto$rt_1.index.smooth,tabela_indicefinanciamento_sul$rt_financiamento_sul.index.smooth,tabela_indicefinanciamento_norte$rt_financiamento_norte.index.smooth,tabela_indicefinanciamento_centro$rt_financiamento_centro.index.smooth,tabela_indicefinanciamento_sudeste$rt_financiamento_sudeste.index.smooth,tabela_indicefinanciamento_leste$rt_financiamento_leste.index.smooth,tabela_indicefinanciamento_oeste$rt_financiamento_oeste.index.smooth,tabela_terrenos$rterrenos.index.smooth,tabela_consolidacao$rconsolidacao.index.smooth,tabela_estruturacao$restruturacao.index.smooth,tabela_centr$rcentr.index.smooth,tabela_naocentr$rnaocentr.index.smooth,tabela_indicefinanciamento_casas$rt_financiamento_casas.index.smooth,tabela_indicefinanciamento_apto$rt_financiamento_apto.index.smooth))

colnames(tabela_indices_todos) <- c('Periodo', 'Tudo', 'Financiamento', 'Apartamentos', 'Sul','Norte','Centro','Sudeste','Leste','Oeste','Terrenos','Consolidacao','Estruturacao','Centralidades','NCentralidade', 'Casas','Apto')

tabela_indice_geral <- tabela_indices_todos %>% pivot_longer(cols=c('Tudo','Financiamento','Apartamentos'), names_to='Grupo', values_to='Indice')

ggplot(data = tabela_indice_geral, aes(x=Periodo, y = as.double(Indice), group = Grupo)) + geom_line(aes(color=Grupo), size = 2) + scale_color_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = tabela_indice_tudo, aes(x=X2, y = X3)) + geom_point() + ylim(0,1000000) + xlim(0,1000000)

### Índices para fazermos comparações... PIB, SELIC, IPCA, etc. Algumas taxas anuais foram calculadas na mão.

PIB <- read.csv(text="
Ano	Valor	Deflacionado
2007	21861254	52871069
2008	24884337	56743365
2009	24566242	53451509
2010	25848130	53643715
2011	25518740	49628762
2012	26584749	49279629
2013	28290100	49149777
2014	30343668	49489219
2015	33889622	50757893
2016	41630243	57285185
2017	39396425	52633112
2018	39694815	50801031
2019	43457370	53805089
",sep="\t",colClasses = c("integer","integer"))

Taxas <- read.csv(text="
Periodo	SelicAnual	IPCATrimestral
2009-Q1	11.25	1.23
2009-Q2	9.25	1.32
2009-Q3	8.75	0.63
2009-Q4	8.75	1.06
2010-Q1	8.75	2.06
2010-Q2	10.25	1
2010-Q3	10.75	0.5
2010-Q4	10.75	2.23
2011-Q1	11.75	2.44
2011-Q2	12.25	1.4
2011-Q3	12	1.06
2011-Q4	11	1.46
2012-Q1	9.75	1.22
2012-Q2	8.5	1.08
2012-Q3	7.5	1.42
2012-Q4	7.25	1.99
2013-Q1	7.25	1.94
2013-Q2	8	1.18
2013-Q3	9	0.62
2013-Q4	10	2.04
2014-Q1	10.75	2.18
2014-Q2	11	1.54
2014-Q3	11	0.83
2014-Q4	11.75	1.72
2015-Q1	12.75	3.83
2015-Q2	13.75	2.26
2015-Q3	14.25	1.39
2015-Q4	14.25	2.82
2016-Q1	14.25	2.62
2016-Q2	14.25	1.75
2016-Q3	14.25	1.04
2016-Q4	13.75	0.74
2017-Q1	12.25	0.96
2017-Q2	10.25	0.22
2017-Q3	8.25	0.59
2017-Q4	7	1.14
2018-Q1	6.5	0.7
2018-Q2	6.5	1.89
2018-Q3	6.5	0.72
2018-Q4	6.5	0.39
2019-Q1	6.5	1.51
2019-Q2	6.5	0.71
2019-Q3	5.5	0.26
2019-Q4	4.5	1.77
2020-Q1	3.75	0.53
2020-Q2	2.25	-0.43
2020-Q3	2	1.24
2020-Q4	2	3.13
2021-Q1	2.75	2.05
2021-Q2	4.25	1.68
2021-Q3	6.25	3.02
2021-Q4	9.25	2.96
2022-Q1	11.75	3.2
2022-Q2	13.25	2.22
2022-Q3	13.75	-1.32
",sep="\t",colClasses = c("character", "double","double"))

TaxasAnuais <- read.csv(text="
Periodo	IPCAAnual	ICS-SJC-Anual
2009-Q1	4.4558079537413	31.5422217293621
2009-Q2	5.198327672077	29.5185662113818
2009-Q3	3.86264142750683	33.749225
2009-Q4	4.30645476164009	31.3588739767046
2010-Q1	5.16167907708176	29.3600000000001
2010-Q2	4.82954586246798	27.2497187851519
2010-Q3	4.69412063179999	23.0436662343277
2010-Q4	5.90619386689999	20.6601466992665
2011-Q1	6.30051440059998	19.0553494124923
2011-Q2	6.72150653683998	17.5027624309393
2011-Q3	7.31617363794081	15.4462403373155
2011-Q4	6.50786439700159	14.5694022289767
2012-Q1	5.23941853050078	14.3172521264853
2012-Q2	4.90730202231773	14.3125822832425
2012-Q3	5.28100703644832	15.0413927440954
2012-Q4	5.83096695887406	16.2480839523641
2013-Q1	6.58376577541617	15.7673520390775
2013-Q2	6.68921073562139	14.2590764505868
2013-Q3	5.84764725121496	12.9266098735383
2013-Q4	5.89953844018016	10.4878790952429
2014-Q1	6.14886048477152	8.66941418899028
2014-Q2	6.52653976698658	8.95171354516651
2014-Q3	6.74886707121105	8.47155842938803
2014-Q4	6.41409994593873	7.85366749288532
2015-Q1	8.13247208247036	7.95972730145837
2015-Q2	8.89921799442008	6.96065905987047
2015-Q3	9.50403364528663	6.50107991360689
2015-Q4	10.6882101790049	6.84768268289571
2016-Q1	9.39828689751974	6.54483104717296
2016-Q2	8.852686209883	5.50681659046912
2016-Q3	8.47692489048799	4.56702494423038
2016-Q4	6.28248797381601	2.76427945511026
2017-Q1	4.56324289452801	0.92240059661659
2017-Q2	2.99094056893954	0.987663960024943
2017-Q3	2.53225170060993	0.399519025639039
2017-Q4	2.93936804645314	-0.542635658914747
2018-Q1	2.67427062478041	-0.287803360298722
2018-Q2	4.38516697224978	-1.70860875951914
2018-Q3	4.52007175111842	-3.05980528511822
2018-Q4	3.74500695169842	-2.04598597038194
2019-Q1	4.57950005627512	-1.73180435291367
2019-Q2	3.36835264174566	-0.802296771148769
2019-Q3	2.89625730601093	0.813008130081361
2019-Q4	4.31070929408042	1.38054505669383
2020-Q1	3.30367062687327	2.4966261808367
2020-Q2	2.13431123342043	6.05399833485312
2020-Q3	3.13263184990511	10.6261859582542
2020-Q4	4.51084133517454	14.0412840436386
2021-Q1	6.09103111762221	16.3071680285018
2021-Q2	8.33921908245279	15.0579439252337
2021-Q3	10.2440374345544	12.4213836477988
2021-Q4	10.0623106202048	10.461114934618
2022-Q1	11.3026012347392	9.34940400879007
2022-Q2	11.8937047424768	8.39560725193327
2022-Q3	7.17987559685123	6.60839160839162
",sep="\t",colClasses = c("character", "double","double"))

TaxasAcumuladas <- Taxas %>% mutate(SelicAcumulada = cumprod(((1+(SelicAnual/100)))^(1/4))) %>% mutate (IPCAAcumulado = 100*cumprod(1+(IPCATrimestral/100)))

PIB <- read.csv(text="
Periodo	PIBAcumulado
2009-Q1	1
2009-Q2	0.996794019402666
2009-Q3	0.993588038805331
2009-Q4	0.990382058207997
2010-Q1	1.00330180877294
2010-Q2	1.01622155933788
2010-Q3	1.02914130990283
2010-Q4	1.04206106046777
2011-Q1	1.03874124107748
2011-Q2	1.03542142168718
2011-Q3	1.03210160229688
2011-Q4	1.02878178290659
2012-Q1	1.03952575615541
2012-Q2	1.05026972940423
2012-Q3	1.06101370265305
2012-Q4	1.07175767590187
2013-Q1	1.08894537837329
2013-Q2	1.1061330808447
2013-Q3	1.12332078331612
2013-Q4	1.14050848578753
2014-Q1	1.16120575912822
2014-Q2	1.18190303246891
2014-Q3	1.2026003058096
2014-Q4	1.22329757915029
2015-Q1	1.2590361469462
2015-Q2	1.29477471474211
2015-Q3	1.33051328253802
2015-Q4	1.36625185033393
2016-Q1	1.44426716254354
2016-Q2	1.52228247475316
2016-Q3	1.60029778696278
2016-Q4	1.6783130991724
2017-Q1	1.65579914212819
2017-Q2	1.63328518508399
2017-Q3	1.61077122803978
2017-Q4	1.58825727099558
2018-Q1	1.59126465102494
2018-Q2	1.5942720310543
2018-Q3	1.59727941108365
2018-Q4	1.60028679111301
2019-Q1	1.63820841303855
2019-Q2	1.67613003496408
2019-Q3	1.71405165688962
2019-Q4	1.75197327881515
",sep="\t",colClasses = c("character", "double"))

CUB <- read.csv(text="
Periodo	CUB
2009-Q1	1,0033
2009-Q2	1,0282
2009-Q3	1,0326
2009-Q4	1,0335
2010-Q1	1,0385
2010-Q2	1,0874
2010-Q3	1,0930
2010-Q4	1,0902
2011-Q1	1,0960
2011-Q2	1,1494
2011-Q3	1,1524
2011-Q4	1,1540
2012-Q1	1,1639
2012-Q2	1,2245
2012-Q3	1,2331
2012-Q4	1,2382
2013-Q1	1,2410
2013-Q2	1,3164
2013-Q3	1,3243
2013-Q4	1,3286
2014-Q1	1,3333
2014-Q2	1,3958
2014-Q3	1,4107
2014-Q4	1,4132
2015-Q1	1,4200
2015-Q2	1,4740
2015-Q3	1,4802
2015-Q4	1,4828
2016-Q1	1,4896
2016-Q2	1,5426
2016-Q3	1,5648
2016-Q4	1,5650
2017-Q1	1,5673
2017-Q2	1,5914
2017-Q3	1,5980
2017-Q4	1,6061
2018-Q1	1,6167
2018-Q2	1,6396
2018-Q3	1,6500
2018-Q4	1,6584
2019-Q1	1,6749
2019-Q2	1,7099
2019-Q3	1,7279
2019-Q4	1,7315
2020-Q1	1,7383
2020-Q2	1,7574
2020-Q3	1,8133
2020-Q4	1,8589
2021-Q1	1,9327
2021-Q2	2,0639
2021-Q3	2,1094
2021-Q4	2,1191
2022-Q1	2,1356
2022-Q2	2,2891
2022-Q3	2,3041
",sep="\t", header=T, dec = ",", colClasses = c("character", "numeric"))	


aaaa <- merge(PIB, TaxasAcumuladas, all.y=TRUE)

bbbb <- merge(aaaa,TaxasAnuais,all.y=TRUE)

cccc <- merge(bbbb,CUB,all.y=TRUE)

Tabela <- merge(cccc,tabela_indices_todos)

### Mais tabelas

Tabelao <- Tabela %>% mutate(Tudo = as.numeric(Tudo)) %>% mutate(Tudo = as.numeric(Tudo)) %>% mutate(Financiamento = as.numeric(Financiamento)) %>% mutate(Apartamentos = as.numeric(Apartamentos)) %>% mutate(Sul = as.numeric(Sul)) %>% mutate(Norte = as.numeric(Norte)) %>% mutate(Centro = as.numeric(Centro)) %>% mutate(Centro = as.numeric(Centro)) %>% mutate(Sudeste = as.numeric(Sudeste)) %>% mutate(Leste = as.numeric(Leste)) %>% mutate(Oeste = as.numeric(Oeste))  %>% mutate(Terrenos = as.numeric(Terrenos)) %>% mutate(Consolidacao = as.numeric(Consolidacao)) %>% mutate(Estruturacao = as.numeric(Estruturacao)) %>% mutate(Centralidades = as.numeric(Centralidades)) %>% mutate(Casas = as.numeric(Casas))%>% mutate(Apto = as.numeric(Apto)) %>% mutate(NCentralidade = as.numeric(NCentralidade)) %>% mutate(Data = as.yearqtr(Periodo, format="%Y-Q%q")) %>% mutate(SelicTrimestral = ((100+SelicAnual)/100)^(1/4)) %>% mutate(CUB = as.numeric(CUB))

# Exemplo
tabelao <- Tabelao %>% pivot_longer(cols=c('Tudo','Financiamento','Apartamentos','Sul','Norte','Sudeste','Oeste','Leste','Centro','PIBAcumulado','SelicAnual','IPCATrimestral','SelicAcumulada','IPCAAcumulado',), names_to='Grupo', values_to='Indice')

### Este é um tema para a geração de gráficos mais bonitos e limpos.

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#dedede"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# Gráficos Tudo

ggplot(data = Tabelao) + geom_line(aes(color =" Índice", x = Data, y = Tudo), size= 1.5 ) + geom_line(aes(color =" IPCA", x = Data, y = IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Índice Real", x = Data, y = 100*Tudo/IPCAAcumulado), size= 1.5, linetype = "dashed" ) + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(100, max(Tabelao$Tudo), by = 50)) + xlab("Ano") + ylab("Índices")

ggplot(data = tabela_indice_tudo, aes(x=X2, y = X3)) + geom_point(alpha = 0.3) + theme_Publication() + scale_colour_Publication(name = "Legenda")  + xlab("Valor Observado") + ylab("Valor Previsto") + scale_y_continuous(labels = scales::label_number_si(), breaks = seq(0, 1500000, by = 250000)) + scale_x_continuous(breaks = seq(0, 1500000, by = 250000)) + xlim(0,1500000) + ylim(0,1500000)

# Gráfico CUB

ggplot(data = Tabelao) + geom_line(aes(color ="Cidade", x = Data, y = 100*Financiamento/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="CUB", x = Data, y = 10000*CUB/IPCAAcumulado), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(90, max(Tabelao$Tudo), by = 5)) + xlab("Ano") + ylab("Índices")

# Gráficos Apartamentos

ggplot(data = Tabelao)  + geom_line(aes(color ="ICS - Apartamentos", x = Data, y = 101.23*Apartamentos/IPCAAcumulado), size= 1.5 ) + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(100, max(Tabelao$Tudo), by = 5)) + xlab("Ano") + ylab("Índice")

ggplot(data = tabela_indiceACC, aes(x=X2, y = X3)) + geom_point(alpha = 0.3) + theme_Publication() + scale_colour_Publication(name = "Legenda")  + xlab("Valor Observado") + ylab("Valor Previsto") + scale_y_continuous(labels = scales::label_number_si(), breaks = seq(0, 1500000, by = 250000)) + scale_x_continuous(breaks = seq(0, 1500000, by = 250000)) + xlim(0,1500000) + ylim(0,1500000)

# Gráficos Financiamentos

ggplot(data = Tabelao)  + geom_line(aes(color ="ICS - Finanaciamentos", x = Data, y = 101.23*Financiamento/IPCAAcumulado), size= 1.5 ) + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(100, max(Tabelao$Tudo), by = 5)) + xlab("Ano") + ylab("Índice")

ggplot(data = tabela_indice_financiamento, aes(x=X2, y = X3)) + geom_point(alpha = 0.3) + theme_Publication() + scale_colour_Publication(name = "Legenda")  + xlab("Valor Observado") + ylab("Valor Previsto") + scale_y_continuous(labels = scales::label_number_si(), breaks = seq(0, 1500000, by = 250000)) + scale_x_continuous(breaks = seq(0, 1500000, by = 250000)) + xlim(0,1500000) + ylim(0,1500000)

# Gráfico regiões geral

ggplot(data = Tabelao) + geom_line(aes(color ="Cidade", x = Data, y = 100*Financiamento/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Sul", x = Data, y = 100*Sul/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Norte/Centro", x = Data, y = 100*Centro/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Leste/Sudeste", x = Data, y = 100*Leste/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Oeste", x = Data, y = 100*Oeste/IPCAAcumulado), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(100, max(Tabelao$Tudo), by = 5)) + xlab("Ano") + ylab("Índices")

# Gráfico macrozonas

ggplot(data = Tabelao) + geom_line(aes(color ="Cidade", x = Data, y = 100*Financiamento/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Estruturação", x = Data, y = 100*Sul/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Consolidação", x = Data, y = 100*Centro/IPCAAcumulado), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(100, max(Tabelao$Tudo), by = 5)) + xlab("Ano") + ylab("Índices")

# Gráfico Centralidades

ggplot(data = Tabelao[40:55,]) + geom_line(aes(color ="Cidade", x = Data, y =Financiamento*17629/(251*IPCAAcumulado)), size= 1.5 ) + geom_line(aes(color ="É Centralidade", x = Data, y = Centralidades*17629/(201*IPCAAcumulado)), size= 1.5 ) + geom_line(aes(color ="Não Centralidade", x = Data, y = NCentralidade*17629/(257*IPCAAcumulado)), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1))  + xlab("Ano") + ylab("Índices") + scale_x_yearqtr(breaks = seq(from = Tabelao$Data[40], to = max(Tabelao$Data),by = 0.5),format = "%YQ%q")+ scale_y_continuous(breaks = seq(94,112, by = 2)) + xlab("Ano")

# Gráfico SELIC E ICS

ggplot(data = Tabelao) + geom_line(aes(color ="Cidade", x = Data, y = 100*Financiamento/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="SELIC REAL", x = Data, y = 10000*SelicAcumulada/IPCAAcumulado), size= 1.5 ) + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(100, max(Tabelao$Tudo), by = 5)) + xlab("Ano") + ylab("Índices")

# Gráfico SELIC, IPCA e diferencial

ggplot(data = Tabelao)  + geom_line(aes(color ="SELIC", x = Data, y = SelicAnual), size= 1.5 )  + geom_line(aes(color ="IPCA", x = Data, y = IPCAAnual), size= 1.5 )  + geom_line(aes(color ="SELIC REAL", x = Data, y = 100*(((1+(SelicAnual/100))/(1+(IPCAAnual/100)))-1)), size= 1.5, linetype = "dashed" ) + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(-10, 30, by = 5)) + xlab("Ano") + ylab("Índices (em %)")

# Gráfico SELIC E PIB 

ggplot(data = Tabelao) + geom_line(aes(color ="Cidade", x = Data, y = ICS.SJC.Anual), size= 1.5 ) + geom_line(aes(color ="SELIC REAL", x = Data, y = 100*(((1+(SelicAnual/100))/(1+(IPCAAnual/100)))-1)), size= 1.5 ) + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(-10, 30, by = 5)) + xlab("Ano") + ylab("Índices (em %)")

# Gráfico Casas e Apartamentos

ggplot(data = Tabelao) + geom_line(aes(color ="Cidade", x = Data, y = 100*Financiamento/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Horizontal", x = Data, y = 100*Casas/IPCAAcumulado), size= 1.5 ) + geom_line(aes(color ="Vertical", x = Data, y = 100*Apto/IPCAAcumulado), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + scale_x_yearqtr(breaks = seq(from = min(Tabelao$Data), to = max(Tabelao$Data),by = 1),format = "%YQ%q") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_y_continuous(breaks = seq(100, max(Tabelao$Tudo), by = 5)) + xlab("Ano") + ylab("Índices")

####Tentativa de Preços Hedônicos

base_cs_financiamento_2 <- base_cs_financiamento[!(is.na(base_cs_financiamento$TIPO_CONST)), ]

base_cs_financiamento_2 <- base_cs_financiamento_2[!(base_cs_financiamento_2$TIPO_CONST)=="20 - Apartamento", ]

base_cs_financiamento_2 <- base_cs_financiamento_2 %>% filter(valor_instrumento < 9999999)%>% filter(AREA_LOTE < 1000)

hed_1 <- hedIndex(trans_df = base_cs_financiamento_2,
                  periodicity = 'quarterly',
                  min_date = '2009-01-01',
                  max_date = '2022-09-01',
                  adj_type = 'clip',
                  date = 'data_transacao',
                  price = 'valor_instrumento',
                  min_period_dist = 4,
                  trans_id = 'ID',
                  prop_id = 'inscricao',
                  estimator = 'robust',
                  log_dep = TRUE,
                  trim_model = TRUE,
                  max_period = 48,
                  dep_var = 'price',
                  ind_var = c('AREA_LOTE', 'AREA_CONST', 'as.factor(SETOR)'),
                  smooth = TRUE,
                  smooth_order = 4)

plot(hed_1,smooth=TRUE)

### Atualizando os valores dos imóveis, ou seja, multiplicando o valor da venda pelo índice atualizado, de forma a gerar uma base para fazemos a krigagem universal no QGis

base_cs_financiamento_3 <- base_cs_financiamento[!(is.na(base_cs_financiamento$TIPO_IMOVE)), ]

base_cs_financiamento_3%>%group_by(TIPO_IMOVE)%>%tally()%>%arrange(-n) %>% kable()

base_cs_financiamento_3%>%group_by(TIPO_CONST)%>%tally()%>%arrange(-n) %>% kable()

base_cs_financiamento_4 <- base_cs_financiamento_3 %>% filter(is.na(TIPO_CONST) | substring(TIPO_CONST,1,2)!= ("30") )

base_cs_financiamento_5 <- base_cs_financiamento_4 %>% filter(is.na(TIPO_CONST) | substring(TIPO_CONST,1,2)!= ("40") )

base_cs_financiamento_6 <- base_cs_financiamento_5 %>% mutate (Data = as.yearqtr(data_transacao))

base_cs_financiamento_7 <- left_join(base_cs_financiamento_6 %>% select(inscricao, valor_instrumento, inscri, TIPO_IMOVE, TIPO_CONST, Data), Tabelao %>% select(Financiamento, IPCAAcumulado, Data), by = c("Data" = "Data"))

base_cs_financiamento_8 <- base_cs_financiamento_7[!(is.na(base_cs_financiamento_7$Financiamento)), ]

base_cs_financiamento_8 <- base_cs_financiamento_8 %>% mutate(ValorReal = Financiamento/ IPCAAcumulado)

base_cs_financiamento_9 <- base_cs_financiamento_8 %>% mutate(ValorAtualizado = valor_instrumento*(max(Financiamento)/Financiamento))

base_financiamento_krigagem <- left_join(base_cs_financiamento_9, Cadastro_imobiliario, match = 'first', by = c('inscricao')) %>% mutate (Valorm2 = ValorAtualizado/ `Área Total Construída`)

base_financiamento_valorm2 <- left_join(base_cs_financiamento_9, Cadastro_imobiliario, match = 'first', by = c('inscricao')) %>% mutate (Valorm2 = ValorAtualizado/ `Área Total Construída`) %>% filter(Valorm2 > 1500 & Valorm2 < 15000)


write.csv(base_financiamento_krigagem, "G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados_Qgis/Dados_krigagem.csv")
          
write.csv(base_financiamento_valorm2, "G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados_Qgis/Dados_valorm2.csv")

write.csv(Cadastro_imobiliario, "G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/MercadoImob/2022/Dados_Qgis/Cadastro.csv")

### Aqui fizemos uma estimação da renda familiar a partir dos dados da RAIS (empregos formais) de 2020, que era a mais atualizada até então. Isso é um "chute", iremos refazer com os dados do Censo de 2022, que devem estar para sair, e darão um panorama melhor. De qualquer forma, precisávamos desse parâmetro para calcular a capacidade de compra das famílias, e, acredito, que não ficou fora da realidade. Para publicarmos, utilizaremos os dados do censo, daí ficará excelente.

RAIS2020Vinculos <- read.csv("G:/SEURBS/DEPTO_PLANEJAMENTO_URBANO/Pesquisa/_Dados/_microdados/MTE/2020/RAIS/rais_sjc2020.txt",sep=";",encoding="latin1",dec=",")

RAIS2020Vinculos1 <- clean_names(RAIS2020Vinculos[,c(-1,-2,-3,-11)]) %>% mutate(CBO1=substr(cbo_ocupacao_2002,1,1)) 

RAIS2020Vinculos2 <- RAIS2020Vinculos1%>%mutate(vl_remun_media_nom_def=vl_remun_media_nom*1.172728797)

dist20 <- RAIS2020Vinculos2 %>% filter(vinculo_ativo_31_12 == 1 & !is.na(vl_remun_media_nom_def) & vl_remun_media_nom_def != 0) %>% mutate(fx_sm = cut(vl_remun_media_nom_def,breaks=c(1:250000)))  %>% group_by(fx_sm) %>% tally() %>% mutate(cum = cumsum(n)/sum(n)) %>% arrange(-cum)  %>% mutate(m=as.integer(fx_sm)) %>%  mutate(OBOI = ifelse((m > 1) & (m < 2424), m*100, ifelse((m > 2425) & (m < 9999999), m*80, "no"))) %>% mutate (cum1.4 = m*1.4)

OOOOOOO <- dist20 %>% mutate(aeee_sm = cut(cum*100,breaks=c(0:25000)))  %>% group_by(aeee_sm)

tabela_valoresmedios <- OOOOOOO %>% group_by(aeee_sm) %>% summarise(Salario_medio=mean(m),.groups = 'drop')  %>% mutate(Percentil = as.integer(aeee_sm))

####(100*100/m

ggplot() + geom_line(data = dist20, aes(x=cum, y = cum1.4, color = "2020"), size=1)  + scale_y_continuous(limits=c(0,25000),breaks=c(1212,2424,6060,12120,24240)) + scale_x_continuous(breaks=seq(0,1,by=0.1),labels=scales::percent) + labs(x = "Acumulado famílias", y = "Média mensal (em R$)") + theme_Publication() + scale_color_brewer(palette="Set1", name= "Ano")  + geom_hline(yintercept=4806, linetype="dashed", color = "green") + geom_hline(yintercept=5752, linetype="dashed", color = "black")  + geom_hline(yintercept=10059, linetype="dashed", color = "blue") 

### Aqui, a partir da renda das famílias, calculei a capacidade de compra de cada percentil no simulador imobiliário da caixa em 28/12/2022.

Caixa_28do12 <- read.csv(text="
Percentil	Caixa20	Caixa50
1	0	0
2	70479,9	98517,84
3	77532,525	109802,04
4	83642,125	119577,4
5	88126,5125	126752,42
6	91613,9625	132332,34
7	94356,5	136720,4
8	96824,4125	140669,06
9	98991,3625	144136,18
10	100237,3625	146129,78
11	102314,025	149452,44
12	104029,525	152197,24
13	105684,8375	154845,74
14	107370,25	157542,4
15	108995,4625	160142,74
16	110680,8625	162839,38
17	112366,275	165536,04
18	113871,1	167943,76
19	115285,6375	170207,02
20	116670,0875	172422,14
21	118024,425	174589,08
22	119348,675	176707,88
23	120552,5375	178634,06
24	121575,825	180271,32
25	122779,675	182197,48
26	123983,5375	184123,66
27	125127,2125	185953,54
28	126300,975	187831,56
29	127504,8375	189757,74
30	128768,9	191780,24
31	129478,2375	193302,18
32	130265,675	194920,28
33	131120,3125	196641,7
34	132009,1375	198433,42
35	132963,2625	200344,62
36	133892,1875	202194,5
37	134851,2	204092,52
38	135821,225	206001,56
39	136765,05	207848,28
40	137757,075	209779,32
41	139119,3875	212314,82
42	139873,6	213864,16
43	141000,1	216019,96
44	142158,7	218225,92
45	143332,3	220446,88
46	144560,0875	222758,14
47	145884,275	225238,04
48	147268,65	227814,24
49	148732,3	230523,88
50	150323,45	233451,32
51	143379,8875	222633,82
52	145031,1	225672,96
53	146810,35	228924,76
54	148680,2375	232317,98
55	145857,275	228180,44
56	147873	231787,2
57	149854,1	235308,56
58	151998,4625	239094,74
59	154696,6375	243824,02
60	157536,6625	248762,86
61	160594,1625	254037,66
62	163780,2875	259491,86
63	157574,9375	249854,3
64	160973,7125	255610,34
65	164678,9375	261832,1
66	168751,7125	268612,54
67	173282,675	276090,08
68	178243,725	284204,76
69	183603,1	292896,16
70	169552,85	270421,76
71	174879,3375	278953,74
72	179156,325	285806,52
73	184081,15	293694,64
74	188934,7375	301468,78
75	194806,35	310873,56
76	187020,2875	299232,46
77	193990,5375	310384,86
78	201759,925	322815,88
79	205482,25	328771,6
80	220051,2875	352082,06
81	230773,05	369236,88
82	243048,6875	388877,9
83	243302,975	389284,76
84	255746,175	409193,88
85	269046,975	430475,16
86	284855	455768
87	301828,15	482925,04
88	320825	513320
89	340705	545128
90	345951,25	553522
91	371862,5	594980
92	399606,0125	639369,62
93	429288,75	686862
94	461951,25	739122
95	505391,25	808626
96	563762,5	902020
97	632737,5	1012380
98	722839,9375	1156543,9
99	853378,525	1365405,64
100	1195780	1913248
",sep="\t", dec = ",", colClasses = c("numeric", "numeric","numeric"))

tabela_caixa <- merge(Caixa_28do12,tabela_valoresmedios) %>% mutate(Percentil1 = Percentil/100) %>% mutate (Caixa20 = Caixa20*1.3)%>% mutate (Caixa50 = Caixa50*1.3)

ggplot(data = tabela_caixa) + geom_line(aes(color ="Cidade", x = Salario_medio, y = Caixa20), size= 1.5 ) + geom_line(aes(color ="Horizontal", x = Salario_medio, y = Caixa50), size= 1.5 )   + theme_Publication() + scale_colour_Publication(name = "Legenda") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("Ano") + ylab("Índices") + scale_x_continuous(breaks = seq(from = 0, to = 30000,by = 1000)) + scale_x_continuous(breaks = seq(from = 0, to = 35000,by = 1000)) + scale_y_continuous(breaks = seq(from = 0, to = 2000000,by = 100000))

###Gráfico comparando capacidade de financiamento e valor dos imóveis

tabela_financiamento_percentual <- base_financiamento_valorm2  %>% mutate(faixa_valor = cut(ValorAtualizado,breaks=c(1:5000000)))  %>% group_by(faixa_valor) %>% tally() %>% mutate(cum = cumsum(n)/sum(n)) %>% arrange(-cum)  %>% mutate(ValorAtualizado=as.integer(faixa_valor)) %>% na.omit()


tabela_financiamento_percentual2 <- tabela_financiamento_percentual %>% mutate(Percentis = cut(cum*100,breaks=c(0:25000)))  %>% group_by(Percentis)

tabela_financiamento_percentual3 <- tabela_financiamento_percentual2 %>% group_by(Percentis) %>% summarise(Valor_medio=mean(ValorAtualizado),.groups = 'drop')  %>% mutate(Percentil = as.integer(Percentis)) %>% mutate (Percentil1 = Percentil/100)

tabela_caixa <- merge(tabela_caixa,tabela_financiamento_percentual3)

ggplot(data = tabela_caixa) + geom_line(aes(color ="Máximo - entrada de 20%", x = Percentil1, y = Caixa20), size= 1.5 ) + geom_line(aes(color ="Máximo - entrada de 50%", x = Percentil1, y = Caixa50), size= 1.5 ) + theme_Publication() + scale_colour_Publication(name = "Legenda") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("Percentis") + ylab("Valores") + scale_x_continuous(breaks = seq(from = 0, to = 1,by = 0.1),labels=scales::percent) + scale_y_continuous(labels = label_number(suffix = " mil", scale = 1e-3),breaks = seq(from = 0, to = 3000000,by = 100000))

ggplot(data = tabela_caixa) + geom_line(aes(color ="Máximo - entrada de 20%", x = Percentil1, y = Caixa20), size= 1.5 ) + geom_line(aes(color ="Máximo - entrada de 50%", x = Percentil1, y = Caixa50), size= 1.5 ) + geom_line(aes(color ="Valor dos imóveis", x = Percentil1, y = Valor_medio), size= 1.5 )   + theme_Publication() + scale_colour_Publication(name = "Legenda") +  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("Percentis") + ylab("Valores") + scale_x_continuous(breaks = seq(from = 0, to = 1,by = 0.1),labels=scales::percent) + scale_y_continuous(labels = label_number(suffix = " mil", scale = 1e-3),breaks = seq(from = 0, to = 3000000,by = 100000))

##### Aqui são apenas os gráficos explicativos do slide, os valores foram apenas para gerá-los.

OfertaDemanda <- read.csv(text="
Preco	Oferta	Demanda	OfertaImob	Demanda2	OfertaImob2
0	0	10	4.5	12	5.4
1	1	9	4.6	11	5.5
2	2	8	4.7	10	5.6
3	3	7	4.8	9	5.7
4	4	6	4.9	8	5.8
5	5	5	5	7	5.9
6	6	4	5.1	6	6
7	7	3	5.2	5	6.1
8	8	2	5.3	4	6.2
9	9	1	5.4	3	6.3
10	10	0	5.5	2	6.4
11			5.6	1	6.5
12			5.7	0	6.6

",sep="\t",colClasses = c("double", "double","double", "double","double","double"))

ggplot(data = OfertaDemanda) + geom_line(aes(color ="Oferta", x = Oferta, y = Preco), size= 1.5 ) + geom_line(aes(color ="Demanda", x = Demanda, y = Preco), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + xlab("Quantidade") + ylab("Preço") + scale_x_discrete(labels = NULL, breaks = NULL) + scale_y_discrete(labels = NULL, breaks = NULL)

ggplot(data = OfertaDemanda) + geom_line(aes(color ="Oferta no Mercado Imobiliário", x = OfertaImob, y = Preco), size= 1.5 ) + geom_line(aes(color ="Oferta", x = Oferta, y = Preco), size= 1.5 ) + geom_line(aes(color ="Demanda", x = Demanda, y = Preco), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + xlab("Quantidade") + ylab("Preço") + scale_x_discrete(labels = NULL, breaks = NULL) + scale_y_discrete(labels = NULL, breaks = NULL)

ggplot(data = OfertaDemanda) + geom_line(aes(color ="Oferta Mercado Imobiliário", x = OfertaImob, y = Preco), size= 1.5 ) + geom_line(aes(color ="Demanda", x = Demanda, y = Preco), size= 1.5 )  + geom_line(aes(color ="Demanda 2", x = Demanda2, y = Preco), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + xlab("Quantidade") + ylab("Preço") + scale_x_discrete(labels = NULL, breaks = NULL) + scale_y_discrete(labels = NULL, breaks = NULL)

ggplot(data = OfertaDemanda) + geom_line(aes(color ="Oferta", x = OfertaImob, y = Preco), size= 1.5 ) + geom_line(aes(color ="Demanda", x = Demanda, y = Preco), size= 1.5 )+ geom_line(aes(color ="Oferta 2", x = OfertaImob2, y = Preco), size= 1.5 )  + geom_line(aes(color ="Demanda 2", x = Demanda2, y = Preco), size= 1.5 )  + theme_Publication() + scale_colour_Publication(name = "Legenda") + xlab("Quantidade") + ylab("Preço") + scale_x_discrete(labels = NULL, breaks = NULL) + scale_y_discrete(labels = NULL, breaks = NULL)

### Desenvolvido por Augusto Maganha Barbosa (mestre em economia pelo IE/UNICAMP) e Marcelo da Silva Reis (bacharel em Ciências Sociais pelo IFCH/UNICAMP). Ambos trabalham na Divisão de Pesquisa do Departamento de Planejamento Urbano da Prefeitura Municipal de São José dos Campos.