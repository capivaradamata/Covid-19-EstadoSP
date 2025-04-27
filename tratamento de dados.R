#AVISOS GERAIs
#Não rode os codigos que contem aviso, use a base da dados baixada, baixar de novo
#a base de dados e tratala de novo pode demorar muito, uns 20 minutos
#Rode primeiro o script de tratamento de dados, depois pode rodar o de graficos
#Não rode tudo de uma vez, rode linha por linha, em especial as primeiras linhas

setwd("C:/Users/astol/Desktop/CS")

install.packages("remotes")
install.packages("read.dbc", repos = "https://packagemanager.posit.co/cran/2024-07-05")
remotes::install_github("rfsaldanha/microdatasus")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("leaflet")
install.packages("sf")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("geobr")
install.packages("scales")
install.packages("RColorBrewer")



library(microdatasus)
library(dplyr)
library(tidyverse)
library(leaflet)
library(sf)
library(ggplot2)
library(lubridate)
library(readxl)
library(factoextra)
library(FactoMineR)
library(geobr)
library(scales)
library(RColorBrewer)

#apartir dessa linha não rode o scrip, siga para o proximo comentário que identifica onde pode rodar
####################################################################################################
#baixar dados do sus e transforma-los
dados <- fetch_datasus(year_start = 2020, year_end = 2022, uf = "SP", information_system = "SIM-DO")
dados_processo <- process_sim(dados)

dados_processo$munResNome = tools::toTitleCase(dados_processo$munResNome)

dados_processo$munResNome[dados_processo$munResNome == "Aparecida d'Oeste"] <- "a"
dados_processo$munResNome[dados_processo$munResNome == "Estrela d'Oeste"] <- "b"
dados_processo$munResNome[dados_processo$munResNome == "Guarani d'Oeste"] <- "c"
dados_processo$munResNome[dados_processo$munResNome == "Palmeira d'Oeste"] <- "d"
dados_processo$munResNome[dados_processo$munResNome == "Santa Bárbara d'Oeste"] <- "e"
dados_processo$munResNome[dados_processo$munResNome == "Santa Clara d'Oeste"] <- "f"
dados_processo$munResNome[dados_processo$munResNome == "Santa Rita d'Oeste"] <- "g"
dados_processo$munResNome[dados_processo$munResNome == "São João Do Pau d'Alho"] <- "h"



dados_processo$munResNome[dados_processo$munResNome == "a"] <- "Aparecida D'oeste"
dados_processo$munResNome[dados_processo$munResNome == "b"] <- "Estrela D'oeste"
dados_processo$munResNome[dados_processo$munResNome == "c"] <- "Guarani D'oeste"
dados_processo$munResNome[dados_processo$munResNome == "d"] <- "Palmeira D'oeste"
dados_processo$munResNome[dados_processo$munResNome == "e"] <- "Santa Bárbara D'oeste"
dados_processo$munResNome[dados_processo$munResNome == "f"] <- "Santa Clara D'oeste"
dados_processo$munResNome[dados_processo$munResNome == "g"] <- "Santa Rita D'oeste"
dados_processo$munResNome[dados_processo$munResNome == "h"] <- "São João Do Pau D'alho"
dados_processo$munResNome[dados_processo$munResNome == "Embu"] <- "Embu das artes"
dados_processo$munResNome[dados_processo$munResNome == "moji Mirim"] <- "Mogi Mirim"

saveRDS(dados_processo, file = "C:/Users/astol/Desktop/CS/dados_processo.RDS")

#Rode apartir daqui, acima dessa linha nao rode, está apenas para demosntrar o que foi feito

dados_processo <- readRDS("C:/Users/astol/Desktop/CS/dados_processo.RDS")
Dados_populacionais = read_excel("C:/Users/astol/Desktop/aulacs/Agregados_por_municipios_basico_BR.xlsx")
Dados_vacina = read_excel("C:/Users/astol/Desktop/aulacs/Vacinas_aplicadas.xlsx")
Dados_pib_2019 = read_excel("C:/Users/astol/Desktop/aulacs/PIB_municipios.xlsx")
######################################################################################

#carregar base de dados das geometrias da cidade e trata-las

mapa_cidades = read_municipality(code_muni = "SP")


mapa_cidades$name_muni = tools::toTitleCase(mapa_cidades$name_muni)


mapa_cidades$name_muni[mapa_cidades$name_muni == "Aparecida D'oeste"] <- "a"
mapa_cidades$name_muni[mapa_cidades$name_muni == "Estrela D'oeste"] <- "b"
mapa_cidades$name_muni[mapa_cidades$name_muni == "Guarani D'oeste"] <- "c"
mapa_cidades$name_muni[mapa_cidades$name_muni == "Palmeira D'oeste"] <- "d"
mapa_cidades$name_muni[mapa_cidades$name_muni == "Santa Bárbara D'oeste"] <- "e"
mapa_cidades$name_muni[mapa_cidades$name_muni == "Santa Clara D'oeste"] <- "f"
mapa_cidades$name_muni[mapa_cidades$name_muni == "Santa Rita D'oeste"] <- "g"
mapa_cidades$name_muni[mapa_cidades$name_muni == "São João Do Pau D'alho"] <- "h"



# Invertendo o processo
mapa_cidades$name_muni[mapa_cidades$name_muni == "a"] <- "Aparecida D'oeste"
mapa_cidades$name_muni[mapa_cidades$name_muni == "b"] <- "Estrela D'oeste"
mapa_cidades$name_muni[mapa_cidades$name_muni == "c"] <- "Guarani D'oeste"
mapa_cidades$name_muni[mapa_cidades$name_muni == "d"] <- "Palmeira D'oeste"
mapa_cidades$name_muni[mapa_cidades$name_muni == "e"] <- "Santa Bárbara D'oeste"
mapa_cidades$name_muni[mapa_cidades$name_muni == "f"] <- "Santa Clara D'oeste"
mapa_cidades$name_muni[mapa_cidades$name_muni == "g"] <- "Santa Rita D'oeste"
mapa_cidades$name_muni[mapa_cidades$name_muni == "h"] <- "São João Do Pau D'alho"
mapa_cidades$name_muni[mapa_cidades$name_muni == "Embu"] <- "Embu Das Artes"
mapa_cidades$name_muni[mapa_cidades$name_muni == "Moji Mirim"] <- "Mogi Mirim"


#####################################################################################


#######################################################################################
#seleciona apenas as causas e nome do municipo, mais leve para trabalhar

Mortes_por_cidade_loc <- dados_processo %>%
  mutate(Ano = year(DTOBITO)) %>%
  select(CAUSABAS, munResNome, Ano ) %>% # Seleciona apenas as colunas necessárias
  rename(
    causa = CAUSABAS,    # Renomeia 'CAUSABAS' para 'causa'
    local = munResNome
  )

#######################################################################################



#Junta as duas tabelas para poder criar o mapa, tazendo o arquvio SHP


juntada = merge(mapa_cidades, Mortes_por_cidade_loc, by.x = "name_muni", by.y = "local")

#######################################################################################


# vai contar qual é a maior causa de mortes por cidade
maior_causa_por_local <- juntada %>%
  group_by(geom, name_muni, causa, Ano) %>%  # Agrupar por geometria, local e causa
  summarise(
    qtd_obitos = n(),              # Contar o número de ocorrências
    .groups = "drop"               # Remover o agrupamento após a sumarização
  ) %>%
  arrange(name_muni,Ano, desc(qtd_obitos)) %>%  # Ordenar por local e quantidade de óbitos
  group_by(name_muni,Ano) %>%  # Agrupar novamente por local
  filter(row_number() == 1)  # Selecionar a maior causa por local


#######################################################################################
#criaçaão dos dados para criação de graficos

dados = subset(dados_processo, select = c(
  "RACACOR",
  "SEXO",
  "CAUSABAS",
  "DTOBITO",
  "ESC"
))
dados$Cor = factor(dados_processo$RACACOR)
dados$sexo = factor(dados_processo$SEXO)

dados_ajustados = subset(dados,
                          RACACOR != "NA's",
                          SEXO != "NA's"
                         )
dados_ajustados = dados_ajustados %>%
  mutate(Ano = year(DTOBITO))
##########################################################################
#Filtrar por mortes de covid apenas

mortes_Covid = dados_ajustados %>%
  filter(CAUSABAS == "B342")

  
total_Mortes_COvid_por_Ano = mortes_Covid %>%
  count(Ano)

##########################################################################
#tratamento dados de vacinas

Dados_vacina = Dados_vacina %>%
  mutate(Ano = year(`Data da Vacina`))

Total_vacinas_por_ano = Dados_vacina %>%
  group_by(Ano) %>%
  summarise(Dados_vacina = sum(`Total de Doses Aplicadas`))


#######################################################################
#tratamento populacional

mapa_população = merge(mapa_cidades, Dados_populacionais, by.x = "name_muni", by.y = "NM_MUN")

########################################################################
#tratamento mortes per capita de covid

mortes_covid_percapita = Mortes_por_cidade_loc %>%
  filter(causa == "B342")

mortes_covid_percapita_PIB = mortes_covid_percapita %>%
  group_by(local, causa, Ano) %>%  # Agrupar por geometria, local e causa
  summarise(
    qtd_obitos = n(),              # Contar o número de ocorrências
    .groups = "drop"               # Remover o agrupamento após a sumarização
  )
  
mortes_covid_percapita = mortes_covid_percapita %>%
  group_by(local, causa, Ano) %>%  # Agrupar por geometria, local e causa
  summarise(
    qtd_obitos = n(),              # Contar o número de ocorrências
    .groups = "drop"               # Remover o agrupamento após a sumarização
  )



mortes_covid_percapita = merge(mapa_cidades, mortes_covid_percapita, by.x ="name_muni", by.y= "local", all.x = TRUE)


mortes_covid_percapita = merge(mortes_covid_percapita, Dados_populacionais, by.x = "name_muni", by.y = "NM_MUN", all.x = TRUE)

mortes_covid_percapita$mortes_percapita_PIB = mortes_covid_percapita$qtd_obitos / mortes_covid_percapita$População

mortes_covid_percapita$mortes_percapita = mortes_covid_percapita$qtd_obitos / mortes_covid_percapita$População

mortes_covid_percapita$log_mortes_percapita = log(mortes_covid_percapita$qtd_obitos / mortes_covid_percapita$População)


mortes_covid_percapita = mortes_covid_percapita %>%
  select(geom,qtd_obitos,name_muni,Ano,causa,mortes_percapita,`Média Total de moradores por domicílio particular ocupado`,log_mortes_percapita)

#apresenta a correlação entre ocupação das residenncias de uma ciade e a quantidade de mortes por pessoas na cidade
cor(mortes_covid_percapita$mortes_percapita,mortes_covid_percapita$`Média Total de moradores por domicílio particular ocupado`)

#para ser mais visivel os dados será multimplicado por cem mil, ou seja mortes por 100mil habitantes
#não é mortalidade, mas o total de mortes a cada 100k
mortes_covid_percapita$mortes_percapita = mortes_covid_percapita$mortes_percapita*100000
summary(mortes_covid_percapita$mortes_percapita)


####################################################################################################################
#analise de correspondência de sexo e escolaridade das mortes por covid
mortes_Covid_Analise = dados_ajustados %>%
  filter(CAUSABAS == "B342")%>%
  select(RACACOR,ESC)


tabela = table(mortes_Covid_Analise$RACACOR,mortes_Covid_Analise$ESC)


CA_1 <- CA(tabela, graph = FALSE)
summary(CA_1)
fviz_screeplot(CA_1, addlabels = TRUE)
fviz_ca_biplot(CA_1, repel = TRUE,axes = c(1,2), title="Biplot - Mortes por covid")

###############################################################################################################
#calculando idade das pessoas que morrerm

dados_idade = dados_processo %>%
  filter(CAUSABAS =="B342") %>%
  select(DTOBITO,DTNASC)

dados_idade$obito = as.Date(dados_idade$DTOBITO, format = "%Y-%m-%d")
dados_idade$nasc = as.Date(dados_idade$DTNASC, format = "%Y-%m-%d")



idade = as.numeric(floor(difftime(dados_idade$obito,dados_idade$nasc, units = "days")/365.25))

summary(idade)
#tratar as idades
idade = idade[!is.na(idade)]

hist(idade, probability = TRUE, main = "Histograma das idades dos obtios por Covid")
lines(density(idade),lty=1, lwd=1, col=c("red"))


###############################################################################################################
#quantos de cada profissão que teve a profissão registrada na ficha de obito
ocupacoes = dados_processo %>%
  filter(CAUSABAS =="B342") %>%
  select(OCUP)

ocupacoes = subset(ocupacoes, OCUP != "NA's")

ocupacoes = ocupacoes %>%
  select(OCUP)

contagem_ocupações = table(ocupacoes)

contagem_ocupações = sort(contagem_ocupações, decreasing =  TRUE)

contagem_ocupações = head(contagem_ocupações, 10)

contagem_ocupações

plot(contagem_ocupações)

###############################################################################################################
#dados em relação ao PIB

Dados_pib_2019_grafico = merge(mapa_cidades, Dados_pib_2019, by.x = "name_muni", by.y = "Municipio")
Dados_pib_2019_grafico$PIB_log = log(Dados_pib_2019_grafico$PIB, base = 10)

Dados_pib_2019_tratado = merge(Dados_populacionais, Dados_pib_2019, by.x = "NM_MUN", by.y = "Municipio")


mortes_covid_percapita_2020 = mortes_covid_percapita_PIB %>%
  filter(Ano == "2020")
mortes_covid_percapita_2020 = subset(mortes_covid_percapita_2020, 
                                     local !=  "Município Ignorado - SP")

Dados_pib_2019_tratado = merge(mortes_covid_percapita_2020, Dados_pib_2019_tratado, by.x = "local", by.y = "NM_MUN", all.x = TRUE)
Dados_pib_2019_tratado$PIB =  as.integer(Dados_pib_2019_tratado$PIB)

#analisando a correlançao entre as mortes percapita e o pib percapita
cor((Dados_pib_2019_tratado$qtd_obitos/Dados_pib_2019_tratado$População),(Dados_pib_2019_tratado$PIB/Dados_pib_2019_tratado$População))

