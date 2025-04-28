#rode todo o script de tratamento de dados antes de rodar esse


###########################################################################################

#cria o mapa de maiores mortes
ggplot() +
  geom_sf(data= maior_causa_por_local, color = "white", aes(fill = causa)) + 
  labs(
    title = "Maior causa de morte por munipio(2020-2022)",
    fill = "CID"
  )+
  theme_minimal()


#criar mapa ano a ano
ggplot() +
  geom_sf(data= maior_causa_por_local, color = "white", aes(fill = causa)) + 
  facet_wrap(~Ano)+
  labs(
    title = "Maior causa de morte por munipio",
    fill = "CID"
  )+
  theme_minimal()

#criar mapa de densiade populcional por residencia
ggplot() +
  geom_sf(data= mapa_população, color = "white", aes(fill = mapa_população$`Média Total de moradores por domicílio particular ocupado`)) + 
  labs(title = "Densidade populacional por cidade - ano base 2022",
    fill = "Média de pessoas por residência"
  ) +
  theme_minimal()

#cria map de mortes por 100mil
ggplot() +
  geom_sf(data= mortes_covid_percapita, color = "white", aes(fill = log_mortes_percapita)) + 
  labs(title = "Mortes per capita de Covid"
    ,
    fill = " 
                 Escala em log
(o resultado está em numeros negativos
    quanto maior o numero, mais mortes)"
  ) +
  theme_minimal()

#criar mapa dos PIBs dos municipios
ggplot() +
  geom_sf(data= Dados_pib_2019_grafico, color = "white", aes(fill = PIB_log)) + 
  labs(title = "PIB municipios - Ano Base 2019" ,
    fill = "PIB em escala Log"
  ) +
  theme_minimal()
  
###########################################################################################
#criaçaão de fator
dados = subset(dados_processo, select = c(
  "RACACOR",
  "SEXO"
))
dados$Cor = factor(dados_processo$RACACOR)
dados$sexo = factor(dados_processo$SEXO)



#plot de graficos base
dados_agrupados <- dados_ajustados %>%
  count(RACACOR)

# Certifique-se de que 'n' está no formato numérico
dados_agrupados <- dados_agrupados %>%
  mutate(n = as.numeric(n))  # Converte 'n' para numérico, se necessário


ggplot(dados_agrupados, aes(x = RACACOR, y = n, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  labs(title = "Quantidade de mortes por Etnia - (2020-2022)",
       x = "Etnia",
       y = "Quantidade",
       fill= "Etnia") +
  theme_minimal() +
  scale_y_continuous(labels = comma) + # Formata o eixo Y com separadores de milhares
  scale_fill_brewer(palette = "Set2")  # Paleta de cores (opcional)

table(dados$sexo,dados$Cor)

#########################################################################################

Contagem_mortes_covid = mortes_Covid %>%
  count(RACACOR)

ggplot(Contagem_mortes_covid, aes(x = RACACOR, y = n, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  labs(title = "Quantidade de mortes por Etnia de Covid- (2020-2022)",
       x = "Etinia",
       y = "Quantidade",
       fill= "Etnia") +
  theme_minimal() +
  scale_y_continuous(labels = comma) + # Formata o eixo Y com separadores de milhares
  scale_fill_brewer(palette = "Set2")  # Paleta de cores (opcional)




ggplot(mortes_Covid, aes(x= Ano))+
  geom_bar(fill = "lightblue")+
  labs(y = "T o t a l", title = "Total de Mortes de Covid por Ano")+
  scale_x_continuous(breaks = c(2020,2021,2022))+
  scale_y_continuous(breaks = seq(from= 0, to= 110000, by = 10000), limits = c(0,110000))+
  geom_hline(yintercept = seq(0,110000, by=10000), linetype = "dashed", color = "gray") +
  theme_minimal()
  
ggplot(Total_vacinas_por_ano, aes(x=Ano, y=Dados_vacina ))+
  geom_bar(fill = "lightblue", stat = "identity")+
  labs(y = "T o t a l", title = "Vacinas Aplicadas")+
  scale_x_continuous(breaks = c(2021,2022,2023,2024))+
  scale_y_continuous(labels = comma)+
  geom_hline(yintercept = seq(0,350000000, by=50000000), linetype = "dashed", color = "gray")+
  theme_minimal()

##########################################################################################


