#remotes::install_github("liibre/coronabr")
library(rdt)

library(coronabr)
dados <- get_corona_br(by_uf = TRUE)
plot_corona_br(df = dados, log = FALSE)
plot_corona_br(df = dados, tipo = "aumento")
plot_uf(df = dados, n = 7)
plot_uf(df = dados,
        tipo = "casos",
        prop_pop = TRUE,
        estados = c("AM", "CE", "PE", "SP"))
library(tmap)
# carregando o shapefile do mundo
data("World") # from tmap
# criando vetor com paises fora da latinoamerica e caribe (lac)
fora <- c("Canada", "United States", "Greenland")
# selecionando apenas paises lac
lac <- World[World$continent %in% c("South America", "North America")
             & !World$name %in% fora, ]
dados_jhu <- get_corona_jhu()
# checando se todos paises lac entao em jhu
lac$name[!lac$name %in% dados_jhu$country_region]
# vamos alterar os nomes em lac para bater com jhu
lac$country_region <- as.character(lac$name)
lac$country_region[lac$country_region == "Dominican Rep."] <- "Dominican Republic"
# selecionando apenas países da lac
dados_lac <- dados_jhu[dados_jhu$country_region %in% lac$country_region, ]
# agregando dados por país
df_lac <- aggregate(confirmed ~ country_region, data = dados_lac, FUN = sum)
#
