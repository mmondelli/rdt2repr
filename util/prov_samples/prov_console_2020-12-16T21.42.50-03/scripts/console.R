library(coronabr)
dados <- get_corona_br(by_uf = TRUE)
plot_corona_br(df = dados, log = FALSE)
plot_corona_br(df = dados, tipo = "aumento")
plot_uf(df = dados, n = 7)
plot_uf(df = dados, tipo = "casos", prop_pop = TRUE, estados = c("AM", 
    "CE", "PE", "SP"))
library(tmap)
data("World")
fora <- c("Canada", "United States", "Greenland")
lac <- World[World$continent %in% c("South America", "North America") & 
    !World$name %in% fora, ]
dados_jhu <- get_corona_jhu()
lac$name[!lac$name %in% dados_jhu$country_region]
lac$country_region <- as.character(lac$name)
lac$country_region[lac$country_region == "Dominican Rep."] <- "Dominican Republic"
dados_lac <- dados_jhu[dados_jhu$country_region %in% lac$country_region, 
    ]
df_lac <- aggregate(confirmed ~ country_region, data = dados_lac, 
    FUN = sum)
