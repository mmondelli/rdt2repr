library(getLattes)
length(xmlsLattes)
getDadosGerais(xmlsLattes[[2]])
lt <- lapply(xmlsLattes, getDadosGerais)
