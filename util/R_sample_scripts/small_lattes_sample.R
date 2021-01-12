library(getLattes)
length(xmlsLattes)
# to import from one curriculum
getDadosGerais(xmlsLattes[[2]])

# to import from two or more curricula
lt <- lapply(xmlsLattes, getDadosGerais)
