library(Rocc)
species <- c("Asplenium truncorum")
data_splink <- rspeciesLink(species = species, filename = "specieslink")
data_gbif <- rgbif2(species = species, filename = "gbif")
species_name_raw <- unique(data_splink$scientificName)
species_name_check <- check_string(species_name_raw)
species_name_check
species_ok <- species_name_check[species_name_check$speciesStatus %in% 
    "possibly_ok", ]
suggest_taxa <- suggest_flora(species_ok$species)
search_taxa <- unique(suggest_taxa$species)
list_taxa <- lapply(search_taxa, check_flora)
list_taxa[[1]]$taxon
list_taxa[[1]]$synonyms
list_taxa <- lapply(search_taxa, check_flora)
list_taxa[[1]]$taxon
list_taxa[[1]]$synonyms
