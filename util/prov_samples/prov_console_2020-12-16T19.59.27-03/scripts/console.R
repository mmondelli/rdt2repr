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
library(modleR)
library(sp)
species <- names(example_occs)
occs <- example_occs[[1]]
test_folder <- "~/Dropbox/Reproducibility/rdt_new/modler_results"
sdmdata_1sp <- modleR::setup_sdmdata(species_name = species[1], 
    occurrences = occs, predictors = example_vars, models_dir = test_folder, 
    partition_type = "crossvalidation", cv_partitions = 5, cv_n = 1, 
    buffer_type = "mean", png_sdmdata = F, n_back = 500, clean_dupl = FALSE, 
    clean_uni = FALSE, clean_nas = FALSE, geo_filt = FALSE, geo_filt_dist = 10, 
    select_variables = TRUE, sample_proportion = 0.5, cutoff = 0.7)
