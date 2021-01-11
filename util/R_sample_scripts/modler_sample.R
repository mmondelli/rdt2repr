library(rdt)
pdir <- './prov_samples'
prov.init(save.debug = T, prov.dir = './prov_samples', annotate.inside.functions = T, overwrite = F)
library(modleR)
library(sp)

species <- names(example_occs)
occs <- example_occs[[1]]

test_folder <- "~/Dropbox/Reproducibility/prov2repr/prov_samples/modler_results"
sdmdata_1sp <- modleR::setup_sdmdata(species_name = species[1],
                                    occurrences = occs,
                                    predictors = example_vars,
                                    models_dir = test_folder,
                                    partition_type = "crossvalidation",
                                    cv_partitions = 5,
                                    cv_n = 1,
                                    buffer_type = "mean",
                                    png_sdmdata = F,
                                    n_back = 500,
                                    clean_dupl = FALSE,
                                    clean_uni = FALSE,
                                    clean_nas = FALSE,
                                    geo_filt = FALSE,
                                    geo_filt_dist = 10,
                                    select_variables = TRUE,
                                    sample_proportion = 0.5,
                                    cutoff = 0.7)
many <- modleR::do_many(species_name = species[1],
                       predictors = example_vars,
                       models_dir = test_folder,
                       png_partitions = F,
                       write_bin_cut = FALSE,
                       write_rda = TRUE,
                       bioclim = FALSE,
                       domain = FALSE,
                       glm = FALSE,
                       svmk = FALSE,
                       svme = FALSE,
                       maxnet = F,
                       rf = F,
                       mahal = FALSE,
                       brt = TRUE,
                       equalize = TRUE)
#file_in('./results/')),
fm <- modleR::final_model(species_name = species[1],
                         algorithms = NULL, #if null it will take all the algorithms in disk
                         models_dir = test_folder,
                         which_models = c("raw_mean",
                                          "bin_mean",
                                          "bin_consensus"),
                         consensus_level = 0.5,
                         uncertainty = TRUE,
                         overwrite = TRUE, png_final = F)
ens <- modleR::ensemble_model(species_name = species[1],
                             occurrences = occs,
                             performance_metric = "pROC",
                             which_ensemble = c("average",
                                                "best",
                                                "frequency",
                                                "weighted_average",
                                                "median",
                                                "pca",
                                                "consensus"),
                             consensus_level = 0.5,
                             which_final = "raw_mean",
                             models_dir = test_folder,
                             overwrite = TRUE, png_ensemble = F)


