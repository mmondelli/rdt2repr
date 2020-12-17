# Loading libs ####
library(rdflib)
library(dplyr)
library(tidyr)
library(tibble)
library(rjson)
library(purrr)
library(gtools)
library(lobstr)
library(rlang)
library(dplyr)
library(testwhat)
library(provParseR)

# Namespace ####
namespace <- c("http://purl.org/net/p-plan/#",
               "https://w3id.org/reproduceme#",
               "http://www.w3.org/ns/prov/#",
               "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
               "http://www.w3.org/2001/XMLSchema#")
names(namespace) <- c('p-plan', 'repr', 'prov', 'rdf', 'xsd')
nsp <- as.data.frame(t(namespace))

rdf <- rdf()

# Output file  ####
rdf_file <- 'repr_modler.ttl'
rdf_file <- 'repr_rocc.ttl'
rdf_file <- 'repr_rocc_modler.ttl'
rdf_file <- 'repr_corona.ttl'

# Read prov.json file  ####
#prov_json <- '~/Dropbox/Reproducibility/prov2repr/prov_rocc/prov_console_2020-11-24T18.17.24-03/prov.json' #rocc_sample
prov_json <- '~/Dropbox/Reproducibility/prov2repr/prov_samples/prov_console_2020-12-16T19.44.32-03/prov.json' # modler TESTED mas nao os args ainda
prov_json <- '~/Dropbox/Reproducibility/prov2repr/prov_samples/prov_console_2020-12-16T19.57.02-03/prov.json' # rocc TESTED E ARGS
prov_json <- '~/Dropbox/Reproducibility/prov2repr/prov_samples/prov_rocc_modler/prov.json' # rocc and modler
prov_json <- '/home/mondelli/Dropbox/Reproducibility/prov2repr/prov_samples/prov_console_2020-12-16T21.42.50-03/prov.json' # corona

prov <- prov.parse(prov_json)

# Read script file and create a data frame with each line ####
script_file_path <- as.character(get.scripts(prov)[1])
process_file <- function(filepath) {
    con <- file(filepath, "r")
    df <- data.frame(cmd = character())
    while ( TRUE ) {
        line <- readLines(con, 1)
        if ( length(line) == 0 ) {
            break
        }
        df[nrow(df)+1,] <- line
    }
    close(con)
    return(df)
}
script_file <- process_file(script_file_path)

# Get base and default packages and functions #####
get_default_functions <- function(){
    default_packages <- as.data.frame(c('package:base', 'package:ggplot2', 'package:utils', 'package:datasets',
                                        'package:graphics', 'package:grDevices', 'package:methods', 'package:stats'))
    names(default_packages) <- 'name'
    env_packages <- as.data.frame(search()); env_packages[,'idx'] <- 1:nrow(env_packages) # To get their order
    default_packages <- env_packages[which(env_packages$`search()` %in% default_packages$name),]

    default_functions <- c()
    for (i in 1:nrow(default_packages)){
        default_functions <- rbind(default_functions,
                                   cbind(default_packages[i,],
                                         'function' = objects(default_packages$`search()`[i]), row.names = NULL))
    }
    default_functions <- default_functions[order(default_functions$'function', default_functions$idx, decreasing=F),]
    default_functions_clean <- default_functions[!duplicated(default_functions$'function'),]
    names(default_functions_clean)[1] <- 'package'; names(default_functions_clean)[3] <- 'function_name'
    return(default_functions_clean)
}
df_func <- get_default_functions()

# Test if it is function or operation ####
check <- function(cmd) {
    ops <- read.csv('./operators.csv')
    func_nodes <- get.func.nodes(prov)
    lang <- str2lang(cmd)

    repr_function <- paste0(nsp$repr, 'Function')
    repr_operation <- paste0(nsp$repr, 'Operation')

    func_call <- ''
    if (class(lang) == "<-") {
        func_call <- gsub('.*:', '', as.character(lang[[3]][1]))
        r <- list(repr_operation, ops[ops$symbol == lang[[1]], 3])
    }
    if (class(lang) == "call") {
        func_call <- call_name(lang)
    }

    if (func_call %in% ops$symbol || func_call %in% c('[[', '[', '$')) { # Se for assignment, então operação <-
        r <- list(repr_operation, ops[ops$symbol == lang[[1]], 3])
    } else if (func_call %in% c(df_func$function_name, func_nodes$name)) {
        r <- list(repr_function, func_call)
    } else if (class(lang) == 'logical') {
        r <- list(repr_operation, 'Console')
    }
    return(r)
}

# GET AND WRITE Script #####
get_script <- function(prov = prov, rdf = rdf, nsp = nsp){
    # TODO: try with more than one script
    script <- get.scripts(prov)
    script$name <- paste0(nsp$repr, file_path_sans_ext(basename(script$script)))
    script[,paste0(nsp$rdf, 'type')] <- paste0(nsp$repr, 'Script')
    env <- get.environment(prov)
    script[,paste0(nsp$repr, 'hasProgrammingLanguage')] <- env[env$label == 'language', 2]
    script[,paste0(nsp$repr, 'hasProgrammingLanguageVersion')] <- env[env$label == 'langVersion', 2]
    script[,paste0(nsp$repr, 'hasOperatingSystem')] <- env[env$label == 'operatingSystem', 2]

    script_triple <- script %>%
        mutate(subject = name) %>%
        select(-'script', -'timestamp', -'name') %>%
        gather(key = predicate, value = object, -subject)

    invisible(apply(script_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))
    return(script)
}
script_df <- get_script(prov, rdf, nsp)

# GET entire expression ####
get_expression <- function(prov = prov, script_df = script_file, activities = df){
    #activities <- get.proc.nodes(prov)
    for (i in 1:nrow(activities)){
        range <- activities$endLine[i] - activities$startLine[i]
        if (!is.na(activities$startLine[i])) {
            if (range >= 1) {
                activities[i, 'cmd'] <- gsub('\\s+', ' ', paste(script_df[activities$startLine[i]:activities$endLine[i],], collapse = ''))
            } else {
                activities[i, 'cmd'] <- gsub('\\s+', ' ', paste(script_df[activities$startLine[i],]))
            }

        } else { activities[i, 'cmd'] <- NA }
    }
    return(activities)
}

# GET Activities ####
get_activities <- function(prov = prov, script_file = script_file){
    activities <- get.proc.nodes(prov)
    informants <- get.proc.proc(prov)
    activities <- get_expression(activities = activities, script_df = script_file) # Get the complete expression
    activities <- merge(activities, informants, by.x = 'id', by.y = 'informed', all.x = T)[,-11] # Ger informants ids
    activities <- activities[mixedorder(activities$id),] # Ordering by rdt id
    for(i in 1:nrow(activities)) { # Get what each activity does
        check_result <- check(activities$cmd[i])
        activities[i, c(paste0(nsp$rdf, 'type'))] <- check_result[[1]]
        activities[i, 'detail'] <- ifelse(length(check_result) > 1, check_result[[2]], list('Not found'))
    }

    functions <- which(activities[,paste0(nsp$rdf, 'type')] == paste0(nsp$repr, 'Function'))
    ops <- which(activities[,paste0(nsp$rdf, 'type')] == paste0(nsp$repr, 'Operation'))

    activities <- rbind(
        activities[functions,] %>%
            group_by(detail) %>%
            mutate(activation = paste0(nsp$repr,detail,'_activation', row_number())),
        activities[ops,] %>%
            mutate(activation = paste0(nsp$repr,'operation', row_number()))
    )
    activities <- activities[mixedorder(activities$id),] # Ordering by rdt id again
    for(j in 2:nrow(activities)){
        slc <- activities[j,]
        activities[j, paste0(nsp$repr, 'informedBy')] <- activities[which(activities$id == slc$informant), c('activation')]
    }

    return(activities)
}
activities_df <- get_activities(prov, script_file = script_file)

# GET AND WRITE Functions ####
get_functions <- function(prov = prov, activities = activities_df, rdf = rdf, df_func = df_func, script = script_file){

    functions <- get.func.lib(prov = prov) # Get functions
    libs <- get.libs(prov) # Get packages
    func_activities <- activities[
        which(activities[,paste0(nsp$rdf, 'type')] == paste0(nsp$repr, 'Function')),] # Get all functions from activities
    other_func <- unique(setdiff(func_activities$detail, functions$'function')) # Check which functions does not exist on rdt func nodes
    other_func <- df_func[which(df_func$function_name %in% other_func), c('function_name', 'package')] # Get the names of the default packages
    other_func$func_id <- paste0('f', seq(nrow(functions)+1, nrow(functions)+nrow(other_func))) # Add these func with new ids
    other_func$library <- libs[which(libs$name %in% gsub('.*:', '', other_func$package)), 'id'] # Check the lib id from rdt
    names(functions)[2] <- 'function_name'; functions <- rbind(functions, other_func[-2]);  # Join existing func with default
    functions[,paste0(nsp$rdf, 'type')] <- paste0(nsp$repr, 'Function') # Add ontologies info
    functions$function_name <- paste0(nsp$repr, functions$function_name)

    functions[,paste0(nsp$`p-plan`, 'isSubPlanOfPlan')] <- script$name

    # Create function triple
    # TODO: add impl
    function_triple <- functions %>%
        mutate(subject = function_name) %>%
        select(-'func_id', -'function_name', -'library') %>% # Removing
        gather(key = predicate, value = object, -subject)

    # Insert into rdf graph
    invisible(apply(function_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))

    # Create pckg triple
    pckg <- merge(functions, libs, by.x = 'library', by.y = 'id')[,c(3,6)]
    pckg$name <- paste0(nsp$repr, pckg$name); names(pckg)[2] <- paste0(nsp$repr, 'hasPackage')
    function_pckg_triple <- pckg %>%
        mutate(subject = function_name) %>%
        select(-'function_name') %>% # Removing
        gather(key = predicate, value = object, -subject)
    invisible(apply(function_pckg_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))

    return(functions)
}
func_df <- get_functions(prov, activities_df, rdf, df_func)

# GET AND WRITE Function Activation ####
get_functions_activations <- function(activities = activities_df, rdf = rdf, nsp = nsp) {

    func_act <- which(activities[,paste0(nsp$rdf, 'type')] == paste0(nsp$repr, 'Function'))
    # Create function activation triple
    func_act <- activities[func_act, c('cmd', 'detail', 'activation', paste0(nsp$repr, 'informedBy'))]
    func_act$detail <- paste0(nsp$repr, func_act$detail)
    func_act[,paste0(nsp$rdf, 'type')] <- paste0(nsp$repr, 'FunctionActivation')
    names(func_act)[1] <- paste0(nsp$rdf, 'value')
    names(func_act)[2] <- paste0(nsp$repr, 'isStepOfPlan')

    function_act_triple <- unique(func_act) %>%
        mutate(subject = activation) %>%
        select(-'activation') %>% #removing function_name (it will be the identifier)
        gather(key = predicate, value = object, -subject)

    invisible(apply(function_act_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))

    return(func_act)
}
func_act_df <- get_functions_activations(activities_df, rdf, nsp)

# GET AND WRITE Operations ####
get_ops <- function(activities = activities_df, rdf = rdf, nsp = nsp) {
    # TODO ajustar o tipo
    ops <- which(activities[,paste0(nsp$rdf, 'type')] == paste0(nsp$repr, 'Operation'))
    ops <- activities[ops, c('cmd', 'detail', 'activation', paste0(nsp$repr, 'informedBy'), paste0(nsp$rdf, 'type'))]
    names(ops)[1] <- paste0(nsp$rdf, 'value')

    ops_triple <- unique(ops) %>%
        mutate(subject = activation) %>%
        select(-'activation') %>% #removing function_name (it will be the identifier)
        gather(key = predicate, value = object, -subject)

    invisible(apply(ops_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))
    return(ops)
}
ops_df <- get_ops(activities_df, rdf, nsp)

# GET AND WRITE Packages ####
get_packages <- function(prov_list = prov, rdf = rdf, nsp = nsp){
    libs <- get.libs(prov)
    libs[,paste0(nsp$rdf, 'type')] <- paste0(nsp$repr, 'Package') # Add ontologies info
    libs$name <- paste0(nsp$repr, libs$name)
    names(libs)[3] <- paste0(nsp$repr, 'hasPackageVersion')
    package_triple <- libs %>%
        mutate(subject = name) %>%
        select(-'id', -'name') %>% # Remove rdt.id and pkg_name
        gather(key = predicate, value = object, -subject)

    invisible(apply(package_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))
    return(libs)
}
pckgs_df <- get_packages(prov, rdf, nsp)

# GET AND WRITE Variables #####
get_entity <- function(prov = prov, rdf = rdf, nsp = nsp) {
    data <- get.data.nodes(prov)
    #data[which(data$type == 'URL'),1] <- basename(df[which(data$rdt.type == 'URL'),1]) # TODO: treat urls!!!

    data$is_output <- data$id %in% get.proc.data(prov)$entity # Test if each id is generated by an activity
    data$is_input <- data$id %in% get.data.proc(prov)$entity # Test if each id is used by an activity

    # Check what are the outputs (only)
    outputs <- which(data$is_output == T & data$is_input == F) # Only outputs
    data[outputs, paste0(nsp$rdf, 'type')] <- paste0(nsp$repr, 'Output')

    # If its output and input, define as Variable
    inputs <- which((data$is_output == T & data$is_input == T) | (data$is_output == F & data$is_input == T))
    data[inputs, paste0(nsp$rdf, 'type')] <- paste0(nsp$`p-plan`, 'Variable')

    data$name <- paste0(nsp$repr, gsub("[.]", "_", data$name)) # Removing dots from names
    colnames(data)[5] <- paste0(nsp$repr, 'hasType') # Renaming col to ontologies (type do repr hasType)

    # Create output triple
    data_triple <- data[,c('name', paste0(nsp$repr, 'hasType'), paste0(nsp$rdf, 'type'))] %>% # Get rdt.name, hasType and type
        mutate(subject = name) %>%
        select(-'name') %>% # Removing rdt.name (it will be the identifier)
        gather(key = predicate, value = object, -subject)
    # Insert into rdf graph

    invisible(apply(data_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3], subjectType = as.character(NA))))

    return(data)
}
entities_df <- get_entity(prov, rdf, nsp)

# GET AND WRITE generated
get_generated <- function(prov = prov, rdf = rdf, entities = entities_df, activities = activities_df, nsp = nsp){

    gen <- merge(get.proc.data(prov), activities[,c('id', 'activation')], by.x = 'activity', by.y = 'id')
    gen <- merge(gen, entities[,c('id', 'name')], by.x = 'entity', by.y = 'id')

    colnames(gen)[5] <- paste0(nsp$prov, 'generated')
    generated_triple <- gen[, c('activation', paste0(nsp$prov, 'generated'))] %>%
        mutate(subject = activation) %>%
        select(-'activation') %>%
        gather(key = predicate, value = object, -subject)

    invisible(apply(generated_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))
    return(gen)
}
gen_df <- get_generated(prov, rdf, entities_df, activities_df, nsp)

# GET AND WRITE used
get_used <- function(prov = prov, rdf = rdf, entities = entities_df, activities = activities_df, nsp = nsp){

    used <- merge(get.data.proc(prov), activities[,c('id', 'activation')], by.x = 'activity', by.y = 'id')
    used <- merge(used, entities[,c('id', 'name')], by.x = 'entity', by.y = 'id')
    used <- used[which('https://w3id.org/reproduceme#package_rds' != used$name),] # Removing packages saved as files by rdt
    colnames(used)[5] <- paste0(nsp$prov, 'used')
    used_triple <- used[, c('activation', paste0(nsp$prov, 'used'))] %>%
        mutate(subject = activation) %>%
        select(-'activation') %>%
        gather(key = predicate, value = object, -subject)

    invisible(apply(used_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))
    return(used)
}
used_df <- get_used(prov, rdf, entities_df, activities_df, nsp)

# Serialize ####
rdf_serialize(rdf, rdf_file, format = "turtle", namespace = namespace)

