#' Start provenance collection
#'
#' This function starts rdt to collect provenance.
#' It collects provenance at the maximum detail level (3) allowed
#' by rdt and saves debug files.
#' @import rdt
#' @export
collectprov <- function(){
    message('Please inform the filepath to save the provenance data (example: /home/user/provenace_test/):')
    dir = readline(prompt="Press [enter] to continue")
    rdt::prov.set.detail(3)
    rdt::prov.init(prov.dir = dir, annotate.inside.functions = F, save.debug = T, overwrite = F)
    message('Provenance collection started.')
}

#'
#' Finish provenance collection
#'
#' This function finish rdt provenance collection.
#' @import rdt
#' @export
endprov <- function(){
    rdt::prov.quit()
    message('Provenance collection completed.')
}

#' Parse the prov.json file
#'
#' This function parses a prov json file.
#'
#' @param rdf_file Path to the output ttl file
#' @import tidyr
#' @importFrom gtools mixedorder
#' @importFrom rdflib rdf_add rdf_serialize rdf
#' @importFrom rlang call_name
#' @importFrom dplyr mutate select group_by row_number
#' @import provParseR
#' @export
parsetordt <- function(rdf_file = 'output.ttl'){
    message('Please inform the filepath to the provenance (prov.json) file (example: /home/user/provenace_test/prov.json):')
    prov_file = readline(prompt="Press [enter] to continue")
    message(paste0('Parsing:', prov_file))
    namespace <- c("http://purl.org/net/p-plan/#",
                   "https://w3id.org/reproduceme#",
                   "http://www.w3.org/ns/prov/#",
                   "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                   "http://www.w3.org/2001/XMLSchema#")
    names(namespace) <- c('p-plan', 'repr', 'prov', 'rdf', 'xsd')
    nsp <- as.data.frame(t(namespace))
    rdf <- rdflib::rdf()

    prov <- provParseR::prov.parse(prov_file)
    script_file_path <- as.character(provParseR::get.scripts(prov)[1])

    script_file <- .process_file(script_file_path)
    df_func <- .get_default_functions()

    script_df <- .get_script(prov, rdf, nsp)

    activities_df <- .get_activities(prov, nsp, script_file, df_func)

    func_df <- .get_functions(prov, nsp, activities_df, rdf, df_func, script_file)

    func_act_df <- .get_functions_activations(activities_df, nsp, rdf)

    ops_df <- .get_ops(activities_df, nsp, rdf)

    pckgs_df <- .get_packages(prov, nsp, rdf)

    entities_df <- .get_entity(prov, nsp, rdf)

    gen_df <- .get_generated(prov, entities_df, activities_df, nsp, rdf)

    used_df <- .get_used(prov, entities_df, activities_df, nsp, rdf)

    args_df <- .get_args(prov, activities_df, nsp, rdf)

    rdf_serialize(rdf, rdf_file, format = "turtle", namespace = namespace)

    return(activities_df)
}

#' Get the contents of the R script.
#'
#' This function takes the path of the R script as a parameter and returns a dataframe with the content of the script.
#' Each line of the datrafame corresponds to one line of the script.
#' The execution of a command can be spread over more than one line.
#'
#' @param filepath Path to the script file
.process_file <- function(filepath) {
    con <- file(filepath, "r")
    df <- data.frame(cmd = character())
    while ( TRUE ) {
        line <- readLines(con, 1)
        if (length(line) == 0) {
            break
        }
        df[nrow(df)+1,] <- line
    }
    close(con)
    return(df)
}

#' Get R default functions.
#'
#' This function builds a dataframe with functions from R's default packages (base, ggplot2, utils, datasets, graphics, grDevices, methods and stats).
#' This is done to associate functions and packages that rdt does not record in the provenance json file
#' but that can be used throughout the script.
#'
.get_default_functions <- function(){
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

#' Check if an activity is a function or operation
#'
#' This function increases the detail level of the activities recorded by rdt.
#'
#' @param prov Parsed provenance
#' @param nsp List with the namespaces of used ontologies
#' @param cmd Command (activity) to check
#' @param df_func Dataframe with R default functions
.check <- function(prov = prov, nsp = nsp, cmd, df_func = df_func) {
    #ops <- load('./data/ops.rda')
    #ops_file <- system.file("data", "operators.csv", package = "mypackagename")
    #ops <- read.csv('~/Dropbox/Reproducibility/prov2repr/rdt2repr/data/operators.csv')
    load(file = "R/ops.rda")
    func_nodes <- get.func.nodes(prov)
    lang <- str2lang(cmd)

    repr_function <- paste0(nsp$repr, 'Function')
    repr_operation <- paste0(nsp$repr, 'Operation')

    func_call <- ''
    r <- ''
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
    } else { r <- list(repr_operation, 'Print to console') }
    return(r)
}

#' Get the executed script and write the triple
#'
#' This function get the script recorded by rdt.
#' It also get a more detailed provenance, including the programming language, its version and operating system
#' in which it was executed.
#' The function builds a dataframe with the script name and its details, and then
#' turn this dataframe into a triple that is added to the rdf object.
#'
#' @param prov Parsed provenance
#' @param rdf RDF object to write the triples
#' @param nsp List with the namespaces of used ontologies
.get_script <- function(prov = prov, rdf = rdf, nsp = nsp){
    # TODO: try with more than one script
    script <- provParseR::get.scripts(prov)
    script$name <- paste0(nsp$repr, tools::file_path_sans_ext(basename(script$script)))
    script[,paste0(nsp$rdf, 'type')] <- paste0(nsp$repr, 'Script')
    env <- provParseR::get.environment(prov)
    script[,paste0(nsp$repr, 'hasProgrammingLanguage')] <- env[env$label == 'language', 2]
    script[,paste0(nsp$repr, 'hasProgrammingLanguageVersion')] <- env[env$label == 'langVersion', 2]
    script[,paste0(nsp$repr, 'hasOperatingSystem')] <- env[env$label == 'operatingSystem', 2]

    script_triple <- script %>%
        mutate(subject = name) %>%
        select(-'script', -'timestamp', -'name') %>%
        tidyr::gather(key = predicate, value = object, -subject)

    invisible(apply(script_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))
    return(script)
}

#' Get the entire expression of a command#' It then creates the triples with properties of the function, which script
#' the function relates to (isSubPlanofPlan), and which package it belongs to (hasPackage).
#'
#' This function gets all the lines corresponding to the execution of an activity
#' and concatenates into one line.
#' The entire expression will be saved as rdf:value of each function activation or operation.
#'
#' @param prov Parsed provenance
#' @param script_df Script dataframe built with the function process_file
#' @param activities Dataframe containing the prov activities recorded by rdt
.get_expression <- function(prov = prov, script_df = script_file, activities = df){
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

#' Get activities
#'
#' This function gets all the activities recorded by rdt.
#' It gets the expression (entire command) of the activity, checks if its
#' function or operation, and gets the exeution flow through the informedBy property from rdt.
#'
#' @param prov Parsed provenance
#' @param nsp List with the namespaces of used ontologies
#' @param script_file Script dataframe built with the function process_file
#' @param df_func Dataframe with R default functions
.get_activities <- function(prov = prov, nsp = nsp, script_file = script_file, df_func = df_func){
    activities <- get.proc.nodes(prov)
    informants <- get.proc.proc(prov)
    activities <- .get_expression(activities = activities, script_df = script_file) # Get the complete expression
    activities <- merge(activities, informants, by.x = 'id', by.y = 'informed', all.x = T)[,-11] # Ger informants ids
    activities <- activities[mixedorder(activities$id),] # Ordering by rdt id
    for(i in 1:nrow(activities)) { # Get what each activity does
        check_result <- .check(prov, nsp = nsp, cmd = activities$cmd[i], df_func = df_func)
        activities[i, c(paste0(nsp$rdf, 'type'))] <- check_result[[1]]
        activities[i, 'detail'] <- ifelse(length(check_result) > 1, check_result[[2]], list('Not found')) # TODO: fix here
    }

    functions <- which(activities[,paste0(nsp$rdf, 'type')] == paste0(nsp$repr, 'Function'))
    ops <- which(activities[,paste0(nsp$rdf, 'type')] != paste0(nsp$repr, 'Function'))

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

#' Get functions and write the triples
#'
#' This function gets all functions recorded by rdt and functions not explicitly
#' defined by rdt (usually functions from default/base packages).
#' This function also relates the functions to the packages they belong to
#' (for default and not default packages).
#' It then creates the triples with properties of the function, which script
#' the function relates to (isSubPlanofPlan), and which package it belongs to (hasPackage).
#'
#' @param prov Parsed provenance
#' @param nsp List with the namespaces of used ontologies
#' @param activities Dataframe containing the prov activities recorded by rdt
#' @param rdf RDF object to write the triples
#' @param df_func Dataframe with R default functions
#' @param script Script dataframe built with the function process_file
.get_functions <- function(prov = prov, nsp = nsp, activities = activities_df, rdf = rdf, df_func = df_func, script = script_file){

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
    pckg <- merge(functions, libs, by.x = 'library', by.y = 'id')[,c('function_name','name')]
    pckg$name <- paste0(nsp$repr, pckg$name); names(pckg)[2] <- paste0(nsp$repr, 'hasPackage')
    function_pckg_triple <- pckg %>%
        mutate(subject = function_name) %>%
        select(-'function_name') %>% # Removing
        gather(key = predicate, value = object, -subject)
    invisible(apply(function_pckg_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))

    return(functions)
}

#' Get function activations and write the triple
#'
#' This function checks in the activity dataframe registered by rdt which
#' activities are functions and how many times each function is activated.
#' It builds a dataframe with the function activation name, which activity (function activation
#' or operation) preceded its execution and its relation to the function (isStepOfPlan).
#' This dataframe turns into a triple that is added to the rdf object.
#'
#' @param activities Dataframe containing the prov activities recorded by rdt
#' @param nsp List with the namespaces of used ontologies
#' @param rdf RDF object to write the triples
.get_functions_activations <- function(activities = activities_df, nsp = nsp, rdf = rdf) {

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

#' Get operations and write the triple
#'
#' This function checks in the activity dataframe registered by rdt which
#' activities are operations (assigments tha do not involve function execution, for example).
#' It builds a dataframe with the operation name, which activity (function activation
#' or operation) preceded its execution and its type.
#' This dataframe turns into a triple that is added to the rdf object.
#'
#' @param activities Dataframe containing the prov activities recorded by rdt
#' @param nsp List with the namespaces of used ontologies
#' @param rdf RDF object to write the triples
.get_ops <- function(activities = activities_df, nsp = nsp, rdf = rdf) {
    ops <- which(activities[,paste0(nsp$rdf, 'type')] == paste0(nsp$repr, 'Operation'))
    ops <- activities[ops, c('cmd', 'detail', 'activation', paste0(nsp$repr, 'informedBy'), paste0(nsp$rdf, 'type'))]
    names(ops)[1] <- paste0(nsp$rdf, 'value')
    names(ops)[2] <- paste0(nsp$repr, 'hasType')

    ops_triple <- unique(ops) %>%
        mutate(subject = activation) %>%
        select(-'activation') %>% #removing function_name (it will be the identifier)
        gather(key = predicate, value = object, -subject)

    invisible(apply(ops_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))
    return(ops)
}

#' Get packages and write the triple
#'
#' This function checks the packages recorded by rdt.
#' It builds a dataframe with the package name and its version.
#' This dataframe turns into a triple that is added to the rdf object.
#'
#' @param prov Parsed provenance
#' @param nsp List with the namespaces of used ontologies
#' @param rdf RDF object to write the triples
.get_packages <- function(prov = prov, nsp = nsp, rdf = rdf){

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

#' Get data (input and output) and write the triple
#'
#' This function checks the entities recorded by rdt.
#' The function checks whether an entity is produced as an output from the
#' execution of an activity and also serves as an input to another activity.
#' In this case, the entity is referred to as a Variable. If it is only produced
#' as a result of an execution, it is referred to as Output.
#'
#' @param prov Parsed provenance
#' @param nsp List with the namespaces of used ontologies
#' @param rdf RDF object to write the triples
.get_entity <- function(prov = prov, nsp = nsp, rdf = rdf) {
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

    #data$name <- paste0(nsp$repr, gsub("[.]", "_", data$name)) # Removing dots from names
    data$name <- ifelse(!(grepl('http://', data$name, fixed = TRUE)), paste0(nsp$repr, gsub("[.]", "_", data$name)), data$name) # Removing dots from names, keep urls (if they exist)
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

#' Get the generate property between activity and entity and write the triple
#'
#' The function checks which entities are generated by which activities
#' (operations or function activations) and it writes the triples.
#'
#' @param prov Parsed provenance
#' @param entities Dataframe containing the prov entities recorded by rdt
#' @param activities Dataframe containing the prov activities recorded by rdt
#' @param nsp List with the namespaces of used ontologies
#' @param rdf RDF object to write the triples
.get_generated <- function(prov = prov, entities = entities_df, activities = activities_df, nsp = nsp, rdf = rdf){

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

#' Get the used property between activity and entity and write the triple
#'
#' The function checks which entities are used by which activities
#' (operations or function activations) and it writes the triples.
#'
#' @param prov Parsed provenance
#' @param entities Dataframe containing the prov entities recorded by rdt
#' @param activities Dataframe containing the prov activities recorded by rdt
#' @param nsp List with the namespaces of used ontologies
#' @param rdf RDF object to write the triples
.get_used <- function(prov = prov, entities = entities_df, activities = activities_df, nsp = nsp, rdf = rdf){

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

#' Get the parameters for the function activations
#'
#' This function first get and load the packages used in the script.
#' For each function activation, it gets the informed arguments.
#' It then matches the informed arguments with all the arguments needed for the function execution,
#' to get a complete list of arguments, since some arguments may have default values (parameters).
#' The function writes the triples, representing all arguments, its parameter value and
#' relates them with the corresponding function activation (isInputVarOf).
#'
#' @param prov Parsed provenance
#' @param activities Dataframe containing the prov activities recorded by rdt
#' @param nsp List with the namespaces of used ontologies
#' @param rdf RDF object to write the triples
.get_args <- function(prov = prov, activities_df = activities_df, nsp = nsp, rdf = rdf) {
    libs <- get.libs(prov)
    invisible(lapply(libs$name, require, character.only = TRUE))

    func <- activities_df[
        which(activities_df[,paste0(nsp$rdf, 'type')] == paste0(nsp$repr, 'Function')), c('detail', 'cmd', 'activation')]
    all <- c()

    for(i in 1:nrow(func)) {
        print(paste0("FUN NUMBER ", i))
        lang <- str2lang(func$cmd[i]); lang
        args_func <- formals(func$detail[i]) # Get all the function args and its default values
        if(length(lang) >= 3) {
            args_activation <- as.list(lang[[3]][-1]) # Get all informed args and parameters of the activation
        } else if(length(lang) == 2) { #|| length(lang) == 3) {
            args_activation <- as.list(lang[-1]) # Get all informed args and parameters of the activation
            #args_activation <- args_activation[[length(args_activation)]]
        }

        if (!is.null(args_func)) { # Some functions have no formals (i.e, NULL)
            args_func_df <- c()
            # Create dataframe with args and default values
            # couldn't do this with unlist (diff col types)

            for (j in 1:length(args_func)){
                args_func_df <- rbind(args_func_df,
                                      data.frame(pos = j,
                                                 name = names(args_func[j]),
                                                 value = deparse(args_func[[j]]),
                                                 type = typeof(args_func[[j]]),
                                                 fun = func$detail[i],
                                                 act = func$activation[i])
                )
            }

            if (length(args_activation) > 0) { # When there is no informed args
                for (k in 1:length(args_activation)){
                    # Change the value according to the cmd

                    if (length(args_activation) > 1) {
                        name <- names(args_activation[k])
                        arg <- args_activation[[k]]
                    } else {
                        name <- names(args_activation)
                        arg <- as.character(args_activation)
                    }
                    n <- ifelse(is.null(name) || name == "", NA, name); n
                    if (is.na(n)){ # If is NA, keep the order
                        args_func_df[k, 'value'] <- deparse1(arg)
                    } else if (length(which(args_func_df$name == n)) > 0) { #If not, adjust in the right position
                        args_func_df[which(args_func_df$name == n), 'value'] <- deparse1(arg)
                    } else {
                        args_func_df <- rbind(args_func_df,
                                              data.frame(pos = k,
                                                         name = name,
                                                         value = deparse1(arg),
                                                         type = typeof(arg),
                                                         fun = func$detail[i],
                                                         act = func$activation[i]))
                    }

                }
            }
            all <- rbind(all, args_func_df)
        }
    }

    all[, paste0(nsp$rdf, 'type')] <- paste0(nsp$repr, 'Argument')
    all$name <- paste0(nsp$repr, gsub("[.]", "_", all$name), '_', all$fun)
    colnames(all)[3] <- paste0(nsp$repr, 'hasParameterValue')
    colnames(all)[4] <- paste0(nsp$repr, 'hasType')
    colnames(all)[6] <- paste0(nsp$repr, 'isInputVarOf')

    arg_triple <- all[,c(-1, -5)] %>%
        mutate(subject = name) %>%
        select(-'name') %>% #removing arg_name (it will be the identifier)
        gather(key = predicate, value = object, -subject)

    invisible(apply(arg_triple, 1, function(x) rdf %>% rdf_add(x[1], x[2], x[3])))
    return(all)
}
