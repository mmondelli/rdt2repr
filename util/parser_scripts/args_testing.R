library(lobstr)
library(rlang)
library(dplyr)
library(testwhat)
lobstr::ast(read.table("important.csv", row.names = FALSE))

lobstr::ast(library(Rocc))
typeof(library(Rocc))
is.call(library(Rocc))

data_splink <- expr(a <- rspeciesLink(species = species, filename = "specieslink"))
a <- str2lang(activities_df$rdt.name[4])
is.function(a)
call_name(lang)

is.function(lapply)
tst <- lobstr::ast(rspeciesLink())
typeof(data_splink)
is.call(data_splink)
a <- as.list(data_splink[[3]][-1])
rlang::call_standardise(data_splink)

is.function(rspeciesLink)

# here
.get_args <- function(activities_df = activities_df, rdf = rdf, nsp = nsp) {
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
    all$name <- paste0(nsp$repr, all$name, '_', all$fun)
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
args_df <- .get_args(activities_df, rdf, nsp)




`%notin%` <- Negate(`%in%`)
df_a <- merge(df_a[,c('pos', 'value', 'type')], df_f[,c('pos', 'name')], by = 'pos')
df_a <- rbind(df_a, df_f[which(df_f$name %notin% df_a$name), ])

sum <- expr(1 + 2)
lobstr::ast(sum)
typeof(sum)
is.call(sum)
as.list(sum[-1])

ast(a + b + c)
