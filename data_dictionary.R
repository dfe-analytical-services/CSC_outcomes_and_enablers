library(data.table)



# first step is to identify the data frames

analyse_data_frames <- function(reload_all = FALSE) {
  # function to list all of the data frames and their columns in the Global Environment after sourcing global.R
  if (reload_all == TRUE) {
    rm(list = ls(envir = .GlobalEnv))
    source("C:/Users/mweller1/Documents/projects/CSC_outcomes_and_enablers/global.R")
  }
  l <- ls(.GlobalEnv)
  table_defs <- Filter(Negate(is.null), lapply(ls(envir = .GlobalEnv), function(x) if (is.data.frame(o <- get(x))) names(o)))
  names(table_defs) <- l[sapply(l, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]

  rbindlist(lapply(names(table_defs), function(td_name, table_defs) {
    data_types <- as.character(sapply(get(td_name), class))
    data.table(df_name = td_name, row_count = nrow(get(td_name)), column_order = 1:length(data_types), column_name = table_defs[[td_name]], data_type = data_types)
  }, table_defs))
}

dt_table_defs <- analyse_data_frames(reload_all = TRUE)
write.csv(dt_table_defs, "C:/Users/mweller1/OneDrive - Department for Education/Documents/CSC shiny dashboard/data_dictionary_df.csv", row.names = F)
dt_table_defs[, .N, by = .(column_name)][order(-N)][1:20]

#
get_csv_files <- function() {
  files <- paste0("./data/", dir("./data/", pattern = "*csv"))
  rbindlist(lapply(files, function(fil) {
    print(fil)
    dt <- fread(fil)
    data_types <- as.character(sapply(dt, class))
    data.table(file_name = fil, file_type = "csv", row_count = nrow(dt), column_order = 1:length(dt), column_name = names(dt), data_type = data_types)
  }))
}

get_csv_files()

# load the data dictionary





# match input files to data.frames, and the functions which get the data

csv <- unique(get_csv_files()[, file_name])
dd <- fread("./datafiles_log.csv")$filename

dd
csv
