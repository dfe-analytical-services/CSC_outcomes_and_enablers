create_qa_dataset_sn <- function(output_identifier, dataset_calc, sn_long, csv_file, id.vars_raw, measure.vars_raw, raw_melt_cols, by.x, by.y, calc_qa_formula, raw_qa_formula_1, raw_qa_formula_2,
                                 measure.vars_calc = NULL,
                                 write_data = FALSE, dataset_raw = NULL) {
  if (is.null(dataset_raw)) dataset_raw <- fread(csv_file)

  # hack for the new outcomes data which has a different column name and likewise
  setnames(dataset_raw, "phase_type_grouping", "school_type", skip_absent = TRUE)
  setnames(dataset_raw, "2_years_or_more_percent", "X2_years_or_more_percent", skip_absent = TRUE)

  # this block of code must be executed for the raw data to transform it as wewish because it has some quirks
  if (stringr::str_count(output_identifier, "workforce") == 1) {
    # old_la_code is a critical field and it's stored as a character in this dataset (with some exceptions e.g. 314 / 318 and 240 / 941)
    # dataset_raw[, original_old_la_code := old_la_code]
    dataset_raw[, old_la_code := as.integer(as.numeric(old_la_code))]
    # dataset_raw[, old_la_code := (original_old_la_code)]
  }

  # this block of code must be executed for the raw data to transform it as wewish because it has some quirks
  if (stringr::str_count(output_identifier, "care_leavers_activity") == 1) {
    # Age column needs to be uniform with the accommodation data as they share the same age range filter
    # "17 to 18 years" sounds better than "aged 17 to 18" but this can be swapped around if needed
    dataset_raw[age == "Aged 17 to 18", age := "17 to 18 years"]
    dataset_raw[age == "Aged 19 to 21", age := "19 to 21 years"]
  }

  if (!("la_name" %in% names(dataset_raw))) dataset_raw[, la_name := geo_breakdown]
  raw_melt <- melt.data.table(
    dataset_raw[geographic_level == "Local authority"],
    id.vars = id.vars_raw,
    measure.vars = measure.vars_raw,
    variable.factor = FALSE
  )

  raw_mapped <- merge.data.table(raw_melt, sn_long, by.x = "old_la_code", by.y = "LA.number", all.x = TRUE, allow.cartesian = TRUE)
  raw_mapped[, SN_rank := sprintf("SN_%02d", as.numeric(SN_rank))]

  raw_with_sn <- merge.data.table(raw_mapped,
    raw_melt[j = .SD, .SDcols = raw_melt_cols],
    by.x = by.x,
    by.y = by.y,
    all.x = TRUE,
    suffixes = c("_la", "_sn")
  )

  raw_qa_1 <- dcast.data.table(raw_with_sn, raw_qa_formula_1, fun.aggregate = function(x) x[1])
  # raw_qa_1[, old_la_code := as.character(old_la_code)]
  # raw_qa_2 <- dcast(raw_with_sn, raw_qa_formula_2, fun.aggregate =  function(x) x[1]  )

  if (is.null(measure.vars_calc)) {
    measure.vars_calc <- measure.vars_raw
  }

  if (length(measure.vars_raw) == 1) {
    if (measure.vars_calc == "Percentage" & measure.vars_raw == "percentage") {
      measure.vars_calc <- measure.vars_raw
      setnames(dataset_calc, "Percentage", "percentage")
    }
  }
  if (class(dataset_calc$old_la_code) != "integer") dataset_calc[, old_la_code := as.integer(old_la_code)]
  setkeyv(dataset_calc, id.vars_calc)
  dataset_calc <- dataset_calc[geographic_level %in% c("Local authority", "Statistical neighbours (median)")]
  dataset_calc[geographic_level == "Statistical neighbours (median)", geo_breakdown := geo_breakdown_sn]
  dataset_calc_melt <- melt.data.table(
    dataset_calc,
    id.vars = id.vars_calc,
    measure.vars = measure.vars_calc,
    variable.factor = FALSE
  )

  dataset_calc_qa <- dcast(dataset_calc_melt, calc_qa_formula, fun.aggregate = function(x) x[1])

  # now some merging of the files
  merged <- merge.data.table(raw_qa_1, dataset_calc_qa,
    by.x = get_join_cols(raw_qa_formula_1, 1),
    by.y = get_join_cols(calc_qa_formula, 0),
    all.x = TRUE
  )

  # now prepare the output
  output_list <- list("raw_qa_1" = raw_qa_1, "calculated_qa" = dataset_calc_qa, "combined_qa_dataset" = merged)
  excel_file <- paste0("C:/Users/mweller1/OneDrive - Department for Education/Documents/CSC shiny dashboard/SN QA Datasets/SN_supporting_data ", output_identifier, ".xlsx")

  if (write_data == TRUE) writexl::write_xlsx(output_list, excel_file)

  return(output_list)
}


get_join_cols <- function(formula_string, num_omit = 0) {
  lhs <- trimws(strsplit(formula_string, "~", fixed = TRUE)[[1]][1])
  lhs_columns <- trimws(strsplit(lhs, "+", fixed = TRUE)[[1]])
  return(lhs_columns[1:(length(lhs_columns) - num_omit)])
}
