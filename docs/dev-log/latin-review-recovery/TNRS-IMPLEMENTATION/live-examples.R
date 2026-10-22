#!/usr/bin/env Rscript

pkgload::load_all(".", quiet = TRUE)

# Only public botanical names are submitted. No user dataset or private names
# are sent to the service.
x_names <- c(
  "Trema tomentosa", "Trema orientalis",
  "Coelogyne candoonensis", "Coussarea petiolaris",
  "Croton heterocarpus", "Croton leiophyllus", "Croton stenopetalus",
  "Bignonia ternata", "Quercus robrr"
)
tree_tips <- c(
  "Trema tomentosum", "Trema orientale",
  "Coelogyne cantonensis", "Coussarea impetiolaris",
  "Croton heterochrous", "Croton leucophyllus", "Croton stenosepalus",
  "Adenocalymma ternatum", "Adenocalymma macrophyllum", "Quercus robur"
)

rec <- reconcile_data(
  data.frame(species = x_names), data.frame(species = tree_tips),
  x_species = "species", y_species = "species", authority = NULL,
  fuzzy = FALSE, quiet = TRUE
)
mapping_before <- rec$mapping
service_error <- NULL
review <- tryCatch(
  reconcile_tnrs(rec, source = "wcvp"),
  error = function(e) {
    service_error <<- conditionMessage(e)
    NULL
  }
)
if (!is.null(review)) {
  stopifnot(identical(review$mapping, mapping_before))
  stopifnot(all(review$candidates$name_y %in% tree_tips))
  stopifnot(all(review$candidates$reason == "tnrs_review_only"))
}

receipt <- list(
  generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  source = "wcvp",
  source_version = if (is.null(review)) NULL else review$tnrs$source_version,
  client_version = as.character(utils::packageVersion("TNRS")),
  service_metadata = if (is.null(review)) NULL else review$tnrs$service_metadata,
  queried_names = x_names,
  target_tips = tree_tips,
  query_status = if (is.null(review)) "unavailable" else review$tnrs$status,
  service_error = service_error,
  warnings = if (is.null(review)) character() else review$tnrs$warnings,
  missing_submitted_names = if (is.null(review)) x_names else review$tnrs$missing_submitted_names,
  target_audit = if (is.null(review)) NULL else review$tnrs$target_audit,
  candidates = if (is.null(review)) NULL else review$candidates,
  raw_results = if (is.null(review)) NULL else review$tnrs$results,
  mapping_unchanged = if (is.null(review)) NA else identical(review$mapping, mapping_before)
)
out_path <- file.path(
  "docs", "dev-log", "latin-review-recovery", "TNRS-IMPLEMENTATION",
  "live-examples.json"
)
jsonlite::write_json(receipt, out_path, auto_unbox = TRUE, pretty = TRUE,
                     na = "null", null = "null", dataframe = "rows")
cat("TNRS_EXAMPLE_RECEIPT:", normalizePath(out_path), "\n")
cat("source: wcvp client:", receipt$client_version,
    "status:", receipt$query_status, "\n")
cat("queried:", length(x_names), "names\n")
if (is.null(review)) {
  cat("service error:", service_error, "\n")
} else {
  cat("version:", review$tnrs$source_version,
      "raw rows:", nrow(review$tnrs$results),
      "review candidates:", nrow(review$candidates), "\n")
  print(review$tnrs$target_audit)
  if (length(review$tnrs$warnings)) print(review$tnrs$warnings)
}
