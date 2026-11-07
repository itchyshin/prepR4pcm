# Live R client, deliberately small public-name test. Not a full service run.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
wcvp <- list(version = "16", names = read.csv(args[[1L]], stringsAsFactors = FALSE))
out <- args[[2L]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
x <- data.frame(species = c("Trema tomentosa", "Trema orientalis",
                            "Glandularia verecunda", "Verbena verecunda", "Bignonia ternata"))
tree <- ape::read.tree(text = "((Trema_tomentosum:1,Trema_orientale:1):1,(Verbena_verecunda:1,Coelogyne_cantonensis:1):1);")
baseline <- reconcile_tree(x, tree, authority = NULL, quiet = TRUE)
start <- proc.time()[["elapsed"]]
result <- reconcile_tree(x, tree, authority = NULL, quiet = TRUE, wcvp = wcvp, tnrs = TRUE)
elapsed <- proc.time()[["elapsed"]] - start
stopifnot(identical(result$mapping, baseline$mapping),
  any(result$candidates$name_x == "Glandularia verecunda" & result$candidates$name_y == "Verbena_verecunda"),
  any(result$candidates$name_x == "Trema tomentosa" & result$candidates$name_y == "Trema_tomentosum"),
  any(result$candidates$name_x == "Trema orientalis" & result$candidates$name_y == "Trema_orientale"),
  !any(result$candidates$name_x == "Bignonia ternata"),
  all(!result$candidates$eligible))
saveRDS(result, file.path(out, "integrated-live-result.rds"))
receipt <- list(kind = "LIVE TNRS R-client integrated smoke; four unresolved public names, nine-record WCVP v16 excerpt",
  elapsed_seconds = elapsed, queried_names = result$tnrs$queried_names,
  response_rows = nrow(result$tnrs$results), source_version = result$tnrs$source_version,
  local_reference_version = result$tnrs$target_reference_version,
  service_metadata = result$tnrs$service_metadata, source_metadata = result$tnrs$source_metadata,
  client_version = result$tnrs$client_version,
  candidates = as.data.frame(result$candidates), mapping_unchanged = identical(result$mapping, baseline$mapping))
jsonlite::write_json(receipt, file.path(out, "integrated-live-receipt.json"),
                      auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("INTEGRATED_LIVE_TNRS_PASS\n")
