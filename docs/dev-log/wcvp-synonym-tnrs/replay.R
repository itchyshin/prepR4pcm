# Exact original inputs; local WCVP; explicitly simulated TNRS responses.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
lib <- normalizePath(args[[1L]])
out <- args[[2L]]
mode <- match.arg(args[[3L]], c("smoke", "full", "cached", "integrated"))
root <- "/home/snakagaw/prepr137"
.libPaths(c(lib, file.path(root, "Rlib"), file.path(root, "merged-Rlib"), .libPaths()))
suppressPackageStartupMessages(library(prepR4pcm))
stopifnot(normalizePath(find.package("prepR4pcm")) == normalizePath(file.path(lib, "prepR4pcm")))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
data.table::setDTthreads(4)
con <- taxadb::td_connect()
DBI::dbExecute(con, "SET threads = 4")
stopifnot(as.character(taxadb:::latest_version()) == "2026")
cache_files <- list.files(file.path(root, "results/col"), "^authority-.*\\.rds$", full.names = TRUE)
cache <- do.call(rbind, lapply(cache_files, readRDS))
cache <- cache[!duplicated(cache$input), , drop = FALSE]
stopifnot(nrow(cache) == 296721L)
env <- get(".pr_cache", asNamespace("prepR4pcm"))
assign("resolved_taxadb_version_col", "2026", env)
assign("lookup_col_2026", cache, env)
assign("lookup_col_latest", cache, env)
search <- readRDS(file.path(root, "data/species.Rds"))
tree <- ape::read.tree(file.path(root, "data/wcvp_tree.tre"))
wcvp <- list(version = "16", names = data.table::fread(
  file.path(root, "wcvp-audit-20261025/wcvp_names.csv"), colClasses = "character",
  select = c("plant_name_id", "taxon_name", "taxon_status", "taxon_rank", "accepted_plant_name_id"),
  data.table = FALSE))
if (mode == "smoke") {
  search <- data.frame(search_name = c("Glandularia verecunda", "Trema orientalis", "Trema tomentosa",
    "Coelogyne candoonensis", "Bignonia ternata"))
  tree <- ape::keep.tip(tree, intersect(tree$tip.label,
    c("Verbena_verecunda", "Trema_orientale", "Trema_tomentosum", "Coelogyne_cantonensis")))
}
baseline <- readRDS(file.path(root, "review-recovery-20261021/raw-verified/result.rds"))
if (mode == "cached") {
  # Revalidate a local-adapter-only change without recomputing the unchanged
  # ordinary cascade. The earlier full pass proved this mapping identical.
  ns <- asNamespace("prepR4pcm")
  cached_cascade <- function(names_x, names_y, ...) {
    stopifnot(setequal(names_x, baseline$mapping$name_x[baseline$mapping$in_x]),
              setequal(names_y, baseline$mapping$name_y[baseline$mapping$in_y]))
    mapping <- baseline$mapping
    attr(mapping, "identity_candidates") <- baseline$candidates
    attr(mapping, "identity_source") <- baseline$identity_source
    mapping
  }
  unlockBinding("pr_run_cascade", ns)
  assign("pr_run_cascade", cached_cascade, ns)
  lockBinding("pr_run_cascade", ns)
}
start <- proc.time()[["elapsed"]]
authority <- if (mode == "smoke") NULL else "col"
local <- if (mode == "integrated") {
  reconcile_wcvp(baseline, wcvp = wcvp)
} else reconcile_tree(search, tree, x_species = "search_name", authority = authority,
                        fuzzy = TRUE, quiet = TRUE, wcvp = wcvp)
local_time <- proc.time()[["elapsed"]] - start
if (mode == "smoke") stopifnot(any(local$wcvp$candidates$name_x == "Glandularia verecunda" &
                                    local$wcvp$candidates$name_y == "Verbena_verecunda"))
stopifnot(setequal(na.omit(local$mapping$name_x), unique(search$search_name)),
          setequal(na.omit(local$mapping$name_y), unique(tree$tip.label)))
if (mode != "smoke") stopifnot(identical(local$mapping, baseline$mapping))
saveRDS(local, file.path(out, "local-result.rds"))
write.csv(local$wcvp$candidates, file.path(out, "wcvp-candidates.csv"), row.names = FALSE)
write.csv(local$wcvp$input_audit, file.path(out, "input-resolution.csv"), row.names = FALSE)
write.csv(local$wcvp$target_audit, file.path(out, "target-resolution.csv"), row.names = FALSE)
cat("LOCAL_FULL_INVENTORY_PASS", local_time, "seconds\n")

# Exercise the one-call integration at the full query/response size without
# submitting 185,000 names to a public service. Responses are simulated from
# independent saved audit records, not represented as live TNRS evidence.
audit <- data.table::fread(file.path(root, "wcvp-audit-20261025/input-audit.csv"),
                          data.table = FALSE)
submitted_names <- NULL
query <- function(names, source) {
  submitted_names <<- names
  rows <- match(names, audit$input_name)
  accepted <- audit$accepted_name[rows]
  accepted[audit$resolution[rows] != "resolved" | is.na(rows)] <- NA_character_
  list(results = data.frame(Name_submitted = names, Accepted_name = accepted,
    Accepted_name_id = audit$accepted_id[rows], Overall_score = 1,
    Source = "WCVP", stringsAsFactors = FALSE),
    source_metadata = data.frame(sourceName = "WCVP", version = "SIMULATED-v16"),
    client_version = "SIMULATED", warnings = character())
}
ns <- asNamespace("prepR4pcm")
replace <- function(name, value) {
  unlockBinding(name, ns); assign(name, value, ns); lockBinding(name, ns)
}
replace(".pr_tnrs_query", query)
replace(".pr_tnrs_client_available", function() TRUE)
start <- proc.time()[["elapsed"]]
integrated <- reconcile_tree(search, tree, x_species = "search_name", authority = authority,
  fuzzy = TRUE, quiet = TRUE, wcvp = wcvp, tnrs = TRUE)
integrated_time <- proc.time()[["elapsed"]] - start
stopifnot(identical(integrated$mapping, local$mapping),
  setequal(submitted_names, local$mapping$name_x[local$mapping$in_x & local$mapping$match_type == "unresolved" &
    !is.na(local$mapping$name_x) & nzchar(trimws(local$mapping$name_x))]),
  all(integrated$candidates$name_y %in% tree$tip.label))
pair <- function(x) paste(x$name_x, x$name_y, sep = "\034")
if (mode != "smoke") stopifnot(!length(setdiff(pair(baseline$candidates), pair(local$candidates))))
pending <- prepR4pcm:::.pr_pending_review_candidates(integrated)
saveRDS(integrated, file.path(out, "integrated-simulated-result.rds"))
write.csv(pending, file.path(out, "pending.csv"), row.names = FALSE)
hash <- function(path) digest::digest(path, file = TRUE, algo = "sha256")
receipt <- list(mode = mode, source_archive_sha256 = Sys.getenv("REVIEW_SOURCE_SHA"),
  input_sha256 = hash(file.path(root, "data/species.Rds")),
  tree_sha256 = hash(file.path(root, "data/wcvp_tree.tre")),
  wcvp_sha256 = hash(file.path(root, "wcvp-audit-20261025/wcvp_names.csv")),
  input_rows = nrow(search), tree_tips = length(tree$tip.label),
  local_elapsed_seconds = local_time, integrated_simulated_elapsed_seconds = integrated_time,
  mapping_identical_to_saved_replay = if (mode != "smoke") identical(local$mapping, baseline$mapping) else NA,
  cascade_reused = mode == "cached",
  local_comparison_uses_saved_cascade = mode == "integrated",
  mapping_identical_with_tnrs = identical(local$mapping, integrated$mapping),
  original_candidate_pairs = nrow(baseline$candidates), local_candidate_pairs = nrow(local$candidates),
  wcvp_exact_pairs = nrow(local$wcvp$candidates),
  wcvp_exact_input_names = length(unique(local$wcvp$candidates$name_x)),
  newly_visible_pairs = length(setdiff(pair(local$candidates), pair(baseline$candidates))),
  simulated_tnrs_query_names = length(submitted_names),
  simulated_tnrs_response_rows = nrow(integrated$tnrs$results),
  simulated_tnrs_candidate_pairs = nrow(integrated$candidates),
  local_resolution_status = as.list(table(local$wcvp$input_audit$status)),
  local_target_status = as.list(table(local$wcvp$input_audit$target_status)),
  occupied_wcvp_pairs = sum(local$wcvp$candidates$name_y %in% local$mapping$name_y[
    local$mapping$in_x & local$mapping$in_y]),
  tnrs_validation_kind = "SIMULATED full-size responses from saved independent WCVP audit; not live TNRS",
  local_result_sha256 = hash(file.path(out, "local-result.rds")),
  integrated_result_sha256 = hash(file.path(out, "integrated-simulated-result.rds")))
jsonlite::write_json(receipt, file.path(out, "receipt.json"), auto_unbox = TRUE, pretty = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
cat(jsonlite::toJSON(receipt, auto_unbox = TRUE), "\nBOTANICAL_FULL_REPLAY_PASS\n")
