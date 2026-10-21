#!/usr/bin/env Rscript

lr_hash <- function(path) {
  if (!file.exists(path)) stop("missing evidence file: ", path)
  if (!requireNamespace("digest", quietly=TRUE)) stop("digest package is required")
  digest::digest(path, algo="sha256", file=TRUE)
}
lr_pair_key <- function(x) {
  stopifnot(is.data.frame(x), all(c("name_x","name_y") %in% names(x)))
  paste(as.character(x$name_x), as.character(x$name_y), sep="\034")
}
lr_find <- function(x, pattern) {
  hit <- grep(pattern, names(x), ignore.case=TRUE, value=TRUE)
  if (length(hit)) hit[[1L]] else NULL
}
lr_assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call.=FALSE)

lr_read_object <- function(path) readRDS(path)
lr_mapping_pairs <- function(x) {
  lr_assert(!is.null(x$mapping), "result object lacks mapping")
  m <- x$mapping
  lr_assert(all(c("name_x","name_y") %in% names(m)), "mapping lacks name_x/name_y")
  m
}
lr_validate_pairs <- function(raw, receipt, paths) {
  new <- lr_read_object(paths$result)
  main <- lr_read_object(paths$main)
  base <- lr_read_object(paths$baseline)
  nm <- lr_mapping_pairs(new); mm <- lr_mapping_pairs(main); bm <- lr_mapping_pairs(base)
  nc <- new$candidates; mc <- main$candidates; bc <- base$candidates
  if (is.null(bc)) bc <- data.frame(name_x=character(), name_y=character())
  lr_assert(is.data.frame(nc) && is.data.frame(mc) && is.data.frame(bc), "result objects lack candidate tables")
  lr_assert(all(c("name_x","name_y","identity_state") %in% names(nc)) &&
    all(c("name_x","name_y","identity_state") %in% names(mc)),
    "new or main candidate table lacks required identity fields")
  pk <- lr_pair_key; nk <- pk(nc); mk <- pk(mc); bk <- pk(bc)
  all_keys <- union(unique(nk),unique(mk))
  n_count <- tabulate(match(nk,all_keys),nbins=length(all_keys)); m_count <- tabulate(match(mk,all_keys),nbins=length(all_keys))
  added <- rep(all_keys,pmax(0L,n_count-m_count)); removed <- rep(all_keys,pmax(0L,m_count-n_count))
  shared <- rep(all_keys,pmin(n_count,m_count))
  lr_assert(identical(nm,mm), "candidate mapping differs from b055018 main result")
  lr_assert(!length(removed), "candidate pairs were removed from b055018 main result")
  # All values asserted here are recomputed from the RDS files, never accepted as receipt totals.
  lr_assert(identical(as.character(receipt$mode), "full"), "receipt is not a full replay")
  lr_assert(identical(as.character(receipt$baseline_kind), "saved_authority_enabled_surrogate_for_reported_count"), "baseline kind is not the documented surrogate")
  lr_assert(grepl("4006", receipt$baseline_count_limitation, fixed=TRUE) && grepl("3944", receipt$baseline_count_limitation, fixed=TRUE), "surrogate limitation must state 4006 versus reported 3944")
  list(new=new,main=main,baseline=base,mapping=list(new=nm,main=mm,baseline=bm),
       added=added,removed=removed,shared=shared,
       baseline_candidate_pairs=length(bk),main_candidate_pairs=length(mk),new_candidate_pairs=length(nk),
       added_count=length(added),removed_count=length(removed),shared_count=length(shared),
       baseline_flagged=sum(bm$match_type == "flagged",na.rm=TRUE),main_flagged=sum(mm$match_type == "flagged",na.rm=TRUE),
       new_flagged=sum(nm$match_type == "flagged",na.rm=TRUE))
}

lr_check_hashes <- function(raw) {
  paths <- list(result=file.path(raw,"result.rds"), main=file.path(raw,"main-result.rds"),
    baseline=file.path(raw,"baseline-result.rds"), authority=file.path(raw,"named-authority.rds"),
    review=file.path(raw,"ordinary-review.txt"), archive=file.path(raw,"source.tar.gz"))
  hashes <- vapply(paths, lr_hash, character(1))
  receipt_path <- file.path(raw,"receipt.json")
  receipt <- jsonlite::fromJSON(receipt_path, simplifyVector=FALSE)
  trusted <- c(input="407b0c75ee1e0e37f62f05937a05bfafce8f735289a03a6a35cfb00fe76c959e",
    tree="bd32213488711351cad96f3f199fba2c86ec11b9629295bc1d61fe0dd75fe098",
    baseline="2dd9e9e47863b6e0f476746d9c8f8889a4dc260d501ac0d9309c73d14887a556",
    main="63439b9673d304e5c90ccb2b7e73e7f0b30e12ee005f50708af55dbf9e248ee8",
    archive=Sys.getenv("REVIEW_SOURCE_SHA"))
  lr_assert(grepl("^[a-f0-9]{64}$", trusted[["archive"]]), "REVIEW_SOURCE_SHA must bind the reviewed source archive")
  receipt_hash_fields <- c(input="input_sha256",tree="tree_sha256",result="result_sha256",
    main="current_main_result_sha256",baseline="baseline_result_sha256",archive="source_archive_sha256")
  for (n in names(receipt_hash_fields)) {
    key <- receipt_hash_fields[[n]]
    expected <- receipt[[key]]
    lr_assert(length(expected)==1L && nzchar(expected), paste("missing hash in raw receipt:",key))
    lr_assert(identical(tolower(as.character(expected)),tolower(if(n %in% names(trusted)) trusted[[n]] else hashes[[n]])),
      paste("trusted/observed hash mismatch:",n))
    if (n %in% names(hashes))
      lr_assert(identical(tolower(as.character(expected)),tolower(hashes[[n]])),paste("raw artifact hash mismatch:",n))
  }
  input_paths <- c(input=Sys.getenv("REVIEW_INPUT_PATH"),tree=Sys.getenv("REVIEW_TREE_PATH"))
  rehashed_inputs <- all(nzchar(input_paths))
  if (rehashed_inputs) for (n in names(input_paths))
    lr_assert(identical(lr_hash(input_paths[[n]]),trusted[[n]]),paste("source input hash mismatch:",n))
  list(paths=paths, receipt=receipt, hashes=c(hashes,input=trusted[["input"]],tree=trusted[["tree"]]),
       input_tree_verification=if (rehashed_inputs) "source files rehashed in place and matched independently audited digests" else
         "receipt hashes match prior independently audited digests; source files not rehashed by this invocation")
}

lr_phase_diagnosis <- function(raw, ev) {
  x <- lr_validate_pairs(raw, ev$receipt, ev$paths)
  names_x <- unique(as.character(x$new$mapping$name_x))
  candidates <- x$new$candidates
  pending <- if (exists(".pr_pending_review_candidates", asNamespace("prepR4pcm")))
    get(".pr_pending_review_candidates",asNamespace("prepR4pcm"))(x$new) else candidates
  expected <- c("Coelogyne candoonensis","Coussarea petiolaris","Croton heterocarpus","Croton leiophyllus","Croton stenopetalus","Trema orientalis","Trema tomentosa")
  dispositions <- setNames(vapply(expected,function(n) sum(candidates$name_x==n,na.rm=TRUE),integer(1)),expected)
  lr_assert(all(expected %in% names_x), "diagnosis lost one or more named input cases")
  lr_assert(all(dispositions > 0L), "one or more named diagnosis cases lack a candidate record")
  lr_assert(length(unique(as.character(pending$name_x[pending$name_x %in% c("Trema orientalis","Trema tomentosa")])))==2L,
             "both Trema review candidates are not pending in raw results")
  txt <- paste(readLines(ev$paths$review,warn=FALSE),collapse="\n")
  lr_assert(all(vapply(c("Trema orientalis","Trema tomentosa"),grepl,logical(1),x=txt,fixed=TRUE)),"ordinary review transcript omits a Trema candidate")
  stages <- lapply(expected, function(n) {
    rows <- function(m) as.list(table(m$match_type[m$name_x %in% n]))
    list(input=n,baseline_mapping=rows(x$mapping$baseline),main_mapping=rows(x$mapping$main),
      repaired_mapping=rows(x$mapping$new),main_candidate_rows=sum(x$main$candidates$name_x %in% n),
      repaired_candidate_rows=sum(candidates$name_x %in% n))
  })
  list(named_case_candidate_rows=dispositions, trema_pending=2L, review_transcript_trema=TRUE,
       named_case_stages=stages,
       baseline_main_flagged_delta=x$main_flagged-x$baseline_flagged,
       baseline_candidate_pairs=x$baseline_candidate_pairs,main_candidate_pairs=x$main_candidate_pairs,
       documented_surrogate_limit=ev$receipt$baseline_count_limitation)
}

lr_phase_identity <- function(raw, ev) {
  x <- lr_validate_pairs(raw, ev$receipt, ev$paths)
  mapping <- x$mapping$new; candidates <- x$new$candidates
  distinct <- c("Coelogyne candoonensis\034Coelogyne cantonensis","Coussarea petiolaris\034Coussarea impetiolaris",
    "Croton heterocarpus\034Croton heterochrous","Croton leiophyllus\034Croton leucophyllus","Croton stenopetalus\034Croton stenosepalus")
  normalize <- function(n) as.character(prepR4pcm:::pr_normalize_names(n, rank="species"))
  key <- paste(normalize(mapping$name_x), normalize(mapping$name_y), sep="\034")
  applied <- mapping$in_x %in% TRUE & mapping$in_y %in% TRUE
  lr_assert(!any(distinct %in% key[applied]),"one of the five distinct-ID pairs is automatically applied")
  candidate_key <- paste(normalize(candidates$name_x),normalize(candidates$name_y),sep="\034")
  for (pair in distinct) {
    rows <- which(candidate_key == pair)
    lr_assert(length(rows) > 0L && all(candidates$identity_state[rows] %in% "conflict"),
      "a distinct-ID control is absent or no longer labelled conflict in the full replay")
  }
  auth <- lr_read_object(ev$paths$authority)
  lr_assert(is.data.frame(auth),"named authority output is not a data frame")
  input_col <- lr_find(auth,"^(input|name|scientific_name)$")
  # pr_lookup_authority returns taxon_id for the accepted concept, not a
  # separate accepted_id column. The raw table is kept with the replay.
  accepted_col <- lr_find(auth,"^(accepted_id|taxon_id)$")
  status_col <- lr_find(auth,"status")
  lr_assert(!is.null(input_col) && !is.null(accepted_col) && !is.null(status_col),"authority output lacks input, status or accepted-ID fields")
  bignonia <- auth[auth[[input_col]] %in% c("Bignonia ternata", "Adenocalymma macrophyllum", "Adenocalymma ternatum"),,drop=FALSE]
  lr_assert(nrow(bignonia)>=2L,"Bignonia query and synonym target are not both present in authority output")
  lr_assert(any(grepl("Bignonia ternata",bignonia[[input_col]],fixed=TRUE)),"Bignonia source name absent")
  lr_assert(any(grepl("Adenocalymma ternatum",bignonia[[input_col]],fixed=TRUE)),"Bignonia comparison name absent")
  chain <- bignonia[bignonia[[input_col]] %in% c("Bignonia ternata", "Adenocalymma macrophyllum"),,drop=FALSE]
  lr_assert(nrow(chain) == 2L && !anyNA(chain[[accepted_col]]) &&
    length(unique(chain[[accepted_col]])) == 1L, "reported Bignonia pair does not share a source accepted ID")
  accepted_ids <- unique(as.character(bignonia[[accepted_col]])); accepted_ids <- accepted_ids[!is.na(accepted_ids)&nzchar(accepted_ids)]
  lr_assert(length(accepted_ids)>=1L,"Bignonia accepted-ID chain is empty")
  list(distinct_id_pairs_excluded=length(distinct),bignonia_records=nrow(bignonia),
       bignonia_status=as.character(bignonia[[status_col]]),bignonia_accepted_ids=accepted_ids,
       bignonia_identity_conclusion="recorded source fields only; no taxonomic certainty inferred")
}

lr_phase_replay <- function(raw, ev) {
  x <- lr_validate_pairs(raw, ev$receipt, ev$paths)
  candidates <- x$new$candidates
  key <- lr_pair_key(candidates)
  added_occurrence <- integer()
  for (k in unique(x$added)) {
    ni <- sum(key==k); mi <- sum(lr_pair_key(x$main$candidates)==k)
    if (ni>mi) added_occurrence <- c(added_occurrence,which(key==k)[seq_len(ni-mi)])
  }
  added <- candidates[added_occurrence,,drop=FALSE]
  lr_assert(nrow(added) > 0L, "replay has no recovered candidate pairs")
  normalize <- function(n) as.character(prepR4pcm:::pr_normalize_names(n,rank="species"))
  recovered_trema <- normalize(added$name_x) == "Trema tomentosa" &
    normalize(added$name_y) == "Trema tomentosum" & added$identity_state %in% "unknown"
  lr_assert(any(recovered_trema), "expected unknown-identity Trema occupied-target pair was not recovered")
  occupied <- x$new$mapping$name_y[x$new$mapping$in_x %in% TRUE & x$new$mapping$in_y %in% TRUE]
  source_unresolved <- added$name_x %in% x$mapping$new$name_x[x$mapping$new$match_type %in% "unresolved" & x$mapping$new$in_x]
  categories <- data.frame(pair=lr_pair_key(added),target_occupied=added$name_y %in% occupied,
    source_unresolved=source_unresolved,stringsAsFactors=FALSE)
  lr_assert(all(categories$target_occupied) && all(categories$source_unresolved),
    "an added pair does not represent an unresolved input and occupied target")
  list(added=x$added_count,removed=x$removed_count,shared=x$shared_count,
       added_by_target_occupation=as.list(table(categories$target_occupied,useNA="ifany")),
       added_by_source_unresolved=as.list(table(categories$source_unresolved,useNA="ifany")),
       mapping_identical=identical(x$mapping$new,x$mapping$main), automatic_outputs_identical=identical(x$mapping$new,x$mapping$main),
       added_by_identity_state=as.list(table(added$identity_state,useNA="ifany")))
}

lr_validate <- function(phase, receipt_path, raw = file.path(dirname(receipt_path),"raw")) {
  if (!requireNamespace("jsonlite",quietly=TRUE)) stop("jsonlite package is required")
  if (!requireNamespace("digest",quietly=TRUE)) stop("digest package is required")
  receipt_path <- file.path(normalizePath(dirname(receipt_path),mustWork=TRUE), basename(receipt_path))
  if (!dir.exists(raw)) stop("raw evidence directory is missing: ",raw)
  ev <- lr_check_hashes(raw)
  # A second receipt is the output. The raw receipt records the producer's provenance.
  derived <- switch(phase, diagnosis=lr_phase_diagnosis(raw,ev), identity=lr_phase_identity(raw,ev), replay=lr_phase_replay(raw,ev))
  output <- list(schema_version=1L,phase=phase,generated_at=format(Sys.time(),tz="UTC",usetz=TRUE),
    loaded_package_path=ev$receipt$loaded_package_path,base_revision="b05501806751518506638167a59218903669cfa6",
    hashes=as.list(ev$hashes),input_tree_verification=ev$input_tree_verification,derived=derived)
  jsonlite::write_json(output,receipt_path,auto_unbox=TRUE,pretty=TRUE,na="null",dataframe="rows")
  token <- c(diagnosis="DIAGNOSIS_ACCOUNTING_VERIFIED",identity="IDENTITY_AUDIT_VERIFIED",replay="REPLAY_ACCOUNTING_VERIFIED")[[phase]]
  cat(token,"\n")
  invisible(output)
}

lr_negative_controls <- function(raw) {
  # Mutate only a scratch receipt and scratch symlinks, never the raw evidence.
  positive <- lr_check_hashes(raw)
  td <- tempfile("latin-replay-oracle-"); dir.create(td)
  on.exit(unlink(td,recursive=TRUE),add=TRUE)
  files <- list.files(raw,full.names=TRUE)
  files <- files[basename(files) != "receipt.json"]
  stopifnot(all(file.symlink(files,file.path(td,basename(files)))))
  bad <- positive$receipt
  bad$result_sha256 <- paste(rep("0",64L),collapse="")
  jsonlite::write_json(bad,file.path(td,"receipt.json"),auto_unbox=TRUE)
  result <- tryCatch({lr_check_hashes(td); NULL},error=identity)
  lr_assert(inherits(result,"error") && grepl("hash mismatch",conditionMessage(result)),
    "changed-result receipt was not rejected")
  jsonlite::write_json(positive$receipt,file.path(td,"receipt.json"),auto_unbox=TRUE)
  unlink(file.path(td,"source.tar.gz"))
  result <- tryCatch({lr_check_hashes(td); NULL},error=identity)
  lr_assert(inherits(result,"error") && grepl("missing evidence",conditionMessage(result)),
    "missing source archive was not rejected")
  no_recovery <- positive
  no_recovery$paths$main <- positive$paths$result
  result <- tryCatch({lr_phase_replay(raw,no_recovery); NULL},error=identity)
  lr_assert(inherits(result,"error") && grepl("no recovered",conditionMessage(result)),
    "no-recovery replay was not rejected")
  cat("REPLAY_VALIDATOR_NEGATIVE_CONTROLS_PASSED\n")
}

lr_cli <- function(args=commandArgs(trailingOnly=TRUE)) {
  value <- function(key) { i <- match(key,args); if (is.na(i)||i==length(args)) stop("missing value for ",key); args[[i+1L]] }
  if ("--self-test" %in% args) return(lr_negative_controls(value("--raw")))
  phase <- value("--phase"); receipt <- value("--receipt")
  if (!phase %in% c("diagnosis","identity","replay")) stop("invalid --phase")
  if ("--raw" %in% args) lr_validate(phase,receipt,value("--raw")) else lr_validate(phase,receipt)
}
if (sys.nframe()==0L) tryCatch(lr_cli(),error=function(e){message("VALIDATION_FAILED: ",conditionMessage(e));quit(status=1L)})
