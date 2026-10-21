args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
lib <- normalizePath(args[[1L]])
outdir <- args[[2L]]
mode <- match.arg(args[[3L]], c("smoke", "full"))
root <- "/home/snakagaw/prepr137"
.libPaths(c(lib, file.path(root,"Rlib"), file.path(root,"merged-Rlib"), .Library.site, .Library))
suppressPackageStartupMessages(library(prepR4pcm))
stopifnot(normalizePath(find.package("prepR4pcm")) == normalizePath(file.path(lib,"prepR4pcm")))
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
con <- taxadb::td_connect()
DBI::dbExecute(con, "SET threads = 4")
stopifnot(as.character(taxadb:::latest_version()) == "2026")
cache_files <- list.files(file.path(root,"results/col"),"^authority-.*\\.rds$",full.names=TRUE)
cache_all <- do.call(rbind,lapply(cache_files,readRDS))
cache <- cache_all[!duplicated(cache_all$input),,drop=FALSE]
stopifnot(nrow(cache)==296721L)
env <- get(".pr_cache",asNamespace("prepR4pcm"))
assign("resolved_taxadb_version_col","2026",env)
assign("lookup_col_2026",cache,env)
assign("lookup_col_latest",cache,env)
search <- readRDS(file.path(root,"data/species.Rds"))
phylo <- ape::read.tree(file.path(root,"data/wcvp_tree.tre"))
normalize <- function(x) as.character(prepR4pcm:::pr_normalize_names(x,rank="species"))
examples_x <- c("Coelogyne candoonensis","Coussarea petiolaris","Croton heterocarpus","Croton leiophyllus","Croton stenopetalus","Trema orientalis","Trema tomentosa","Trema tomentosum var. tomentosum","Bignonia ternata")
examples_y <- c("Coelogyne cantonensis","Coussarea impetiolaris","Croton heterochrous","Croton leucophyllus","Croton stenosepalus","Trema orientale","Trema tomentosum","Adenocalymma macrophyllum")
if(mode=="smoke") {
 search <- search[search$search_name %in% examples_x,,drop=FALSE]
 phylo <- ape::keep.tip(phylo, phylo$tip.label[normalize(phylo$tip.label) %in% examples_y])
}
start<-proc.time()[["elapsed"]]
rec <- reconcile_tree(search,phylo,x_species="search_name",authority="col",fuzzy=TRUE,quiet=TRUE)
elapsed<-proc.time()[["elapsed"]]-start
stopifnot(inherits(rec,"reconciliation"),nrow(rec$mapping)>0L)
stopifnot(setequal(na.omit(rec$mapping$name_x),unique(search$search_name)),setequal(na.omit(rec$mapping$name_y),unique(phylo$tip.label)))
pending <- prepR4pcm:::.pr_pending_review_candidates(rec)
for(i in 1:5) {
 stopifnot(!any(normalize(rec$mapping$name_x)==examples_x[[i]] & normalize(rec$mapping$name_y)==examples_y[[i]] & rec$mapping$in_x & rec$mapping$in_y,na.rm=TRUE))
}
stopifnot(any(pending$name_x=="Trema orientalis"),any(pending$name_x=="Trema tomentosa"))
# Exercise real saved-workload candidates in the ordinary interactive review path.
small <- rec
small$candidates <- rec$candidates[rec$candidates$name_x %in% c("Trema orientalis","Trema tomentosa"),,drop=FALSE]
small$mapping <- rec$mapping[rec$mapping$match_type!="flagged",,drop=FALSE]
review <- reconcile_review
io <- new.env(parent=environment(review));io$interactive<-function()TRUE;io$readline<-function(...)"s";environment(review)<-io
review_text <- capture.output(reviewed <- review(small,quiet=TRUE))
stopifnot(any(grepl("Trema orientalis",review_text,fixed=TRUE)),any(grepl("Trema tomentosa",review_text,fixed=TRUE)),identical(reviewed$mapping,small$mapping))
writeLines(review_text,file.path(outdir,"ordinary-review.txt"))
saveRDS(rec,file.path(outdir,"result.rds"))
saveRDS(pending,file.path(outdir,"pending.rds"))
write.csv(pending,file.path(outdir,"pending.csv"),row.names=FALSE)
old <- readRDS(file.path(root,"results/latin-aware-latin-aware-b055018/result.rds"))
baseline <- readRDS(file.path(root,"results/col-cached/result.rds"))
mapping_identical <- if(mode=="full") identical(rec$mapping,old$mapping) else NA
if(mode=="full") stopifnot(mapping_identical)
pairs <- function(x) paste(x$name_x,x$name_y,sep="\034")
old_key<-pairs(old$candidates);new_key<-pairs(rec$candidates)
added<-setdiff(new_key,old_key);removed<-setdiff(old_key,new_key);shared<-intersect(old_key,new_key)
if(mode=="full") stopifnot(!length(removed))
raw_names<-unique(c(examples_x,examples_y,"Adenocalymma ternatum"))
lookup<-prepR4pcm:::pr_lookup_authority(raw_names,"col","2026")
saveRDS(lookup,file.path(outdir,"named-authority.rds"))
write.csv(lookup,file.path(outdir,"named-authority.csv"),row.names=FALSE)
hash<-function(f)digest::digest(f,algo="sha256",file=TRUE)
receipt<-list(schema_version=1L,mode=mode,elapsed_seconds=elapsed,source_archive_sha256=Sys.getenv("REVIEW_SOURCE_SHA"),loaded_package_path=find.package("prepR4pcm"),
 input_sha256=hash(file.path(root,"data/species.Rds")),tree_sha256=hash(file.path(root,"data/wcvp_tree.tre")),
 provider="col",snapshot="2026",input_rows=nrow(search),unique_input_names=length(unique(search$search_name)),tree_tips=length(phylo$tip.label),
 mapping_rows=nrow(rec$mapping),match_types=as.list(table(rec$mapping$match_type)),candidate_pairs=nrow(rec$candidates),pending_pairs=nrow(pending),pending_inputs=length(unique(pending$name_x)),
 mapping_identical_to_b055018=mapping_identical,candidate_added=length(added),candidate_removed=length(removed),candidate_shared=length(shared),
 baseline_kind="saved_authority_enabled_surrogate_for_reported_count",baseline_result_sha256=hash(file.path(root,"results/col-cached/result.rds")),
 current_main_result_sha256=hash(file.path(root,"results/latin-aware-latin-aware-b055018/result.rds")),result_sha256=hash(file.path(outdir,"result.rds")),
 baseline_count_limitation="Saved baseline has 4006 flagged rows, not reporter 3944; it establishes a matched-data mechanism comparison only.")
jsonlite::write_json(receipt,file.path(outdir,"receipt.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
writeLines(capture.output(sessionInfo()),file.path(outdir,"session-info.txt"))
cat(jsonlite::toJSON(receipt,auto_unbox=TRUE),"\n")
cat("EXACT_SOURCE_REVIEW_REPLAY_PASSED\n")
