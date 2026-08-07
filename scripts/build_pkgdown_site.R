#!/usr/bin/env Rscript

private_pkgdown_pages <- c(
  "CLAUDE.html",
  "CLAUDE.md",
  "AGENTS.html",
  "AGENTS.md"
)

private_pkgdown_pattern <- "(?:CLAUDE|AGENTS)\\.(?:html|md)"

cleanup_pkgdown_private_pages <- function(site_dir = "docs") {
  private_paths <- file.path(site_dir, private_pkgdown_pages)
  unlink(private_paths)

  sitemap_path <- file.path(site_dir, "sitemap.xml")
  if (file.exists(sitemap_path)) {
    sitemap <- readLines(sitemap_path, warn = FALSE)
    sitemap <- sitemap[!grepl(private_pkgdown_pattern, sitemap, perl = TRUE)]
    writeLines(sitemap, sitemap_path)
  }

  search_path <- file.path(site_dir, "search.json")
  if (file.exists(search_path)) {
    search <- jsonlite::fromJSON(search_path, simplifyVector = FALSE)
    keep <- vapply(
      search,
      function(entry) {
        path <- entry[["path"]]
        if (length(path) != 1 || is.na(path)) {
          path <- ""
        }
        !grepl(private_pkgdown_pattern, path, perl = TRUE)
      },
      logical(1)
    )
    jsonlite::write_json(
      search[keep],
      search_path,
      auto_unbox = TRUE,
      null = "null"
    )
  }

  leaked_pages <- private_paths[file.exists(private_paths)]
  if (length(leaked_pages) > 0L) {
    stop(
      "Private pkgdown pages remain after cleanup: ",
      paste(leaked_pages, collapse = ", "),
      call. = FALSE
    )
  }

  llms_path <- file.path(site_dir, "llms.txt")
  if (file.exists(llms_path)) {
    llms <- paste(readLines(llms_path, warn = FALSE), collapse = "\n")
    if (grepl(private_pkgdown_pattern, llms, perl = TRUE)) {
      stop("docs/llms.txt references private agent instructions.", call. = FALSE)
    }
  }
}

main <- function() {
  pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
  cleanup_pkgdown_private_pages()
}

if (sys.nframe() == 0) {
  main()
}
