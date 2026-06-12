#!/usr/bin/env Rscript

private_pkgdown_pages <- c("CLAUDE.html")

cleanup_pkgdown_private_pages <- function(site_dir = "docs") {
  private_paths <- file.path(site_dir, private_pkgdown_pages)
  unlink(private_paths)

  sitemap_path <- file.path(site_dir, "sitemap.xml")
  if (file.exists(sitemap_path)) {
    sitemap <- readLines(sitemap_path, warn = FALSE)
    sitemap <- sitemap[!grepl("CLAUDE\\.html", sitemap)]
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
        !grepl("CLAUDE\\.html", path)
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
}

main <- function() {
  pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
  cleanup_pkgdown_private_pages()
}

if (sys.nframe() == 0) {
  main()
}
