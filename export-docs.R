# export-docs.R -- Regenerates docs/ (GitHub Pages bundle) using the project's
# uvr library (versions pinned in uvr.lock).
#
# uvr is enabled by default for any R opened at the project root (the .Rprofile
# prepends .uvr/library/ onto .libPaths()). Run this script from the root:
#
#   Rscript export-docs.R
#
# Source of truth: `R/graph-model.R` + `R/interactions-app/`. shinylive bundles
# a single flat app directory, so we stage a flat copy of the three loose R
# files in a temporary directory (resolving app-utils.R's third source
# candidate, "graph-model.R"), export from there, then let the temp dir be
# cleaned up automatically. No persistent staging dir is kept in the repo.
#
# Note on version scope (see the uvr investigation):
#  - uvr pins EXACT versions (major.minor.patch) in `.uvr/library/`, which feeds
#    the *reporting* `version` field of `metadata.rds`.
#  - shinylive ALWAYS downloads the latest build published on repo.r-wasm.org
#    for the same major.minor (`get_wasm_assets()`), so the shipped webR binary
#    is capped at major.minor; the patch divergence (e.g. igraph 2.3.4 local vs
#    2.3.1 wasm) is silent.

root <- normalizePath(".")                 # project root (public/ locally)
stopifnot(
  "R must be launched from the project root (public/ locally, repo root in CI)" =
    (basename(root) == "public" || file.exists(file.path(root, ".github"))) &&
      file.exists(file.path(root, "uvr.toml"))
)

# The whole export runs inside a function so that on.exit() reliably fires on both
# success and error (on.exit does NOT fire at the top level of an Rscript, which
# left staging dirs behind and made the export fragile).
main <- function(root) {
  docs <- file.path(root, "docs")
  app_files <- c("app.R", "app-utils.R", "graph-model.R")

  # Flat staging copy in a unique dir under the project root (guaranteed to exist
  # locally and in CI; the OS temp dir proved unreliable on the CI runner). Always
  # cleaned up on exit, so nothing persists in the repo.
  stage <- tempfile("bebimpro-stage-", tmpdir = root)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  src <- c(
    file.path(root, "R", "graph-model.R"),
    file.path(root, "R", "interactions-app", "app-utils.R"),
    file.path(root, "R", "interactions-app", "app.R")
  )
  ok <- file.copy(src, file.path(stage, basename(src)), overwrite = TRUE)
  stopifnot("Could not stage the app files into the export dir" = all(ok))

  # 1) Make sure the pinned versions are installed (fast, uvr cache).
  uvr::sync()

  # 2) Export. `prepare_wasm_metadata()` reads packageDescription() from
  #    `.uvr/library/` (first on .libPaths()) -> the reporting field = pinned versions.
  shinylive::export(stage, docs)

  cat("Export finished to", docs, "\n")
  cat("Manifest: ", file.path(docs, "shinylive", "webr", "packages", "metadata.rds"),
      "\n", sep = "")
}

main(root)
