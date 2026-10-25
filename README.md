# Bebimpro — interactive ggraph explore app

This repository versions **only the interactive Shiny app** of the Bebimpro–Varades
research project (musical improvisation & reading with toddlers), together with the
configuration needed to build and deploy it to GitHub Pages.

The app is a single interactive surface — a ggraph-based exploration tool — shared
byte-identically across the research report, the slides deck, and this standalone
deployment. It renders directed interaction graphs over 6-second windows
(`time_step`): musical **improvisation** vs **reading** sessions, with edge and node
encodings defined in the model.

## What's in this repo

| Path | Purpose |
|------|---------|
| `R/graph-model.R` | Single source of truth: graph model, `simulate_graph_ts()`, ggraph rendering, encodings |
| `R/interactions-app/app.R` | Shiny UI + server (`id_safe`, `page_sidebar`, accordion navigation) |
| `R/interactions-app/app-utils.R` | Guarded source of graph-model, encodings, legend/logic utilities |
| `export-docs.R` | Builds the static `docs/` bundle via `shinylive::export()` |
| `.github/workflows/deploy.yaml` | GitHub Actions: rebuilds and deploys the app to Pages |
| `uvr.toml`, `uvr.lock`, `.Rprofile`, `.r-version` | uvr-local package/version pinning (managed outside git) |

Your local IDE interpreter config (`.vscode/settings.json`, pointing at your
machine's uvr R) is **intentionally not tracked** — it is machine-specific and
not needed to build or deploy the app.

## Repo scope: this is not the research project

By design, this repository contains **only the app-related files**. The full
research workspace — including the confidential session data (`edges`/`nodes`/
subject tables) and research reports — is deliberately **excluded** from git (see
`.gitignore`, a strict whitelist) and therefore never pushed to GitHub.
`uvr` independently manages the whole local folder; git and uvr operate
independently.

## How the deploy works

1. On push to `main` (or manual `workflow_dispatch`), GitHub Actions runs on
   `ubuntu-latest`.
2. It sets up R `4.6.1`, installs the `uvr` wrapper + binary and `shinylive`.
3. `uvr::sync()` installs the exact pinned package versions into `.uvr/library/`.
4. `source("export-docs.R")` stages a flat copy of the app and runs
   `shinylive::export()` to produce `docs/`.
5. `docs/` is uploaded and deployed via `actions/deploy-pages`.

The resulting site bundles a WebAssembly (webR) build of the R graph stack, so it
runs entirely in the browser. The bundle is served over HTTPS (required by
shinylive's service worker).

## One-time setup & manual deploys (gh CLI)

There is no build artifact committed to the repo — CI regenerates everything, so a
deploy is just a push plus (optionally) a manual run.

```sh
# 1. First commit (the whitelist tracks only app + config files)
git add -A
git status          # confirm: only the whitelisted files, nothing private
git commit -m "Add Bebimpro Shiny app + uvr/deploy config"

# 2. Create the remote and push `main`
gh repo create bebimpro --public --source=. --push
#   use `--private` instead if you want the code kept private

# 3. Enable GitHub Pages, sourced from the Actions artifact (one-time)
gh api repos/astamm/bebimpro/pages -f build_type=workflow -X POST

# 4. Trigger the deploy and watch it
gh workflow run deploy.yaml --ref main
gh run watch

# 5. Read the live URL when it's done
gh api repos/astamm/bebimpro/pages --jq '.html_url'
```

Subsequent deploys happen automatically on every push to `main`. A manual
re-deploy is simply `gh workflow run deploy.yaml --ref main`.

> **Prerequisites**: the repo must be hosted on GitHub and Pages enabled for the
> workflow source (step 3 does this). GitHub Pages for a free/public repo is served
> over HTTPS, which shinylive requires — never serve it from `file://`.

## Local development

The app sources are the single source of truth: any change here propagates to the
report and deck via `{{< include >}}` (outside this repo). To run the app locally:

```r
# from the project root (uvr-pinned R session)
shiny::runApp("R/interactions-app")
```

To rebuild `docs/` manually:

```sh
Rscript export-docs.R
```

Preview the built site locally (shinylive needs an HTTP origin, not `file://`):

```r
httpuv::runStaticServer("docs", port = 8000)
# then open http://127.0.0.1:8000
```

## Notes

- **Version pinning scope**: `uvr.toml`/`uvr.lock` pin the native R packages exactly
  (e.g. `igraph 2.3.4`). The bundled WebAssembly binary is capped at the latest
  build on the webR repo within the same major.minor (e.g. `igraph@2.3.1`), reported
  in the site manifest as `metadata$ref`.
- Confidential research data must never be added to this repo, even with `git add -f`.
