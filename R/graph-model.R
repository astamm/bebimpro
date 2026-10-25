# Helpers for building and plotting fake-but-faithful interaction graphs
# (Bebimpro - Varades). Shared by the report (`varades/report/index.qmd`) and
# the Shiny app (`varades/report/app.R`). These functions are deliberately free
# of any enclosing environment so they can be sourced and used independently.
#
# The simulated data is fictional but conforms to the schema documented in the
# report and reproduces its semantic constraints: dense `distance` (every
# directed pair is listed in `edges`, so every pair has a distance), and the
# I-hub rule (any edge involving I, the intervenante pivoting on the activity,
# is necessarily `en_lien_avec_activite == 1`).

# -----------------------------------------------------------------------------
# Shared visual encodings (agreed design -- do not change).
# -----------------------------------------------------------------------------

# One colour & linetype per interaction type. `dans_les_bras` shares the colour
# of `tactile` (dashed); `dispute` shares the colour of `joue_avec` (dashed).
# There is no separate `sonore` type: vocal/musical engagement is captured by
# `joue_avec` (in a sonic session it means "producing a sound / playing music
# with"; in another activity, e.g. reading, it takes its other meaning).
type_cols <- c(
  orientation_corps = "#1f77b4", regard = "#ff7f0e", joue_avec = "#d62728",
  tactile = "#2ca02c", dans_les_bras = "#2ca02c", dispute = "#d62728"
)
type_lty <- c(
  orientation_corps = "solid", regard = "solid", joue_avec = "solid",
  tactile = "solid", dans_les_bras = "dashed", dispute = "dashed"
)
type_levels <- names(type_cols)

# Profil (role + age) mapped to shapes 21-25.
profil_shapes <- c(
  "Enfant CAT1" = 21, "Enfant CAT2" = 22, "Enfant CAT3" = 23,
  "Animatrice" = 24, "Intervenant.e" = 25
)

# Order of the 5 profiles for the interaction-preference matrix `p_pref`.
# NOTE: this order differs from `names(profil_shapes)` (which is ordered for the
# node legend as CAT1..CAT3 / Animatrice / Intervenant.e). All probability /
# preference lookups use PROF_ORDER positions; the plotting factor `profil`
# keeps `names(profil_shapes)` order. The two must NOT be conflated.
PROF_ORDER <- c("Intervenant.e", "Animatrice", "Enfant CAT1", "Enfant CAT2", "Enfant CAT3")

# Default tunable probabilities of the simulation, as top-level constants (single
# source of truth). `simulate_graph_ts_impl()` falls back on these when the
# corresponding argument is NULL, and the dev Shiny UI reads them to set the
# default values of its sliders, so the two can never drift apart.
DEFAULT_TYPE_PERSISTENCE <- c(
  orientation_corps = 0.7, regard = 0.7, joue_avec = 0.6,
  dispute = 0.5, tactile = 0.3, dans_les_bras = 0.9)
DEFAULT_P_ENGAGE_STATE <- list(
  "Enfant CAT1" = c(desengage = 0.35, passif = 0.35, actif = 0.30),
  "Enfant CAT2" = c(desengage = 0.25, passif = 0.35, actif = 0.40),
  "Enfant CAT3" = c(desengage = 0.18, passif = 0.32, actif = 0.50))
DEFAULT_ATTEND_PROB <- 0.10

# Build the anonymised roster from node counts. Prefixes: I (intervenante),
# A* (animatrices), B* (CAT1), C* (CAT2), D* (CAT3).
roster_nodes <- function(n_intervenants, n_animatrices, n_cat1, n_cat2, n_cat3) {
  nom <- c(paste0("I", seq_len(n_intervenants)),
           paste0("A", seq_len(n_animatrices)),
           paste0("B", seq_len(n_cat1)),
           paste0("C", seq_len(n_cat2)),
           paste0("D", seq_len(n_cat3)))
  tibble::tibble(
    nom = nom,
    age  = c(rep("ADULTE", n_intervenants + n_animatrices),
             rep("CAT1", n_cat1), rep("CAT2", n_cat2), rep("CAT3", n_cat3)),
    role = c(rep("Intervenant.e.s", n_intervenants), rep("Animatrice", n_animatrices),
             rep("Enfant", n_cat1 + n_cat2 + n_cat3))
  )
}

# Combined role+age profile (factor ordered as `names(profil_shapes)` for plots).
profil_of <- function(rook) {
  factor(
    dplyr::case_when(
      rook$role == "Enfant"          & rook$age == "CAT1"   ~ "Enfant CAT1",
      rook$role == "Enfant"          & rook$age == "CAT2"   ~ "Enfant CAT2",
      rook$role == "Enfant"          & rook$age == "CAT3"   ~ "Enfant CAT3",
      rook$role == "Animatrice"      & rook$age == "ADULTE" ~ "Animatrice",
      rook$role == "Intervenant.e.s"                         ~ "Intervenant.e"
    ),
    levels = names(profil_shapes)
  )
}

# Default 5x5 RELATIVE interaction-preference matrix (rows/cols in PROF_ORDER).
# Only ratios matter; the absolute density is set by `p_density`. Encodes the
# observed social rules:
#  - Intervenante engages intensely with CAT1, moderately/variously with older
#    children, and rarely with animatrices.
#  - Animatrices (appeasement) connect mainly WITH children, little with the
#    intervenante.
#  - CAT1 interact a lot with the intervenante & animatrices, nicely with the
#    activity, and very little with other children.
#  - CAT2/CAT3 have varied levels, with more peer contact (highest among CAT3).
default_p_pref <- function() {
  m <- matrix(0.3, nrow = 5, ncol = 5, dimnames = list(PROF_ORDER, PROF_ORDER))
  m["Intervenant.e", "Animatrice"]   <- 0.02   # intervenante <-> animatrice : rare
  m["Intervenant.e", "Enfant CAT1"]  <- 1.0    # constant, intense
  m["Intervenant.e", "Enfant CAT2"]  <- 0.55
  m["Intervenant.e", "Enfant CAT3"]  <- 0.45
  m["Animatrice", "Intervenant.e"]   <- 0.03   # animatrices avoid intervenante
  m["Animatrice", "Animatrice"]      <- 0.02
  m["Animatrice", "Enfant CAT1"]     <- 0.75   # appeasement -> with kids
  m["Animatrice", "Enfant CAT2"]     <- 0.55
  m["Animatrice", "Enfant CAT3"]     <- 0.40
  m["Enfant CAT1", "Intervenant.e"]  <- 1.0    # CAT1 respond to intervenante
  m["Enfant CAT1", "Animatrice"]     <- 0.80
  m["Enfant CAT1", "Enfant CAT1"]    <- 0.06   # little child-child for CAT1
  m["Enfant CAT1", "Enfant CAT2"]    <- 0.15
  m["Enfant CAT1", "Enfant CAT3"]    <- 0.10
  m["Enfant CAT2", "Intervenant.e"]  <- 0.55   # older: varied
  m["Enfant CAT2", "Animatrice"]     <- 0.50
  m["Enfant CAT2", "Enfant CAT1"]    <- 0.15
  m["Enfant CAT2", "Enfant CAT2"]    <- 0.20
  m["Enfant CAT2", "Enfant CAT3"]    <- 0.30
  m["Enfant CAT3", "Intervenant.e"]  <- 0.45
  m["Enfant CAT3", "Animatrice"]     <- 0.40
  m["Enfant CAT3", "Enfant CAT1"]    <- 0.10
  m["Enfant CAT3", "Enfant CAT2"]    <- 0.35
  m["Enfant CAT3", "Enfant CAT3"]    <- 0.45   # more peer contact among oldest
  m
}

# MDS layout of the nodes from a dense pairwise-distance table.
#
# `cmdscale` collapses to a single point any set of nodes whose pairwise
# distances are exactly identical (e.g. several same-profile children whose
# around-profile distances are structurally equal in a noiseless window). Such
# identical coordinates are degenerate: the touch rule then cannot separate
# "coincident" non-tactile pairs. A tiny deterministic, symmetric-perturbing
# jitter is therefore folded into the distance matrix (derived from the node
# identifiers, so it is reproducible and does not grow with window count) to
# break the tie and give every distinct individual its own position while
# keeping same-profile nodes tightly clustered.
mds_layout <- function(dist_pairs, individus = NULL, tie_jitter = 1e-3) {
  if (is.null(individus)) {
    individus <- sort(unique(c(dist_pairs$p1, dist_pairs$p2)))
  }
  mat <- matrix(0, length(individus), length(individus),
                dimnames = list(individus, individus))
  # deterministic per-pair perturbation from the pair of identifiers
  h <- function(a, b) { # small hash of the two names -> (0, tie_jitter]
    key <- sum(vapply(strsplit(paste0(a, b), "")[[1]], function(ch)
      utf8ToInt(ch) %% 251, numeric(1)))
    tie_jitter * (0.5 + (key %% 97) / 108)
  }
  for (i in seq_len(nrow(dist_pairs))) {
    a <- dist_pairs$p1[i]; b <- dist_pairs$p2[i]
    eps <- if (tie_jitter > 0) h(a, b) else 0
    mat[a, b] <- dist_pairs$d[i] + eps
    mat[b, a] <- dist_pairs$d[i] + eps
  }
  coords <- cmdscale(as.dist(mat), k = 2)
  tibble::tibble(nom = individus, x = coords[, 1], y = coords[, 2])
}

# Align configuration Y onto reference X by Procrustes: centre both, estimate an
# orthogonal Procrustes rotation/reflection `R = U V'`, then place the rotated Y
# onto X's centroid. X and Y are n x 2 matrices **in the same row order**. MDS
# returns coordinates up to an arbitrary rotation, reflection and translation, so
# without alignment each window's layout would "tumble" arbitrarily. NOTE: a
# window whose *raw* MDS is the mirror image of the reference (a chance
# eigenvector sign) must be allowed to reflect, or it would stay flipped.
# Reflection is therefore ALLOWED (proper / general Procrustes): the sign of
# det R is not constrained, so every window can undo its chance mirror and align
# consistently onto the single window-1 reference.
procrustes_align <- function(X, Y) {
  cx <- colMeans(X); cy <- colMeans(Y)
  Xc <- sweep(X, 2, cx); Yc <- sweep(Y, 2, cy)
  M <- crossprod(Xc, Yc)
  s <- svd(M)
  R <- s$u %*% t(s$v)
  sweep(Yc %*% R, 2, cx, "+")
}

# Enforce the physical-contact rule on a node layout. Non-tactile persons must
# keep their nodes apart: any non-tactile pair stays at least `touch`*`gap` away
# (so their silhouettes do not touch). A `tactile` interaction means the two
# persons lean in and touch (or partially overlap): their nodes must come
# together at, or closer than, `touch`, but stay far enough apart to remain
# visually distinguishable (>= `vis_floor`). The layout is otherwise left
# untouched, so it remains Procrustes-aligned and spatially stable across
# windows.
#
# Tactile pairs are already gathered to `touch` globally, IN the MDS distance
# matrix (see simulate_graph_ts_impl), so the closeness is part of the geometry
# rather than an afterthought. This two-phase solver then pins the *final*
# distances against the observation rules (the monolithic joint relaxation
# diverges: tactile attraction and non-tactile repulsion fight over one cramped
# reference region and the layout grows without bound):
#  1. Pack: pure repulsion so every NON-tactile pair is at least `touch*gap`
#     apart (their silhouettes do not touch), while tactile pairs keep their
#     `touch` floor and are never pushed apart again. Unconditionally stable
#     (only ever separates), giving a feasible, compact packing.
#  2. Pinch: a local, iterative pass pulls each tactile pair in toward `touch`
#     (and nudges anything it dragged too close back out — tactile to `vis_floor`,
#     non-tactile to `touch*gap`), converging because phase 1 started from a
#     feasible packing and phase 2 only readjusts the few involved nodes.
enforce_touch_layout <- function(layout, tactile, touch = 0.25, gap = 1.2,
                                 vis_floor = 0.08, n_iter = 6000L,
                                 pinch_iter = 5000L, step = 0.5,
                                 fixed = NULL) {
  n <- nrow(layout)
  xy <- as.matrix(layout[, c("x", "y")])
  floor_d <- touch * gap
  ij <- t(combn(n, 2))                      # every unordered pair (i, j)
  i <- ij[, 1]; j <- ij[, 2]
  tac_pairs <- as.logical(tactile[cbind(i, j)])
  tac_pairs[is.na(tac_pairs)] <- FALSE
  # `fixed` gives the row indices (1-based, matching `layout`) whose accumulated
  # force must be zeroed at the end of every step — e.g. an anchored
  # intervenant.e.s during the first third of the session. The *other* nodes move
  # around them while the fixed node itself never translates during the
  # relaxation.
  fixed <- if (is.null(fixed)) integer(0) else as.integer(fixed)

  # Symmetric repulsion over all pairs: if a pair is under `floorvec` (per-pair),
  # push its endpoints apart toward it. Pure separation => no attraction, so the
  # layout never diverges. A closed sweep-mover: repeat until nothing moves.
  repulse <- function(floorvec, n_iter) {
    for (iter in seq_len(n_iter)) {
      dxv <- xy[j, 1] - xy[i, 1]; dyv <- xy[j, 2] - xy[i, 2]
      dv <- sqrt(dxv^2 + dyv^2)
      active <- which(dv < floorvec - 1e-9)
      if (length(active) == 0) break
      tosep <- floorvec[active] - dv[active]
      ux <- dxv[active]; uy <- dyv[active]
      near0 <- dv[active] < 1e-12            # coincident pairs get a +x push
      ux[near0] <- 1; uy[near0] <- 0
      nr <- sqrt(ux^2 + uy^2)
      ux <- ux / nr; uy <- uy / nr
      fo <- tosep * 0.5 * step
      Fx <- Fy <- numeric(n)
      Fx[i[active]] <- Fx[i[active]] - fo * ux; Fy[i[active]] <- Fy[i[active]] - fo * uy
      Fx[j[active]] <- Fx[j[active]] + fo * ux; Fy[j[active]] <- Fy[j[active]] + fo * uy
      if (length(fixed)) { Fx[fixed] <- 0; Fy[fixed] <- 0 }
      xy[, 1] <- xy[, 1] + Fx; xy[, 2] <- xy[, 2] + Fy
      if (any(!is.finite(xy))) break
    }
    xy
  }

  ## Phase 1: pack -- separate only the pairs that must NOT touch. Tactile pairs
  ## were already gathered to `touch` directly in the MDS distance matrix, so
  ## they keep their floor at `touch` (never pushed apart again); non-tactile
  ## pairs are held at `floor_d` so their silhouettes do not touch. Pure
  ## repulsion, therefore unconditionally stable.
  pack_floor <- ifelse(tac_pairs, touch, floor_d)
  xy <- repulse(pack_floor, n_iter)

  ## Phase 2: pinch -- pull tactile pairs in to `touch`, keeping everyone else
  ## (and tactile pairs themselves) from overlapping below their floor.
  for (iter in seq_len(pinch_iter)) {
    dxv <- xy[j, 1] - xy[i, 1]; dyv <- xy[j, 2] - xy[i, 2]
    dv <- sqrt(dxv^2 + dyv^2)
    # tactile pair farther than `touch`: attract it together.
    act <- which(tac_pairs & dv > touch + 1e-9)
    if (length(act) > 0) {
      mis <- dv[act] - touch
      ux <- dxv[act]; uy <- dyv[act]; nr <- sqrt(ux^2 + uy^2); ux <- ux / nr; uy <- uy / nr
      mv <- mis * 0.12
      Fx <- Fy <- numeric(n)
      Fx[i[act]] <- Fx[i[act]] + mv * ux; Fy[i[act]] <- Fy[i[act]] + mv * uy
      Fx[j[act]] <- Fx[j[act]] - mv * ux; Fy[j[act]] <- Fy[j[act]] - mv * uy
      if (length(fixed)) { Fx[fixed] <- 0; Fy[fixed] <- 0 }
      xy[, 1] <- xy[, 1] + Fx; xy[, 2] <- xy[, 2] + Fy
    }
    # recompute; separate any over-crowded pair: tactile under `vis_floor`,
    # non-tactile under `floor_d`.
    dxv <- xy[j, 1] - xy[i, 1]; dyv <- xy[j, 2] - xy[i, 2]
    dv <- sqrt(dxv^2 + dyv^2)
    sep <- which((tac_pairs & dv < vis_floor - 1e-9) | (!tac_pairs & dv < floor_d - 1e-9))
    if (length(sep) > 0) {
      tg <- ifelse(tac_pairs[sep], vis_floor, floor_d)
      tosep <- tg - dv[sep]
      ux <- dxv[sep]; uy <- dyv[sep]; nr <- sqrt(ux^2 + uy^2); ux <- ux / nr; uy <- uy / nr
      fo <- tosep * 0.3
      Fx <- Fy <- numeric(n)
      Fx[i[sep]] <- Fx[i[sep]] - fo * ux; Fy[i[sep]] <- Fy[i[sep]] - fo * uy
      Fx[j[sep]] <- Fx[j[sep]] + fo * ux; Fy[j[sep]] <- Fy[j[sep]] + fo * uy
      if (length(fixed)) { Fx[fixed] <- 0; Fy[fixed] <- 0 }
      xy[, 1] <- xy[, 1] + Fx; xy[, 2] <- xy[, 2] + Fy
    }
    d2 <- sqrt((xy[j, 1] - xy[i, 1])^2 + (xy[j, 2] - xy[i, 2])^2)
    if (all((!tac_pairs | d2 <= touch + 1e-9) &
            (!tac_pairs | d2 >= vis_floor - 1e-9) &
            (tac_pairs | d2 >= floor_d - 1e-9))) break
    if (any(!is.finite(xy))) break
  }

  layout$x <- xy[, 1]
  layout$y <- xy[, 2]
  layout
}

# -----------------------------------------------------------------------------
# Data simulation
# -----------------------------------------------------------------------------

#' Simulate a fake-but-faithful time series of interaction graphs
#'
#' Builds, for a set of individuals observed over 6-second windows, the `nodes`
#' and `edges` tables documented in `report/index.qmd`, plus the MDS layout. The
#' data is fictional but follows the documented schema and its semantic
#' constraints (dense `distance`, I-hub `en_lien_avec_activite`).
#'
#' The temporal structure builds on a tidygraph-style *type-preference* model:
#' each directed pair `(i,j)` is active in a window independently, with baseline
#' probability `p_density * p_pref[prof_i, prof_j]`, evolved over the windows as
#' a two-state Markov chain with `persistence` (so a pair tends to stay active
#' — temporal coherence — while still visiting its stationary probability).
#' Interaction types and `en_lien_avec_activite` are then sampled per active
#' pair / per window (see below), while the binary node attributes come from an
#' independent node-level activity-engagement state (see `action` /
#' `marque_interet` / `isolement`), and the distances give per-window MDS
#' layouts that are Procrustes-aligned and touch-constrained.
#'
#' Interaction *types* follow their own per-type two-state Markov chains over the
#' windows (`type_persistence`), so each type tends to stay present while still
#' revisiting its stationary per-window probability. `tactile` is the most
#' transient physical contact (weakest persistence); `dans_les_bras`, by contrast,
#' is a stable hold that can last most of the session (high persistence). The
#' orienting / play / dispute types persist moderately. Type sampling only occurs
#' on active edges: when a pair goes quiet the type is absent, and it restarts
#' from scratch if the pair returns.
#'
#' @param n_intervenants,n_animatrices,n_cat1,n_cat2,n_cat3 Number of nodes of
#'   each profile (defaults 1 / 3 / 3 / 3 / 3 -> 13 nodes).
#' @param n_states Number of temporal windows (default 6).
#' @param seed Random seed for reproducibility.
#' @param persistence Probability a pair stays active from one window to the
#'   next (`[0,1]`), the temporal-coherence parameter.
#' @param p_density Global edge density: `p_density * p_pref` is each directed
#'   pair's baseline activation probability.
#' @param engage_scale Multiplier on the intervenante's preference row
#'   (boosts I->child engagement).
#' @param p_pref Optional 5x5 relative preference matrix (rows/cols in
#'   [PROF_ORDER]); defaults to [default_p_pref()].
#' @param anchor_fraction Fraction of the session (`[0,1]`) during which the
#'   intervenante.e.s are anchored to their window-1 position (default 1/3): the
#'   adult leading the session stays put while the group works around her/him.
#'   After that window the intervenante is free to move. Anchoring snaps the
#'   intervenante's coordinates before the touch relaxation and pins those rows
#'   (`fixed=`) so they never translate during it.
#' @param engage_persist Persistence of the node-level activity-engagement state
#'   (default 0.85): from one window to the next, a child keeps their latent
#'   engagement state with this probability, else redraws from the profile's
#'   stationary distribution. Mirrors `persistence` but for the node attribute
#'   model (which is independent of the edge graph).
#'
#' @return A list (same contract as before) with:
#'   - `individus`: character vector of node names
#'   - `sujets`: subject table (`nom`, `age`, `role`)
#'   - `dist_pairs`: symmetric inter-individual distances per window (dense,
#'     one row per pair per `time_step`)
#'   - `nodes_ts`: node attributes per window, with the combined `profil`
#'   - `active_edges`: observed directed edges with one 0/1 flag per interaction
#'     type (one row per active edge per window)
#'   - `edges`: dense directed edges (all pairs x windows) with aggregated type
#'     flags and per-window `distance`
#'   - `edges_long`: long form (one edge per active type per window)
#'   - `layouts`: list of per-window MDS node positions, Procrustes-aligned onto
#'     window 1 so absolute positions stay stable as distances evolve
#'   - `layout`: the window-1 (reference frame) MDS positions, kept for callers
#'     that anchor the panel geometry to a single fixed frame
#'   - `graphs`: list of `tidygraph::tbl_graph`, one per window (active nodes =
#'     that window's node attributes, active edges = that window's long-form
#'     edges)
#' @export
simulate_graph_ts <- function(
    n_intervenants = 1L, n_animatrices = 3L,
    n_cat1 = 3L, n_cat2 = 3L, n_cat3 = 3L,
    n_states = 6L, seed = 731,
    persistence = 0.7, p_density = 0.30, engage_scale = 1.0,
    p_pref = NULL, anchor_fraction = 1/3, max_contact_nodes = 2L,
    engage_persist = 0.85,
    type_persistence = NULL, p_engage_state = NULL, attend_prob = 0.10) {
  # The intervenant.e.s is anchored for the first `floor(n_states * anchor_fraction)`
  # windows. With the default anchor_fraction = 1/3 the anchoring only lands on a
  # whole number of windows when n_states is a multiple of 3 — the UI duration
  # selector must therefore only offer multiples of 3 (e.g. 3, 6, ..., 21).
  if (anchor_fraction > 0 && n_states %% 3 != 0L)
    stop("n_states must be a multiple of 3 so the 1/3 intervenant anchoring covers a whole number of windows.")
  simulate_graph_ts_impl(
    n_intervenants, n_animatrices, n_cat1, n_cat2, n_cat3,
    n_states, seed, persistence, p_density, engage_scale, p_pref,
    anchor_fraction, max_contact_nodes, engage_persist,
    type_persistence, p_engage_state, attend_prob
  )
}

# Internal implementation of [simulate_graph_ts()]. Assembles the raw temporal
# model into the documented `gdata` contract (see [assemble_gdata()]).
simulate_graph_ts_impl <- function(
    n_intervenants, n_animatrices, n_cat1, n_cat2, n_cat3,
    n_states, seed, persistence, p_density, engage_scale, p_pref,
    anchor_fraction = 1/3, max_contact_nodes = 2L, engage_persist = 0.85,
    type_persistence = NULL, p_engage_state = NULL, attend_prob = 0.10) {
  # Defaults for the tunable probabilities if the caller passes NULL. Defaults
  # mirror the historical hard-fixed values so existing callers are unchanged
  # (and are read from the shared constants DEFAULT_TYPE_PERSISTENCE /
  # DEFAULT_P_ENGAGE_STATE so the UI sliders can use the same source).
  if (is.null(type_persistence)) type_persistence <- DEFAULT_TYPE_PERSISTENCE
  if (is.null(p_engage_state)) p_engage_state <- DEFAULT_P_ENGAGE_STATE
  if (is.null(p_pref)) p_pref <- default_p_pref()
  if (!all(rownames(p_pref) == PROF_ORDER) || !all(colnames(p_pref) == PROF_ORDER))
    stop("p_pref must be a 5x5 matrix with dimnames in PROF_ORDER")

  # Relative preference -> probability for a directed pair (clamped to (0,1]).
  p_adj <- p_pref
  p_adj[PROF_ORDER[1L], ] <- p_adj[PROF_ORDER[1L], ] * engage_scale
  # pmin/pmax would drop matrix dims; rebuild explicitly to keep structure.
  p_adj <- matrix(pmin(1, pmax(0.001, p_density * p_adj)),
                  nrow = 5L, ncol = 5L, dimnames = dimnames(p_pref))
  set.seed(seed)

  rook <- roster_nodes(n_intervenants, n_animatrices, n_cat1, n_cat2, n_cat3)
  individus <- rook$nom
  n <- nrow(rook)
  prof <- profil_of(rook)                                    # factor (plot order)
  prof_po <- match(as.character(prof), PROF_ORDER)           # index in PROF_ORDER

  windows <- seq_len(n_states)

  # ---- Directed pair existence: type-preference + temporal persistence ----
  pairs <- tidyr::expand_grid(i = seq_len(n), j = seq_len(n)) |>
    dplyr::filter(i != j)
  pairs$P <- p_adj[cbind(prof_po[pairs$i], prof_po[pairs$j])]

  # Two-state Markov chain over windows: active_1 ~ Ber(P); then with prob
  # `persistence` keep previous state, else redraw from Ber(P). Stationary
  # probability = P exactly, but short runs show temporal coherence.
  active <- matrix(0L, nrow = nrow(pairs), ncol = n_states)
  u <- matrix(runif(nrow(pairs) * n_states), nrow = nrow(pairs), ncol = n_states)
  s <- matrix(runif(nrow(pairs) * n_states), nrow = nrow(pairs), ncol = n_states)
  for (t in seq_len(n_states)) {
    if (t == 1L) {
      active[, t] <- as.integer(u[, t] < pairs$P)
    } else {
      keep <- s[, t] < persistence
      active[keep, t] <- active[keep, t - 1L]
      redraw <- !keep
      if (any(redraw)) active[redraw, t] <- as.integer(u[redraw, t] < pairs$P[redraw])
    }
  }

  # ---- Intervenante constant engagement: >=1 active out-edge to a child/window.
  ikids <- which(prof %in% c("Enfant CAT1", "Enfant CAT2", "Enfant CAT3"))
  int_nodes <- which(rook$role == "Intervenant.e.s")
  ikids_out <- which(pairs$i %in% int_nodes & pairs$j %in% ikids)
  ikids_mat <- split(seq_along(ikids_out), pairs$i[ikids_out])
  for (t in seq_len(n_states)) {
    for (inode in names(ikids_mat)) {
      sel <- ikids_mat[[inode]]
      if (!any(active[sel, t] == 1L)) {
        pick <- sample(sel, 1)
        active[pick, t] <- 1L
      }
    }
  }

  interaction <- active   # n_pairs x n_states of 0/1

  # ---- Interaction types present per active directed pair ----
  # Independent Bernoulli per type. `dispute` mostly among CAT2/CAT3; regard/joue
  # common. `tactile` and `dans_les_bras` are role-structured: a toddler's
  # physical contact is overwhelmingly with an adult, most often the Animatrice
  # (and most often "dans les bras" for the Animatrice-CAT1 dyad). Toddler-
  # toddler and CAT2/CAT3->adult tactile contact is rare. Rows/cols follow
  # PROF_ORDER = [Intervenant.e, Animatrice, CAT1, CAT2, CAT3].
  p_tactile <- matrix(c(
    #         Interv Anim CAT1  CAT2  CAT3
    0.05,     0.05, 0.30, 0.05, 0.05,   # Intervenant.e
    0.05,     0.05, 0.45, 0.10, 0.10,   # Animatrice
    0.30,     0.50, 0.05, 0.05, 0.05,   # Enfant CAT1
    0.05,     0.15, 0.05, 0.03, 0.03,   # Enfant CAT2
    0.05,     0.15, 0.05, 0.03, 0.03),  # Enfant CAT3
    nrow = 5L, byrow = TRUE,
    dimnames = list(PROF_ORDER, PROF_ORDER))
  p_dans_bras <- matrix(c(
    #         Interv Anim CAT1  CAT2  CAT3
    0.05,     0.05, 0.30, 0.05, 0.05,   # Intervenant.e
    0.05,     0.05, 0.55, 0.10, 0.10,   # Animatrice (most often dans les bras)
    0.30,     0.55, 0.05, 0.05, 0.05,   # Enfant CAT1
    0.05,     0.08, 0.05, 0.02, 0.02,   # Enfant CAT2
    0.05,     0.08, 0.05, 0.02, 0.02),  # Enfant CAT3
    nrow = 5L, byrow = TRUE,
    dimnames = list(PROF_ORDER, PROF_ORDER))
  type_prob <- function(i, j) {
    base <- c(orientation_corps = 0.7, regard = 0.6, joue_avec = 0.45,
              tactile = 0, dans_les_bras = 0, dispute = 0.06)
    base["tactile"] <- p_tactile[prof_po[i], prof_po[j]]
    base["dans_les_bras"] <- p_dans_bras[prof_po[i], prof_po[j]]
    base["dispute"] <- if (prof[i] %in% c("Enfant CAT2", "Enfant CAT3") &&
                           prof[j] %in% c("Enfant CAT2", "Enfant CAT3")) 0.35 else base["dispute"]
    base
  }
  # Per-type persistence: each type follows its own two-state Markov chain over
  # the windows (like edge activation), so a type that is present tends to stay
  # present while still revisiting its stationary per-window probability. Because
  # a type can only be observed on an active edge, we only resample while the
  # edge is active; when the edge goes quiet the type is (trivially) absent and
  # restarts fresh if the edge returns. `tactile` is the most transient physical
  # contact (weakest persistence); `dans_les_bras`, in contrast, is a *stable*
  # hold that can last most of the session, so it gets high persistence. The
  # orienting / play types persist moderately.
  type_t <- array(0L, dim = c(nrow(pairs), length(type_levels), n_states),
                  dimnames = list(NULL, type_levels, NULL))
  for (k in seq_len(nrow(pairs))) {
    pr <- type_prob(pairs$i[k], pairs$j[k])
    prev <- rep(0L, length(type_levels))
    names(prev) <- type_levels
    for (t in seq_len(n_states)) {
      if (interaction[k, t] != 1L) {
        prev[] <- 0L                      # edge quiet this window: type absent
        next
      }
      for (ty in type_levels) {
        if (pr[ty] <= 0) { prev[ty] <- 0L; next }
        if (t > 1L && prev[ty] == 1L && runif(1) < type_persistence[ty]) {
          next                            # keep previous type state
        }
        prev[ty] <- as.integer(runif(1) < pr[ty])
      }
      type_t[k, , t] <- prev
    }
  }
  # Every active pair must carry >=1 observable type: force `regard` if none.
  for (t in seq_len(n_states)) {
    for (k in seq_len(nrow(pairs))) {
      if (interaction[k, t] == 1L && sum(type_t[k, , t]) == 0) type_t[k, "regard", t] <- 1L
    }
  }
  # Limit simultaneous physical contact per node. A tactile interaction means
  # two nodes must sit  at `touch` apart; a node in contact with many people at
  # once (who are themselves far apart) is geometrically infeasible — no layout
  # can place it close to all of them. It is also implausible for a toddler
  # (contact is rare and brief). We therefore cap the number of *distinct*
  # contact partners (tactile OR dans_les_bras) a node can have per window at
  # `max_contact_nodes`. When a node exceeds the cap we drop the least durable
  # excess links: tactile before dans_les_bras, and (for a same-type choice) a
  # hold that was NOT present in the previous window, so a stable dans_les_bras
  # is preserved once started.
  if (max_contact_nodes > 0L) {
    for (t in seq_len(n_states)) {
      # Physical contact is symmetric: an incident contact pair {i,j} involves
      # BOTH nodes, so for each node we must consider the pair whether the node
      # appears as source (i) or destination (j). The `pairs` table holds BOTH
      # directed orientations of each physical pair, so a contact with a partner
      # typically spans two pair indices (i->j and j->i): the cap must drop a
      # whole PARTNER (every directed pair between node and that partner), not a
      # single oriented row, or the reverse row would keep the hold alive.
      contact_of <- lapply(seq_len(n), function(k) integer(0))
      for (k in seq_len(nrow(pairs))) {
        if (type_t[k, "tactile", t] == 1L || type_t[k, "dans_les_bras", t] == 1L) {
          contact_of[[pairs$i[k]]] <- c(contact_of[[pairs$i[k]]], k)
          contact_of[[pairs$j[k]]] <- c(contact_of[[pairs$j[k]]], k)
        }
      }
      # how to score a directed pair (higher = more worth keeping)
      keep_score <- function(k) {
        tac <- type_t[k, "tactile", t] == 1L
        bras <- type_t[k, "dans_les_bras", t] == 1L
        prev <- t > 1L && (type_t[k, "tactile", t-1L] == 1L ||
                           type_t[k, "dans_les_bras", t-1L] == 1L)
        (bras * 100L) + (prev * 10L) + (tac * 1L)
      }
      for (node in seq_len(n)) {
        ks <- contact_of[[node]]
        ks <- ks[type_t[ks, "tactile", t] == 1L | type_t[ks, "dans_les_bras", t] == 1L]
        if (length(ks) == 0L) next
        # group the directed pairs by their partner (the other end of each pair)
        ends <- ifelse(pairs$i[ks] == node, pairs$j[ks], pairs$i[ks])
        partner_by <- split(ks, ends)
        partners_ <- as.integer(names(partner_by))
        if (length(partners_) <= max_contact_nodes) next
        # score each distinct partner from its best (most durable) directed pair
        score_partner <- function(pa) max(vapply(partner_by[[as.character(pa)]],
                                                 keep_score, integer(1)))
        pscores <- vapply(partners_, score_partner, integer(1))
        # drop the (partners - cap) lowest-scoring partners
        ndrop <- length(partners_) - max_contact_nodes
        drop_partners <- partners_[order(pscores, sample.int(length(partners_)))][seq_len(ndrop)]
        # release every directed pair to each dropped partner (both orientations)
        for (pa in drop_partners) {
          for (k in partner_by[[as.character(pa)]]) {
            type_t[k, "tactile", t] <- 0L; type_t[k, "dans_les_bras", t] <- 0L
          }
        }
      }
    }
  }

  # ---- en_lien_avec_activite (link with the activity) ----
  lien_prob <- c("Intervenant.e" = 1, "Animatrice" = 0.1,
                 "Enfant CAT1" = 0.85, "Enfant CAT2" = 0.6, "Enfant CAT3" = 0.55)
  en_lien <- matrix(0L, nrow = nrow(pairs), ncol = n_states)
  for (t in seq_len(n_states)) {
    for (k in seq_len(nrow(pairs))) {
      if (interaction[k, t] != 1L) next
      i <- pairs$i[k]; j <- pairs$j[k]
      if (prof[i] == "Intervenant.e" || prof[j] == "Intervenant.e") {
        en_lien[k, t] <- 1L
      } else {
        pp <- max(lien_prob[prof[i]], lien_prob[prof[j]])
        en_lien[k, t] <- as.integer(runif(1) < pp)
      }
    }
  }

  # ---- Distances & layout (reuse mds/procrustes/enforce_touch) ----
  # Base inter-individual distance from preference (higher pref => closer),
  # bounded in [0.2, 1.6], plus a little noise per window so time is not static.
  # Two extra ingredients make the geometry richer than pure preference:
  #  * occasional "far wander": with small probability a child drifts far from
  #    the activity centre (distances to everyone inflated toward the max). Edge
  #    types (regard / joue_avec / ...) are drawn independently of distance, so
  #    a distant child can remain engaged — "far away yet still involved".
  #  * tactile / dans_les_bras pairs are placed CLOSE (<= touch) directly in the
  #    MDS distance matrix, so those nodes are gathered together globally rather
  #    than only pinched after the fact. This resolves the geometric
  #    infeasibility of a node in contact with several far-apart others.
  touch_d <- 0.25
  wander_prob <- 0.10           # p(a child wanders far from centre) per window
  base_d <- matrix(0.9, n, n, dimnames = list(individus, individus))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i < j) {
      d <- 0.9 - 0.5 * p_pref[prof_po[i], prof_po[j]]
      base_d[i, j] <- d; base_d[j, i] <- d
    }
  }
  perw_d <- lapply(windows, function(t) {
    d <- base_d
    if (t > 1L) {
      d <- base_d + (t - 1L) * matrix(rnorm(n * n, 0, 0.04), n, n) +
        matrix(rnorm(n * n, 0, 0.03), n, n)
    }
    # tactile / dans_les_bras: gather the pair to <= touch (symmetric)
    tac <- which(type_t[, "tactile", t] == 1L | type_t[, "dans_les_bras", t] == 1L)
    in_touch <- logical(n)
    if (length(tac)) {
      a <- pairs$i[tac]; b <- pairs$j[tac]
      in_touch[unique(c(a, b))] <- TRUE
      for (k in seq_along(tac)) {
        d[a[k], b[k]] <- touch_d; d[b[k], a[k]] <- touch_d
      }
    }
    # far wander: push a few children's distances to everyone toward max. A child
    # can drift far from the centre yet stay involved (regard/joue_avec edges are
    # drawn independently of distance), but NOT one in physical contact — touching
    # someone means being close, which contradicts drifting far.
    wander <- prof %in% c("Enfant CAT1", "Enfant CAT2", "Enfant CAT3") &
      !in_touch & runif(n) < wander_prob
    if (any(wander)) {
      for (k in which(wander)) {
        d[k, ] <- pmax(d[k, ], 1.25); d[, k] <- d[k, ]
      }
    }
    matrix(pmin(1.6, pmax(0.2, d)), n, n, dimnames = dimnames(base_d))
  })
  up <- which(upper.tri(base_d), arr.ind = TRUE)
  dist_pairs <- dplyr::bind_rows(lapply(windows, function(t) {
    tibble::tibble(p1 = individus[up[, 1]], p2 = individus[up[, 2]],
                   d = perw_d[[t]][up], time_step = t)
  }))

  # ---- Node binary attributes: a node-level activity-engagement state ----
  # The three binary node attributes evaluate an individual's engagement with
  # the ACTIVITY (a node-level property), and are INDEPENDENT of the
  # individual-individual edge graph. They follow from a single latent
  # engagement state per node per window:
  #   isolement      = 1  <=> disengaged from the activity (isolation w.r.t. the
  #                          activity, regardless of any inter-individual arcs);
  #   marque_interet = 1  <=> shows interest in the activity (i.e. not
  #                          disengaged - attending passively OR producing);
  #   action         = 1  <=> actively producing something activity-related
  #                          (making music, handling a book / instrument).
  # Only the CHILDREN's states actually vary (that is where the attribute is
  # informative). The adults are pinned to fixed observational rules:
  #   - Intervenant.e.s: always producing & interested, never isolated;
  #   - Animatrices: asked not to run the activity -> action = 0, and never
  #     isolated (isolement = 0); marked interested in it EXCEPT on a window
  #     where they are pulled away to attend a particular child in need
  #     (marque_interet = 0), a rare per-window event.
  # The children's latent state S is a 3-level ordinal drawn over the windows
  # as a reusing two-"step" chain (like edge activation): with probability
  # `engage_persist` the state carries over, else it is redrawn from the
  # profile's stationary distribution. So a toddler tends to stay in one mode
  # for a short run (temporal coherence) while drifting toward the profile's
  # baseline. Because the state is node-level it does NOT inherit coherence from
  # the edge state - the two are independent, as the activity and the
  # inter-person contact are distinct facets of the observation.
  int_rows <- which(rook$role == "Intervenant.e.s")
  anim_rows <- which(rook$role == "Animatrice")
  kids_rows <- which(rook$role == "Enfant")
  # Stationary probability of each latent state per child profile. CAT1 are the
  # least autonomous: most often disengaged or merely passive; CAT3 the most.
  pS <- matrix(0, n, 3L, dimnames = list(individus, c("desengage", "passif", "actif")))
  for (k in kids_rows) pS[k, ] <- p_engage_state[[as.character(prof[k])]]
  # Reusing chain over the 3 ordinal states (adults never sampled; margins 0).
  states <- colnames(pS)
  S <- matrix("", nrow = n, ncol = n_states, dimnames = list(individus, NULL))
  U <- matrix(runif(n * n_states), nrow = n, ncol = n_states)
  R <- matrix(runif(n * n_states), nrow = n, ncol = n_states)
  for (t in seq_len(n_states)) {
    for (k in kids_rows) {
      if (t == 1L) {
        S[k, t] <- states[findInterval(U[k, t], cumsum(pS[k, ])) + 1L]
      } else if (R[k, t] >= engage_persist) {
        S[k, t] <- states[findInterval(U[k, t], cumsum(pS[k, ])) + 1L]
      } else {
        S[k, t] <- S[k, t - 1L]
      }
    }
  }
  # Probability that an Animatrice is pulled away to attend a child in need on
  # a given window (rare; provisional stand-in for "a child requires particular
  # attention", to be refined against the field notes). Then marque_interet = 0.
  attr_t <- lapply(windows, function(t) {
    at <- matrix(0L, n, 3L,
                 dimnames = list(individus, c("marque_interet", "action", "isolement")))
    # Intervenant.e.s
    at[int_rows, c("marque_interet", "action")] <- 1L
    # Animatrices
    attend <- runif(length(anim_rows)) < attend_prob
    at[anim_rows, "marque_interet"] <- as.integer(!attend)
    # Children: read from the latent engagement state.
    for (k in kids_rows) {
      s <- S[k, t]
      at[k, "isolement"]      <- as.integer(s == "desengage")
      at[k, "marque_interet"] <- as.integer(s != "desengage")
      at[k, "action"]         <- as.integer(s == "actif")
    }
    at
  })

  # ---- Assemble into the documented `gdata` structure ----
  assemble_gdata(sim = list(
    rook = rook, prof = prof, pairs = pairs, interactions = interaction,
    type_t = type_t, en_lien = en_lien, dist_pairs = dist_pairs, attr_t = attr_t,
    anchor_fraction = anchor_fraction
  ))
}

# Turn the internal simulation into the documented `gdata` list (the contract
# consumed by `plot_subgraph`, `plot_graph`, `plot_graph_static` and
# `plot_subgraph_layers`). Kept as a separate step so the raw model can be
# reused / debugged independently.
assemble_gdata <- function(sim) {
  rook <- sim$rook; individus <- rook$nom; n <- nrow(rook)
  prof <- sim$prof
  windows <- seq_len(ncol(sim$interactions))
  dist_pairs <- sim$dist_pairs
  anchor_fraction <- if (is.null(sim$anchor_fraction)) 0 else sim$anchor_fraction

  # nodes_ts: one row per (node, window) with binary attrs + combined profile.
  nodes_ts <- dplyr::bind_rows(lapply(windows, function(t) {
    at <- sim$attr_t[[t]]
    tibble::tibble(nom = individus, time_step = t,
                   marque_interet = at[, "marque_interet"],
                   action = at[, "action"], isolement = at[, "isolement"],
                   profil = prof)
  }))

  # active_edges: one row per active directed (i,j) per window, with one 0/1
  # flag per interaction type. Built DRY over `type_levels` so adding a type
  # updates here and in the `edges` summarise automatically.
  nm1 <- individus[sim$pairs$i]; nm2 <- individus[sim$pairs$j]
  active_edges <- dplyr::bind_rows(lapply(windows, function(t) {
    act <- which(sim$interactions[, t] == 1L)
    if (length(act) == 0) return(NULL)
    base_tbl <- tibble::tibble(
      nom1 = nm1[act], nom2 = nm2[act], time_step = t,
      en_lien_avec_activite = sim$en_lien[act, t])
    for (ty in type_levels) base_tbl[[ty]] <- as.integer(sim$type_t[act, ty, t])
    base_tbl
  }))

  # edges: dense full grid (every directed pair x window) + distance + flags.
  edges <- tidyr::expand_grid(nom1 = individus, nom2 = individus, time_step = windows) |>
    dplyr::filter(nom1 != nom2) |>
    dplyr::mutate(pair = purrr::map2_chr(nom1, nom2, ~ paste(sort(c(.x, .y)), collapse = "|")))
  dpw <- dist_pairs |>
    dplyr::mutate(pair = paste(pmin(p1, p2), pmax(p1, p2), sep = "|")) |>
    dplyr::select(pair, time_step, d)
  edges <- edges |>
    dplyr::left_join(dpw, by = c("pair", "time_step")) |>
    dplyr::rename(distance = d) |>
    dplyr::left_join(
      active_edges |>
        dplyr::group_by(nom1, nom2, time_step) |>
        dplyr::summarise(
          en_lien_avec_activite = as.integer(any(en_lien_avec_activite == 1)),
          dplyr::across(tidyselect::all_of(type_levels),
                        ~ as.integer(any(.x == 1))),
          .groups = "drop"),
      by = c("nom1", "nom2", "time_step")) |>
    dplyr::mutate(
      interaction = ifelse(!is.na(en_lien_avec_activite), 1L, 0L),
      en_lien_avec_activite = ifelse(is.na(en_lien_avec_activite), 0L, en_lien_avec_activite)) |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(type_levels), ~ replace(.x, is.na(.x), 0L)))

  # edges_long: one row per active type per window.
  edges_long <- edges |>
    tidyr::pivot_longer(tidyselect::all_of(type_levels),
                        names_to = "type", values_to = "present") |>
    dplyr::filter(interaction == 1, present == 1) |>
    dplyr::select(-present) |>
    dplyr::mutate(
      type = factor(type, levels = type_levels),
      lien = factor(ifelse(en_lien_avec_activite == 1, "en lien", "hors activité"),
                    levels = c("hors activité", "en lien")))

  # layouts: per-window MDS, Procrustes-aligned onto window 1, then touch rule.
  mds_w <- lapply(windows, function(t) {
    mds_layout(dist_pairs |> dplyr::filter(time_step == t), individus)
  })
  Xref <- as.matrix(mds_w[[1]][, c("x", "y")])
  layouts <- lapply(windows, function(t) {
    if (t == 1L) return(mds_w[[1]])
    a <- procrustes_align(Xref, as.matrix(mds_w[[t]][, c("x", "y")]))
    tibble::tibble(nom = mds_w[[t]]$nom, x = a[, 1], y = a[, 2])
  })
  tactile_mat <- lapply(windows, function(t) {
    # Physical contact = tactile OR dans les bras (holding a toddler in arms is
    # the literal overlap form, most common for the CAT1-Animatrice dyad).
    te <- edges |> dplyr::filter(time_step == t, tactile == 1 | dans_les_bras == 1)
    m <- matrix(FALSE, n, n, dimnames = list(individus, individus))
    for (k in seq_len(nrow(te))) {
      m[te$nom1[k], te$nom2[k]] <- TRUE; m[te$nom2[k], te$nom1[k]] <- TRUE
    }
    m
  })
  # Change A: anchoring. The animatrices are pinned for the WHOLE session —
  # they never move (snapped to a fixed reference position in the reference
  # frame and passed as `fixed` for every window). The intervenante.e.s are
  # pinned only for the first `anchor_fraction` of the session, then set free.
  # This mirrors the observed structure where the adults stay anchored while
  # the children move around them. Snapping is done BEFORE the touch
  # relaxation, and pinned rows are passed as `fixed` so the relaxation never
  # drags them.
  #
  # The reference positions are NOT the raw window-1 MDS coordinates: in a
  # somewhat homogeneous adult set (e.g. 4 mutually-far adults) cmdscale with
  # k=2 collapses coincident-distance nodes onto a single point, so two adults
  # can overlap exactly. Worse, because every adult is `fixed` (force zeroed) in
  # the touch relaxation, nothing can ever separate them. We therefore
  # pre-separate the adults ONCE in the reference frame: run the same
  # physical-contact rule on just the adult subset of Xref (no fixed rows, so
  # non-tactile adults pack apart to the floor), producing `adult_anchor`, a
  # deterministic, overlap-free adult configuration that is snapped identically
  # in every anchored window (so animatrices stay frozen across the whole
  # session and the intervenant's frozen fragment is consistent too).
  anim_rows <- which(rook$role == "Animatrice")
  int_rows <- which(rook$role == "Intervenant.e.s")
  adult_rows <- c(anim_rows, int_rows)
  # Anchor count from the session fraction. A small tolerance is essential: the
  # app slider delivers anchor_fraction as a ~15-digit decimal (e.g.
  # 0.333333333333333, one ULP below 1/3), so `floor(6 * 0.333333333333333)`
  # truncates 1.9999... to 1 and the intervenant.e.s would de-anchor one window
  # too early. `+ 1e-9` snaps values sitting a hair below an integer; genuine
  # sub-integer proportions are unaffected at window counts used here.
  n_anchor <- floor(length(windows) * anchor_fraction + 1e-9)
  # adult-only physical contact, in the adult coordinate system
  adult_tact <- tactile_mat[[1L]][adult_rows, adult_rows, drop = FALSE]
  adult_anchor <- enforce_touch_layout(
    tibble::tibble(
      nom = individus[adult_rows],
      x = Xref[adult_rows, 1], y = Xref[adult_rows, 2]),
    tactile = adult_tact)
  # full-matrix row indices of each adult's position within `adult_anchor`
  anim_an <- match(anim_rows, adult_rows)
  int_an <- match(int_rows, adult_rows)
  for (t in windows) {
    lay <- layouts[[t]]
    lay$x[anim_rows] <- adult_anchor$x[anim_an]
    lay$y[anim_rows] <- adult_anchor$y[anim_an]
    layouts[[t]] <- lay
  }
  for (t in windows[windows <= n_anchor]) {
    lay <- layouts[[t]]
    lay$x[int_rows] <- adult_anchor$x[int_an]
    lay$y[int_rows] <- adult_anchor$y[int_an]
    layouts[[t]] <- lay
  }
  layouts <- lapply(windows, function(t) {
    fixed <- anim_rows                                   # animatrices: always pinned
    fixed <- unique(c(fixed, if (t <= n_anchor) int_rows else integer(0)))
    # A physical contact (tactile / dans les bras) with a pinned adult leaves the
    # child endpoint FREE, so the pinch relaxation pulls the child in to `touch`.
    # We deliberately do NOT pin the child side of a pair: freezing it would
    # strand the pair at whatever MDS gave, instead of honoring the ~touch target.
    enforce_touch_layout(layouts[[t]], tactile_mat[[t]], fixed = fixed)
  })
  layout <- layouts[[1L]]

  # one tbl_graph per window (same contract as before).
  graphs <- lapply(windows, function(t) {
    tidygraph::tbl_graph(
      nodes = nodes_ts |> dplyr::filter(time_step == t),
      edges = edges_long |> dplyr::filter(time_step == t),
      directed = TRUE, node_key = "nom")
  })

  list(
    individus   = individus,
    sujets      = rook,
    dist_pairs  = dist_pairs,
    nodes_ts    = nodes_ts,
    active_edges = active_edges,
    edges       = edges,
    edges_long  = edges_long,
    layouts     = layouts,
    layout      = layout,
    graphs      = graphs
  )
}

# -----------------------------------------------------------------------------
# Plotting
# -----------------------------------------------------------------------------

#' Decorate and draw an interaction graph
#'
#' Core plotting routine shared by [plot_graph()] (one window) and
#' [plot_graph_static()] (a single illustrative graph): directed fan edges
#' coloured and dashed by interaction type, thickness by `lien` (honouring the
#' I-hub constraint), nodes encoded by shape (profil), size (marque_intérêt),
#' fill (action) and border colour (isolement). Every attribute is carried by a
#' distinct visual property (no double encoding).
#'
#' @param graph A `tidygraph::tbl_graph` whose active nodes carry the node
#'   attributes (`nom`, `profil`, `marque_interet`, `action`, `isolement`) and
#'   whose active edges carry `type` and `lien`.
#' @param layout A data frame of MDS positions (`nom`, `x`, `y`) for the graph's
#'   nodes, in the same coordinate frame (per-window positions, Procrustes-
#'   aligned so they stay stable across windows).
#' @param title,subtitle Optional titles.
#'
#' @return A `ggraph` object.
#' @export
plot_subgraph <- function(graph, layout, title = NULL, subtitle = NULL) {
  nodes <- graph |> tidygraph::activate(nodes) |> tidygraph::as_tibble()
  lay_f <- dplyr::left_join(
    tibble::tibble(nom = nodes$nom), layout, by = "nom"
  )

  ggraph::ggraph(graph, layout = "manual", x = lay_f$x, y = lay_f$y) +
    ggraph::geom_edge_fan(
      ggplot2::aes(colour = type, width = lien, linetype = type),
      arrow = ggplot2::arrow(length = ggplot2::unit(3, 'mm'), type = "closed"),
      end_cap = ggraph::circle(7, 'mm'), start_cap = ggraph::circle(7, 'mm'),
      alpha = 0.85
    ) +
    # same name for colour & linetype => a single legend for the type
    ggraph::scale_edge_colour_manual(values = type_cols, name = "Type d'interaction") +
    ggraph::scale_edge_linetype_manual(values = type_lty, name = "Type d'interaction") +
    # en_lien_avec_activite is binary => discrete width (2 levels)
    ggraph::scale_edge_width_manual(
      values = c("hors activité" = 0.6, "en lien" = 2),
      name = "Lien avec l'activité"
    ) +
    ggraph::geom_node_point(
      aes(
        shape = profil, size = factor(marque_interet),
        fill = factor(action), colour = factor(isolement)
      ),
      stroke = 1.3
    ) +
    ggplot2::scale_shape_manual(
      values = profil_shapes, name = "Profil",
      guide = ggplot2::guide_legend(ncol = 2)
    ) +
    ggplot2::scale_fill_manual(
      name = "Action", values = c(`0` = "white", `1` = "grey25"),
      guide = ggplot2::guide_legend(
        ncol = 2, override.aes = list(size = 4, shape = 21)
      )
    ) +
    # isolement acts on the border: hollow keys to show it
    ggplot2::scale_colour_manual(
      name = "Isolement", values = c(`0` = "black", `1` = "#d62728"),
      guide = ggplot2::guide_legend(
        ncol = 2, override.aes = list(fill = "white", size = 4,
                                      shape = 21, stroke = 2)
      )
    ) +
    ggplot2::scale_size_manual(
      name = "Marque intérêt", values = c(`0` = 4, `1` = 7),
      guide = ggplot2::guide_legend(
        ncol = 2, override.aes = list(shape = 21, fill = "grey70")
      )
    ) +
    ggraph::geom_node_text(ggplot2::aes(label = nom), vjust = 2.5, fontface = "bold") +
    # expansion so nodes/labels are not clipped at the edge
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.35)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.35)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggraph::theme_graph() +
    ggplot2::theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.key.height = ggplot2::unit(0.75, "cm"),
      legend.spacing.y = ggplot2::unit(0.2, "cm"),
      legend.title = ggplot2::element_text(size = 9, face = "bold"),
      legend.text = ggplot2::element_text(size = 9)
    )
}

#' Plot the interaction graph of one time window
#'
#' Draws the graph of a single 6-second window `t` using the shared simulation
#' data and layout. See [plot_subgraph()] for the visual encodings.
#'
#' @param gdata A list returned by [simulate_graph_ts()].
#' @param t     The time window (an integer `time_step`) to draw.
#'
#' @return A `ggraph` object.
#' @export
plot_graph <- function(gdata, t) {
  plot_subgraph(
    gdata$graphs[[t]], gdata$layouts[[t]],
    title = "Évolution du graphe d'interactions",
    subtitle = paste0("Fenêtre temporelle : ", t)
  )
}

#' Plot a single illustrative interaction graph (all types)
#'
#' Aggregates the active edges over all windows (keeping each directed
#' interaction type once) and draws a single graph on the first window's node
#' attributes. This shows the full 6-type mapping on one figure.
#'
#' @param gdata A list returned by [simulate_graph_ts()].
#'
#' @return A `ggraph` object.
#' @export
plot_graph_static <- function(gdata) {
  nodes <- gdata$nodes_ts |>
    dplyr::filter(time_step == min(time_step))
  edges <- gdata$edges_long |>
    dplyr::select(-time_step) |>
    dplyr::distinct(nom1, nom2, type, lien, .keep_all = TRUE)
  g <- tidygraph::tbl_graph(
    nodes = nodes, edges = edges, directed = TRUE, node_key = "nom"
  )
  # Frame of reference (window 1): same coordinate frame as each window in the app.
  plot_subgraph(
    g, gdata$layouts[[1]],
    title = "Graphe des interactions (exemple)",
    subtitle = "Ensemble des types d'interaction observés"
  )
}

#' Shiny renderer for the time-series of interaction graphs
#'
#' A `renderPlot`-compatible server-side renderer that draws a chosen time
#' window in a Shiny app. Wire it to a `sliderInput` over the windows: the
#' native play animation of the slider provides the temporal navigation while
#' keeping full ggraph rendering (arrow caps included), unlike an animated GIF.
#'
#' @param gdata A list returned by [simulate_graph_ts()].
#' @param input_value A reactive expression returning the selected window.
#'
#' @return A `shiny::renderPlot` expression to assign to an `output$...`.
#' @export
renderGraphTS <- function(gdata, input_value) {
  shiny::renderPlot({
    plot_graph(gdata, as.integer(input_value()))
  }, height = 680, res = 120)
}

#' Extract the assembled legend from a ggraph/ggplot plot
#'
#' Pulls the fully assembled right-hand legend (a `gtable` grob, `~7 x 14.5 cm`,
#' containing the edge and node guides) out of a rendered plot so it can be drawn
#' separately, e.g. moved out of the plotting panel and into a Shiny sidebar. The
#' legend only depends on stable factor levels, so it is identical across time
#' windows: build it once (e.g. from window 1) and reuse it.
#'
#' @param p A `ggraph`/`ggplot` object with a right-side legend.
#'
#' @return A `gtable` grob suitable for `grid::grid.draw()` inside a
#'   `shiny::renderPlot()`.
#' @export
legend_grob <- function(p) {
  g <- ggplot2::ggplotGrob(p)
  i <- which(g$layout$name == "guide-box-right")
  if (length(i) == 0) stop("No right-hand legend found in the plot")
  g$grobs[[i]]
}

#' Scale a grid grob so it fits inside a device
#'
#' Uniformly shrinks a grob produced by [grid::grid.draw()] — such as the gtable
#' legend from [legend_grob()], whose absolute `cm` sizes would otherwise be
#' clipped by a smaller device — by rescaling every absolute length unit and
#' viewport in the grob tree, plus text font sizes. Relative units (`npc`,
#' `null`, `strwidth`, ...) are left untouched.
#'
#' @param x A grob / gtable / gTree (e.g. a legend extracted via `legend_grob()`).
#' @param s Positive scale factor (e.g. `0.6` draws it at 60%).
#'
#' @return A structurally identical grob whose physical size is `s`-scaled.
#' @export
scale_grob <- function(x, s) {
  if (is.null(x)) return(x)
  if (s <= 0 || s == 1) return(x)
  if (is.unit(x)) return(scale_unit_(x, s))
  if (inherits(x, "gtable")) {
    x$widths  <- scale_unit_(x$widths, s)
    x$heights <- scale_unit_(x$heights, s)
    x$grobs   <- lapply(x$grobs, scale_grob, s = s)
    x$vp <- NULL
    return(x)
  }
  for (slot in intersect(c("x", "y", "width", "height", "x0", "y0", "x1", "y1", "r"), names(x))) {
    v <- x[[slot]]
    if (grid::is.unit(v)) x[[slot]] <- scale_unit_(v, s)
  }
  if (!is.null(x$children)) x$children <- lapply(x$children, scale_grob, s = s)
  if (!is.null(x$gp) && is.list(x$gp)) {
    if (is.numeric(x$gp$fontsize)) x$gp$fontsize <- x$gp$fontsize * s
    if (is.numeric(x$gp$cex)) x$gp$cex <- x$gp$cex * s
  }
  x
}

# Rescale absolute length units by factor `s` (internal helper).
scale_unit_ <- function(u, s) {
  if (length(u) == 0) return(u)
  kinds <- rep(attr(u, "unit") %||% "npc", length.out = length(u))
  out <- u
  for (k in seq_along(u)) {
    if (kinds[k] %in% c("cm", "mm", "inches", "in", "bigpts", "pt", "points")) {
      val_cm <- grid::convertUnit(u[k], "cm", valueOnly = TRUE)
      out[k] <- grid::unit(val_cm * s, "cm")
    }
  }
  out
}

#' Draw a legend scaled to fit a rendering device
#'
#' Convenience wrapper for use inside a `shiny::renderPlot()` (or any grid
#' device): scales a legend grob — usually from [legend_grob()] — so it fits the
#' current graphics device with a margin, preserving its aspect ratio. The scale
#' is computed from the device size at `res` (pixels per inch).
#'
#' @param legend A legend grob (gtable) to draw.
#' @param res Device resolution in pixels per inch (matches the `res` argument
#'   of `shiny::renderPlot`/`png`).
#' @param margin Fraction of the limiting device dimension the legend may occupy
#'   (default `0.9` leaves a small border).
#'
#' @return Invisible `NULL`; draws to the active device.
#' @export
draw_scaled_legend <- function(legend, res = 120, margin = 0.9) {
  devw_px <- as.numeric(grid::convertWidth(grid::unit(1, "npc"), "cm", valueOnly = TRUE)) / 2.54 * res
  devh_px <- as.numeric(grid::convertHeight(grid::unit(1, "npc"), "cm", valueOnly = TRUE)) / 2.54 * res
  gw <- as.numeric(grid::convertWidth(grid::grobWidth(legend), "cm", valueOnly = TRUE))
  gh <- as.numeric(grid::convertHeight(grid::grobHeight(legend), "cm", valueOnly = TRUE))
  s <- min(devw_px / (gw * res / 2.54), devh_px / (gh * res / 2.54)) * margin
  scaled <- scale_grob(legend, s)
  grid::grid.draw(scaled)
  invisible(NULL)
}
