# -----------------------------------------------------------------------------
# app-utils.R -- dependencies and utility functions of the ggraph app.
# -----------------------------------------------------------------------------
# This file groups ALL the app's utility functions/values (mappings, layout
# choices, scales, legend, default dataset...), plus the required `library()`
# calls. It is consumed two ways, which must stay in sync:
#   * standalone app -- `app.R` does `source("app-utils.R")` (and app-utils.R
#     loads `R/graph-model.R`, the shared source of truth) ;
#   * inside the `{shinylive-r}` block -- the `{{< include >}}` shortcode
#     injects `R/graph-model.R` just before it (and app-utils.R / app.R right
#     after), leaving the block self-contained for webR (no local `source()`).
#     The `file.exists()` guards below make the file harmless in webR (none of
#     these paths exist in the runtime), while still loading the dependencies
#     on its own.

# Load the shared source of truth (simulation + encodings) on its own. In the
# shinylive block, `R/graph-model.R` is already injected above via {{< include >}}:
# none of these paths exist in webR -> no-op. From `R/interactions-app/`,
# `../graph-model.R` walks up to `R/`.
for (.graph_model_src in c("../graph-model.R", "graph-model.R", "../../R/graph-model.R")) {
  if (file.exists(.graph_model_src)) {
    source(.graph_model_src)
    break
  }
}
rm(.graph_model_src)

# Packages required by `R/graph-model.R` and by `plot_subgraph()` (ggraph and
# its C++ dependencies, on the webR side), plus `bslib` for the legend popover.
# Do NOT install them manually here: shinylive detects these `library()` / `::`
# calls (via `renv::dependencies()`) and installs them **before** starting the
# app (the webR runtime's `.start_app()` function), which avoids the race: the
# graph is only requested after ggraph/tidygraph/ggplot2 are loaded.
library(shiny)
library(bslib)        # popover() for the legend + modern page_sidebar()
library(shinyWidgets) # pickerInput(): lists all layouts but greys out those
                      # incompatible with at least one graph of the series
library(ggraph)
library(tidygraph)
library(graphlayouts)  # advanced layouts (stress, ...) via the layout selector
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(rlang)     # conditional aes construction (aes(!!!node_aes))
library(gtable)    # legend: grob extraction (guide-box-right)
library(grid)      # legend: standalone rendering via grid.draw()

# ------------------------------------------------------------------------------
# Dev-local plot generator: flexible version of `plot_subgraph()`
# ------------------------------------------------------------------------------
# `plot_subgraph()` in `R/graph-model.R` is frozen (source of truth of the
# report); it encodes ALL layers every time. Here `plot_subgraph_layers()` lets
# the user map EACH visual property (edge colour/width/linetype; node
# shape/colour/fill/size) onto a criterion (edge or node), or none; only the
# corresponding mappings + scales are added (so the legend follows the active
# mappings). The locked encodings (type_cols, type_lty, profil_shapes, guides)
# are strictly identical to the report for the canonical criterion -> property
# combinations.
# Available criteria and their mapping type (edge vs node).
edge_crit <- c("type", "lien")
node_crit <- c("profil", "marque_interet", "action", "isolement")

# Criterion labels (legend title and menu labels).
crit_labels <- c(
  type = "Type d'interaction", lien = "Lien avec l'activité",
  profil = "Profil", marque_interet = "Marque d'intérêt",
  action = "Action", isolement = "Isolement"
)

# Menu of each visual property: the list of compatible criteria (edge or node),
# labelled "criterion (mapping type)", with "Aucun" (= no mapping) by default.
edge_choices <- setNames(c("", edge_crit),
                         c("Aucun", crit_labels[edge_crit]))
node_choices <- setNames(c("", node_crit),
                         c("Aucun", crit_labels[node_crit]))

# Default mapping = the report's locked canonical encoding. It pre-selects the
# menus at open (the graph shows the full encoding right away) and is applied by
# the "Tout encoder" button; only "Tout vider" resets everything to "".
default_mapping <- c(
  edge_colour = "type", edge_width = "lien", edge_linetype = "type",
  shape = "profil", colour = "isolement", fill = "action", size = "marque_interet"
)

# Node placement layouts. "manual" = the home-made MDS + Procrustes (Distance)
# placement, anchored per window in `gdata$layouts`; the others are the placement
# algorithms computed by ggraph, igraph or graphlayouts, the stored name (the
# "value" sent to the server) serving as `layout=` for ggraph(). Unlike a
# restricted list, we list ALL layouts here (including the large graphlayouts
# family); those incompatible with the temporal series are simply GREYED OUT
# (disabled) in the selector. A layout is disabled as soon as it fails on AT
# LEAST one graph of the series: navigating between windows must remain possible
# for any active choice. The failures come from the (sometimes disconnected)
# nature of the graphs -- `eigen` requires a connected graph, `pmds`/
# `sparse_stress` likewise, `backbone` requires the optional `oaqc` package,
# `focus`/`centrality` need per-graph arguments. Note: for the selector, the
# VALUE is the vector element and the NAME is the displayed label.
layout_choices <- setNames(
  c("manual", "fr", "kk", "nicely", "mds", "circle", "star", "grid",
    "tree", "sugiyama", "dh", "drl", "gem", "graphopt",
    "stress", "eigen", "pmds", "sparse_stress", "backbone", "focus", "centrality"),
  c("Distance",
    "ggraph : Fruchterman-Reingold",
    "ggraph : Kamada-Kawai",
    "ggraph : nicely",
    "ggraph : MDS",
    "ggraph : cercle",
    "ggraph : étoile",
    "ggraph : grille",
    "ggraph : arbre",
    "ggraph : Sugiyama",
    "igraph : Davidson-Harel",
    "igraph : DrL",
    "igraph : GEM",
    "igraph : Graphopt",
    "graphlayouts : Stress",
    "graphlayouts : Eigen",
    "graphlayouts : PMDS",
    "graphlayouts : Sparse stress",
    "graphlayouts : Backbone",
    "graphlayouts : Focus",
    "graphlayouts : Centrality")
)

# Generic palette for NON-canonical (property, criterion) combinations.
qual_pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
              "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

# CANONICAL levels of each criterion, independent of the temporal window. They
# serve as `limits` for the manual scales: the legend is thus FULL and FIXED for
# a given mapping (every level of the criterion is always shown), and only
# changes when the mapping changes -- not when navigating in time. Without these
# `limits`, ggplot only shows the levels present in the current window (e.g.
# `tactile` or `dans_les_bras` disappear depending on the window). Since these are
# declared factors (type, lien, profil) or 0/1 binaries (marque_interet, action,
# isolement), the vocabulary is fixed.
crit_levels <- function(var) {
  switch(var,
    type            = type_levels,
    lien            = c("hors activité", "en lien"),
    profil          = names(profil_shapes),
    marque_interet  = c("0", "1"),
    action          = c("0", "1"),
    isolement       = c("0", "1")
  )
}

# Scale values for (property, criterion). Canonical mappings -> the report's
# locked encodings; otherwise generic default palettes/scales.
prop_values <- function(prop, var, levels) {
  canon <- switch(paste(var, prop, sep = "|"),
    "type|edge_colour"    = type_cols,
    "type|edge_linetype"  = type_lty,
    "lien|edge_width"     = c("hors activité" = 0.6, "en lien" = 2),
    "profil|shape"        = profil_shapes,
    "action|fill"         = c(`0` = "white", `1` = "grey25"),
    "isolement|colour"    = c(`0` = "black", `1` = "#d62728"),
    "marque_interet|size" = c(`0` = 4, `1` = 7)
  )
  if (!is.null(canon)) return(canon)
  n <- length(levels)
  switch(prop,
    edge_colour = setNames(qual_pal[seq_len(n)], levels),
    colour      = setNames(qual_pal[seq_len(n)], levels),
    fill        = setNames(qual_pal[seq_len(n)], levels),
    edge_linetype = setNames(c("solid", "dashed", "dotted", "dotdash",
                               "longdash", "twodash")[seq_len(n)], levels),
    edge_width = setNames(seq(0.6, 2, length.out = n), levels),
    shape      = setNames(rep(c(21, 22, 23, 24, 25), length.out = n), levels),
    size       = setNames(seq(4, 7, length.out = n), levels)
  )
}

# The scale that builds the legend entry for an active (property, criterion).
# `limits = levels` is systematic: it forces the legend to show ALL canonical
# levels of the criterion (cf. `crit_levels`), even those absent from the current
# window, so it stays full and fixed from one time step to the next.
build_scale <- function(prop, var, levels) {
  values <- prop_values(prop, var, levels)
  name <- crit_labels[[var]]
  # type -> colour AND linetype share the title => the guides merge.
  if (var == "type" && prop %in% c("edge_colour", "edge_linetype")) {
    fn <- if (prop == "edge_colour") scale_edge_colour_manual else scale_edge_linetype_manual
    return(list(fn(values = values, name = name, limits = levels)))
  }
  switch(prop,
    edge_colour   = list(scale_edge_colour_manual(values = values, name = name, limits = levels)),
    edge_linetype = list(scale_edge_linetype_manual(values = values, name = name, limits = levels)),
    edge_width    = list(scale_edge_width_manual(values = values, name = name, limits = levels)),
    # Each node scale imposes an `override.aes` so the legend key shows THE
    # mapped property (and nothing else): the other aesthetics are fixed to a
    # single value, otherwise the keys inherit the other mappings and become
    # misleading (e.g. Action imprecise because identical full keys; Isolement
    # rendered as fill instead of outline). Cf. the frozen report.
    shape = list(scale_shape_manual(values = values, name = name, limits = levels,
                                    guide = guide_legend(ncol = 2))),
    colour = list(scale_colour_manual(values = values, name = name, limits = levels,
                                      guide = guide_legend(
                                        ncol = 2,
                                        override.aes = list(fill = "white", size = 4,
                                                            shape = 21, stroke = 2)))),
    fill = list(scale_fill_manual(values = values, name = name, limits = levels,
                                  guide = guide_legend(
                                    ncol = 2,
                                    override.aes = list(size = 4, shape = 21)))),
    size = list(scale_size_manual(values = values, name = name, limits = levels,
                                  guide = guide_legend(
                                    ncol = 2,
                                    override.aes = list(shape = 21, fill = "grey70"))))
  )
}

# Fully customisable plot: `mapping` is a list named by visual property
# (edge_colour, edge_width, edge_linetype, shape, colour, fill, size) mapping to
# a criterion ("type", "profil", ...) or "" if unmapped. Each unmapped property
# gets a constant value (no mapping -> no legend).
# `parts` restricts the plot to a single layer kind: "both" (default) draws
# edges + nodes; "edges" draws ONLY edges (edge legend); "nodes" draws ONLY nodes
# (node legend). Scales are restricted to the requested kind so that each group
# legend only contains its own criteria.
plot_subgraph_layers <- function(graph, layout, mapping = list(),
                                 title = NULL, subtitle = NULL,
                                 parts = c("both", "edges", "nodes"),
                                 layout_mode = "manual", limits = NULL,
                                 edge_alpha = 0.6) {
  parts <- match.arg(parts)
  node_data <- graph |> activate(nodes) |> as_tibble()
  edge_data <- graph |> activate(edges) |> as_tibble()

  p <- function(key) if (is.null(mapping[[key]])) "" else mapping[[key]]
  e_col <- p("edge_colour"); e_w <- p("edge_width"); e_lt <- p("edge_linetype")
  n_shape <- p("shape"); n_col <- p("colour"); n_fill <- p("fill"); n_size <- p("size")

  # Edges: conditional mapping on each edge property.
  edge_aes <- rlang::exprs()
  if (e_col != "") edge_aes$colour   <- rlang::sym(e_col)
  if (e_w   != "") edge_aes$width    <- rlang::sym(e_w)
  if (e_lt  != "") edge_aes$linetype <- rlang::sym(e_lt)
  edge_glyph <- list(
    arrow = arrow(length = unit(3, 'mm'), type = "closed"),
    end_cap = circle(7, 'mm'), start_cap = circle(7, 'mm'), alpha = edge_alpha
  )
  if (e_col == "") edge_glyph$colour <- "grey40"
  if (e_lt  == "") edge_glyph$linetype <- "solid"
  if (e_w   == "") edge_glyph$width <- 1
  edge_layer <- do.call(geom_edge_fan, c(list(mapping = aes(!!!edge_aes)), edge_glyph))

  # Nodes: conditional mapping on each node property. Non-factor (binary)
  # criteria are coerced to factor for a discrete scale.
  node_aes <- rlang::exprs()
  nmap <- c(shape = n_shape, colour = n_col, fill = n_fill, size = n_size)
  for (a in names(nmap)) {
    v <- nmap[[a]]
    if (v == "") next
    node_aes[[a]] <- if (is.factor(node_data[[v]]))
      rlang::sym(v) else rlang::call2("factor", rlang::sym(v))
  }
  node_glyph <- list(stroke = 1.3)
  if (n_shape == "") node_glyph$shape  <- 21
  if (n_col   == "") node_glyph$colour <- "black"
  if (n_fill  == "") node_glyph$fill   <- "grey50"
  if (n_size  == "") node_glyph$size   <- 5
  node_layer <- do.call(geom_node_point, c(list(mapping = aes(!!!node_aes)), node_glyph))

  # Scales for each active (property, criterion) (these build the legend: guides
  # merged on the right). Restricted to the kind requested by `parts` to isolate
  # the edge legend from the node one.
  active <- c(edge_colour = e_col, edge_width = e_w, edge_linetype = e_lt,
              shape = n_shape, colour = n_col, fill = n_fill, size = n_size)
  active_scales <- list()
  for (pr in names(active)) {
    v <- active[[pr]]
    if (v == "") next
    if (parts == "edges" && pr %in% c("shape", "colour", "fill", "size")) next
    if (parts == "nodes" && pr %in% c("edge_colour", "edge_width", "edge_linetype")) next
    # CANONICAL levels of the criterion (all, regardless of the window): this is
    # what makes the legend full and fixed for a given mapping (see crit_levels).
    active_scales <- c(active_scales, build_scale(pr, v, crit_levels(v)))
  }

  # Assemble the layers according to `parts` (geometries + scales), then the themes.
  plot_layers <- list()
  if (parts %in% c("both", "edges")) plot_layers <- c(plot_layers, list(edge_layer))
  if (parts %in% c("both", "nodes")) plot_layers <- c(plot_layers, list(node_layer))
  if (parts %in% c("both", "nodes"))
    plot_layers <- c(plot_layers,
                     list(geom_node_text(aes(label = nom), vjust = 2.5, fontface = "bold")))
  plot_layers <- c(plot_layers, active_scales)

  # Placement: "manual" uses the per-window MDS + Procrustes positions
  # (`gdata$layouts[[w]]`); any other mode is passed as-is to ggraph() (base or
  # graphlayouts layout computed directly on the current graph).
  if (layout_mode == "manual") {
    lay_f <- left_join(tibble(nom = node_data$nom), layout, by = "nom")
    p <- ggraph::ggraph(graph, layout = "manual", x = lay_f$x, y = lay_f$y)
  } else {
    p <- ggraph::ggraph(graph, layout = layout_mode)
  }
  for (l in plot_layers) p <- p + l
  # Time-stable coordinates: for the network in "manual" mode we fix common
  # limits across ALL windows (union of the layouts), which yields inter-window
  # STABILITY: for a given panel size, the (x,y) -> pixels mapping of a node is a
  # CONSTANT affine map (same limits, same size) identical from one window to
  # the next, so anchored nodes stay at exactly the same on-screen position.
  # Note we NO LONGER impose coord_fixed(ratio=1) here: it forced a square frame
  # that left vertical/horizontal margins in the wide main panel. Without it, the
  # two axes stretch to FILL the panel (the graph occupies all available space),
  # while keeping stable on-screen positions (the common limits + fixed size
  # guarantee this stability; coord_fixed only guaranteed the
  # isotropic/isometric character of the scale, a distinct property, at the cost
  # of filling). NULL limits => auto (legends built on the constant
  # `legend_universe` graph stay as-is, as do the other layout modes).
  if (layout_mode == "manual" && !is.null(limits)) {
    p <- p +
      scale_x_continuous(limits = limits$x, expand = expansion(mult = 0.15)) +
      scale_y_continuous(limits = limits$y, expand = expansion(mult = 0.15))
  } else {
    p <- p +
      scale_x_continuous(expand = expansion(mult = 0.35)) +
      scale_y_continuous(expand = expansion(mult = 0.35))
  }
  p <-
    p +
    labs(title = title, subtitle = subtitle) +
    theme_graph() +
    theme(
      legend.position = "right", legend.box = "vertical",
      legend.key.width = unit(1.5, "cm"), legend.key.height = unit(0.75, "cm"),
      legend.spacing.y = unit(0.2, "cm"),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 9)
    )
  # Note: coord_fixed() was SILENTLY REMOVED by the ggraph `+` while adding
  # layers/scales (the operator rebuilds the coordinate as CoordCartesian), hence
  # the direct assignment to `p$coordinates` we used to do. We no longer apply it
  # to the main panel (cf. comment above): inter-window stability is ensured by
  # the common limits, not by coord_fixed.
  p
}

# Pads a grob on the left up to width `W_cm`: we add an empty column on the
# right so that, when placing the grob in a wider cell, gtable does not RE-CENTER
# it (it centers by default any grob narrower than its cell, which shifts the
# legend groups unevenly). The content stays anchored at x = 0.
pad_left <- function(g, W_cm) {
  w <- as.numeric(grid::convertWidth(grid::grobWidth(g), "cm", valueOnly = TRUE))
  h <- as.numeric(grid::convertHeight(grid::grobHeight(g), "cm", valueOnly = TRUE))
  gtable::gtable(
    widths  = grid::unit(c(w, max(0.001, W_cm - w)), "cm"),
    heights = grid::unit(h, "cm")
  ) |> gtable::gtable_add_grob(g, t = 1, l = 1, clip = "off")
}

# Stacks grobs/gtables vertically, ALL aligned on the same left edge (x = 0 of
# the widest column), with a `gap` between each element.
vstack <- function(grobs, gap = grid::unit(0.6, "cm")) {
  ws <- vapply(grobs, function(g)
    as.numeric(grid::convertWidth(grid::grobWidth(g), "cm", valueOnly = TRUE)), numeric(1))
  hs <- vapply(grobs, function(g)
    as.numeric(grid::convertHeight(grid::grobHeight(g), "cm", valueOnly = TRUE)), numeric(1))
  W <- max(ws)
  gap_cm <- as.numeric(grid::convertHeight(gap, "cm", valueOnly = TRUE))
  n <- length(grobs)
  # one row per grob, separated by a `gap` row (except after the last one).
  heights_u <- unlist(lapply(seq_len(n), function(i) {
    row <- list(grid::unit(hs[i], "cm"))
    if (i < n) row <- c(row, list(grid::unit(gap_cm, "cm")))
    row
  }), recursive = FALSE)
  gt <- gtable::gtable(widths = grid::unit(W, "cm"), heights = do.call(unit.c, heights_u))
  r <- 1
  for (i in seq_len(n)) {
    gt <- gtable::gtable_add_grob(gt, pad_left(grobs[[i]], W), t = r, l = 1, clip = "off")
    r <- r + if (i < n) 2 else 1
  }
  gt
}

# Legend group: a title above a content (often 1 guide grob), with a `gap`
# (net) between title and content. Content and title are left-aligned on the same
# column.
legend_group <- function(title, grobs, gap = grid::unit(0.35, "cm")) {
  header <- grid::textGrob(title, x = 0, hjust = 0,
                           gp = grid::gpar(fontsize = 10, fontface = "bold"))
  vstack(c(list(header), grobs), gap = gap)
}

# Default dataset (same `nodes`/`edges`/`layouts` tables as the report). The real
# dataset is (re)computed REACTIVELY in the server from the "Simulation"
# controls. We only compute a DEFAULT instance here, at compile time, to (a) size
# the window slider and (b) pre-fill the greyed-out layout selector options: the
# UI is built only once, before the server's reactives exist.
gdata_default <- simulate_graph_ts()
nb_fenetres <- max(gdata_default$edges$time_step)

# Constant "universe" graph for the legend: it contains ONE exemplar of EVERY
# level of EVERY criterion (all interaction types `type_levels`, the 2 `lien`
# values, the 5 profiles, and both values of each binary
# marque_interet/action/isolement). The legend is built from THIS source, not
# from the current window: it is thus FULL (every level, GLYPHS drawn) and FIXED
# from one window to the next for a given mapping -- it only changes when the
# mapping changes. Without this universe graph, ggplot only draws the levels
# present in the window: the keys of absent levels (e.g. tactile, dans_les_bras)
# get an NA aesthetic and their glyph stays WHITE. This is the fix. The only
# requirement is that the graph covers every level; its positions serve no
# purpose (we only extract the legend from it), hence a trivial layout.
# One arc per interaction type (length = length(type_levels)), so the legend
# always covers ALL types, even those absent from a window or still inactive in
# the current graph. from/to/lien are derived from `type_levels`: adding/removing
# a type automatically updates this graph (otherwise a length mismatch between
# `type` and `from`/`to` breaks construction, as happened after adding `sonore`).
# `lien` alternates between en lien / hors activité to cover both levels of the
# factor.
legend_n_types <- length(type_levels)
legend_universe <- tbl_graph(
  nodes = tibble(
    nom            = c("I", "N", "A", "B", "C"),
    profil         = factor(c("Intervenant.e", "Animatrice", "Enfant CAT1",
                              "Enfant CAT2", "Enfant CAT3"),
                            levels = names(profil_shapes)),
    marque_interet = c(0, 1, 0, 1, 0),
    action         = c(0, 0, 1, 1, 1),
    isolement      = c(0, 0, 0, 1, 1)),
  edges = tibble(
    from = seq_len(legend_n_types) %% 5L + 1L,
    to   = (seq_len(legend_n_types) + 1L) %% 5L + 1L,
    type = factor(type_levels, levels = type_levels),
    lien = factor(ifelse(seq_len(legend_n_types) %% 2L == 1L,
                         "en lien", "hors activité"),
                  levels = c("hors activité", "en lien"))))
legend_univ_layout <- tibble(nom = c("I", "N", "A", "B", "C"),
                             x = 0:4, y = rep(0, 5))

# Compatibility of each layout on the WHOLE temporal series: we try computing the
# layout on EACH graph; a single failure (disconnected window, missing optional
# package, required argument...) makes it unavailable in the selector (we must be
# able to navigate between ALL windows with the active choice). "manual" (MDS +
# Procrustes) is not a ggraph layout: it is always available and excluded from
# the test. Precomputed at compile time on the DEFAULT dataset for the UI (the
# server recomputes it reactively and updates the greyed-out options on each
# re-simulation via `updatePickerInput`).
layout_ok <- setNames(rep(TRUE, length(layout_choices)), layout_choices)
for (lv in setdiff(layout_choices, "manual")) {
  # Layouts like `eigen`/`stress` call layout_with_eigen, which warns "g is
  # directed. undirected version is used" on a directed graph: this is only a
  # compatibility diagnostic (we keep only success/failure), so we suppress the
  # warnings to avoid polluting the console at load time.
  ok <- all(vapply(gdata_default$graphs, function(g)
    !inherits(try(suppressWarnings(ggraph(g, layout = lv)), silent = TRUE), "try-error"),
    logical(1)))
  layout_ok[[lv]] <- ok
}
