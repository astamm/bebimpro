# -----------------------------------------------------------------------------
# app.R -- Shiny app from the `ggraph-shinylive` dev (ui() + server()).
# -----------------------------------------------------------------------------
# This file holds the UI and the LOGIC; all utility functions (plotting,
# legend, choices...) live in `app-utils.R`. It is consumed two ways:
#   * standalone app -- `shiny::runApp()` or `shinylive::export()`: it
#     `source("app-utils.R")` (guarded by `file.exists()`) ;
#   * inside the `{shinylive-r}` block -- the `{{< include app.R >}}` shortcode
#     injects it as-is, `app-utils.R` (and `R/graph-model.R`) already having
#     been included just before; the `source("app-utils.R")` below is then a
#     no-op (the file does not exist in the webR runtime).
#
# Important: when this file is INCLUDED in a Quarto block, the last line
# `shinyApp(ui = ui, server = server)` does start the app for webR (the
# shinylive runtime runs the block as a Shiny script). The guarded
# `source("app-utils.R")` has no bundling effect.
for (.app_utils_src in "app-utils.R") {
  if (file.exists(.app_utils_src)) source(.app_utils_src)
}
rm(.app_utils_src)

# Helper: HTML-safe identifier (no spaces / accents) for dynamically generated
# inputs (per-type persistence, latent-state probabilities).
id_safe <- function(x) gsub("[^A-Za-z0-9]", "", x)

# -----------------------------------------------------------------------------
# User Interface (UI)
# -----------------------------------------------------------------------------
# The legend (~7 x 14.5 cm, very tall) does not fit in any side column without
# being shrunk until illegible. `scale_grob()` (gtable scaling) proved useless
# (null/npc units re-expand), so we no longer shrink it: we render it at native
# size inside a bslib popover and let the popover scroll if it exceeds the view
# height. The popover is a button in the card header; the rest of the page keeps
# its full width for the graph. Dropdowns map each visual property (edge
# colour/width/linetype; point shape/colour/fill/size) onto a criterion (edge or
# node), or "Aucun" (no default mapping); the legend reflects the active
# mappings.
# Note: native size differs per mapping -> `renderPlot` sizes the device exactly
# to the built legend size (width=/height= functions). The grouped legend (~wider
# than a single legend) does not fit the default Bootstrap popover (max-width
# 276px) -> we widen .popover here. As it is the page's only popover, a global
# rule has no side effects.
ui <- page_sidebar(
  title = "Réseaux avec ggraph et Shinylive (plot_subgraph_layers)",
  theme = bs_theme(version = 5),
  tags$head(tags$style(HTML(".popover { max-width: 640px; }"))),
  sidebar = sidebar(
    # Default sidebar WIDTH: we do not widen it (that would come at the expense
    # of the main panel). Room for the graph comes from the widened global window
    # (iframe.app-frame, cf. shinylive-layout.css).
    accordion(
      # `open = "nav"`: Navigation (first panel) is unfolded by default; the
      # others (Simulation, Layout, Mapping) stay folded to keep the sidebar
      # compact. The simulation starts with sensible defaults right on load, so
      # temporal navigation is usable immediately. Note bslib: `open = FALSE`
      # would fold ALL panels; `open` opens by `data-value` (accordion() source
      # -> `is_open` <- data-value %in% open).
      open = "nav",
      accordion_panel(
        "Navigation", icon = icon("forward"), value = "nav",
        sliderInput(
          "w", "Fenêtre temporelle",
          min = 1, max = nb_fenetres, value = 1, step = 1,
          # Pause between temporal states: short but visible (the cache makes the
          # switch instantaneous; this interval is what creates the "pause").
          animate = animationOptions(interval = 1000, loop = TRUE)
        )
      ),
      accordion_panel(
        "Simulation", icon = icon("sliders"), value = "sim",
        # Model parameters: change the dataset on the fly (re-simulation).
        # Defaults = those of simulate_graph_ts(). The model's "fixed"
        # probabilities are ALL exposed here, defaulting to the shared constant
        # (DEFAULT_* in R/graph-model.R) so they cannot drift by mistake.
        p("Réglez la composition et la dynamique du réseau, puis laissez le
          graphe se re-simuler.",
          style = "font-size: 0.85em; color: #666;"),

        h6("Composition"),
        sliderInput("n_cat1", "Enfants CAT1", 1, 6, 3, step = 1),
        sliderInput("n_cat2", "Enfants CAT2", 1, 6, 3, step = 1),
        sliderInput("n_cat3", "Enfants CAT3", 1, 6, 3, step = 1),
        sliderInput("n_anim", "Animatrices", 1, 5, 3, step = 1),
        sliderInput("n_int", "Intervenante.s", 1, 2, 1, step = 1),
        # `n_states` only offers multiples of 3: the intervenant.e.s anchoring
        # (anchor_fraction = 1/3 by default) is only defined on a whole number of
        # windows (simulate_graph_ts() stops otherwise). Default raised to 6
        # windows; maximum 21.
        sliderInput("n_states", "Fenêtres temporelles", 3, 21, 6, step = 3),

        h6("Dynamique des interactions"),
        sliderInput("p_density", "Densité d'arêtes", 0.05, 0.8, 0.30, step = 0.05),
        sliderInput("persistence", "Persistance temporelle (+/- dyades)",
                    0, 1, 0.7, step = 0.05),
        sliderInput("engage_scale", "Multiplicateur d'engagement intervenant.e.s",
                    0.2, 2, 1.0, step = 0.05),
        sliderInput("max_contact_nodes", "Contact simultané max par noeud",
                    0, 4, 2, step = 1),
        # anchor_fraction is an anchoring ratio, not a probability: it stays in
        # the Dynamique tab because it drives spatial rigidity.
        sliderInput("anchor_fraction", "Fraction d'ancrage intervenant.e.s",
                    0, 1, 1/3, step = 0.05),
        numericInput("seed", "Graine (seed)", value = 731, min = 1, step = 1),

        # Per-type interaction persistence, generated from
        # DEFAULT_TYPE_PERSISTENCE (R/graph-model.R): adding/removing a type
        # automatically updates the list of sliders.
        h6("Persistance par type d'interaction"),
        do.call(tagList, lapply(names(DEFAULT_TYPE_PERSISTENCE), function(ty) {
          sliderInput(paste0("tpers_", ty),
                      paste("Persistance", ty),
                      0, 1, unname(DEFAULT_TYPE_PERSISTENCE[[ty]]),
                      step = 0.05)
        })),

        # Latent engagement (children): persistence + stationary probabilities
        # of the 3 states per profile (normalised to 1 on the server side).
        h6("Engagement latent (enfants)"),
        sliderInput("engage_persist", "Persistance de l'état latent", 0, 1, 0.85, step = 0.05),
        sliderInput("attend_prob", "Probabilité attention animatrice", 0, 1, 0.10, step = 0.01),
        do.call(tagList, lapply(names(DEFAULT_P_ENGAGE_STATE), function(pr) {
          tagList(
            p(pr, style = "font-size: 0.85em; color: #666; margin: 0.5rem 0 0;"),
            sliderInput(paste0("peng_", id_safe(pr), "_desengage"),
                        "P(état désengagé)", 0, 1,
                        unname(DEFAULT_P_ENGAGE_STATE[[pr]]["desengage"]), step = 0.05),
            sliderInput(paste0("peng_", id_safe(pr), "_passif"),
                        "P(état passif)", 0, 1,
                        unname(DEFAULT_P_ENGAGE_STATE[[pr]]["passif"]), step = 0.05),
            sliderInput(paste0("peng_", id_safe(pr), "_actif"),
                        "P(état actif)", 0, 1,
                        unname(DEFAULT_P_ENGAGE_STATE[[pr]]["actif"]), step = 0.05)
          )
        }))
      ),
      accordion_panel(
        "Layout", icon = icon("vector-square"), value = "layout",
        # pickerInput: lists ALL layouts (base ggraph/igraph and graphlayouts),
        # but greys out (choicesOpt$disabled) those incompatible with at least one
        # graph of the series (`layout_ok`): they cannot be selected, and the
        # animation stays possible with any active choice. A bslib tooltip on the
        # selector + a discreet caption explain why they are greyed out.
        tooltip(
          pickerInput("layout", "Placement des noeuds",
                      choices = layout_choices, selected = "manual",
                      choicesOpt = list(disabled = !layout_ok[layout_choices])),
          paste("Les layouts grisés exigent un graphe connexe (eigen, pmds,",
                "sparse_stress), un paquet optionnel (backbone) ou des arguments",
                "par graphe (focus, centrality) : ils échouent sur au moins une",
                "fenêtre de la série temporelle et sont donc indisponibles.")),
        p("Les layouts grisés (désactivés) échouent sur au moins une fenêtre de la
          série (requièrent un graphe connexe ou un paquet optionnel).",
          style = "font-size: 0.8em; color: #999; margin-top: 0.25rem;")
      ),
      accordion_panel(
        "Mapping", icon = icon("palette"), value = "mapping",
        h6("Mapping"),
        p("Chaque propriété visuelle peut être mappée sur un critère (arête ou noeud), ou laissée sans mapping (Aucun) - valeur constante par défaut.",
          style = "font-size: 0.85em; color: #666;"),
        selectInput("edge_colour", "Couleur des arêtes",
                    choices = edge_choices, selected = default_mapping[["edge_colour"]]),
        selectInput("edge_width", "Épaisseur des arêtes",
                    choices = edge_choices, selected = default_mapping[["edge_width"]]),
        selectInput("edge_linetype", "Type de ligne des arêtes",
                    choices = edge_choices, selected = default_mapping[["edge_linetype"]]),
        selectInput("shape", "Forme des points",
                    choices = node_choices, selected = default_mapping[["shape"]]),
        selectInput("colour", "Contour des points",
                    choices = node_choices, selected = default_mapping[["colour"]]),
        selectInput("fill", "Remplissage des points",
                    choices = node_choices, selected = default_mapping[["fill"]]),
        selectInput("size", "Taille des points",
                    choices = node_choices, selected = default_mapping[["size"]]),
        sliderInput("edge_alpha", "Opacité des arêtes", 0.1, 1, 0.6, step = 0.05),
        # Full-width stacked buttons (normal size): clearly visible in the
        # sidebar, not crushed to the left.
        actionButton("enc_all", "Tout encoder",
                     class = "btn-outline-secondary",
                     style = "width: 100%; margin-bottom: 0.25rem;"),
        actionButton("clear_all", "Tout vider",
                     class = "btn-outline-danger",
                     style = "width: 100%;")
      )
    )
  ),
  card(
    full_screen = TRUE,
    card_header(
      "Graphe d'interactions",
      popover(
        actionButton("legend_btn", "Légende",
                     class = "btn-outline-secondary btn-sm"),
        title = "Légende",
        div(style = "max-height: 62vh; overflow-y: auto; min-width: 420px;",
            plotOutput("legendPlot"))
      )
    ),
    # Graph without legend or title -> full width is used.
    plotOutput("networkPlot", height = "680px")
  )
)

# -----------------------------------------------------------------------------
# Server Logic
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # ---- Reactive simulation: re-runs simulate_graph_ts() as soon as any
  # parameter changes. All node names change with the composition, so the layout
  # selector and the window slider are also hooked up.
  # Latent-state probabilities, re-assembled from the 9 sliders (3 profiles x 3
  # states) AND normalised per profile to a sum of 1 (these are stationary
  # distributions).
  p_engage_state_r <- reactive({
    profiles <- names(DEFAULT_P_ENGAGE_STATE)
    states <- names(DEFAULT_P_ENGAGE_STATE[[1L]])   # desengage, passif, actif
    out <- lapply(profiles, function(pr) {
      v <- vapply(states, function(s)
        input[[paste0("peng_", id_safe(pr), "_", s)]], numeric(1))
      setNames(v, states)
    })
    names(out) <- profiles
    # Normalise each profile to a valid distribution (sum = 1).
    lapply(out, function(v) if (sum(v) > 0) v / sum(v) else v)
  })

  # Per-type persistence: named vector rebuilt from the sliders. We iterate over
  # `names(DEFAULT_TYPE_PERSISTENCE)` (the source of truth shared with the ui
  # sliders), NOT over `names(type_levels)` which is NULL (type_levels <-
  # names(type_cols) is an unnamed vector): otherwise we produced a non-NULL
  # empty vector, passed as-is to simulate_graph_ts_impl() (which then skips the
  # default), and type_persistence["regard"] -> NA -> "missing value where
  # TRUE/FALSE needed" error at graph-model.R#523.
  type_persistence_r <- reactive({
    setNames(vapply(names(DEFAULT_TYPE_PERSISTENCE), function(ty)
      input[[paste0("tpers_", ty)]], numeric(1)), names(DEFAULT_TYPE_PERSISTENCE))
  })

  gdata <- reactive(simulate_graph_ts(
    n_intervenants = input$n_int, n_animatrices = input$n_anim,
    n_cat1 = input$n_cat1, n_cat2 = input$n_cat2, n_cat3 = input$n_cat3,
    n_states = input$n_states,
    p_density = input$p_density, persistence = input$persistence,
    seed = input$seed,
    engage_scale = input$engage_scale,
    anchor_fraction = input$anchor_fraction,
    max_contact_nodes = input$max_contact_nodes,
    engage_persist = input$engage_persist,
    attend_prob = input$attend_prob,
    type_persistence = type_persistence_r(),
    p_engage_state = p_engage_state_r()
  ))

  nb_fenetres <- reactive(max(gdata()$edges$time_step))

  # The temporal slider's max follows the length of the re-simulated series, and
  # we bring the current window back within bounds if it goes out of range.
  observe({
    mx <- nb_fenetres()
    updateSliderInput(session, "w", max = mx)
    if (input$w > mx) updateSliderInput(session, "w", value = mx)
  })

  # Layout compatibility on the CURRENT series: only "manual" (MDS + Procrustes)
  # is always available and excluded from the test. We grey out the incompatible
  # choices via updatePickerInput (choicesOpt$disabled).
  layout_ok_r <- reactive({
    lok <- setNames(rep(TRUE, length(layout_choices)), layout_choices)
    gs <- gdata()$graphs
    for (lv in setdiff(layout_choices, "manual")) {
      ok <- all(vapply(gs, function(g)
        !inherits(try(ggraph(g, layout = lv), silent = TRUE), "try-error"),
        logical(1)))
      lok[[lv]] <- ok
    }
    lok
  })
  observe({
    updatePickerInput(session, "layout",
                      choicesOpt = list(disabled = !layout_ok_r()[layout_choices]))
  })

  # Full mapping: each visual property -> chosen criterion ("" = none).
  mapping <- reactive(list(
    edge_colour   = input$edge_colour,
    edge_width    = input$edge_width,
    edge_linetype = input$edge_linetype,
    shape         = input$shape,
    colour        = input$colour,
    fill          = input$fill,
    size          = input$size
  ))

  # "Tout encoder" / "Tout vider" buttons: reset all 7 menus at once. Observed
  # via observeEvent: updateSelectInput forces each select to its target
  # (canonical encoding by default, or "" to clear everything).
  reset_all <- function(target) {
    for (pr in names(target)) {
      updateSelectInput(session, pr, selected = target[[pr]])
    }
  }
  observeEvent(input$enc_all,  reset_all(default_mapping))
  observeEvent(input$clear_all, reset_all(setNames(rep("", 7L), names(default_mapping))))

  # Is there at least one active mapping per group (nodes vs edges)?
  has_node_map <- reactive(any(unlist(mapping()[c("shape", "colour", "fill", "size")]) != ""))
  has_edge_map <- reactive(any(unlist(mapping()[c("edge_colour", "edge_width", "edge_linetype")]) != ""))

  # Cache: we precompute the network plot (`all`) for ALL temporal windows, plus
  # BOTH legend variants (`edges`/`nodes`), only once, as soon as the mapping OR
  # the layout changes.
  # The legend is built from the constant UNIVERSE graph (`legend_universe`), not
  # from the current window: it is therefore full (every level, drawn glyphs) and
  # FIXED from window to window for a given mapping -- it only changes when the
  # mapping changes. The network plot, in contrast, does use the current window's
  # data. During slider animation mapping+layout are constant -> we only switch
  # between already-built objects (instantaneous); the heavy (ggraph) cost is
  # paid only once per (mapping, layout).
  plots_cache <- reactive({
    m <- mapping()
    lm <- input$layout
    gd <- gdata()
    nfet <- nb_fenetres()
    ea <- input$edge_alpha
    # Bounds common to all windows for a time-stable frame of reference (only
    # relevant in "manual" mode; cf. plot_subgraph_layers / limits).
    if (lm == "manual") {
      all_xy <- do.call(rbind, lapply(gd$layouts, function(L) L[, c("x", "y")]))
      limits <- list(x = range(all_xy$x), y = range(all_xy$y))
    } else {
      limits <- NULL
    }
    list(
      nets  = lapply(seq_len(nfet), function(w)
        plot_subgraph_layers(gd$graphs[[w]], gd$layouts[[w]], mapping = m,
                             layout_mode = lm, limits = limits, edge_alpha = ea)),
      edges = plot_subgraph_layers(legend_universe, legend_univ_layout, mapping = m, parts = "edges", layout_mode = "manual", edge_alpha = ea),
      nodes = plot_subgraph_layers(legend_universe, legend_univ_layout, mapping = m, parts = "nodes", layout_mode = "manual")
    )
  })

  wi <- reactive(as.integer(input$w))

  # Network plot for the current window (cached -> instantaneous during animation).
  current_plot <- reactive(plots_cache()$nets[[wi()]])

  # Grouped legend: an "Interactions individu-activité" block (node criteria)
  # then an "Interactions individu-individu" block (edge criteria), each with its
  # own title; NULL if no mapping (no legend). Both blocks come from the universe
  # graph -> identical whatever the window.
  legend_grob_r <- reactive({
    gr <- list()
    if (has_node_map())
      gr <- c(gr, list(legend_group("Interactions individu-activité",
                                    list(legend_grob(plots_cache()$nodes)))))
    if (has_edge_map())
      gr <- c(gr, list(legend_group("Interactions individu-individu",
                                    list(legend_grob(plots_cache()$edges)))))
    if (length(gr) == 0) NULL else vstack(gr)
  })

  # Native (pixel) size of the device to draw the legend WITHOUT scaling: we size
  # the device exactly to the grob size (+ small margin), which eliminates any
  # clipping (the scale_grob() approach was a no-op).
  legend_px <- reactive({
    if (is.null(legend_grob_r())) return(c(w = 240L, h = 120L))
    lg <- legend_grob_r()
    gw <- as.numeric(grid::convertWidth(grid::grobWidth(lg), "cm", valueOnly = TRUE))
    gh <- as.numeric(grid::convertHeight(grid::grobHeight(lg), "cm", valueOnly = TRUE))
    c(w = as.integer(ceiling((gw + 0.4) / 2.54 * 120)),
      h = as.integer(ceiling((gh + 0.4) / 2.54 * 120)))
  })

  # Main panel: graph WITHOUT legend or title -> full width.
  output$networkPlot <- renderPlot({
    current_plot() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_blank()
      )
  }, height = 680, res = 120)

  # Legend in the popover: rendered at native size (no scale_grob).
  output$legendPlot <- renderPlot({
    grid::grid.newpage()
    if (is.null(legend_grob_r())) {
      grid::grid.text("Aucun encodage sélectionné : légende vide")
    } else {
      grid::grid.draw(legend_grob_r())
    }
  }, res = 120,
     width = function() legend_px()[["w"]],
     height = function() legend_px()[["h"]])
}

# Launch the app
shinyApp(ui = ui, server = server)
