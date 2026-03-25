library(tidyverse)
library(shiny)
library(shinyjs)
library(detourr)
library(ggiraph)
library(ggthemes)
source("application.R")

# ── Shared helper ─────────────────────────────────────────────────────────────
get_available_divisions <- function(base_data, sel_state, changed_divs,
                                    sel_change_val, div_search) {
  divs <- sort(unique(na.omit(base_data$DivisionNm)))

  if (!is.null(sel_state) && sel_state != "All") {
    state_divs <- base_data |>
      filter(StateAb == sel_state, !is.na(DivisionNm)) |>
      pull(DivisionNm) |> unique()
    divs <- intersect(divs, state_divs)
  }

  if (length(changed_divs) > 0 && sel_change_val != "none") {
    divs <- intersect(divs, changed_divs)
  }

  query <- tolower(trimws(div_search %||% ""))
  if (nchar(query) > 0) {
    divs <- divs[grepl(query, tolower(divs), fixed = TRUE)]
  }

  sort(divs)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Australian Federal Election",
  useShinyjs(),

  # ══ Tab 1: First Preference Shifts ══════════════════════════════════════════
  tabPanel(
    "First Preference Shifts",

    sidebarLayout(
      sidebarPanel(
        width = 2,

        checkboxGroupInput(
          "sel_year", "Year",
          choices  = c("2019", "2022", "2025"),
          selected = c("2019", "2022", "2025")
        ),

        hr(),

        selectizeInput(
          "sel_state", "State",
          choices  = c("All", sort(unique(na.omit(detour_df_first$StateAb)))),
          selected = "All",
          multiple = FALSE
        ),

        hr(),

        tags$label("Changed Divisions"),
        radioButtons(
          "sel_change", label = NULL,
          choices  = c("None"           = "none",
                       "Changed in 2022" = "2022",
                       "Changed in 2025" = "2025",
                       "Changed in Both" = "both"),
          selected = "none"
        ),

        hr(),

        tags$label("Division"),
        div(style = "margin-bottom:6px;",
            textInput("div_search", label = NULL,
                      placeholder = "Search division...")),
        div(
          style = "height:240px; overflow-y:auto; border:1px solid #ddd;
                   border-radius:4px; padding:4px;",
          uiOutput("division_checklist")
        ),

        hr(),
        actionButton("clear_sel", "Clear selection",
                     class = "btn-sm btn-secondary w-100")
      ),

      mainPanel(
        width = 10,
        fluidRow(
          column(6, uiOutput("tour_plot",   height = "520px")),
          column(6, girafeOutput("map_plot", height = "520px"))
        ),
        fluidRow(
          column(12, tags$small(style = "color:#666;",
                                textOutput("sel_summary", inline = TRUE)))
        )
      )
    )
  ),

  # ══ Tab 2: Flow of Preference 2025 ══════════════════════════════════════════
  tabPanel(
    "Flow of Preference (2025)",

    sidebarLayout(
      sidebarPanel(
        width = 2,

        radioButtons(
          "sel_rounds", "Rounds to show",
          choices  = c("All rounds"         = "all",
                       "First & last only"  = "endpoints"),
          selected = "all"
        ),

        hr(),

        selectizeInput(
          "sel_state2", "State",
          choices  = c("All", sort(unique(na.omit(detour_df_flow$StateAb)))),
          selected = "All",
          multiple = FALSE
        ),

        hr(),

        tags$label("Changed Divisions"),
        radioButtons(
          "sel_change2", label = NULL,
          choices  = c("All divisions"                = "none",
                       "Changed most preferred party" = "changed"),
          selected = "none"
        ),

        hr(),

        tags$label("Division"),
        div(style = "margin-bottom:6px;",
            textInput("div_search2", label = NULL,
                      placeholder = "Search division...")),
        div(
          style = "height:240px; overflow-y:auto; border:1px solid #ddd;
                   border-radius:4px; padding:4px;",
          uiOutput("division_checklist2")
        ),

        hr(),
        actionButton("clear_sel2", "Clear selection",
                     class = "btn-sm btn-secondary w-100")
      ),

      mainPanel(
        width = 10,
        fluidRow(
          column(6, uiOutput("tour_plot2",    height = "520px")),
          column(6, girafeOutput("map_plot2", height = "520px"))
        ),
        fluidRow(
          column(12, tags$small(style = "color:#666;",
                                textOutput("sel_summary2", inline = TRUE)))
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 1 — First Preference Shifts (unchanged from original)
  # ════════════════════════════════════════════════════════════════════════════

  # ── Changed divisions lookup ────────────────────────────────────────────────
  changed_divs <- reactive({
    switch(input$sel_change,
      "none" = character(0),
      "2022" = change_22,
      "2025" = change_25,
      "both" = change_both
    )
  })

  # ── Dynamic division checklist (state + change shortcut + search) ───────────
  available_divisions <- reactive({
    get_available_divisions(
      detour_df_first, input$sel_state,
      changed_divs(), input$sel_change, input$div_search
    )
  })

  observeEvent(available_divisions(), {
    if (input$sel_change != "none" ||
        (!is.null(input$sel_state) && input$sel_state != "All")) {
      updateCheckboxGroupInput(session, "sel_division",
                               selected = available_divisions())
    }
  })

  output$division_checklist <- renderUI({
    choices <- available_divisions()
    checkboxGroupInput(
      "sel_division", label = NULL,
      choices  = choices,
      selected = intersect(input$sel_division, choices)
    )
  })

  # ── detourr_data: year + division ──────────────────────────────────────────
  detourr_data <- reactive({
    years <- input$sel_year
    divs  <- input$sel_division

    detour_df_first |>
      filter(
        is.na(DivisionNm) |
          (as.character(Year) %in% years &
             (length(divs) == 0 | DivisionNm %in% divs))
      )
  }) |> debounce(400)

  # ── map_data: division only, fixed 2025 ────────────────────────────────────
  map_data <- reactive({
    divs <- input$sel_division
    detour_df_first |>
      filter(
        !is.na(DivisionNm), !is.na(long), !is.na(lat),
        as.character(Year) == "2025",
        length(divs) == 0 | DivisionNm %in% divs
      )
  }) |> debounce(400)

  # ── detourr: recomputes on year OR division change ──────────────────────────
  output$tour_plot <- renderUI({
    fd <- detourr_data()
    edges <- detour_edges_first

    detour(
      fd,
      tour_aes(
        projection = x1:x4,
        colour     = pref1_party,
        label      = c(text)
      )
    ) |>
      tour_path(grand_tour(3), fps = 60) |>
      show_scatter(
        axes = FALSE, 
        edges = edges, 
        palette = party_colors,
        size = 1)
  })

  # ── ggiraph map: recomputes on division change only ─────────────────────────
  output$map_plot <- renderGirafe({
    md <- map_data()

    p <- ggplot() +
      geom_polygon(
        data = elb_df,
        aes(x = long, y = lat, group = group),
        fill = "grey90", color = "white"
      ) +
      geom_point_interactive(
        data = md,
        aes(x = long, y = lat, color = pref1_party,
            tooltip = text, data_id = DivisionNm),
        size = 2, alpha = 0.8
      ) +
      scale_color_manual(
        values = party_colors,
        name = "Party"
      ) +
      labs(title = "First Preference — 2025") +
      coord_equal() + theme_map()

    girafe(
      ggobj = p,
      options = list(
        opts_selection(type = "multiple",
                       css  = "stroke:black;stroke-width:2px;opacity:1;"),
        opts_hover(css = "opacity:1;r:5px;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })

  # Map click → sync to sidebar (triggers detourr recompute via sel_division)
  observeEvent(input$map_plot_selected, ignoreNULL = FALSE, {
    divs <- input$map_plot_selected %||% character(0)
    updateCheckboxGroupInput(session, "sel_division", selected = divs)
    # map highlights itself on click — no sendCustomMessage needed here
  })

  # Division sidebar change → push highlight to map (no recompute)
  observeEvent(input$sel_division, ignoreNULL = FALSE, {
    divs <- input$sel_division %||% character(0)
    session$sendCustomMessage(type = "map_plot_set", message = divs)
  })

  # Clear → wipe highlights + uncheck divisions (year filter untouched)
  observeEvent(input$clear_sel, {
    updateCheckboxGroupInput(session, "sel_division", selected = character(0))
    session$sendCustomMessage(type = "map_plot_set", message = character(0))
  })

  # ── Summary ─────────────────────────────────────────────────────────────────
  output$sel_summary <- renderText({
    years <- input$sel_year %||% character(0)
    divs  <- input$sel_division %||% character(0)
    paste0(
      "Years: ", paste(years, collapse = ", "), " | ",
      if (length(divs) == 0) "All divisions"
      else paste("Divisions: ", paste(divs, collapse = ", "))
    )
  })

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 2 — Flow of Preference 2025
  # ════════════════════════════════════════════════════════════════════════════

  available_divisions2 <- reactive({
    get_available_divisions(
      detour_df_flow, input$sel_state2,
      diff_party_25,
      if (!is.null(input$sel_change2) && input$sel_change2 == "changed") "changed" else "none",
      input$div_search2
    )
  })

  observeEvent(available_divisions2(), {
    if (!is.null(input$sel_change2) && input$sel_change2 == "changed") {
      updateCheckboxGroupInput(session, "sel_division2",
                               selected = available_divisions2())
    } else {
      updateCheckboxGroupInput(session, "sel_division2", selected = character(0))
    }
  })

  output$division_checklist2 <- renderUI({
    choices <- available_divisions2()
    checkboxGroupInput(
      "sel_division2", label = NULL,
      choices  = choices,
      selected = intersect(input$sel_division2, choices)
    )
  })

  detourr_data2 <- reactive({
    divs <- input$sel_division2
    base <- detour_df_flow |>
      filter(is.na(DivisionNm) | length(divs) == 0 | DivisionNm %in% divs)

    if (!is.null(input$sel_rounds) && input$sel_rounds == "endpoints") {
      base <- base |>
        filter(is.na(DivisionNm)) |>
        bind_rows(
          detour_df_flow |>
            filter(!is.na(DivisionNm),
                   length(divs) == 0 | DivisionNm %in% divs) |>
            group_by(DivisionNm) |>
            filter(CountNumber == 0 | CountNumber == max(CountNumber)) |>
            ungroup()
        )
    }
    base
  }) |> debounce(400)

  map_data2 <- reactive({
    divs <- input$sel_division2
    detour_df_flow |>
      filter(!is.na(DivisionNm), !is.na(long), !is.na(lat),
            length(divs) == 0 | DivisionNm %in% divs) |>
      group_by(DivisionNm) |>
      filter(CountNumber == max(CountNumber)) |>
      ungroup()
  }) |> debounce(400)

  output$tour_plot2 <- renderUI({
    fd <- detourr_data2()
    edges <- detour_edges_flow

    detour(
      fd, 
      tour_aes(
        projection = x1:x4, 
        colour = pref1_party,
        label = c(text))) |>
      tour_path(grand_tour(3)) |>
      show_scatter(
        axes = FALSE, edges = edges, 
        palette = party_colors, size = 1)
  })

  output$map_plot2 <- renderGirafe({
    md <- map_data2()

    p <- ggplot() +
      geom_polygon(data = elb_df,
                   aes(x = long, y = lat, group = group),
                   fill = "grey90", color = "white") +
      geom_point_interactive(
        data = md,
        aes(x = long, y = lat, color = Winner,
            tooltip = text, data_id = DivisionNm),
        size = 2, alpha = 0.8) +
      scale_color_manual(values = party_colors, name = "Party") +
      labs(title = "Elected Party — 2025") +
      coord_equal() + theme_map()
    
    girafe(
      ggobj = p, 
      options = list(
        opts_selection(
          type = "multiple", 
          css = "stroke:black;stroke-width:2px;opacity:1;"),
        opts_hover(css = "opacity:1;r:5px;"),
        opts_sizing(rescale = TRUE)
    ))
  })

  observeEvent(input$map_plot2_selected, ignoreNULL = FALSE, {
    updateCheckboxGroupInput(
      session, "sel_division2",
      selected = input$map_plot2_selected %||% character(0))
  })
  observeEvent(input$sel_division2, ignoreNULL = FALSE, {
    session$sendCustomMessage(
      type = "map_plot2_set",
    message = input$sel_division2 %||% character(0))
  })
  observeEvent(input$clear_sel2, {
    updateCheckboxGroupInput(session, "sel_division2", selected = character(0))
    session$sendCustomMessage(type = "map_plot2_set", message = character(0))
  })

  output$sel_summary2 <- renderText({
    divs <- input$sel_division2 %||% character(0)
    paste0(
      "2025 | Rounds: ", input$sel_rounds, " | ",
      if (length(divs) == 0) "All divisions"
      else paste("Divisions:", paste(divs, collapse = ", "))
    )
  })
}

shinyApp(ui, server)