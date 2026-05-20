library(prefviz)
library(dplyr)
library(ggplot2)
library(tourr)
library(detourr)
library(plotly)
library(tibble)
library(prefio)
library(patchwork)
library(ggiraph)

party_cols <- c("ALP", "LNP", "Other")
party_colors <- c(
  "ALP"   = "#E13940",
  "LNP"   = "#1C4F9C",
  "GRN"   = "#10C25B",
  "IND"   = "#19d3e0",
  "XEN"   = "#ee8a08",
  "KAP"   = "#721be4",
  "Other" = "#95A5A6"
)
winners <- aecdop_2025 |>
  filter(Elected == "Y") |>
  distinct(DivisionNm, true_winner = PartyAb) |> 
  mutate(true_winner = case_when(
    true_winner %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ true_winner
  ))

pref25_2d <- aecdop_2025 |>
  filter(CalculationType == "Preference Percent") |>
  mutate(Party = case_when(
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    !(PartyAb %in% c("ALP", "LP", "NP", "LNP", "LNQ")) ~ "Other",
    TRUE ~ PartyAb
  )) |> 
  dop_transform(
    key_cols = c(DivisionNm, CountNumber),
    value_col = CalculationValue,
    item_col = Party,
    winner_col = Elected,
    winner_identifier = "Y") |> 
  left_join(winners, by = c("DivisionNm")) |>
  # highest pref party in the round
  rowwise() |>
  mutate(
    pref1_party = {
      vals <- c_across(all_of(party_cols))
      party_cols[which.max(vals)]
    }
  ) |>
  ungroup()

tern_2d <- as_ternable(pref25_2d, items = c(ALP:Other))

### 2D scatter plot of first-preference vote shares across electorates

input_data <- get_tern_data2d(tern_2d) |>
  mutate(
    text = paste0(
      DivisionNm, "\n",
      "ALP: ",   round(ALP   * 100, 1), "%\n",
      "LNP: ",   round(LNP   * 100, 1), "%\n",
      "Other: ", round(Other * 100, 1), "%"
    ),
    text = gsub("'", "", text)
  )

p2d_scatter <- ggplot(
  input_data |> filter(CountNumber == 0),
  aes(x = x1, y = x2)) +
  add_ternary_base() +
  geom_ternary_region(
    aes(fill = after_stat(vertex_labels)),
    vertex_labels = tern_2d$vertex_labels,
    alpha = 0.3, color = "black", show.legend = FALSE
  ) +
  add_vertex_labels(tern_2d$simplex_vertices) +
  scale_fill_manual(
    values = party_colors
  ) +
  geom_point_interactive(
    aes(color = true_winner, tooltip = text, data_id = DivisionNm)) +
  scale_color_manual(
    values = party_colors,
    name   = "Elected party"
  ) 

p2d_scatter_interactive <- girafe(
  ggobj = p2d_scatter,
  options = list(
    opts_hover(css = "fill-opacity:1;stroke:black;stroke-width:2;"),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_tooltip(
      css = "background-color:white;padding:8px;border-radius:5px;box-shadow:2px 2px 5px rgba(0,0,0,0.3);font-size:20px;"
    )
  )
)

### 2D paths of vote shares across counting rounds for selected electorates

path_input <- input_data |>
  filter(DivisionNm %in% c("Fowler", "Melbourne", "Richmond")) |>
  mutate(text = paste0("Round: ", CountNumber, "\n", text))

p2d_line <- ggplot(path_input, aes(x = x1, y = x2)) +
  add_ternary_base() +
  geom_ternary_region(
    aes(fill = after_stat(vertex_labels)),
    vertex_labels = tern_2d$vertex_labels,
    alpha = 0.3, color = "black", show.legend = FALSE
  ) +
  stat_ordered_path(
    aes(group = DivisionNm, 
      order_by = CountNumber, color = DivisionNm),
    linewidth = 0.5
  ) +
  add_vertex_labels(tern_2d$simplex_vertices) +
  geom_point_interactive(
    aes(color = pref1_party, shape = DivisionNm, 
      tooltip = text, data_id = DivisionNm), size = 1.3) +
  scale_fill_manual(
    values = c(party_colors)
  ) +
  scale_color_manual(
    values = party_colors,
    name = "Elected party"
  )

p2d_line_interactive <- girafe(
  ggobj = p2d_line,
  options = list(
    opts_hover(css = "fill-opacity:1;stroke:black;stroke-width:2;"),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_tooltip(
      css = "background-color:white;padding:8px;border-radius:5px;box-shadow:2px 2px 5px rgba(0,0,0,0.3);font-size:20px;"
    )
  )
)

### High dimensional tour of vote shares across counting rounds for all electorates

pref25_hd_all <- aecdop_2025 |>
  filter(CalculationType == "Preference Percent") |>
  mutate(Party = case_when(
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    !(PartyAb %in% c("ALP", "LP", "NP", "LNP", "LNQ", "GRN", "IND")) ~ "Other",
    TRUE ~ PartyAb
  )) |>
  dop_transform(
    key_cols          = c(DivisionNm, CountNumber),
    value_col         = CalculationValue,
    item_col          = Party,
    winner_col        = Elected,
    winner_identifier = "Y"
  )

tern_hd <- as_ternable(
  pref25_hd_all |> filter(CountNumber == 0),
  items = c(ALP, LNP, GRN, IND, Other))

dtour_data <- get_tern_datahd(tern_hd) |>
  mutate(
    Winner = factor(coalesce(Winner, labels), levels = c("ALP", "LNP", "GRN", "IND", "Other")),
    text = if_else(
      labels == "",
      paste0(
        "<b>", DivisionNm, "</b><br>",
        "Elected Party: ", Winner, "</b><br>",
        "ALP: ", round(ALP * 100, 1), "%<br>",
        "LNP: ", round(LNP * 100, 1), "%<br>",
        "GRN: ", round(GRN * 100, 1), "%<br>",
        "IND: ", round(IND * 100, 1), "%<br>",
        "Other: ", round(Other * 100, 1), "%"
      ),
      labels)
  )

set.seed(327)
lt <- save_history(dtour_data[,1:4], little_tour(), max_bases=6)
lt <- lt[,,c(1, 4, 6, 5, 3, 2)]

detour_colors <- c(
  "ALP"   = "#E13940",
  "LNP"   = "#1C4F9C",
  "GRN"   = "#10C25B",
  "IND"   = "#19d3e0",
  "Other" = "#95A5A6"
)

detour_scatter <- detour(
  dtour_data,
  tour_aes(projection = x1:x4, colour = Winner, labels = text)
) |>
  tour_path(planned_tour(lt), fps = 60) |>
  show_scatter(
    axes    = FALSE,
    palette = detour_colors,
    edges   = get_tern_edges(tern_hd),
    size    = 1)

