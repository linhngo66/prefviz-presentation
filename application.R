library(tidyverse)
library(prefviz)
library(ggplot2)
library(tourr)
library(detourr)
library(ggiraph)
library(crosstalk)
library(ggthemes)

# Map data
elb_df <- read_csv(here::here("data/elb_map.csv"))
elb_centroid <- read_csv(here::here("data/elb_centroid.csv"))

# DOP
transformer <- function(data, year) {
  data |> 
    filter(CalculationType == "Preference Percent") |> 
    mutate(
      Party = case_when(
        !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ", "GRN", "IND")) ~ "Other",
        PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
        TRUE ~ PartyAb),
      Year = year
    ) |> 
    dop_transform(
      key_cols = c(DivisionNm, CountNumber, Year, StateAb),
      value_col = CalculationValue, item_col = Party,
      winner_col = Elected, winner_identifier = "Y"
    )
}

dop19 <- read_csv("https://results.aec.gov.au/24310/Website/Downloads/HouseDopByDivisionDownload-24310.csv", skip = 1) |>
  transformer(year = 2019)
dop22 <- transformer(aecdop_2022, 2022)
dop25 <- transformer(aecdop_2025, 2025)

aecdop_ts <- dop19 |> 
  bind_rows(dop22, dop25) |> 
  mutate(Year = factor(Year, levels = c(2019, 2022, 2025))) |> 
  arrange(DivisionNm, Year, CountNumber)

#### Change in first preference ---------------------------------------------
party_cols <- c("ALP", "LNP", "GRN", "IND", "Other")

first_pref_df <- aecdop_ts |> 
  # highest pref party in the round
  rowwise() |>
  mutate(
    pref1_party = {
      vals <- c_across(all_of(party_cols))
      party_cols[which.max(vals)]
    }
  ) |>
  ungroup()

change_all <- first_pref_df |> 
  filter(CountNumber == 0) |>
  filter(!is.na(DivisionNm), !is.na(Year)) |>
  arrange(DivisionNm, Year) |>
  group_by(DivisionNm) |>
  mutate(prev_party = lag(pref1_party),
         prev_year  = lag(Year)) |>
  filter(!is.na(prev_party), pref1_party != prev_party) |>
  ungroup() 

change_22 <- change_all |>
  filter(Year == 2022) |> pull(DivisionNm)
change_25 <- change_all |>
  filter(Year == 2025) |> pull(DivisionNm)
change_both <- change_all |> pull(DivisionNm)

#### Change in flow of preference ---------------------------------------------
diff_party_25 <- first_pref_df |>
  filter(Year == 2025) |>
  group_by(DivisionNm) |>
  filter(CountNumber == 0 | CountNumber == max(CountNumber)) |>
  slice(c(1, n())) |>  # ensure only first (0) and last (max) rows
  summarise(
    pref_first = pref1_party[CountNumber == 0],
    pref_last  = pref1_party[CountNumber == max(CountNumber)],
    .groups = "drop"
  ) |>
  filter(pref_first != pref_last) |> 
  pull(DivisionNm)

#### Data for detourr ---------------------------------------------------------
tsern <- as_ternable(
  first_pref_df, items = c(ALP:IND))

detour_transform <- function(data) {
  tsern$simplex_vertices |> 
    bind_rows(data) |>
    mutate(pref1_party = coalesce(pref1_party, labels)) |> 
    # combined with map data 
    left_join(
      elb_centroid[, c("long", "lat", "elect_div")],
      by = c("DivisionNm" = "elect_div"))
}

map_edges <- function(detour_transform_df) {
  data_rows <- detour_transform_df |> 
    mutate(.row = row_number()) |>
      filter(!is.na(DivisionNm)) |>
      group_by(DivisionNm) |>
      mutate(Var1 = .row, Var2 = lead(.row, default = last(.row))) |>
      ungroup() |>
      select(Var1, Var2)
  
  edges <- data_rows |>
      bind_rows(as_tibble(tsern$simplex_edges)) |>
      as.matrix()
  
  edges
}

#### Color setup ---------------------------------------------
party_colors <- c(
  "ALP" = "#E13940",
  "GRN" = "#10C25B", 
  "IND" = "#F39C12",
  "LNP" = "#1C4F9C", 
  "Other" = "#95A5A6" 
)

#### Output for Shiny App ---------------------------------------------
detour_df_first <- get_tern_data(tsern, plot_type = "2D") |>
  filter(CountNumber == 0) |> 
  detour_transform() |> 
  mutate(
    text = if_else(
      is.na(labels), 
      paste0(
        "<b>", DivisionNm, "</b><br>",
        "Year: ", Year, "<br>",
        "<b> Most preferred: ", pref1_party, "</b><br>",
        "<b> Elected Party: ", Winner, "</b><br>",
        "ALP: ", round(ALP * 100, 1), "%<br>",
        "LNP: ", round(LNP * 100, 1), "%<br>",
        "GRN: ", round(GRN * 100, 1), "%<br>",
        "IND: ", round(IND * 100, 1), "%<br>",
        "Other: ", round(Other * 100, 1), "%"
      ),
      labels)
  )
detour_edges_first <- map_edges(detour_df_first)

detour_df_flow <- get_tern_data(tsern, plot_type = "2D") |>
  filter(Year == 2025) |> 
  detour_transform() |> 
  mutate(
    text = if_else(
      is.na(labels), 
      paste0(
        "<b>", DivisionNm, "</b><br>",
        "Round: ", CountNumber, "<br>",
        "<b> Most preferred: ", pref1_party, "</b><br>",
        "<b> Elected Party: ", Winner, "</b><br>",
        "ALP: ", round(ALP * 100, 1), "%<br>",
        "LNP: ", round(LNP * 100, 1), "%<br>",
        "GRN: ", round(GRN * 100, 1), "%<br>",
        "IND: ", round(IND * 100, 1), "%<br>",
        "Other: ", round(Other * 100, 1), "%"
      ),
      labels)
  )
detour_edges_flow <- map_edges(detour_df_flow)

# de <- detour(
#   detour_df_flow,
#   tour_aes(
#     projection = x1:x4, colour = pref1_party, 
#     # label = c(text)
#     ) 
# ) |>
#   tour_path(grand_tour(3)) |>
#   show_scatter(
#     axes = FALSE,
#     # ALP, GRN, IND, LNP, Other
#     palette = party_colors,
#     edges = detour_edges_flow,
#     size = 1
#   )

# # Run map
# map <- ggplot() +
#   geom_polygon(
#     data = elb_df,
#     aes(x = long, y = lat, group = group),
#     fill = "grey90", color = "white"
#   ) +
#   geom_point(
#     data = detour_df_flow,
#     aes(x = long, y = lat, color = pref1_party),
#     size = 1, alpha = 0.8
#   ) +
#   scale_color_manual(values = party_colors) +
#   coord_equal() +
#   theme_map()