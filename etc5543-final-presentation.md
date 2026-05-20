## PRD: prefviz presentation for ETC5543 students

### Overview

This presentation introduces **prefviz** as a visualisation toolkit for preferential data, demonstrated through a live case study on the 2025 Australian House of Representatives election. The talk is **10 minutes long** and targets students who know R, the tidyverse, and ggplot2. It should feel like a **package demo**, not a lecture: move fast, show the output, and let the code orient the audience — not dominate it.

The output is a **Quarto revealjs `.qmd` file**. Visual slides with minimal text and progressive reveal throughout. Reference `numbat-presentation.qmd` for layout patterns and CSS already established.

---

### Audience

**ETC5543 students.** They are comfortable with:

- R, tidyverse (`dplyr`, `tibble`), ggplot2
- The concept of tidy data (one row per observation)
- Basic electoral concepts (candidates, votes, parties)

They do **not** need:

- Explanation of what ggplot2 is
- Introduction to the tidyverse
- Extended background on Australian politics

The presentation should orient them quickly to what **preferential data** is, why existing tools fall short, and then spend most of the time showing **what the package can do** through a concrete case study.

---

### Core framing

> **prefviz** is a toolkit for exploring preferential voting data — from single-electorate summaries to high-dimensional visualisations of all electorates at once.

The talk builds from simple to complex: first-choice summaries → pairwise comparisons → 2D ternary plots → high-dimensional tours. Each step adds something the previous view could not show.

---

### Timing budget

10 minutes, approximately:

| Section | Slides | Time |
|---|---|---|
| Part 1 — What is preferential data | 1 | ~1 min |
| Part 2 — Motivation | 1 | ~1 min |
| Part 3 — Design principles | 1 | ~1 min |
| Part 4 — Case study | 7–8 | ~7 min |

Keep Parts 1–3 tight. The case study is the payload.

---

### General rules

- **Minimal on-slide text.**
- **Every content slide shows a plot or a table.** No walls of bullet points.
- **2-column layout** for slides 6 and 7, pairing a question with a plot.
- **Panel tabset** (`## Plot` / `## Code`) for all plot slides in the case study, so code is available but not the default view.
- **`echo: false`** by default; code is shown only in the Code tab.
- Fragments and incremental reveals where content needs to be introduced step by step.
- Use `ggiraph` for interactivity on ggplot2 plots wherever the demo file uses static `geom_point`.
- Base all plots on `final_presentation_demo.R`. Make edits only where needed for interactivity or clarity.
- Use R 4.5.3.

---

### Format

Copy the YAML front matter and CSS block from `numbat-presentation.qmd` (lines 1–49) with the following change:

- **`fontsize: 30px`** (increase from 24px — audience is students in a lecture room, larger text needed)

Setup chunk: source `final_presentation_demo.R` instead of `package_demo.R`.

---

## Title slide

- **Title:** *prefviz: A visualisation toolkit for preferential election data*
- Logo in `images/prefviz-sticker.png`
- Template follows that of `ref/sugarglider-presentation/presentation.qmd`

## Part 1 — What is preferential data

### Slide 1

**Intent:** Define the object of interest quickly and anchor it visually.

**Layout:** 2-column — definition left, ballot image right (`images/ballot.jpg`).

**On-slide content:**

> Preferential data record how people rank alternatives, not just which option they choose first.

Fragment — common contexts:
- Elections — voters rank candidates in order
- Consumer surveys — rank products or services
- Food research — rank dishes, wines, flavours

**Source:** Copy slide layout from `numbat-presentation.qmd` lines 75–99 (`## Data that captures rankings, not just first choices`). The structure and image reference are identical; update column widths if needed for the larger font.

---

## Part 2 — Motivation

### Preferential data comes in many shapes, making it hard to visualise

**Intent:** Show different shapes of preferential data and highlight the gaps in handling them

**Source:** Copy slide layout from `numbat-presentation.qmd` lines 106 - 174. However, the slide PrefLib (from line 143), less text, add a shape of prefio to illustrate how all data can be transformed to a common format using prefio. The code is not shown, but showing the output. 

### No way to compare multiple preference sets

**Intent:** Establish the gap in handling multiple preference sets

**On-slide content:**

- **Small text:** How do you compare first preference votes between 2 major parties (Labor and Liberal), across 150 electorates during the 2025 Australian House of Representatives election?
- Tabs or fragments showing the data shapes and the plot which looks troublesome. 

**Sources:** Data source can be a dummy data set. Copy slide layout from `numbat-presentation.qmd` lines 183 - 216, but show the dataset in composition form by campus first, then show the plot. 

---

## Part 4 — Case study: 2025 Australian House of Representatives

### Slide 4 - Context and Key questions

**Intent:** Introduce the case study and the key questions

**Content:** In fragment:

- Introduce Australian House of Representatives election, 2025: 150 electorates, 28 parties, 8 parties won seats, major parties: Labor (ALP), Liberal (LNP). Other parties of increasing interest: Greens (GRN) & Independents (IND)
- **Three questions this case study explores:**

1. How is the first-preference vote distributed across the two major parties (ALP and LNP) across 150 electorates?
2. For selected electorates, how do preference shares shift through the IRV counting rounds?
3. What additional structure in first preference distribution is revealed when Greens and Independents are separated from "Others"?

**Sources:** New slide — no direct equivalent in `numbat-presentation.qmd`. Write fresh.

### Slide 5 — Transform the data

**Intent:** Show students what the raw data looks like and the transformation pipeline before plots appear.

**Layout:** From: `echo: false` code slide (the one code-forward slide in the deck).

Show: `head(aecdop_2025 |> select(DivisionNm, CountNumber, PartyAb, CalculationType, CalculationValue, Elected) |> filter(CalculationType == "Preference Percent"))`

To: Transformed data with `dop_transform()`, show code and output.

- Use fragment

**Source:** Adapt from `numbat-presentation.qmd` lines 509–540 (`## ternable object` / chunk `step2`). Replace the `as_ternable` demo with a `head()` preview of the raw data + the three case-study questions.

---

### Slide 6 — How is the first-preference vote distributed across the two major parties (ALP and LNP) across 150 electorates? 

Panel tabset (Plot / Code). Interactive plot using `geom_point_interactive` from `ggiraph`. Tooltip shows electorate name and vote shares. Colour encodes winning party.

**Base code:**

```r
tern_2d <- as_ternable(pref25_2d, items = ALP:Other)
input_data <- get_tern_data2d(tern_2d) |>
  mutate(text = paste0(DivisionNm, "\nALP: ", round(ALP*100,1), "%\n",
                       "LNP: ", round(LNP*100,1), "%\nOther: ", round(Other*100,1), "%"))

ggplot(input_data |> filter(CountNumber == 0), aes(x = x1, y = x2)) +
  add_ternary_base() +
  geom_ternary_region(aes(fill = after_stat(vertex_labels)),
    vertex_labels = tern_2d$vertex_labels, alpha = 0.3, color = NA, show.legend = FALSE) +
  add_vertex_labels(tern_2d$simplex_vertices) +
  geom_point_interactive(aes(color = Winner, tooltip = text, data_id = DivisionNm)) +
  scale_color_manual(values = party_colors, name = "Elected") +
  theme_void()
```

Wrap output with `girafe()`.

**Very important:** Follow the code reveal protocol. Refer to line 476 - 740 in `archive\ternary-standard\prefviz-presentation.qmd` and section Code reveal in line 285. 

**Source:** Copy panel tabset structure from `numbat-presentation.qmd` lines 580–623 (chunks `ternary-interactive-plot` and `ternary-interactive-code`). Replace `p2d_interactive` with the inline ggplot call above using 2025 data from `final_presentation_demo.R`.

**Speaker notes:** What can and can't be learnt from this plot:

- Explain the regions, the vertices, and the edges
- Most in the center, 2 on the edges
- While first preferences for a number of electorates are in the Other region, they invariably end up with an ALP or LNP win, but the converse doesn’t happen.
- Question: How does one in Other end up with a majority? 

---

### Slide 9 — Flow of preference (`stat_ordered_path`)

**Slide title:** *For selected electorates, how do preference shares shift through IRV counting rounds?*

Panel tabset (Plot / Code). Show paths for Fowler and Monash. Wrap with `girafe()`.

**Base code:**

```r
path_input <- input_data |>
  filter(DivisionNm %in% c("Fowler", "Monash")) |>
  mutate(text = paste0("Round: ", CountNumber, "\n", text))

ggplot(path_input, aes(x = x1, y = x2)) +
  add_ternary_base() +
  geom_ternary_region(aes(fill = after_stat(vertex_labels)),
    vertex_labels = tern_2d$vertex_labels, alpha = 0.3, color = "grey50", show.legend = FALSE) +
  stat_ordered_path(aes(group = DivisionNm, order_by = CountNumber, color = DivisionNm),
    linewidth = 0.5) +
  add_vertex_labels(tern_2d$simplex_vertices) +
  geom_point_interactive(aes(color = DivisionNm, tooltip = text, data_id = DivisionNm)) +
  scale_fill_manual(values = c("ALP" = "#E13940", "LNP" = "#1C4F9C", "Other" = "#95A5A6")) +
  scale_color_viridis_d(name = "Electorate") +
  theme_void()
```

**Source:** Copy panel tabset structure from `numbat-presentation.qmd` lines 627–665 (chunks `flow-plot` and `flow-code`). Replace `p2d_line_interactive` with the inline code above; switch electorates to Fowler and Monash (from `final_presentation_demo.R`) instead of Hotham/Fowler/Monash.

**Speaker notes:** What can and can't be learnt from this plot:

- Explain the lines

---

### Slide 11 — What structure is hidden in "Others"? (High-dimensional tour)

**Slide title:** *What additional structure is revealed when Greens and Independents are separated from "Others"?*

Panel tabset (Plot / Code). Show the `detourr` animated tour from `final_presentation_demo.R` with 5-party simplex (ALP, LNP, GRN, IND, Other).

**Base code:**

```r
tern_hd <- as_ternable(
  pref25_hd_all |> filter(CountNumber == 0),
  items = c(ALP, LNP, GRN, IND, Other)
)

detour(dtour_data, tour_aes(projection = x1:x4, colour = Winner, labels = text)) |>
  tour_path(planned_tour(lt), fps = 60) |>
  show_scatter(axes = FALSE, palette = party_colors,
               edges = get_tern_edges(tern_hd), size = 1)
```

**Source:** Copy panel tabset structure from `numbat-presentation.qmd` lines 669–703 (chunks `detourr-plot` and `detourr-code`). Replace the plot object `de` with the inline `detour()` call above using `final_presentation_demo.R` objects (`dtour_data`, `lt`, `tern_hd`).

---

## Part 5 - Additional: Other things you can do with `prefviz`

### Slide 12 - Quick bar chart exploration of 1 electorates

**Sources:** Copy slide layout from `numbat-presentation.qmd` lines 270 - 305

### Slide 13 - Quick heatmap exploration of pairwise comparisons

**Sources:** Copy slide layout from `numbat-presentation.qmd` lines 217 - 268

## Output requirements

- **File:** a `.qmd` revealjs presentation file
- **YAML + CSS:** Copy from `numbat-presentation.qmd` lines 1–49; change `fontsize` to `30px`
- **Setup chunk:** source `final_presentation_demo.R` (not `package_demo.R`)
- **Images needed:**
  - `images/ballot.jpg` — ballot paper (Slide 1)
  - `images/file_structure.png` — function workflow placeholder (Slide 4)
- **Interactivity:** `ggiraph` for ggplot2 plots; `detourr` for the high-dimensional tour
- **`execute` options:** `echo: false`, `warning: false`, `message: false`, `eval: true` by default; flip `echo: true` only on Slide 5 (dataset setup) and Slide 10 (how to build)
- **Content & quarto:** Refer to `numbat-presentation.qmd` where noted per slide above

## Success criteria

By the end of the 10 minutes, students should be able to:

- Explain what preferential data is and why standard plots fall short
- Name the three design principles of prefviz
- Identify which function to use for: single-preference summaries, 2D ternary plots, and high-dimensional tours
- Understand that ternary plots encode three-component compositions and can show all electorates simultaneously
- See that the same ggplot2 workflow they already know extends naturally to ternary geometry via prefviz


## Code reveal

:::{.columns}

:::{.column width="50%"}

:::{.fragment fragment-index=1 .code-reveal}
```{r}
#| echo: true
#| eval: false
simple_graph |>
  ggraph(x = long, y = lat)
```
:::

:::{.fragment fragment-index=2 .code-reveal}
```{r}
#| echo: true
#| eval: false
simple_graph |>
  ggraph(x = long, y = lat) +
  geom_sf(data = vic_map, color = "white")
```
:::

:::{.fragment fragment-index=3 .code-reveal}
```{r}
#| echo: true
#| eval: false
simple_graph |>
  ggraph(x = long, y = lat) +
  geom_sf(data = vic_map, color = "white") +
  geom_edge_link(alpha = 0.1)
```
:::

:::{.fragment fragment-index=4 .code-reveal}
```{r}
#| echo: true
#| eval: false
simple_graph |>
  ggraph(x = long, y = lat) +
  geom_sf(data = vic_map, color = "white") +
  geom_edge_link(alpha = 0.1) +
  geom_node_point(aes(color = category))
```
:::

:::



:::{.column width="50%"}

:::{.r-stack}
![](pictures/test1.png){.fragment fragment-index=1}

![](pictures/test2.png){.fragment fragment-index=2}

![](pictures/test3.png){.fragment fragment-index=3}

![](pictures/test4.png){.fragment fragment-index=4}
:::

:::

:::

.code-reveal {
  position: absolute !important;
  top: 20% !important;
  left: 0 !important
}