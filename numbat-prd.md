## PRD: prefviz presentation for senior researchers

### Overview

This presentation introduces **prefviz** as a **visualization toolkit for preferential data**. Unlike the student-facing presentation, this talk should not be centred on teaching one plot type from first principles. Instead, it should present a broader framework for **exploratory visual analysis of preferential data**, with **ternary plots** positioned as one important component of the toolkit.  

The audience is a group of **senior researchers**. They can be assumed to already understand R, tidy workflows, and general ideas in data visualisation. The presentation should therefore focus less on basic instruction and more on **data representation**, **visual tasks**, **scope of the package**, and **why this toolkit is useful for research on elections and rankings**.  

The output should be a **Quarto presentation file (`.qmd`)**. The presentation should still prefer **visual slides**, **minimal on-slide text**, **clear progression**, and **speaker notes**, but compared with the student version it can carry slightly more conceptual content. The original PRD used a 2025 Australian Federal Election case study to introduce the package and emphasised visual slides with notes and progressive reveal, which should still guide the structure here  .

### Audience

The intended audience is **senior researchers** in statistics, political science, data visualisation, computational social science, or related fields. They do not need introductory explanations of ggplot2 or tidyverse syntax. Instead, the presentation should help them understand:

- what preferential data are,
- what forms preferential data can take,
- what kinds of exploratory visualisations are useful,
- why current practice is fragmented,
- and how **prefviz** provides a cleaner workflow for these tasks.  

### Core framing

The presentation should frame **preferential data** as data that express an **ordering or ranking of alternatives**. Elections are the main motivating example, because preferential voting records more information than a single choice and makes transfer patterns substantively important  . The Australian Electoral Commission explains that House of Representatives voters rank candidates in order of preference and that preferences are redistributed during counting until a candidate secures an absolute majority  .

A key framing point should be introduced early:

> Preferential data come in different structural forms, and there is not yet a sufficiently uniform workflow for transforming and visualising them. **prefviz** should be presented as part of a tidy approach to this problem, with **prefio** as the preferred infrastructure for data handling and transformation.  

### Presentation structure

The presentation should follow this structure:

1. **What is preferential data**
2. **Forms of preferential data**
3. **Visualisation for preferential data**
4. **Case study: Australian House of Representatives**

The overall story should move from **definition**, to **data representation**, to **visualisation tasks**, and then to a **substantive election case study**.

### General rules

These constraints should govern the full presentation build.

- Prioritise **visual communication** over dense prose.  
- Keep on-slide text **short and minimal**, with explanation moved into speaker notes.  
- Use **fragments and transitions** so content is introduced progressively.  
- Focus on **exploratory analysis visualisation**, not preferential model visualisation, because the modelling space is too broad for this talk.  
- Present **prefio** as the tidy approach for data transformation and structure management.
- Use the existing materials in `prefviz-presentation.qmd` / `etc5543-pitch.md` as the source for the Australian election case-study slides, except where explicitly replaced.
- Use R 4.5.3 version

## Part 1

### What is preferential data

This opening section should give a quick definition of preferential data and anchor it visually.

#### Intent

The aim is to define the object of interest clearly and quickly. Preferential data should be described as data where observations express a **ranked ordering of alternatives**, rather than only a single chosen category. This is especially natural in elections, where voters can rank candidates and those rankings affect the eventual outcome through preference transfers  .

#### Slide guidance

- Use **one quick definition** only.
- Include a **picture of a ballot** in a column layout.
- Use the image file `images/ballot`.
- Keep this slide simple and visual.

#### On-slide content

A short possible definition:

> Preferential data record how people rank alternatives, not just which option they choose first.

Speaker notes can expand this to mention elections, consumer rankings, and other ranking contexts.

## Part 2

### Forms of preferential data

This section should explain that the same preferential structure can be stored in multiple formats, and that a tidy workflow needs to support conversion and transformation across them.

### Part 2.1

#### Long and wide

This slide should use a **two-column layout**.

- Left column: **long form**
- Right column: **wide form**

The purpose is to show how preferential data can be represented differently depending on the task. Use a simple toy example so the structural distinction is visually obvious.

#### Intent

This slide should emphasise that different representations make different tasks easier:

- **wide form** is often intuitive for reading a full ranking per unit,
- **long form** is more convenient for grouped summaries, plotting, and tidy transformations.

#### Slide guidance

- Show small example tables side by side.
- Keep labels minimal.
- Let the example structure do the explanatory work.

### Part 2.2

#### PrefLib

This slide should introduce **PrefLib-style** data representation.

- Add a screenshot using `images/preflib`.
- Explain briefly that this is a standard way ranked data may be stored or exchanged.

#### Intent

The point is not to teach the full PrefLib specification, but to show that preferential data ecosystems already contain structured formats beyond ordinary rectangular tables. This motivates the need for a tidy bridge.

#### Required framing

This slide should explicitly state:

- **adopt prefio as the tidy approach**
- most transformation operations should use **prefio**

That sentence can appear either on-slide as a short statement or in speaker notes, depending on density.

### Key message for Part 2

End this section with the idea that **preferential data have multiple valid forms, but researchers need a coherent transformation workflow across them**. The package should be presented as building on this need.

## Part 3

### Visualisation for preferential data

This section should define the scope of the package’s visual contribution.

#### Scope statement

The presentation should explicitly state that the focus is on **exploratory-analysis visualisation**, not **preference model visualisation**. That distinction matters because the package is about helping analysts inspect and understand the structure of raw preferential data before or alongside modelling, rather than covering the much broader space of model-based visualisation.  

#### Organising questions

This section should answer two high-level questions:

1. **What are the general preferences in the data?**
2. **How do we compare multiple elections or multiple preference contexts?**

This framing will help the audience see the toolkit as task-oriented rather than plot-oriented.

### Part 3.1

#### Distribution of preference bar chart

This subsection should introduce a **bar-chart view of preference distributions**.

#### Intent

This display should be framed as applicable to:

- **majority systems**, and
- **IRV selected rounds**

The point is to show that basic distributional summaries still matter and should be part of the toolkit.

#### Required content

- Show an example from **sushi data**
`sushi_data <- prefio::read_preflib(
  "00014 - sushi/00014-00000001.soc",
  from_preflib = TRUE
)
sushi_data`
- Code will be provided later
`irv_tbl <- dop_irv(
  sushi_data,
  value_type    = "percent",
  preferences_col = preferences,
  frequency_col   = frequency
)
irv_winner <- unique(irv_tbl$winner)

dop_bar(irv_tbl, -c(round, winner), at_round = 1)`

- The slide should focus on the visual output, not the code

### Part 3.2

#### Pairwise

This subsection should introduce **pairwise comparison visualisation**.

pw_result <- pairwise_calculator(
  sushi_data,
  preferences_col = preferences,
  frequency_col   = frequency
)
pw_result

pairwise_heatmap(pw_result, value = "tcp")

#### Intent

This display should be framed as applicable to:

- **Condorcet analysis**
- **2CP in IRV**

This is an important bridge between ranking data and familiar election-analysis summaries.

#### Required content

- Show an example from **sushi data**
- Code will be provided later
- Focus on what the pairwise display reveals that a simple first-choice summary does not

### Part 3.3

#### Ternary plot

This subsection should introduce **ternary plots** as one of the key contributions within the broader toolkit.

#### Intent

The framing here should be:

- ternary plots support **three-way contests**
- they are especially useful for **multiple elections**
- they help move beyond standard 2D summaries
- they connect naturally to higher-dimensional thinking

#### Required content

- Show the example of **first preference ternary plot 2D** for the **Australian Federal Election**
- Source should follow `prefviz-presentation.qmd`

#### Key contribution statement

This slide should make clear that the contribution is not just “a ternary plot”, but the ability to use this visual language for **multiple elections**, **three-way contests**, and extension to **higher-dimensional exploration**.

## Part 4

### Case study: Australian House of Representatives

This section should be based on `prefviz-presentation.md`, except for slide 4.1, which is newly specified here. The goal is to show how the toolkit works in a real election setting and to connect the general visual ideas from earlier sections to a concrete application. The student PRD already used the Australian federal election as the main case study and treated it as the payoff section of the talk, which is still a useful structure here  .

### Part 4.1

#### First preference overall using `dop_bar`

This slide should show the overall first-preference distribution using **`dop_bar`**.

#### Intent

This is the baseline summary view. It shows that the package handles standard descriptive displays as well as more specialised geometry-based views.

### Part 4.2

#### Multiple electorates on one plot using ternary plot 2D

This slide should show **multiple electorates** together on a single **2D ternary plot**.

#### Intent

This is the main visual payoff for the case study. It should demonstrate how ternary space can reveal contest structure across electorates in a way that is not visible in simpler summaries.

### Part 4.3

#### 2PP and flow of preference

This slide should show:

- **2PP**
- **flow of preference**

#### Intent

This is where the talk returns to familiar election-analysis quantities and shows how they fit within the toolkit. It should also help link the package to the substantive questions election researchers already ask.

### Part 4.4

#### Higher-dimension exploration with `detourr` and `tourr`

This slide should introduce higher-dimensional exploration using:

- **detourr**
- **tourr**

#### Intent

This slide should push beyond ternary plots and show that the package opens the door to exploring preferential structures that cannot be fully represented in a simple 2D triangle. This is important for a senior researcher audience because it signals a broader methodological agenda.

### Speaker notes

Speaker notes are essential and should do most of the explanatory work. For this audience, notes should:

- explain the purpose of each slide,
- state why a given representation is useful,
- connect the slide to the broader package contribution,
- and clarify how the toolkit moves from simple summaries to richer geometric views.  

### Visual design principles

The presentation should still strongly prefer visuals over prose. The original PRD already specified minimal text, strong use of plots, and speaker notes as the main explanatory channel, which should be retained here  .

- Use large visual examples.
- Keep text to short labels or framing statements.
- Use consistent colour logic across slides.
- Let the progression from simple to advanced views structure the talk.
- Use code only where it helps orient the audience, not as the main content.

### Output requirements

The final output should be a **`.qmd` presentation file**.

The presentation should include or reference these image assets:

- `images/ballot`
- `images/preflib`

It should also draw on these existing sources where relevant:

- `prefviz-presentation.qmd`
- `etc5543-pitch.md`

### Success criteria

The presentation succeeds if, by the end of the talk, the audience can:

- understand what **preferential data** are,
- recognise the main **forms of preferential data**,
- see why **prefio** is a useful tidy approach for transformations,
- understand the scope of **exploratory visualisation** for preferential data,
- distinguish between bar-chart, pairwise, ternary, and higher-dimensional views,
- and see how **prefviz** supports comparative and election-based analysis beyond a single plot type.  