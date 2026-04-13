## PRD: prefviz presentation

### Overview

This presentation introduces the **prefviz** package through the case study of the **2025 Australian Federal Election**. It is designed for **1st year Master students** who are already familiar with **ggplot2** and basic **tidyverse** workflows for data transformation and analysis, but are **not familiar with ternary plots, preferential data, or high-dimensional data analysis**. The presentation should last **20–25 minutes** and should prioritise **visual communication, minimal text, clear code highlighting, speaker notes, and progressive reveal through fragments and transitions**. [results.aec.gov](https://results.aec.gov.au/31496/Website/HouseDefault-31496.htm)

Here is the package repository: <https://github.com/numbats/prefviz>

### Audience

The audience knows enough R to follow tidy data preparation and ggplot2 syntax, so the presentation should not spend time on basic R concepts. The conceptual challenge is instead to help them understand **what preferential data are**, **why existing visualisation methods are insufficient**, and **how ternary plots offer a useful representation for three-part electoral compositions**. [aec.gov](https://www.aec.gov.au/learn/preferential-voting.htm)

### Presentation structure

The presentation should follow this exact four-part structure provided by the user.

1. **Problem statement**: Present an election scenario and the problem of existing visualization methods, then introduce the solution, which is the ternary plot.
2. **Introduction to ternary plot**: Explain how to read ternary plot using an example of ABC News.
3. **Use case of ternary plot in analysis of 2025 Australian Federal Election**: Show types of plot the package can produce and what analysis can be done with it, specifically first preference distribution, flow of preference in a selected electorate, linking ternary plot with map for more context, present the Shiny App.
4. **Brief introduction of how to use the package**: Main function, how to recreate the plot shown in previous slides.

### General rules

These constraints should govern the full presentation build.

- Highlight code where necessary.
- Prioritize visual slides with minimal to no text.
- Put the explanatory burden in the speaker notes.
- Use fragments and transitions as much as possible so information arrives in bite-sized pieces.
- Keep continuity with the existing source materials, especially `@etc5543-pitch.qmd` and `@120226_progress.qmd`.

### Part 1

Part 1 should **follow `@etc5543-pitch.qmd`** closely, rather than being substantially reinterpreted. The role of this section is to set up the election scenario, identify the limitations of existing visualization methods, and then motivate ternary plots as the solution.

#### Intent

The narrative should begin from the election context rather than from the geometry of the chart. Australian House elections use **preferential voting**, where voters rank candidates and preferences can be redistributed during counting. This makes the data richer than a simple two-party bar chart, because first preferences alone do not fully capture electoral structure or competition. [abc.net](https://www.abc.net.au/news/2025-04-24/election-data-rise-independents-major-party-drift/105144918)

#### What to preserve from the user's source

- Start with the election scenario.
- Show why existing visualizations are not enough.
- Transition into ternary plot as the proposed solution.
- Keep this section aligned with the existing framing already developed in `@etc5543-pitch.qmd`.

#### Slide guidance

- Use the current story arc in `@etc5543-pitch.qmd` as the backbone.
- Keep on-slide text minimal.
- Use speaker notes to explain why compositional electoral data are difficult to see in ordinary charts.
- Introduce the ternary plot only after the limitation is felt.

### Part 2

Part 2 should still **follow `@etc5543-pitch.qmd`**, but this is the one section where the current markdown file’s expanded explanation should be retained. The main purpose is to teach students how to read a ternary plot in a slow, concrete, highly visual way using the ABC News example. [abc.net](https://www.abc.net.au/news/elections/federal/2025/results)

#### Intent

Students are unfamiliar with ternary plots, so this section must act as the conceptual bridge for the whole talk. It should expand the explanation of how to read the plot, especially by giving specific examples of:

- A point that sits on one of the vertices.
- A point that sits along an edge.
- A point in the middle.

#### What to include

Retain the current explanatory structure already drafted in the existing markdown:

- Introduce the ABC News ternary example as a real-world anchor. [abc.net](https://www.abc.net.au/news/elections/federal/2025/results)
- Explain the anatomy of the triangle: three corners, three components, one point per electorate or unit.
- Explain points at the **vertices** as extreme cases, such as a hypothetical 100/0/0 split.
- Explain points on an **edge** as cases where one component is zero, such as 60/40/0.
- Explain a point in the **centre** as roughly balanced support across the three groups.
- Return to the ABC chart and interpret a few real or representative positions visually.

#### Slide guidance

- Build the explanation gradually using fragments.
- Reuse the same ternary diagram several times, adding one concept at a time.
- Use very little text on the slide itself.
- Put the interpretive explanation in the speaker notes.
- Ask short audience questions in the notes, such as “Where would a safe Labor seat sit?” or “What does it mean if a point lies on this edge?”

### Part 3

Part 3 should **follow `@etc5543-pitch.qmd`**, with the additional requirement to introduce the **Shiny App**. The central aim is to demonstrate what the package can produce and what kinds of electoral analysis become possible through these plots. [results.aec.gov](https://results.aec.gov.au/31496/Website/HouseDefault-31496.htm)

#### What must be shown

This part should cover the use of ternary plots in the analysis of the **2025 Australian Federal Election** by showing:

- First preference distribution.
- Flow of preference in a selected electorate.
- Linking ternary plot with a map for more context.
- The Shiny App.

#### Intent

This section should function as the payoff for the conceptual work done earlier. After students learn how to read the plot, they should now see multiple outputs from **prefviz** and understand that the package supports both static analysis and exploratory interaction. 

#### Slide guidance

- Keep close to the structure and sequence already present in `@etc5543-pitch.qmd`.
- Add a slide or sub-section introducing the Shiny App.
- Use visuals to show the range of outputs rather than long descriptions.
- Use speaker notes to explain what analysis each plot enables.

#### Required analytical examples

- **First preference distribution**: show how electorates are distributed in ternary space.
- **Selected electorate preference flow**: show how one electorate can be followed through preference movement.
- **Linked ternary plot and map**: show how geographic context complements compositional context.
- **Shiny App**: show how a user can explore electorates, groupings, or views interactively.

### Part 4

Part 4 should **follow `@120226_progress.qmd`, starting from “Overview of prefviz function”**, but it requires **major edits**. The content should remain based on the user’s existing material, but be reorganised so it becomes more teachable for this audience.

#### What must change

This section should not simply reproduce the current progress material. It needs to be edited so that it more clearly explains:

- What each of the components of the **ternary object** means.
- The different **ggplot2 extension functions** used when recreating the plot.

#### Intent

The goal here is not to provide full package documentation. It is to give students just enough understanding that they can mentally connect the visual outputs from earlier slides to the code structure that produced them. Because the audience already knows ggplot2 and tidyverse basics, the explanation should focus on what is **new** in prefviz rather than repeating generic plotting concepts. [rdrr](https://rdrr.io/github/aeggers/votevizr/f/vignettes/votevizr_overview.Rmd)

#### Slide guidance

- Start exactly from the “Overview of prefviz function” entry point in `@120226_progress.qmd`.
- Break down the ternary object into understandable components.
- Break down the plotting workflow into staged pieces.
- Show only the code necessary to recreate plots shown earlier in the deck.
- Highlight only the important parts of the code.
- Use speaker notes to explain what each component is doing and why it exists.

#### Suggested teaching sequence

1. Introduce the overview of prefviz functions.
2. Explain the ternary object and its main components.
3. Show the core plotting workflow.
4. Recreate one of the earlier plots in a small number of clear code steps.
5. Briefly introduce the ggplot2 extension functions used in that reconstruction.

### Speaker notes

Speaker notes are essential and should carry most of the explanation. Each slide should include notes that:

- Explain the point of the slide in plain language.
- Clarify any concept that is visually introduced but not fully explained on-slide.
- Guide when fragments or transitions should be triggered.
- Suggest small prompts or questions to keep the audience engaged.

### Visual design principles

The presentation should strongly prefer visuals over prose. This is especially important because the subject matter is new to the audience and can become abstract quickly if over-explained in text.

- Use large plot images and annotated examples. [abc.net](https://www.abc.net.au/news/2025-04-24/election-data-rise-independents-major-party-drift/105144918)
- Keep slide text to keywords, short phrases, or labels.
- Use visual consistency across party colours and plot structure.
- Break complex explanations across multiple fragments or transitions instead of placing everything on one slide.

### Success criteria

The presentation succeeds if, by the end of the talk, students can:

- Understand why standard election visualizations are insufficient for some preferential-data questions. [aec.gov](https://www.aec.gov.au/learn/preferential-voting.htm)
- Read a ternary plot at the level of vertices, edges, and centre. [abc.net](https://www.abc.net.au/news/elections/federal/2025/results)
- Recognise the kinds of outputs and analyses that prefviz supports for the 2025 Australian Federal Election. [results.aec.gov](https://results.aec.gov.au/31496/Website/HouseDefault-31496.htm)
- Follow a basic prefviz workflow well enough to see how a ternary plot is recreated in R. [rdrr](https://rdrr.io/github/aeggers/votevizr/f/vignettes/votevizr_overview.Rmd)