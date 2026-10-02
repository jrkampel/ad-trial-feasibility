library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(countrycode)
library(httr)
library(jsonlite)

# ── Data preparation ─────────────────────────────────────────────────────────

pull_trials <- function(page_token = NULL) {
  query <- list(
    "query.cond" = "Alzheimer's Disease",
    "filter.overallStatus" = "RECRUITING",
    "pageSize" = 100,
    "format" = "json"
  )
  if (!is.null(page_token)) query$pageToken <- page_token
  response <- GET("https://clinicaltrials.gov/api/v2/studies", query = query)
  fromJSON(content(response, as = "text", encoding = "UTF-8"))
}

all_trial_dfs <- list()
page_token <- NULL

for (page in 1:5) {
  cat("Pulling page", page, "\n")
  result <- pull_trials(page_token)
  studies <- result$studies
  
  page_df <- lapply(1:nrow(studies), function(i) {
    nct_id <- studies$protocolSection$identificationModule$nctId[i]
    title <- studies$protocolSection$identificationModule$briefTitle[i]
    sponsor <- studies$protocolSection$sponsorCollaboratorsModule$leadSponsor$name[i]
    phases <- studies$protocolSection$designModule$phases[[i]]
    phase <- if (length(phases) > 0) paste(phases, collapse = ", ") else "Not specified"
    locations <- studies$protocolSection$contactsLocationsModule$locations[[i]]
    countries <- if (!is.null(locations) && "country" %in% names(locations)) {
      unique(locations$country)
    } else {
      NA
    }
    data.frame(
      nct_id = nct_id,
      title = title,
      sponsor = sponsor,
      phase = phase,
      country = countries,
      stringsAsFactors = FALSE
    )
  })
  
  all_trial_dfs[[page]] <- do.call(rbind, page_df)
  page_token <- result$nextPageToken
  if (is.null(page_token)) break
  Sys.sleep(0.5)
}

trial_df <- do.call(rbind, all_trial_dfs)
trial_df <- trial_df[!is.na(trial_df$country), ]

# prevalence data
prevalence_data <- data.frame(
  country = c(
    "United States", "China", "Japan", "Germany", "France",
    "Italy", "United Kingdom", "Brazil", "Canada", "Spain",
    "South Korea", "Australia", "Netherlands", "Sweden", "Belgium",
    "Switzerland", "Austria", "Denmark", "Finland", "Norway",
    "Israel", "Greece", "Portugal", "Poland", "Argentina",
    "Mexico", "India", "Turkey", "Iran", "Taiwan",
    "Singapore", "New Zealand", "Ireland", "Czech Republic", "Hungary",
    "Romania", "Bulgaria", "Croatia", "Slovakia", "Slovenia",
    "Estonia", "Latvia", "Lithuania", "Luxembourg", "Cyprus",
    "Malta", "Iceland", "Russia", "Saudi Arabia", "Malaysia"
  ),
  prevalence_thousands = c(
    6500, 9000, 4600, 1800, 1200,
    1300, 900, 1800, 650, 800,
    840, 450, 290, 160, 220,
    170, 130, 80, 90, 90,
    120, 220, 210, 480, 520,
    1100, 5300, 650, 580, 300,
    80, 75, 55, 160, 190,
    310, 150, 90, 80, 40,
    20, 25, 35, 6, 12,
    8, 4, 580, 180, 85
  ),
  stringsAsFactors = FALSE
)

country_summary <- trial_df %>%
  group_by(country) %>%
  summarise(trial_count = n_distinct(nct_id), .groups = "drop") %>%
  arrange(desc(trial_count)) %>%
  left_join(prevalence_data, by = "country") %>%
  mutate(
    prevalence_thousands = case_when(
      country == "Czechia" ~ 160,
      country == "Korea, Republic of" ~ 840,
      TRUE ~ replace_na(prevalence_thousands, 0)
    ),
    iso3 = countrycode(country, origin = "country.name", destination = "iso3c"),
    prevalence_score = (prevalence_thousands / max(prevalence_thousands)) * 100,
    competition_score = (1 - (trial_count / max(trial_count))) * 100,
    composite_score = (0.5 * prevalence_score) + (0.5 * competition_score)
  ) %>%
  arrange(desc(composite_score))

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "AD Trial Feasibility"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Trial Landscape", tabName = "trials", icon = icon("flask")),
      menuItem("Site Scoring", tabName = "scoring", icon = icon("ranking-star")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_trials"),
                valueBoxOutput("total_countries"),
                valueBoxOutput("top_country")
              ),
              fluidRow(
                box(
                  title = "Alzheimer's Trial Activity by Country",
                  width = 12,
                  plotlyOutput("world_map", height = "500px")
                )
              )
      ),
      
      tabItem(tabName = "trials",
              fluidRow(
                box(title = "Trials by Country", width = 6,
                    plotlyOutput("bar_chart", height = "400px")),
                box(title = "Trials by Phase", width = 6,
                    plotlyOutput("phase_chart", height = "400px"))
              ),
              fluidRow(
                box(title = "Trial Table", width = 12, DTOutput("trial_table"))
              )
      ),
      
      tabItem(tabName = "scoring",
              fluidRow(
                box(
                  title = "Adjust Scoring Weights", width = 4,
                  sliderInput("prevalence_weight", "Patient Prevalence Weight",
                              min = 0, max = 100, value = 50, step = 5),
                  sliderInput("competition_weight", "Low Competition Weight",
                              min = 0, max = 100, value = 50, step = 5),
                  helpText("Weights are normalised automatically.")
                ),
                box(title = "Top 20 Countries by Composite Score", width = 8,
                    plotlyOutput("score_chart", height = "400px"))
              ),
              fluidRow(
                box(title = "Full Country Ranking", width = 12,
                    DTOutput("score_table"))
              )
      ),
      
      tabItem(tabName = "about",
              box(
                title = "About This Dashboard", width = 12,
                p("This dashboard supports clinical trial feasibility analysis for 
            Alzheimer's Disease trials."),
                p("Data sources:"),
                tags$ul(
                  tags$li("Trial data: ClinicalTrials.gov API v2 (500 recruiting AD trials)"),
                  tags$li("Prevalence estimates: Global Burden of Disease 2019")
                ),
                p("Scoring methodology:"),
                tags$ul(
                  tags$li("Prevalence score: patient pool size normalised to 0-100"),
                  tags$li("Competition score: inverse of trial count normalised to 0-100"),
                  tags$li("Composite score: user-weighted combination of the two")
                ),
                p("Built by Julia Kampel, MSc Cancer Genetics and Data Science, 2026.")
              )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  scored_data <- reactive({
    total_weight <- input$prevalence_weight + input$competition_weight
    pw <- input$prevalence_weight / total_weight
    cw <- input$competition_weight / total_weight
    country_summary %>%
      mutate(composite_score = (pw * prevalence_score) + (cw * competition_score)) %>%
      arrange(desc(composite_score))
  })
  
  output$total_trials <- renderValueBox({
    valueBox(length(unique(trial_df$nct_id)), "Recruiting AD Trials",
             icon = icon("clipboard"), color = "blue")
  })
  
  output$total_countries <- renderValueBox({
    valueBox(length(unique(trial_df$country)), "Countries with Active Trials",
             icon = icon("globe"), color = "green")
  })
  
  output$top_country <- renderValueBox({
    top <- country_summary %>% arrange(desc(trial_count)) %>% slice(1)
    valueBox(top$country, paste("Most Active Country —", top$trial_count, "trials"),
             icon = icon("trophy"), color = "yellow")
  })
  
  output$world_map <- renderPlotly({
    plot_geo(country_summary) %>%
      add_trace(
        z = ~trial_count, color = ~trial_count, colors = "Blues",
        text = ~paste(country, "<br>Trials:", trial_count,
                      "<br>Prevalence:", prevalence_thousands, "k"),
        locations = ~iso3, type = "choropleth", hoverinfo = "text"
      ) %>%
      colorbar(title = "Trial Count") %>%
      layout(
        title = "Recruiting Alzheimer's Trials by Country",
        geo = list(showframe = FALSE, showcoastlines = TRUE)
      )
  })
  
  output$bar_chart <- renderPlotly({
    top20 <- country_summary %>% arrange(desc(trial_count)) %>% head(20)
    plot_ly(top20, x = ~reorder(country, trial_count), y = ~trial_count,
            type = "bar", marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Country"),
             yaxis = list(title = "Number of Trials"),
             title = "Top 20 Countries by Trial Count")
  })
  
  output$phase_chart <- renderPlotly({
    phase_summary <- trial_df %>%
      distinct(nct_id, phase) %>%
      count(phase) %>%
      arrange(desc(n))
    plot_ly(phase_summary, labels = ~phase, values = ~n,
            type = "pie", hole = 0.4) %>%
      layout(title = "Trial Distribution by Phase")
  })
  
  output$trial_table <- renderDT({
    trial_df %>%
      distinct(nct_id, title, sponsor, phase, country) %>%
      rename("NCT ID" = nct_id, "Title" = title, "Sponsor" = sponsor,
             "Phase" = phase, "Country" = country) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$score_chart <- renderPlotly({
    top20 <- scored_data() %>% head(20)
    plot_ly(top20, x = ~reorder(country, composite_score),
            y = ~composite_score, type = "bar",
            marker = list(color = "seagreen")) %>%
      layout(xaxis = list(title = "Country"),
             yaxis = list(title = "Composite Score"),
             title = "Top 20 Countries by Feasibility Score")
  })
  
  output$score_table <- renderDT({
    scored_data() %>%
      select(country, trial_count, prevalence_thousands,
             prevalence_score, competition_score, composite_score) %>%
      mutate(across(where(is.numeric), ~round(., 1))) %>%
      rename("Country" = country, "Active Trials" = trial_count,
             "Prevalence (thousands)" = prevalence_thousands,
             "Prevalence Score" = prevalence_score,
             "Competition Score" = competition_score,
             "Composite Score" = composite_score) %>%
      datatable(options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui, server)