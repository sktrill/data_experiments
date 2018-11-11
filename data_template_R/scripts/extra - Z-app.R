library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
library(shiny, warn.conflicts=FALSE, quietly=TRUE)
library(survival, warn.conflicts=FALSE, quietly=TRUE)
library(survminer, warn.conflicts=FALSE, quietly=TRUE)
library(rms, warn.conflicts=FALSE, quietly=TRUE)
library(DT, warn.conflicts=FALSE, quietly=TRUE)
library(cluster, warn.conflicts=FALSE, quietly=TRUE)
library(factoextra, warn.conflicts=FALSE, quietly=TRUE)
library(purrr, warn.conflicts=FALSE, quietly=TRUE)
library(shinythemes, warn.conflicts=FALSE, quietly=TRUE)

# load key datasets
players <- tbl_df(read.csv("data/players_backup.csv", stringsAsFactors=FALSE))
cluster_data <- tbl_df(read.csv("data/cluster_backup.csv", stringsAsFactors=FALSE))
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# define ui
ui <-  fluidPage(theme = shinytheme("journal"),
      titlePanel(title="", windowTitle="Chasing GOATs"),
      navbarPage(img(src = "cap.webp", height = 30, width = 30),
            tabPanel("Introduction",
                     sidebarLayout(
                       sidebarPanel(
                         h1(strong("Because the Test is Greater Still"), align = "center"),
                         br(),
                         p(em("a data driven exploration of Test cricket"), align = "center"),
                         br(),
                         div(HTML('<center><img src="cricket.jpg", style = "height: 100%; width: 100%; object-fit: contain"></center>')), 
                         br()
                       ),
                       mainPanel(
                         includeHTML("notes/problem.html")
                       )
                     )
            ), 
            tabPanel("Methodology",
                     sidebarLayout(
                       sidebarPanel(
                         h4(strong("In Summary")),
                         includeHTML("notes/method_summary.html"), 
                         br()
                       ),
                       mainPanel(
                         includeHTML("notes/methodology.html")
                       )
                     )
            ),
            tabPanel("Data",
                     # create a new Row in the UI for selectInputs
                     fluidRow(
                       column(4,
                              selectInput("plyr",
                                          "Player:",
                                          c("All",
                                            unique(as.character(players$player))))
                       ),
                       column(4,
                              selectInput("tm",
                                          "Team:",
                                          c("All",
                                            unique(as.character(players$team))))
                       ),
                       column(4,
                              selectInput("coh",
                                          "Cohort:",
                                          c("All",
                                            unique(as.character(players$cohort))))
                       )
                     ),
                     # create a new row for the table
                     fluidRow(
                       DT::dataTableOutput("table")
                     )
            ),
            tabPanel("Play",
                      sidebarLayout(
                        # create panel to pick plot options
                        sidebarPanel(
                          radioButtons("radio", label = "Pick to compare",
                                       choices = list("Players" = 1, "Teams" = 2, "Cohorts" = 3),
                                       selected = 1),
                          
                          uiOutput("ui"), 
                          
                          # checkboxes for two factors; home/away and top team
                          checkboxInput("top", "Compare against top teams", FALSE),
                          checkboxInput("where", "Compare home vs away", FALSE), 
                          checkboxInput("hazard", "Show cum. hazard plot", FALSE), 
                          
                          # action button to update graphs
                          actionButton(inputId = "update", label = "Plot")
                        ),
                        # plot panel
                        mainPanel(
                          plotOutput("plotSurvival")
                        )
                      )
             ),
            tabPanel("Discussion",
                     # pick cohort and cluster centers
                     sidebarPanel(
                       selectInput("cohort", 'Pick one or more cohort', unique(cluster_data$cohort), multiple=TRUE, selectize=TRUE),
                       numericInput('clusters', 'Cluster count', 5, min = 1, max = 9)
                     ),
                     # plot cluster
                     mainPanel(
                       plotOutput('plotCluster'), 
                       includeHTML("notes/discussion.html")
                     )
            ),
            tabPanel("Conclusion",
                     includeHTML("notes/conclusion.html")
                     
            )

  )
)

# define server logic
server <- function(input, output) {

  # update select list based on radio button pick of player / team / cohort
  output$ui <- renderUI({
    switch(input$radio,
           "1" = selectInput("selected", 'Pick one or more', unique(players$player), multiple=TRUE, selectize=TRUE),
           "2" = selectInput("selected", 'Pick one or more', unique(players$team), multiple=TRUE, selectize=TRUE),
           "3" = selectInput("selected", 'Pick one or more', unique(players$cohort), multiple=TRUE, selectize=TRUE)
    )
  })
  
  # filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    show_data <- subset(players, select = c(runs, balls.faced, player, team, opposition, country, year, dismissal, event, home, top.team, cohort, avg, rmedian))
    if (input$plyr != "All") {
      show_data <- show_data[show_data$player == input$plyr,]
    }
    if (input$tm != "All") {
      show_data <- show_data[show_data$team == input$tm,]
    }
    if (input$coh != "All") {
      show_data <- show_data[show_data$cohort == input$coh,]
    }
    show_data}, options = list(pageLength = 25))
  )

  # filter players by pick of player / team / cohort
  dataPlayers <- eventReactive(input$update, {
    if (!is.null(input$selected)){
      if (input$radio == "1") 
        players %>% filter(player %in% input$selected)
      else if (input$radio == "2") 
        players %>% filter(team %in% input$selected)
      else 
        players %>% filter(cohort %in% input$selected)
    }
    else 
      players
  })
  
  # create group for survial curve analysis based on pick of player / team / cohort
  dataGroup <- eventReactive(input$update, {
    if (input$radio == "1")
      dataPlayers()$player
    else if (input$radio == "2") 
      dataPlayers()$team
    else 
      dataPlayers()$cohort
  })
  
  # update survival table reactively (maybe isolate, check lag)
  dataSC <- eventReactive(input$update, {
    Surv(dataPlayers()$balls.faced, dataPlayers()$event)
  })
  
  group <- eventReactive(input$update, {
    unlist(dataGroup())
  })
  
  # create Kaplan-Meier survival curve fit based on whether variables home / top team were selected
  dataFit <- eventReactive(input$update, {
    if (!is.null(input$selected)){
      if (input$top & input$where)
        survfit(dataSC() ~ dataPlayers()$home + group() + dataPlayers()$top.team, type = "kaplan-meier")
      else if (input$top & !input$where)
        survfit(dataSC() ~ dataPlayers()$top.team + group(), type = "kaplan-meier")
      else if (!input$top & input$where)
        survfit(dataSC() ~ dataPlayers()$home + group(), type = "kaplan-meier")
      else
        survfit(dataSC() ~ group(), type = "kaplan-meier")
    }
    else
      survfit(dataSC() ~ 1, type = "kaplan-meier")
  })
  
  graphSC <- eventReactive(input$update, {
    if (!input$hazard)
       g <- ggsurvplot(dataFit(),
                      data = dataPlayers(),
                      # pval = TRUE,
                      # conf.int = TRUE,
                      surv.median.line = "hv",
                      linetype = "strata",
                      ggtheme = theme_bw(),
                      tables.theme = theme_bw(),
                      # facet.by = 2,
                      # risk.table = 'percentage',
                      # risk.table.col = "strata",
                      # risk.table.height = 0.5,
                      # tables.theme = theme_bw(),
                      # palette = rainbow(N),
                      # conf.int.fill = rainbow(N),
                      # legend.labs = gsub("x=","",names(dataFit()$strata)),
                      censor = FALSE,
                      legend = "bottom",
                      legend.horiz = FALSE,
                      legend.ncol = 2,
                      break.time.by = 50,
                      xlab = "balls faced",
                      ylab = "probability of survival"
      )
    else 
      g <- ggsurvplot(dataFit(),
                      data = dataPlayers(),
                      # pval = TRUE,
                      # conf.int = TRUE,
                      fun = 'cumhaz',
                      linetype = "strata",
                      ggtheme = theme_bw(),
                      tables.theme = theme_bw(),
                      # facet.by = 2,
                      # risk.table = 'percentage',
                      # risk.table.col = "strata",
                      # risk.table.height = 0.5, 
                      # tables.theme = theme_bw(),
                      # palette = rainbow(N),
                      # conf.int.fill = rainbow(N),
                      # legend.labs = gsub("x=","",names(dataFit()$strata)),
                      censor = FALSE,
                      legend = "bottom",
                      legend.horiz = FALSE,
                      legend.ncol = 2,
                      break.time.by = 50,
                      xlab = "balls faced",
                      ylab = "cumulative hazard"
      )
      
     
     # if no player is selected
     if (!is.null(input$selected)){
       # if 2 or more players are selected facet the graph according to home / top team    
       if (length(input$selected) >= 2) {
         if (input$top & input$where)
           g + facet_grid(top.team ~ home, labeller = label_both)
         else if (input$top & !input$where)
           g + facet_grid( ~ top.team, labeller = label_both)
         else if (!input$top & input$where)
           g + facet_grid( ~ home, labeller = label_both)
         else 
           g
       }
       else {
         # if only one player is being looked at
         if (input$top & input$where)
           g + facet_grid( ~ home, labeller = label_both)
         else
           g
       }
     }
  })
  
  # output survival curve plots, facetted according to home / top team selection
  output$plotSurvival <-renderPlot({
    graphSC()
  })
  
  # update cluster data based on cohort picks
  selectedData <- reactive({
    if (!is.null(input$cohort)) 
      data <- cluster_data %>% filter(cohort %in% input$cohort)
    else
      data <- cluster_data
    tmp <- data$player
    data <- subset(data, select = -c(player,cohort))
    row.names(data) <- tmp
    data
  })
  
  # perform k-means cluster analysis
  clusters <- reactive({
    kmeans(selectedData(), centers = input$clusters, nstart = 25)
  })
  
  # plot cluster diagram
  output$plotCluster <- renderPlot({
    fviz_cluster(clusters(), data = selectedData())

    # par(mar = c(5.1, 4.1, 0, 1))
    # plot(selectedData(),
    #      col = clusters()$cluster,
    #      pch = 20, cex = 3)
    # points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}

shinyApp(ui = ui, server = server)