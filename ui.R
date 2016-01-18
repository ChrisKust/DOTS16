shinyUI(
  fluidPage(
    fluidRow(
      column(6,
            textInput("vorname", "Vorname", ""),
             textInput("nachname", "Nachname", ""),
             checkboxInput("statistiker", "Ich kenne mich in Statistik aus", FALSE),
             sliderInput("est1", "Schätzung für die 1. Box", 0, 150, 1, ticks = FALSE),
             sliderInput("est2", "Schätzung für die 2. Box", 0, 150, 1, ticks = FALSE),
             sliderInput("est3", "Schätzung für die 3. Box", 0, 150, 1, ticks = FALSE),
             sliderInput("est4", "Schätzung für die 4. Box", 0, 150, 1, ticks = FALSE),
             actionButton("submit", "Submit"),actionButton("show", "Zeige"),actionButton("hideit", "Verstecke"),actionButton("evaluate", "Auswerten"),actionButton("random", "Zufallsschätzung")),
      column(6,plotOutput("boxPlot"),DT::dataTableOutput( "summary" ),tags$hr())),
    fluidRow(tags$div(id="table", class="shiny-input", 
                      DT::dataTableOutput( "results2" )
    ),  tags$div(id="table", class="shiny-input",
                 DT::dataTableOutput( "results" )
    ),actionButton("clear","Lösche Alles"),passwordInput("psw","Password","")  ) 
  )
  
  )