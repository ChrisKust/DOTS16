shinyUI(
  fluidPage(
    fluidRow(
      column(6,
            textInput("vorname", "Vorname", ""),
             textInput("nachname", "Nachname", ""),
             checkboxInput("statistiker", "Ich kenne mich in Statistik aus", FALSE),
             sliderInput("est1", "Schätzung für die Box A", 0, 150, 1, ticks = FALSE),
             sliderInput("est2", "Schätzung für die Box B", 0, 150, 1, ticks = FALSE),
             sliderInput("est3", "Schätzung für die Box C", 0, 150, 1, ticks = FALSE),
             #sliderInput("est4", "Schätzung für die 4. Box", 0, 150, 1, ticks = FALSE),
            #sliderInput("est5", "Schätzung für die 5. Box", 0, 150, 1, ticks = FALSE),
            #sliderInput("est6", "Schätzung für die 6. Box", 0, 150, 1, ticks = FALSE),
             actionButton("submit", "Senden")),
      column(6,plotOutput("boxPlot"),DT::dataTableOutput( "summary" ),tags$hr())),
    fluidRow(tags$div(id="table", class="shiny-input", 
                      DT::dataTableOutput( "results2" )
    ),  tags$div(id="table", class="shiny-input",
                 DT::dataTableOutput( "results" )
    ),actionButton("clear","Lösche Alles"),actionButton("show", "Zeige"),actionButton("hideit", "Verstecke"),actionButton("evaluate", "Auswerten"),actionButton("evaluatetot", "Auswerten Total"),passwordInput("psw","Password","")  ) 
  )
  
  )