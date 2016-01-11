library(shiny)
library(DT)

##### REMOVE TABLE
if(length(ls())>0)
save(responses,file="Backup.Rdata")
rm(list=ls())
##### REMOVE TABLE

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}


library(shiny)

# Define the fields we want to save from the form
fields <- c("vorname", "nachname", "statistiker","est1","est2","est3","est4")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    tags$div(id="table", class="shiny-input", 
             dataTableOutput( "results2" )
    ),
    textInput("vorname", "Vorname", ""),
    textInput("nachname", "Nachname", ""),
    checkboxInput("statistiker", "Ich kenne mich in Statistik aus", FALSE),
    sliderInput("est1", "Schätzung für die 1. Box", 0, 150, 1, ticks = FALSE),
    sliderInput("est2", "Schätzung für die 2. Box", 0, 150, 1, ticks = FALSE),
    sliderInput("est3", "Schätzung für die 3. Box", 0, 150, 1, ticks = FALSE),
    sliderInput("est4", "Schätzung für die 4. Box", 0, 150, 1, ticks = FALSE),
    actionButton("submit", "Submit"),actionButton("show", "Zeige"),actionButton("hideit", "Verstecke"),actionButton("evaluate", "Auswerten"),
    tags$div(id="table", class="shiny-input", 
             dataTableOutput( "results" )
    ),tags$hr()
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
      updateTextInput(session, "vorname", value = "Bitte Eintragen")
      updateTextInput(session, "nachname", value = "Bitte Eintragen")
      updateSliderInput(session, "est1", value = 0)
      updateSliderInput(session, "est2", value = 0)
      updateSliderInput(session, "est3", value = 0)
      updateSliderInput(session, "est4", value = 0)
    })
    
    observeEvent(input$show, {
      output$results <- renderDataTable({
          input$submit
          loadData()
    })
    })
    
############## ERROR WHEN HIDE IS PRESSED => MISSING VALUE IN IF!
      observeEvent(input$hideit, {
        output$results <- renderDataTable({
          input$submit
          loadData()
        },options=list(pageLength=0))
      })
################ ERROR WHEN HIDE IS PRESSED => MISSING VALUE IN IF!  
      
      observeEvent(input$evaluate, {
        ##calculate scores
        true_values<-c(20,50,100,34)
        num_part<-dim(responses)[1]
        scores<-numeric(num_part)
        for(i in 1:num_part)
        {
          scores[i]<-sum((as.numeric(responses[i,4:7])-true_values)^2)  ### as.numeric geht nicht => Eingabe ist faktor. Falsche Umwandlung!!!
        }
        which(scores==sort(scores,decreasing=FALSE)[1])->winner1
        which(scores==sort(scores,decreasing=FALSE)[2])->winner2
        which(scores==sort(scores,decreasing=FALSE)[3])->winner3
        output$results2 <- renderDataTable({
          cbind(responses[c(winner1,winner2,winner3),],scores[c(winner1,winner2,winner3)])
        })
      })
    
    
      
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    }) 

    
  }
)


