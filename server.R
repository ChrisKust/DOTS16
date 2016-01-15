shinyServer(
  
  function(input, output, session) {
    
    ##### REMOVE TABLE (used in local application)
#    if(length(ls())>0)
#      save(responses,file="Backup.Rdata")
#    rm(list=ls())
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
    
    fields <- c("vorname", "nachname", "statistiker","est1","est2","est3","est4")
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
      updateCheckboxInput(session,"statistiker",value=FALSE)
      updateTextInput(session, "vorname", value = "Bitte Eintragen")
      updateTextInput(session, "nachname", value = "Bitte Eintragen")
      updateSliderInput(session, "est1", value = 0)
      updateSliderInput(session, "est2", value = 0)
      updateSliderInput(session, "est3", value = 0)
      updateSliderInput(session, "est4", value = 0)
    })
    
    observeEvent(input$random, {
      updateCheckboxInput(session,"statistiker",value=rbinom(1,1,0.5))
      updateTextInput(session, "vorname", value = sample(c("A","B","C","D","E"),10,replace=T))
      updateTextInput(session, "nachname", value = sample(c("A","B","C","D","E"),10,replace=T))
      updateSliderInput(session, "est1", value = round(runif(1,min=0,max=150),digits=0))
      updateSliderInput(session, "est2", value = round(runif(1,min=0,max=150),digits=0))
      updateSliderInput(session, "est3", value = round(runif(1,min=0,max=150),digits=0))
      updateSliderInput(session, "est4", value = round(runif(1,min=0,max=150),digits=0))
      saveData(formData())
    })
    
    observeEvent(input$show, {
      output$results <- DT::renderDataTable({
        input$submit
        loadData()
      })
    })
    
    
    observeEvent(input$hideit, {
      output$results <- DT::renderDataTable({
        input$submit
        loadData()
      },options=list(pageLength=0))
    })
    
    
    observeEvent(input$evaluate, {
      ##calculate scores
      true_values<-c(20,50,100,34)
      num_part<-dim(responses)[1]
      scores<-numeric(num_part)
      for(i in 1:num_part)
      {
        e1<-responses[i,4:7]$est1
        e1<-as.numeric(levels(e1))[e1]
        e2<-responses[i,4:7]$est2
        e2<-as.numeric(levels(e2))[e2]
        e3<-responses[i,4:7]$est3
        e3<-as.numeric(levels(e3))[e3]
        e4<-responses[i,4:7]$est4
        e4<-as.numeric(levels(e4))[e4]
        dat<-c(e1,e2,e3,e4)
        scores[i]<-sum((dat-true_values)^2)  
      }
      which(scores==sort(scores,decreasing=FALSE)[1])->winner1
      which(scores==sort(scores,decreasing=FALSE)[2])->winner2
      which(scores==sort(scores,decreasing=FALSE)[3])->winner3
      winners<-c(as.vector(winner1),as.vector(winner2),as.vector(winner3))
      output$results2 <- DT::renderDataTable({
        cbind(responses[winners,],scores[winners])
      })
      t1<-responses[,4:7]$est1
      t1<-as.numeric(levels(t1))[t1]
      t2<-responses[,4:7]$est2
      t2<-as.numeric(levels(t2))[t2]
      t3<-responses[,4:7]$est3
      t3<-as.numeric(levels(t3))[t3]
      t4<-responses[,4:7]$est3
      t4<-as.numeric(levels(t4))[t4]
      output$boxPlot <- renderPlot({
        boxplot(t1,t2,t3,t4,col=c(1,2,3,4))
        abline(h=true_values,lty=2,col=c(1,2,3,4))
      })
      m<-matrix(c(mean(t1),mean(t2),mean(t3),mean(t4),true_values),ncol=4,byrow=TRUE)
      rownames(m)<-c("Mittlere Schätzug","Wahrer Wert")
      colnames(m)<-c("Box1","Box2","Box3","Box4")
      output$summary <- DT::renderDataTable({
        m
      })
      ### Here some further evaluations (boxplots, comp with true values and winners, mean estimate)
      ### would be nice. these results could be (partially) presented in the UI or used to generate a final 
      ### presentation!
      
      ### TO DO: KNITR output for a final presentation!
    })
    
    
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    }) 
    
    
  }
)