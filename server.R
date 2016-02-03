shinyServer(
  
  function(input, output, session) {
    
    ##### REMOVE TABLE (used in local application)
#    if(length(ls())>0)
#      save(responses,file="Backup.Rdata")
#    rm(list=ls())
    ##### REMOVE TABLE
    
  #  saveData <- function(data) {
  #    data <- as.data.frame(t(data))
  #    if (exists("responses")) {
  #      responses <<- rbind(responses, data)
  #    } else {
  #      responses <<- data
  #    }
  #  }
    
  #  loadData <- function() {
  #    if (exists("responses")) {
  #      responses
  #    }
  #  }
    

    outputDir <- "responses"
    dir.create("responses")
    
    saveData <- function(data,surname) {
      data <- t(data)
      # Create a unique file name
      fileName <- sprintf("%s_%s_%s.csv", surname,as.integer(Sys.time()), digest::digest(data))
      # Write the file to the local system
      write.csv(
        x = data,
        file = file.path(outputDir, fileName), 
        row.names = FALSE, quote = TRUE
      )
    }
    
    loadData <- function() {
      # Read all the files into a list
      files <- list.files(outputDir, full.names = TRUE)
      data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
      # Concatenate all data together into one data.frame
      data <- do.call(rbind, data)
      data
    }
    
    fields <- c("vorname", "nachname", "statistiker","est1","est2","est3","est4","est5","est6")
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData(),input$nachname)
      updateCheckboxInput(session,"statistiker",value=FALSE)
      updateTextInput(session, "vorname", value = "Bitte Eintragen")
      updateTextInput(session, "nachname", value = "Bitte Eintragen")
      updateSliderInput(session, "est1", value = 0)
      updateSliderInput(session, "est2", value = 0)
      updateSliderInput(session, "est3", value = 0)
      updateSliderInput(session, "est4", value = 0)
      updateSliderInput(session, "est5", value = 0)
      updateSliderInput(session, "est6", value = 0)
    })
    
    observeEvent(input$random, {
      true_values<-c(118,69,66,38,70,70)
      updateCheckboxInput(session,"statistiker",value=rbinom(1,1,0.5))
      updateTextInput(session, "vorname", value = sample(c("A","B","C","D","E"),10,replace=T))
      updateTextInput(session, "nachname", value = sample(c("A","B","C","D","E"),10,replace=T))
      updateSliderInput(session, "est1", value = true_values[1] + round(runif(1,min=-0.2*true_values[1],max=0.2*true_values[1]),digits=0))
      updateSliderInput(session, "est2", value = true_values[2] + round(runif(1,min=-0.2*true_values[2],max=0.2*true_values[2]),digits=0))
      updateSliderInput(session, "est3", value = true_values[3] + round(runif(1,min=-0.2*true_values[3],max=0.2*true_values[3]),digits=0))
      updateSliderInput(session, "est4", value = true_values[4] + round(runif(1,min=-0.2*true_values[4],max=0.2*true_values[4]),digits=0))
      updateSliderInput(session, "est5", value = true_values[5] + round(runif(1,min=-0.2*true_values[5],max=0.2*true_values[5]),digits=0))
      updateSliderInput(session, "est6", value = true_values[6] + round(runif(1,min=-0.2*true_values[6],max=0.2*true_values[6]),digits=0))
      surname<-input$nachname
      saveData(formData(),surname)
    })
    
    observeEvent(input$show, {
      if(input$psw=="setosa")
      {
      output$results <- DT::renderDataTable({
        input$submit
        loadData()
      })
      }
      else{}
      updateTextInput(session,"psw",value="")
    })
    
    
    observeEvent(input$hideit, {
      output$results <- DT::renderDataTable({
        input$submit
        loadData()
      },options=list(pageLength=0))
    })
    
    observeEvent(input$clear, {
      if(input$psw=="setosa")
      {
      unlink("responses",recursive=T)
      dir.create("responses")
      }
      else{}
      updateTextInput(session,"psw",value="")
    })
    
    
    observeEvent(input$evaluate, {
      if(input$psw=="setosa")
      {
       ##calculate scores
      responses<-loadData()
      true_values<-c(118,69,66,38,70,70)
      num_part<-dim(responses)[1]
      scores<-numeric(num_part)
      for(i in 1:num_part)
      {
        e1<-responses$est1[i]
       # e1<-as.numeric(levels(e1))[e1]
        e2<-responses$est2[i]
      #  e2<-as.numeric(levels(e2))[e2]
        e3<-responses$est3[i]
      #  e3<-as.numeric(levels(e3))[e3]
        e4<-responses$est4[i]
    #    e4<-as.numeric(levels(e4))[e4]
        e5<-responses$est5[i]
        e6<-responses$est6[i]
        dat<-c(e1,e2,e3,e4,e5,e6)
        scores[i]<-sum((dat-true_values)^2)
      }
      which(scores==sort(scores,decreasing=FALSE)[1])->winner1
      which(scores==sort(scores,decreasing=FALSE)[2])->winner2
      which(scores==sort(scores,decreasing=FALSE)[3])->winner3
      winners<-c(as.vector(winner1),as.vector(winner2),as.vector(winner3))
      output$results2 <- DT::renderDataTable({
        cbind(responses[winners,],scores[winners])
      })
      t1<-responses$est1
     # t1<-as.numeric(levels(t1))[t1]
      t2<-responses$est2
     # t2<-as.numeric(levels(t2))[t2]
      t3<-responses$est3
    #  t3<-as.numeric(levels(t3))[t3]
      t4<-responses$est4
   #   t4<-as.numeric(levels(t4))[t4]
      t5<-responses$est5
      t6<-responses$est6
      output$boxPlot <- renderPlot({
        boxplot(t1,t2,t3,t4,t5,t6,col=c(1,2,3,4,5,6))
        abline(h=true_values,lty=2,col=c(1,2,3,4,5,6))
      })
      m<-matrix(c(mean(t1),mean(t2),mean(t3),mean(t4),mean(t5),mean(t6),true_values),ncol=6,byrow=TRUE)
      rownames(m)<-c("Mittlere Schätzug","Wahrer Wert")
      colnames(m)<-c("Box1","Box2","Box3","Box4","Box5","Box6")
      output$summary <- DT::renderDataTable({
        m
      })
      ### Here some further evaluations (boxplots, comp with true values and winners, mean estimate)
      ### would be nice. these results could be (partially) presented in the UI or used to generate a final 
      ### presentation!
      }
      else{}
      ### TO DO: KNITR output for a final presentation!
      updateTextInput(session,"psw",value="")
    })
    
    
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    }) 
    
    
  }
)