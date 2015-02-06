shinyServer(function(input, output) {
  
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    
    selectInput("arg","Argument:",ArgNames())
  })
  
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
    }
  })
  
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    return(Dataset)
  })
  
  # Select the y_variables:
  output$varselect_1 <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Variable selection: 
    checkboxGroupInput('y_vars',"select the y variables:",
      as.list(setNames(names(Dataset()),names(Dataset()))))   
         
  })
  # Select the x_variables:
  output$varselect_2 <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Variable selection: 
    checkboxGroupInput('x_vars',"select the x variables:",
      as.list(setNames(names(Dataset()),names(Dataset()))))   
         
  })

  # Show table:
  output$table <- renderTable({
    
    if (is.null(Dataset())) return(NULL)
    if (is.null(input$x_vars)) return(NULL)
    if (is.null(input$y_vars)) return(NULL)
    if (input$inter == TRUE) {
            fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),sep=''))
               ,data = Dataset())     
    }
    if (input$inter == FALSE) {
      fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),'-1',sep=''))
                    ,data = Dataset())

    }

           summary(fit)  
    
  })
  #Show graph:
  output$lplot = renderPlot({
    if (is.null(Dataset())) return(NULL)
    if (is.null(input$x_vars)) return(NULL)
    if (is.null(input$y_vars)) return(NULL)

    da = Dataset()
    y_vars = input$y_vars
    x_vars = input$x_vars
    plot.new()
    par(col="black")
    par(mfrow=c(length(y_vars),length(x_vars)))
    for(j in 1:length(y_vars)) {
      for(i in 1:length(x_vars)) {
        graphics::plot(as.matrix(da[x_vars[i]])[,1],
                       as.matrix(da[y_vars[j]])[,1], xlab = x_vars[i], ylab=y_vars[j])
      }
    }    
    })


  # Show qq-plot:
  output$qq <- renderPlot({
    
    if (is.null(Dataset())) return(NULL)
    if (is.null(input$x_vars)) return(NULL)
    if (is.null(input$y_vars)) return(NULL)
    if (input$inter == TRUE) {
            fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),sep=''))
               ,data = Dataset())     
    }
    if (input$inter == FALSE) {
      fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),'-1',sep=''))
                    ,data = Dataset())

    }
           plot(fit,which=2 , sub = '')  
    
  } ,height = 400, width = 400) 

    # Show anova:
  output$anova <- renderTable({
    
    if (is.null(Dataset())) return(NULL)
    if (is.null(input$x_vars)) return(NULL)
    if (is.null(input$y_vars)) return(NULL)

    if (input$inter == TRUE) {
            fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),sep=''))
               ,data = Dataset())     
    }
    if (input$inter == FALSE) {
      fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),'-1',sep=''))
                    ,data = Dataset())

    }
           anova(fit)  
    
  })


    # Show residual:
  output$res <- renderPlot({
    
    if (is.null(Dataset())) return(NULL)
    if (is.null(input$x_vars)) return(NULL)
    if (is.null(input$y_vars)) return(NULL)

    if (input$inter == TRUE) {
            fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),sep=''))
               ,data = Dataset())     
    }
    if (input$inter == FALSE) {
      fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),'-1',sep=''))
                    ,data = Dataset())

    }
           plot(residuals(fit))
    
  },height = 400, width = 600)

output$R = renderText({
    if (input$inter == TRUE) {
            fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),sep=''))
               ,data = Dataset())     
    }
    if (input$inter == FALSE) {
      fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),'-1',sep=''))
                    ,data = Dataset())

    }
    paste('the R-squared for the regression is ', summary(fit)$r.squared)  
  })
output$R_2 = renderText({
    if (input$inter == TRUE) {
            fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),sep=''))
               ,data = Dataset())     
    }
    if (input$inter == FALSE) {
      fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),'-1',sep=''))
                    ,data = Dataset())

    }
    paste('the adjusted R-squared for the regression is ', summary(fit)$adj.r.squared)  
  })
###################################################################################
# text part ##############################
###################################################################################
  output$a <- renderUI({
    if(is.null(input$x_vars)) return(NULL)
    h4('Part I: basic data analysis: ')
    })   
  output$b <- renderUI({
    if(is.null(input$x_vars)) return(NULL)
    h4('Part II: Linear regression: ')
    })   
  output$c <- renderUI({
    if(is.null(input$x_vars)) return(NULL)
    h4('Part III: Residuals analysis: ')
    })   

  # the head of the plots.. 
  output$text_plot <- renderUI({
    if(is.null(input$x_vars)) return(NULL)
    h5('The plot of selected y-variable and x variables ')
    })   

  # the head of the table.. 
  output$text_fit <- renderUI({
    if(is.null(input$x_vars)) return(NULL)
    h5('Linear regression result ')
    })

  # the error  test.. 
  output$text_error <- renderUI({
    if(is.null(input$x_vars)) return(NULL)
    h5('Analysis of the regression residuals ')
    })

  output$SW = renderText({
    if (input$inter == TRUE) {
            fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),sep=''))
               ,data = Dataset())     
    }
    if (input$inter == FALSE) {
      fit  = lm( as.formula(paste(paste(input$y_vars,collapse='+'),'~' ,paste(input$x_vars,collapse='+'),'-1',sep=''))
                    ,data = Dataset())

    }
    final = shapiro.test(residuals(fit))
    text = paste('Shapiro-Wilk normality test for the residual: ', 'W = ', final$statistic, ',' ,'p_value = ', final$p.value, sep = '')

    text 
  })

#####################################################
   # for server: 
 output$tabs = renderUI({
    if (is.null(Dataset())) return(NULL)
    if (is.null(input$x_vars)) return(NULL)
    if (is.null(input$y_vars)) return(NULL)

    tabsetPanel(
        
        tabPanel('residuals plot',plotOutput('res')), 
        tabPanel('QQ-plot of residuals', plotOutput('qq')),
        tabPanel('normality test', textOutput('SW'))
      )
  })

  output$part1 = renderUI({
    if (is.null(Dataset())) return(NULL)
    if (is.null(input$x_vars)) return(NULL)
    if (is.null(input$y_vars)) return(NULL)

    tabsetPanel(
        tabPanel('coefficients', tableOutput('table')),
        tabPanel("anova table", tableOutput('anova')),
        tabPanel('R-squared', textOutput('R')),
        tabPanel('adjusted R-sqr', textOutput('R_2'))
      )
  })
  
  
})
 
 
