 
shinyUI(pageWithSidebar(
  
  # Header:
  headerPanel("Linear data analysis"),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$style(type='text/css', ".well { max-width: 20em; }"),
    # Tags:
    tags$head(
      tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
      tags$style(type="text/css", "select { width: 100%}"),
      tags$style(type="text/css", "input { width: 19em; max-width:100%}")
    ),
    
    # Select filetype:
    selectInput("readFunction", "Function to read data:", c(
      # Base R:
      "read.table",
      "read.csv",
      "read.csv2",
      "read.delim",
      "read.delim2",
 
      # foreign functions:
      "read.spss",
      "read.arff",
      "read.dbf",
      "read.dta",
      "read.epiiinfo",
      "read.mtp",
      "read.octave",
      "read.ssd",
      "read.systat",
      "read.xport",
      
      # Advanced functions:
      "scan",
      "readLines"
      )),
    
    # Argument selecter:
    htmlOutput("ArgSelect"),
    
    # Argument field:
    htmlOutput("ArgText"),
    
    # Upload data:
    fileInput("file", "Upload data-file:"),
    
    # Variable selection:
    htmlOutput("varselect_1"),
    htmlOutput("varselect_2"),
    
    br(),
    checkboxInput("inter", label = "intercept", value = TRUE),
    textInput("name","Dataset name:","Data")
  
  ),
  
  # Main:
  mainPanel(
    htmlOutput('a'),
    HTML('<hr style="color: purple;">'),
    htmlOutput('text_plot'),
    plotOutput('lplot'),
    br() ,
    br() ,
    br() ,
    htmlOutput('b'),
    HTML('<hr style="color: purple;">'),
    htmlOutput('text_fit') , 
    uiOutput('part1') ,
    br(),
    br(),
    br(),
    htmlOutput('c'),
    #draw a line using such statement. 
    HTML('<hr style="color: purple;">'),
    htmlOutput('text_error'),
    uiOutput('tabs') ,
    br() , 
    br() 
    
    #plotOutput('qq')

  )
))
