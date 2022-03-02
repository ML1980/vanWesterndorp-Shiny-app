
library(shiny)
library(tidyverse)
library(plotly)
library(svglite)

library(pricesensitivitymeter) 
library(readxl) # To read excell files
library(ggplot2) # To create plots
library(DT) #filter data
library(dplyr)

source("psm_plot.R")

####################################################################################################

ui <- navbarPage("Make vanWesterndorp plots",
    tabPanel("Analyze",    
    sidebarLayout(
        sidebarPanel(width = 3,
                     h2("Settings"),
                     br(),
        p(strong("Insert dataset:")),
        fileInput("upload", NULL, accept = c(".csv", ".tsv")),
        radioButtons(inputId = 'sep', label = 'Separator', 
                     choices = c(Comma=',' ,Semicolon=';',Tab='\t', Space=''),
                     selected = ';'),
        textInput("hlavicka", "Heading above the plot:", value = "Total"),
        textInput("currency", "Currency (only for label):", value = "CZK"),
        numericInput("xmeritko", "Maximum of x axis:", value = 50, min = 1, step = 0.5),
        numericInput("pmcposun", "Shift of PMC label:", value = 0, min = 0, step = 0.1),
        numericInput("pmeposun", "Shift of PME label:", value = 0, min = 0, step = 0.1),
        actionButton("run_button", "Run VanW", class = "btn-success")
        ),
    mainPanel(width = 9,
        DTOutput("head"), #was tableOutput
        p(h2("Summary:")),
        textOutput("output_summary_interval"),
        textOutput("output_summary_idp"),
        textOutput("output_summary_opp"),
        p(h2("Plot:")),
        plotOutput("psmplot", width = "800px"),
        downloadButton("downloadPlot")
        ),
        ),
    ),
    tabPanel("Help",
             p("Dataset must be in csv format. You can choose separator."),
             br(),
             p("Column names must be named:"),
             p("toocheap"),
             p("cheap"),
             p("expensive"),
             p("tooexpensive"),
             p("You can set the appearance of the chart in the left panel."),
             p("When you have everything set up and the data loaded, press 'Run VanW' button to calculate the analysis."),
             p("You can use filtr at the top of the data and then press 'Run VanW' button, the plot will be recalculated according this filter.")
             
    )
)

####################################################################################################

server <- function(input, output, session) {
    #upload a validace datoveho souboru
    mydata <- reactive({
        req(input$upload)
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = input$sep),
               tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
    })
    

    filtered_frame <-  reactive({
        frame <- req(mydata())
        indexes <- req(input$head_rows_all)
        
        frame[indexes,]
    })


    #ukázka nactenych dat - původní
    #output$head <- renderTable({
    #    head(mydata(), input$n)
    #})
    
  
    output$head <- renderDT(mydata(),
                            filter = "top",
                            options = list(
                                pageLength = 5
                            ),  
                            escape = FALSE
    )
    
    # analyza ----------------------------------------------------------
    summary_output <- eventReactive(input$run_button,{
        psm_analysis(toocheap = filtered_frame()$toocheap, 
                    cheap =  filtered_frame()$cheap, 
                    expensive =  filtered_frame()$expensive,
                    tooexpensive =  filtered_frame()$tooexpensive,
                    interpolate =TRUE,
                    validate = FALSE
                    )
    })
   
    interval  <- eventReactive(input$run_button,{
        paste ("Accepted Price Range:", summary_output()$pricerange_lower, "-", summary_output()$pricerange_upper)
    })
    
    idp  <- eventReactive(input$run_button,{
        paste ("Indifference Price Point:", summary_output()$idp)
    })
    
    opp  <- eventReactive(input$run_button,{
        paste ("Optimal Price Point:", summary_output()$opp)
    })
    
    output$output_summary_interval <- renderText({
        (interval())
    })
    
    output$output_summary_idp <- renderText({
        (idp())
    })
    
    output$output_summary_opp <- renderText({
        (opp())
    })
    
    output$psmplot <- renderPlot(
        psm_plot(summary_output(), input$hlavicka, input$xmeritko, input$pmcposun, input$pmeposun, input$currency)
        )
    
    plotInput = function() {
        
        ##################################################
        ##################################################
        myplot <- psm_plot(summary_output(), input$hlavicka, input$xmeritko, input$pmcposun, input$pmeposun, input$currency)
        ##################################################
        ##################################################
        
    }
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste("vanW_plot_", input$hlavicka, '.svg', sep='') },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "svg", width=22.3, height=13.2, units="cm")
        }
    )

}


####################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
