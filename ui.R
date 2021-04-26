library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    includeCSS("www/style.css"),
    
    # Tabs
    navbarPage(
        "SpeechExplorer",
        tabPanel(
           "Trends",
           
           # Input Row
           fluidRow(
               inputPanel(
                   uiOutput('corpusSelectInput_Trends'),
                   uiOutput("dictionarySelectInput_Trends"),
                   uiOutput("dictionaryVarSelectInput_Trends"),
                   uiOutput("dateRangeSelectInput_Trends")
               )
           ),
           hr(style = "border-top: 1px solid #ededed; margin: 0 0 5px;"),
           
           # Plots
            plotOutput(
                "trendPlot",
                brush = brushOpts(
                    id = "trendPlot_brush",
                    direction = "x"
                ),
                width = "100%"
            ),
            plotOutput(
                "focusPlot",
                click = "focusPlot_click",
                width = "100%"
            ),
        ),
        navbarMenu(
            "Compare",
            tabPanel(
                "Units",
                fluidRow(
                    inputPanel(
                        uiOutput('corpusSelectInput_Compare_Unit'),
                        uiOutput("dictionarySelectInput_Compare_Unit"),
                        uiOutput("dictionaryVarSelectInput_Compare_Unit"),
                        uiOutput("unitSelectInput_Compare_Unit")
                    )
                ),
                hr(style = "border-top: 1px solid #ededed; margin: 0 0 5px;"),
                plotOutput("unitCompare", width="100%"),
                plotOutput("unitCompareTrend", width="100%")
            )
        ),
        tabPanel(
            "Speech Search",
            # Input Row
            fluidRow(
                inputPanel(
                    uiOutput('corpusSelectInput_Search'),
                    uiOutput("dictionarySelectInput_Search"),
                    uiOutput("dictionaryVarSelectInput_Search"),
                    uiOutput("demographicVarSelectInput_Search"),
                    uiOutput("dateRangeSelectInput_Search"),
                    numericInput("wordCountThreshold_Search", "Word Count Threshold", value=1000, min=1)
                )
            ),
            hr(style = "border-top: 1px solid #ededed; margin: 0 0 5px;"),
            DT::dataTableOutput("speechSearchTable")
        ),
        tabPanel(
            "Jobs",
            inputPanel(
                uiOutput('corpusSelectInput_DictUpload'),
                uiOutput("dictionarySelectInput_DictUpload"),
                actionButton("processDictionary_DictUpload", "Process/Update Dictionary", style="margin-top: 1.8em;")
            ),
            hr(style = "border-top: 1px solid #ededed; margin: 0 0 5px;"),
            textOutput("did_it_work")
        )
    )
))
