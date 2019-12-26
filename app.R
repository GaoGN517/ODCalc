library(shiny)
library(readxl)

runApp(
    list(
        ui = fluidPage(
            titlePanel("Test for homogeneity"),
            sidebarLayout(
                sidebarPanel(
                    fileInput('file1', 'Choose xlsx file',
                              accept = c(".xlsx")
                    ),
                    # Input: Select number of rows to display ----
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             All = "all"),
                                 selected = "head"),
                    textInput("col1", "No. of Property column:", value = "", width = NULL,
                              placeholder = NULL),
                    radioButtons("All_First", "Test all levels of the property column or pairwise comparison?",
                                 choices = c(All = "all",
                                             Pairwise = "pairwise"),
                                 selected = "all"),
                    textInput("lev1", "First level of the property column to compare:", value = "", width = NULL,
                              placeholder = NULL),
                    textInput("lev2", "Second level of the property column to compare:", value = "", width = NULL,
                              placeholder = NULL),
                    textInput("col2", "No. of Result column:", value = "", width = NULL,
                              placeholder = NULL)
                    
                ),
                mainPanel(
                    h4("Contents"),
                    tableOutput('contents'),
                    h4("Levels of property column"),
                    textOutput('levels'),
                    h4("Frequency Table"),
                    tableOutput("table"),
                    h4("Fisher's Exact Test"),
                    tableOutput("fisherTest"))
                    
                    
            )
        ),
        server = function(input, output){
            Data <- reactive({
                inFile <- input$file1
                
                if(is.null(inFile))
                    return(NULL)
                
                ext <- tools::file_ext(inFile$name)
                file.rename(inFile$datapath,
                            paste(inFile$datapath, ext, sep="."))
                return(read_excel(paste(inFile$datapath, ext, sep="."), 1))
            })
            
            Table <- reactive({
                Col1 <- as.numeric(input$col1)
                Col2 <- as.numeric(input$col2)
                Var1 <- as.numeric(input$lev1)
                Var2 <- as.numeric(input$lev2)
                
                property <- df[, Col1]
                level_prop <- unique(property)
                
                df <- Data()
                if(input$All_First == "all") {
                    df_table <- table(df[, c(Col1, Col2)])
                }
                else {
                    df_table <- table(df[(df[, Col1] == as.character(level_prop[Var1, ]) | df[, Col1] == 
                                              as.character(level_prop[Var2, ])), c(Col1, Col2)])
                }
                return(df_table)
            })
            
            output$contents <- renderTable({
                df <- Data()
                if(input$disp == "head") {
                    return(head(df))
                }
                else {
                    return(df)
                }
            })
            
            output$levels <- renderText({
                df <- Data()
                property <- df[, as.numeric(input$col1)]
                level_prop <- unique(property)
                outprint <- level_prop[1, ]
                for(i in 2:nrow(level_prop)) {
                    outprint <- paste0(outprint, "; ", level_prop[i, ])
                }
                
                paste0("The ", nrow(level_prop), " levels of the property column are: ", outprint)
            })
            
            output$table <- renderTable({
                return(Table())
            })
            
            output$fisherTest <- renderText({
                df <- Data()
                Col1 <- as.numeric(input$col1)
                Col2 <- as.numeric(input$col2)
                df_table <- table(df[, c(Col1, Col2)])
                p_value <- round(fisher.test(table)$p.value, 5)
                return(paste0("p value is: ", p_value))
            })
        }
    )
)