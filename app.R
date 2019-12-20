library(shiny)
library(readxl)

runApp(
    list(
        ui = fluidPage(
            titlePanel("Use readxl"),
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
                    textInput("First_column", "No. of Property column:", value = "", width = NULL,
                              placeholder = NULL),
                    radioButtons("All_First", "Test all levels of the property column or pairwise comparison?",
                                 choices = c(All = "all",
                                             Pairwise = "pairwise"),
                                 selected = "all"),
                    textInput("First_level", "First level of the property column to compare:", value = "", width = NULL,
                              placeholder = NULL),
                    textInput("Second_level", "Second level of the property column to compare:", value = "", width = NULL,
                              placeholder = NULL),
                    textInput("Second_column", "No. of Result column:", value = "", width = NULL,
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
                col_1 <- input$First_column
                col_2 <- input$Second_column
                var_1 <- input$First_level
                Var_2 <- input$Second_level
                
                df <- Data()
                if(input$All_First == "all") {
                    df_table <- df_table <- table(df[, c(col_1, col_2)])
                }
                else {
                    df_table <- df_table <- table(df[(df[, col_1] == var_1 | df[, col_1] == var_2), c(col_1, col_2)])
                }
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
                property <- df[, input$First_column]
                level_prop <- levels(property)
                paste0("The levels of the property column are: ", level_prop)
            })
            
            output$table <- renderTable({
                return(Table())
            })
            
            output$fisherTest <- renderText({
                df <- Data()
                df_table <- table(df[, c(col_1, col_2)])
                p_value <- round(fisher.test(table)$p.value, 5)
                return(paste0("p value is: ", p_value))
            })
        }
    )
)