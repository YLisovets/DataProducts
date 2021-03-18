library(shiny)
library(plotly)
library(dplyr)

Sys.setlocale("LC_ALL","English")
data <- read.csv2(text = readLines("Data/data2.csv", warn = FALSE),header=TRUE, sep = ",")
# data <- read.csv("Data/data2.csv", stringsAsFactors = FALSE)
data$Item <- as.character(data$Item)
data$Category <- as.character(data$Category)
data$Sales <- as.numeric(data$Sales)
data$Gr_Margin <- as.numeric(data$Gr_Margin)

ui <- fluidPage(
    
    # Application title
    titlePanel("Sales vs Profit Quadrant chart"),
    
    # Sidebar with a select input
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "shop",
                        label = "Choose Shop:",
                        choices = c("All", unique(data$Shop)),
                        #size = 10,selectize = FALSE,
                        selected = "ALL"
            ),
            selectInput(inputId = "category",
                        label = "Choose Category:",
                        choices = c("All", unique(sort(as.numeric(data$Category)))),
                        #size = 10,selectize = FALSE,
                        selected = "All"
            ),
            radioButtons(inputId = "level",
                         label = "Aggregate level:",
                         list("Group", "Item")
            ),
            br(),
            br(),
            br(),
            # actionButton
            downloadButton('describe_download',"Download Report",class="butt" ),
            tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
            
            width = 3
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            plotlyOutput("Plot"),
            br(),
            DT::dataTableOutput("Table")
        )
    )
)

server <- function(input, output) {
    
    data_filter <- reactive({
        data_new <- data
        
        # filter by shop
        if (input$shop != "All") {
            data_new <- filter(data_new, Shop == input$shop)
        }
        # filter by category
        if (input$category != "All") {
            data_new <- filter(data_new, Category == input$category)
        }
        # group by aggregate level, summarise and quadrant definition
        data_new %>% 
            group_by(!!rlang::sym(input$level)) %>% 
            summarise(sale = sum(Sales),
                      marg = sum(Gr_Margin)) %>%
            mutate(Quadrant = ifelse(marg >= mean(marg),
                                     ifelse(sale >= mean(sale), "Star Performers", "Cash Machines"),
                                     ifelse(sale >= mean(sale), "Core Traffic", "Act!")))
    })
    output$Plot <- renderPlotly({
        p <- data_filter() %>% 
            ggplot(aes(x = marg, y = sale, color = Quadrant, text = paste0(input$level,": ", !!rlang::sym(input$level)))) +
            geom_point() +
            geom_hline(aes(yintercept = mean(sale))) +
            geom_vline(aes(xintercept = mean(marg))) +
            labs(title = paste0("Shop: ", input$shop, ", Category: ", input$category),
                 x = 'Gross Margin', y = 'Sales', color = '') +
            theme_bw()
        ggplotly(p)
        
    })
    output$Table <- DT::renderDataTable(data_filter())
    output$describe_download <- downloadHandler(
        filename = function(){
            paste("Result-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            openxlsx::write.xlsx(data_filter(), file, sheetName = paste0(input$shop, "_Category_", input$category))
        }
    )
}

shinyApp(ui = ui, server = server)
