library(shiny)
library(ggvis)
library(dplyr)
library(FactoMineR)
library(factoextra)

# Define UI for app
ui <- fluidPage(
  titlePanel("NCCU_DS2023_hw4_110354012"),
  sidebarPanel(
    p('iris data'),
    sliderInput("num",
                label = "Number of data:",
                min = 50,
                max = 150,
                value = 50),
  ),
  # Create a tabset with three tabs
  tabsetPanel(
    tabPanel("PCA", 
             sidebarLayout(
               sidebarPanel(width = 2,
                            # Add input widgets for PCA analysis
                            checkboxInput("scale", "Scale the data:", value = TRUE),
                            selectInput("pc1", "Select principal component(x-axis):", choices = 1:4, selected = 1),
                            selectInput("pc2", "Select principal component(y-axis):", choices = 1:4, selected = 2)
               ),
               mainPanel(
                 # Add output plot for PCA analysis using ggvis
                 br(),
                 ggvisOutput("pca_plot")
               )
             )),
    
    tabPanel("CA",
             mainPanel(
                 # Add output plot for CA analysis using ggvis
                 plotOutput("ca_plot")
               )
             ),
    
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(width = 2,
                 checkboxInput("show_correlation", "Correlation matrix"),
                 checkboxInput("show_histogram", "Histogram"),
                 checkboxInput("show_summary", "Summary"),
               ),
               mainPanel(
                 fluidRow(
                   tags$div(
                     style="height:400px;overflow-y:scroll;",
                     # 在這個區域中添加需要顯示的元素
                     dataTableOutput("data_table")
                   ),
                   
                 ),
                 column(6,plotOutput("plot1")),
                 column(6,plotOutput("plot2")),
                 column(6,plotOutput("plot3"))
                 # Add output table for data
                 # dataTableOutput("data_table")
               )
             ))
  )
)

# Define server logic for app
server <- function(input, output) {
  # Perform PCA analysis and render plot using ggvis
  pca_data <- reactive({
    data <- iris[1:input$num, 1:4]
    pca <- PCA(data, ncp = 4, scale.unit = input$scale, graph = FALSE)
    pca_df <- as.data.frame(pca$ind$coord[, as.numeric(c(input$pc1, input$pc2))])
    
    colnames(pca_df) <- c('Dim.1', 'Dim.2')
    pca_df$species <- iris[1:input$num,]$Species
    pca_df
  })
  
  pca_plot <- reactive({
    pca_data() %>%
      ggvis(~Dim.1, ~Dim.2) %>%
      layer_points(fill = ~species) %>% 
      add_axis("x", title=paste("PC", input$pc1)) %>%
      add_axis("y", title=paste("PC", input$pc2)) %>%
      add_axis("x",orient="top", title = "biplot for iris dataset")
  })
  
  pca_plot %>% bind_shiny("pca_plot")
  
  # Perform CA analysis and render plot using ggvis
  ca_data <- reactive({
    data <- iris[1:input$num,1:4]
    ca <- CA(data, graph = FALSE)
    ca
  })
  
  output$ca_plot <- renderPlot({
    ca_data() %>%
      fviz_ca_biplot()
  })
  
  # Upload data file and render table
  output$data_table <- renderDataTable({
    iris[1:input$num,]
  })
  
  # Visualization
  output$plot1 <- renderPlot({
    if(input$show_correlation) {
      library(ggcorrplot)
      corr_matrix <- cor(iris[1:input$num,1:4])
      corr_plot <- ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE, title = "Correlation Matrix")
      print(corr_plot)
    }
  })
  
  output$plot2 <- renderPlot({
    if(input$show_histogram) {
      hist_plot <- ggplot(iris[1:input$num,1:4], aes(x = Sepal.Length)) + geom_histogram()
      print(hist_plot)
    }
  })
  
  output$plot3 <- renderPlot({
    if(input$show_summary) {
      summary_plot <- ggplot(iris[1:input$num,], aes(x = Species, y = Sepal.Width)) + geom_boxplot()
      print(summary_plot)
    }
  })
  
}

# Run the app
shinyApp(ui, server)
