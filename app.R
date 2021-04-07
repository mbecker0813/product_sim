library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(reshape2)
library(coop)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("cerulean"),

    # Application title
    titlePanel("Newly Carried Title Sales Predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "filter",
                        label = "Only look at existing titles selling less than __ copies:",
                        min = 10, max = 100, value = 50, step = 5),
            sliderInput(inputId = "minbuy",
                        label = "Minimum buy quantity:",
                        min = 0, max = 100, value = 10, step = 5),
            fileInput(inputId = "dataset",
                      label = "Upload existing product dataset in CSV format:",
                      accept = c(".csv")),
            fileInput(inputId = "newdata",
                      label = "Upload new product dataset in CSV format:",
                      accept = c(".csv")),
            actionButton(inputId = "run",
                         label = "Run"),
            h5("The app can take 30-60 seconds to run once files are uploaded. It depends on how big your datasets are and the slider parameters above.", style = "color:#3474A7")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            downloadButton(outputId = "download",
                           label = "Download"),
            DTOutput(outputId = "preview")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    whole_process <- eventReactive(input$run, {
        req(input$newdata)
        req(input$dataset)
        # Data Prep
        options(scipen = 999)
        new_prods <- read.csv(input$newdata$datapath)
        new_prods_cartons <- new_prods %>% select(ISBN13, CartonQty)
        new_prods_cartons <- distinct(new_prods_cartons)
        new_prods_cartons$CartonQty[is.na(new_prods_cartons$CartonQty)] <- 1
        new_prods_index <- levels(as.factor(new_prods$ISBN13))
        new_prods$Sales <- 0
        colnames(new_prods_cartons) <- c('Product','CartonQty')
        new_prods <- new_prods[,-4]
        prod_sim <- read.csv(input$dataset$datapath)
        colnames(prod_sim) <- colnames(new_prods)
        prod_sim <- prod_sim %>% filter(Sales < input$filter)
        prod_sim <- rbind(prod_sim, new_prods)
        colnames(prod_sim) <- c('Product','Title','Publisher','Cost','Catalog','Line','FIC','BIL',
                                'BISAC','Pages','Int','GRL','Sales')
        prod_sim$BIL[prod_sim$BIL %in% c('FALSE','False')] <- 0
        prod_sim$BIL[prod_sim$BIL %in% c('TRUE','True')] <- 1
        prod_sim$Sales[prod_sim$Sales < 0] <- 0
        prod_sim$Product <- as.factor(prod_sim$Product)
        new_prods_cartons$Product <- as.factor(new_prods_cartons$Product)
        prod_sim$Catalog <- as.factor(prod_sim$Catalog)
        prod_sim$Line <- as.factor(prod_sim$Line)
        prod_sim$FIC <- as.factor(prod_sim$FIC)
        prod_sim$BIL <- as.factor(prod_sim$BIL)
        prod_sim$BISAC <- as.factor(prod_sim$BISAC)
        prod_sim$Pages <- as.factor(prod_sim$Pages)
        prod_sim$Int <- as.factor(prod_sim$Int)
        prod_sim$GRL <- as.factor(prod_sim$GRL)
        
        # Reshape and One-Hot Encode
        cg <- reshape2::dcast(prod_sim, Product ~ Catalog, value.var = 'Catalog',
                              fun.aggregate = length)
        pl <- reshape2::dcast(prod_sim, Product ~ Line, value.var = 'Line',
                              fun.aggregate = length)
        fn <- reshape2::dcast(prod_sim, Product ~ FIC, value.var = 'FIC', fun.aggregate = length)
        bl <- reshape2::dcast(prod_sim, Product ~ BIL, value.var = 'BIL', fun.aggregate = length)
        bsc <- reshape2::dcast(prod_sim, Product ~ BISAC, value.var = 'BISAC', fun.aggregate = length)
        pg <- reshape2::dcast(prod_sim, Product ~ Pages, value.var = 'Pages', fun.aggregate = length)
        int <- reshape2::dcast(prod_sim, Product ~ Int, value.var = 'Int', fun.aggregate = length)
        grl <- reshape2::dcast(prod_sim, Product ~ GRL, value.var = 'GRL', fun.aggregate = length)
        combined <- left_join(cg, pl, by = 'Product')
        combined <- left_join(combined, fn, by = 'Product')
        combined <- left_join(combined, bl, by = 'Product')
        combined <- left_join(combined, bsc, by = 'Product')
        combined <- left_join(combined, pg, by = 'Product')
        combined <- left_join(combined, int, by = 'Product')
        combined <- left_join(combined, grl, by = 'Product')
        encode_fn <- function(x) {as.integer(x > 0)}
        combined <- combined %>% mutate_at(vars(-Product), funs(encode_fn))
        
        # Create product index
        prods <- distinct(prod_sim %>% select(Product, Title, Publisher, Cost, Int, GRL, Sales))
        
        # Create sim matrix
        prodToProdSimMatrix <- cosine(as.matrix(
            # Excluding ISBN column
            t(combined[, 2:dim(combined)[2]])))
        colnames(prodToProdSimMatrix) <- combined$Product
        
        # Create blank data frame for results
        results <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(results) <- c('Product','QTY')
        
        # Loop for each product
        for (i in 1:length(new_prods_index)){
            sim_prods <- combined$Product[
                order(prodToProdSimMatrix[, new_prods_index[i]], decreasing = T)[1:31]]
            sim_prods_des <- unique(
                prods[which(prods$Product %in% sim_prods),
                      c('Product','Title','Publisher','Int','GRL','Sales')])
            sim_prods_des <- sim_prods_des %>% filter(!Product %in% new_prods_index)
            new_row <- data.frame(Product = new_prods_index[i],
                                  QTY = round(mean(as.integer(sim_prods_des[,6])),0))
            new_row <- left_join(new_row, new_prods_cartons, by = 'Product')
            if(nrow(sim_prods_des) < 1){
                new_row$QTY <- input$minbuy
            }
            if((new_row$QTY / new_row$CartonQty) < 1){
                if((new_row$QTY / new_row$CartonQty) > .75){
                    new_row$QTY <- new_row$CartonQty
                }
            }
            if((new_row$QTY / new_row$CartonQty) > 1){
                cc <- floor(new_row$QTY / new_row$CartonQty)
                ca <- new_row$CartonQty * cc
                loose <- new_row$QTY - ca
                if(loose / new_row$CartonQty > .75){
                    new_row$QTY <- new_row$CartonQty * (cc + 1)
                }
            }
            new_row <- new_row %>% select(Product, QTY)
            results <- rbind(results,new_row)
        }
        # Round anything less than 10 up to 10 as an initial buy quantity
        results$QTY[results$QTY < input$minbuy] <- input$minbuy
        results <- left_join(results, prods, by = 'Product')
        results$CostExt <- results$Cost * results$QTY
        reorg_df <- cbind(results[,1:2], ExtCost = results[,9], results[,3:4])
        reorg_df
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste("newly_carried_titles_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(whole_process(), file)
        }
    )
    output$preview <- renderDT(whole_process())
}

# Run the application 
shinyApp(ui = ui, server = server)
