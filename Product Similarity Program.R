library(dplyr)
library(reshape2)

# Data Prep
new_prods <- read.csv('newprod.csv')
new_prods_cartons <- new_prods %>% select(ISBN13, CartonQty)
new_prods_cartons <- distinct(new_prods_cartons)
new_prods_cartons$CartonQty[is.na(new_prods_cartons$CartonQty)] <- 1
new_prods_index <- levels(as.factor(new_prods$ISBN13))
new_prods$Sales <- 0
colnames(new_prods_cartons) <- c('Product','CartonQty')
new_prods <- new_prods[,-4]
prod_sim <- read.csv('prodsim.csv')
prod_sim <- rbind(prod_sim, new_prods)
colnames(prod_sim) <- c('Product','Title','Publisher','Catalog','Category','Int',
                        'GRL','Sales')
prod_sim$Product <- as.factor(prod_sim$Product)
new_prods_cartons$Product <- as.factor(new_prods_cartons$Product)
prod_sim$Catalog <- as.factor(prod_sim$Catalog)
prod_sim$Category <- as.factor(prod_sim$Category)
prod_sim$Int <- as.factor(prod_sim$Int)
prod_sim$GRL <- as.factor(prod_sim$GRL)

# Reshape and One-Hot Encode
cg <- reshape2::dcast(prod_sim, Product ~ Catalog, value.var = 'Catalog',
                      fun.aggregate = length)
ct <- reshape2::dcast(prod_sim, Product ~ Category, value.var = 'Category',
                      fun.aggregate = length)
int <- reshape2::dcast(prod_sim, Product ~ Int, value.var = 'Int', fun.aggregate = length)
grl <- reshape2::dcast(prod_sim, Product ~ GRL, value.var = 'GRL', fun.aggregate = length)
combined <- left_join(cg, ct, by = 'Product')
combined <- left_join(combined, int, by = 'Product')
combined <- left_join(combined, grl, by = 'Product')
encode_fn <- function(x) {as.integer(x > 0)}
combined <- combined %>% mutate_at(vars(-Product), funs(encode_fn))

# Create product index
prods <- distinct(prod_sim %>% select(Product, Title, Publisher, Int, GRL, Sales))

# Create sim matrix
library(coop)
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
results$QTY[results$QTY < 10] <- 10
results <- left_join(results, prods, by = 'Product')
write.csv(results, 'new_prod_results.csv')

# Query for results
query_prod <- '9781419748684'
sim_prods <- combined$Product[
  order(prodToProdSimMatrix[, query_prod], decreasing = T)[1:31]]
sim_prods_des <- unique(
  prods[which(prods$Product %in% sim_prods),
        c('Product','Title','Publisher','Int','GRL','Sales')])
sim_prods_des <- sim_prods_des %>% filter(!Product %in% new_prods_index)
mean(as.integer(sim_prods_des[,6]))
write.csv(sim_prods_des, paste(query_prod, '- query.csv'))
