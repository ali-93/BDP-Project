library(arules)
library(tkrplot)
library(arulesViz)

summer_tran <- read.transactions(file = "Summer.csv", "basket", sep = ",")

winter_tran <- read.transactions(file = "Winter.csv", "basket", sep = ",")

autumn_tran <- read.transactions(file = "Autumn.csv", "basket", sep = ",")

spring_tran <- read.transactions(file = "Autumn.csv", "basket", sep = ",")

# plot frequent items
itemFrequencyPlot (summer_tran, topN = 10, type = "absolute", 
                   main = "Top Ten Summer Frequent Items", col = "red")

itemFrequencyPlot (winter_tran, topN = 10, type = "absolute", 
                   main = "Top Ten Winter Frequent Items", col = "blue") 

itemFrequencyPlot (autumn_tran, topN = 10, type = "absolute", 
                   main = "Top Ten Autumn Frequent Items", col = "orange") 

itemFrequencyPlot (spring_tran, topN = 10, type = "absolute", 
                   main = "Top Ten Spring Frequent Items", col = "green") 


# calculates support for frequent items
summer_itemsets <- eclat(summer_tran, 
                         parameter = list(supp = 0.07, maxlen = 15)) 

winter_itemsets <- eclat(winter_tran, 
                         parameter = list(supp = 0.07, maxlen = 15)) 

autumn_itemsets <- eclat(autumn_tran, 
                         parameter = list(supp = 0.07, maxlen = 15)) 

spring_itemsets <- eclat(spring_tran, 
                         parameter = list(supp = 0.07, maxlen = 15))

# Min Support as 0.001 (0.1%), confidence as 0.8 (80%)
summer_rules <- apriori (summer_tran, 
                         parameter = list(supp = 0.001, conf = 0.8))

winter_rules <- apriori (winter_tran, 
                         parameter = list(supp = 0.001, conf = 0.8))

autumn_rules <- apriori (autumn_tran, 
                         parameter = list(supp = 0.001, conf = 0.8))

spring_rules <- apriori (spring_tran, 
                         parameter = list(supp = 0.001, conf = 0.8))

# show the support, lift and confidence for all rules

# In Summer Transactions..
quality(summer_rules)

# In Winter Transactions..
quality(winter_rules)

# In Autumn Transactions..
quality(autumn_rules)

# In Spring Transactions..
quality(spring_rules)

# Show the top 5 rules, but only 2 digits
options (digits=2)

# In Summer Transactions..
inspect (summer_rules[1:5])

# In Winter Transactions..
inspect (winter_rules[1:5])

# In Autumn Transactions..
inspect (autumn_rules[1:5])

# In Spring Transactions..
inspect (spring_rules[1:5])

# Interactive Plot
# Summer Plots..

plot (summer_rules[1:10], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (summer_rules, measure = c("support", "lift"),
      shading = "confidence")

# Winter Plots..

plot (winter_rules[1:10], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (winter_rules, measure = c("support", "lift"), 
      shading = "confidence")

# Autumn Plots..

plot (autumn_rules[1:10], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (autumn_rules, measure = c("support", "lift"), 
      shading = "confidence")

# Spring Plots..

plot (spring_rules[1:10], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (spring_rules, measure = c("support", "lift"), 
      shading = "confidence")

# 'high-confidence' rules.

# In Summer Transactions..
summer_rules <- sort (summer_rules, by="confidence", decreasing=TRUE)

inspect(summer_rules[1:5])

# In Winter Transcations..
winter_rules <- sort (winter_rules, by="confidence", decreasing=TRUE)

inspect(winter_rules[1:5])

# In Autumn Transactions..
autumn_rules <- sort (autumn_rules, by="confidence", decreasing=TRUE)

inspect(autumn_rules[1:5])

# In Spring Transactions..
spring_rules <- sort (spring_rules, by="confidence", decreasing=TRUE)

inspect(spring_rules[1:5])



# Question-2
# What products are usually bought
# before a certain product[user-defined]?

# In Summer Transactions..
summer_prior_buying_rules <- apriori (data = summer_tran, 
                  parameter = list (supp = 0.001, conf = 0.08), 
                  appearance = list (default="lhs",rhs="soda"), 
                  control = list (verbose=F))

summer_prior_buying_rules <- sort (
  summer_prior_buying_rules, by="confidence", decreasing=TRUE)

inspect(summer_prior_buying_rules[1:5])



# In Winter Transcations..
winter_prior_buying_rules <- apriori (data = winter_tran, 
                               parameter = list (supp = 0.001, conf = 0.08), 
                               appearance = list (default="lhs",rhs="whole milk"), 
                               control = list (verbose=F))

winter_prior_buying_rules <- sort (
  winter_prior_buying_rules, by="confidence", decreasing=TRUE)

inspect(winter_prior_buying_rules[1:5])

# In Autumn Transactions..
autumn_prior_buying_rules <- apriori (data = autumn_tran, 
                               parameter = list (supp = 0.015, conf = 0.08), 
                               appearance = list (default="lhs",rhs="yogurt"), 
                               control = list (verbose=F))

autumn_prior_buying_rules <- sort (
  autumn_prior_buying_rules, by="confidence", decreasing=TRUE)

inspect(autumn_prior_buying_rules[1:5])

# In Spring Transactions..
spring_prior_buying_rules <- apriori (data = spring_tran, 
                               parameter = list (supp = 0.015, conf = 0.08), 
                               appearance = list (default="lhs",rhs="rolls/buns"), 
                               control = list (verbose=F))

spring_prior_buying_rules <- sort (
  spring_prior_buying_rules, by="confidence", decreasing=TRUE)

inspect(spring_prior_buying_rules[1:5])

# Interactive Plot

# Summer Plots "Event-[X] -> Event-A"..

plot (summer_prior_buying_rules[1:5], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (summer_prior_buying_rules, measure = c("support", "lift"),
      shading = "confidence")

# Winter Plots "Event-[X] -> Event-A"..

plot (winter_prior_buying_rules[1:5], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (winter_prior_buying_rules, measure = c("support", "lift"), 
      shading = "confidence")

# Autumn Plots "Event-[X] -> Event-A"..

plot (autumn_prior_buying_rules[1:5], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (autumn_prior_buying_rules, measure = c("support", "lift"), 
      shading = "confidence")

# Spring Plots "Event-[X] -> Event-A"..

plot (spring_prior_buying_rules[1:5], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (spring_prior_buying_rules, measure = c("support", "lift"), 
      shading = "confidence")

# Question-3
# What products customers usually buy after 
# buying a certain product[user-defined]?

# In Summer Transactions..
summer_post_buying_rules <- apriori (data = summer_tran, 
             parameter = list (supp = 0.001, conf = 0.15, minlen = 2), 
             appearance = list (default="rhs",lhs="yogurt"), 
             control = list (verbose=F))

summer_post_buying_rules <- sort (
  summer_post_buying_rules, by="confidence", decreasing=TRUE)

inspect(summer_post_buying_rules[1:5])

# In Winter Transactions..
winter_post_buying_rules <- apriori (data = winter_tran, 
            parameter = list (supp = 0.001, conf = 0.15, minlen = 2), 
            appearance = list (default="rhs",lhs="ready soups"), 
            control = list (verbose=F))

winter_post_buying_rules <- sort (
  winter_post_buying_rules, by="confidence", decreasing=TRUE)

inspect(winter_post_buying_rules[1:5])

# In Autumn Transactions..
autumn_post_buying_rules <- apriori (data = autumn_tran, 
            parameter = list (supp = 0.015, conf = 0.15, minlen = 2), 
            appearance = list (default="rhs",lhs="bottled beer"), 
            control = list (verbose=F))

autumn_post_buying_rules <- sort (
  autumn_post_buying_rules, by="confidence", decreasing=TRUE)

inspect(autumn_post_buying_rules[1:5])

# In Spring Transactions..
spring_post_buying_rules <- apriori (data = spring_tran, 
            parameter = list (supp = 0.015, conf = 0.15, minlen = 2), 
            appearance = list (default="rhs",lhs="brown bread"), 
            control = list (verbose=F))

spring_post_buying_rules <- sort (
  spring_post_buying_rules, by="confidence", decreasing=TRUE)

inspect(spring_post_buying_rules[1:5])

# Interactive Plot

# Summer Plots "Event-A -> Event[X]"..

plot (summer_post_buying_rules[1:5], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (summer_post_buying_rules, measure = c("support", "lift"),
      shading = "confidence")

# Winter Plots "Event-A -> Event[X]"..

plot (winter_post_buying_rules[1:1], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (winter_post_buying_rules, measure = c("support", "lift"), 
      shading = "confidence")

# Autumn Plots "Event-A -> Event[X]"..

plot (autumn_post_buying_rules[1:5], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (autumn_post_buying_rules, measure = c("support", "lift"), 
      shading = "confidence")

# Spring Plots "Event-A -> Event[X]"..

plot (spring_post_buying_rules[1:1], method="graph", 
      interactive=TRUE, shading = "confidence")

plot (spring_post_buying_rules, measure = c("support", "lift"), 
      shading = "confidence")

