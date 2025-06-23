
# Homework <- read_excel("/Users/varunrao/Downloads/Preferences 2025.xlsx")    
# Homework <- read_excel("Class1b/Preferences 2025.xlsx")   
library(dplyr)
library(readxl)
Homework <- read_excel("/Users/sravyabhaskara/Documents/Advanced Statistics/Homeworks/Preferences 2025.xlsx") 
head(Homework)



x <- as.matrix(Homework[,4:8])       # 5 x variables 
y <- as.matrix(Homework[,3])         # price as y variable
n <- nrow(x)                     # number of observations
p <- ncol(x)                  # number of variables in xx data
colnames(x) <- c("Screen Size 75", "Screen Size 85", "Resolution", "Sony", "Price")
colnames(y) <- c("Estimates")


# Partworths, Attribute Importance, WTP


# Below code has the Function created - my_lm to calculate following: 
# 1. Partworths, SE, t-value for each attribute level
# 2. Attribute Importance of each attribute
# 3. Willingness to pay for each non-price attribute level




my_lm <- function(y,x) {
  
  ########################################## 1. Partworths, SE, t-value for each attribute level.
  
  n <- nrow(x)                    # number of observations
  one <- matrix(1,n,1)            # creates column of ones for the itercept
  xx <- cbind(one,x)              # attaches ones to the x data
  colnames(xx) <- c("Intercept", colnames(x)) # assign names
  p <- ncol(xx)                   # number of variables in xx data
  
  xpx <- t(xx) %*% xx             # X'X matrix, t() denotes transpose
  xpxi <- solve(xpx)              # solve() inverts the matrix X'X
  xpy <- t(xx) %*% y              # X'Y matrix
  bhat <- xpxi %*% xpy            # estimate bhat = Inverse(X'X)*X'Y as per OLS estimator in class notes
  
  yhat <- xx %*% bhat             # forecasted y values = X*bhat
  err <- y-yhat                   # residuals = y - yhat
  sse <- t(err) %*% err           # sum of square residuals to compute error variance s2
  
  s2 <- sse/(n-p)                 # error variance, sigma^2 in class notes
  se <- sqrt(c(s2)*diag(xpxi))       # standard errors of bhat given by diagonal of sigma^2 * Inverse(X'X)
  tval <- bhat/se                 # t-values = estimates divided by their std errors
  
  my_estimates <- cbind(bhat, se, tval)    # output from my OLS built from first principles
  colnames(my_estimates) <- c("Estimates", "Std Errors", "t-values")
  rownames(my_estimates) <- colnames(xx)
  
  CI_lower <- bhat - qt(0.975, df = n-p) * se		# lower bound based on t-dist
  CI_upper <- bhat + qt(0.975,df = n-p) * se 		# upper bound based on t-dist
  
  my_CI <- cbind(CI_lower, CI_upper)
  colnames(my_CI) <- c("2.5%", "97.5%")
  rownames(my_CI) <- colnames(xx)
  
  #print(my_estimates)
  #print(my_CI)
  
  ########################################## 2. Attribute Importance of each attribute
  
  # Regression output part-worth coefficients
  partworths <- c(
    `Screen Size 75` = 2.600052,
    `Screen Size 85` = 4.330155,
    `Resolution` = 5.446445,
    `Sony` = 2.083766,
    `Price` = -3.951746
  )
  # Define attributes and their levels
  attributes <- list(
    `Screen Size` = c("Screen Size 75", "Screen Size 85"),
    `Resolution` = c("Resolution"),
    `Brand Name` = c("Sony"),
    `Price` = c("Price")
  )
  # Calculate attribute ranges
  attribute_ranges <- sapply(attributes, function(levels) {
    if (length(levels) > 1) {
      # For attributes with multiple levels, calculate the range as max - min
      max(partworths[levels]) - min(partworths[levels])
    } else {
      # For single-level attributes, use the absolute value of the part-worth
      abs(partworths[levels])
    }
  })
  # Normalize to calculate importance percentages
  attribute_importance <- attribute_ranges / sum(attribute_ranges) * 100
  
  # Combine results
  attribute_results <- data.frame(
    Attribute = names(attribute_ranges),
    Range = attribute_ranges,
    Importance = round(attribute_importance, 1)  # Rounded for better readability
  )
  
  # Display results
  # print(attribute_results)
  
  
  ########################################## 3.Willingness to pay for each non-price attribute level
  coefficients <- c(
    `Intercept` = 8.385877,
    `Screen Size 75` = 2.600052,
    `Screen Size 85` = 4.330155,
    `Resolution` = 5.446445,
    `Sony` = 2.083766,
    `Price` = -3.951746
  )
  
  # Calculate utils per dollar
  price_partworth <- coefficients["Price"]
  util_per_dollar <- abs(500 / price_partworth)  
  #print("util_per_dollar")
  #print(util_per_dollar)
  
  # Calculate WTP for each attribute level
  wtp <- coefficients[c("Screen Size 75", "Screen Size 85", "Sony", "Resolution")] * util_per_dollar
  names(wtp) <- c("Screen Size 75", "Screen Size 85", "Sony Brand", "4K Resolution")
  
  # Display results
  wtp <- round(wtp, 2)  # Round to two decimal places for readability
  #print(wtp)
  
  return(list(my_estimates, my_CI, attribute_results,  wtp)) 	
  # R returns only ONE output. 
  # If you have more than one output, create a list(out1, out2, out3, etc) to return  multiple outputs. 
  
}

my_function_out <- my_lm(y, x)



cat("1 a. Partworths for each attribute level (Estimates, Std Errors, T-values: \n")
print(my_function_out[[1]])  

cat("\n 1 b. Confidence Interval:\n ")
print(my_function_out[[2]])


cat("\n2. Attribute Importance of each attribute:\n")
print(my_function_out[[3]])  # Access the second element (my_CI)

cat("\n3. Willingness to Pay (WTP):\n")
print(my_function_out[[4]])  # Access the third element (wtp)


# Creating Profiles



## 1. Define your partworths (Excel row 1)

partworths <- c(
  Intercept  = 8.4,
  Screen75   = 2.6,
  Screen85   = 4.3,
  Resolution = 5.4,
  Sony       = 2.1,
  Price      = -4.0
)


## 2. Define the profiles (Excel rows):
##    Each row has 1/0 for each attribute + numeric price.

profiles <- data.frame(
  Intercept  = c(1, 1, 1),
  Screen75   = c(0, 1, 0),
  Screen85   = c(1, 0, 1),
  Resolution = c(0, 1, 1),
  Sony       = c(0, 1, 0),
  Price      = c(1500, 2500, 2000),
  row.names  = c("My design", 
                 "Competing Brand 1 (Sony)",
                 "Competing Brand 2 (Sharp)")
)

profiles

# Creating Utility, Attractiveness, Market_Share for the profiles


calculate_profiles <- function(profiles, partworths, minPrice = 2000, maxPrice = 2500) {
  ##
  ## 3. Create a function matching the Excel formula for Utility:
  ##    = $B$1*B4 + $C$1*C4 + $D$1*D4 + $E$1*E4 + $F$1*F4 
  ##      + $G$1*((G4 - 2000)/(2500 - 2000))
  ##
  calc_utility_excel_style <- function(one_row, partworths, minPrice = 2000, maxPrice = 2500) {
    # Sum of non-price attributes:
    base_utility <- 
      partworths["Intercept"]  * one_row["Intercept"]  +
      partworths["Screen75"]   * one_row["Screen75"]   +
      partworths["Screen85"]   * one_row["Screen85"]   +
      partworths["Resolution"] * one_row["Resolution"] +
      partworths["Sony"]       * one_row["Sony"]
    
    # Rescale Price into [0, 1] range: (Price - 2000)/(2500 - 2000)
    scaled_price_contribution <- 
      partworths["Price"] * ((one_row["Price"] - minPrice) / (maxPrice - minPrice))
    
    # Final utility:
    total_utility <- base_utility + scaled_price_contribution
    return(total_utility)
  }
  
  ##
  ## 4. Apply that function row-by-row to calculate each profile's Utility
  ##
  utilities <- apply(
    profiles, 
    1,  # MARGIN=1 => apply function to each row
    calc_utility_excel_style,
    partworths = partworths
  )
  
  # Add the 'Utility' column to your profiles
  profiles$Utility <- utilities
  
  ##
  ## 5. Compute Attractiveness (exp(Utility)) and Market Share
  ##
  profiles$Attractiveness <- exp(profiles$Utility)
  
  # Calculate total attractiveness, but don't add as a new row
  total_attractiveness <- sum(profiles$Attractiveness)
  
  # Market Share = Attractiveness / total attractiveness
  profiles$Market_Share <- profiles$Attractiveness / total_attractiveness
  
  ##
  ## 6. See the final table (no extra row)
  ##
  return(profiles)
}


profiles <- calculate_profiles(profiles, partworths)
profiles



# Optimal Price, Maximum Profit, Market share associated with optimal price, Market Share v/s Price, Profit v/s Price


# Below code has the Function created - analyze_design to calculate following: 
# 4. Optimal price
# 5. Maximum profit
# 6. Market share associated with optimal price
# 7. Plot market shares as a function of prices
# 8. Plot profit as a function of prices

profiles_baseline <- data.frame(
  Intercept  = c(1, 1, 1),
  Screen75   = c(0, 1, 0),
  Screen85   = c(1, 0, 1),
  Resolution = c(0, 1, 1),
  Sony       = c(0, 1, 0),
  Price      = c(1500, 2500, 2000),  # We'll override My design's Price in the loop
  row.names  = c("My design", 
                 "Competing Brand 1 (Sony)",
                 "Competing Brand 2 (Sharp)")
)

#############################
calc_utility_excel_style <- function(one_row, partworths,
                                     minPrice = 2000, maxPrice = 2500) {
  # Sum of non-price attributes:
  base_utility <- 
    partworths["Intercept"]  * one_row["Intercept"]  +
    partworths["Screen75"]   * one_row["Screen75"]   +
    partworths["Screen85"]   * one_row["Screen85"]   +
    partworths["Resolution"] * one_row["Resolution"] +
    partworths["Sony"]       * one_row["Sony"]
  
  # Price contribution: Price is rescaled: (Price - minPrice)/(maxPrice - minPrice)
  scaled_price_contribution <-
    partworths["Price"] * ((one_row["Price"] - minPrice) / (maxPrice - minPrice))
  
  total_utility <- base_utility + scaled_price_contribution
  return(total_utility)
}

analyze_design <- function(
    price_range = seq(1500, 2500, by = 100),  # Range of prices to test
    base_df     = profiles_baseline,          # Baseline data frame
    partworths  = partworths,                 # Partworth values
    market_size = 100,                        # Market size
    net_cost    = 2000,                       # Net cost for "My design"
    minPrice    = 2000,                       # Minimum price for scaling
    maxPrice    = 2500                        # Maximum price for scaling
) {
  #############################
  # Recompute Scenario Function
  #############################
  recompute_scenario <- function(
    my_price,
    base_df,
    partworths,
    market_size,
    net_cost,
    minPrice,
    maxPrice
  ) {
    # Copy baseline so we don't overwrite it
    df <- base_df
    
    # Override My design's price
    df["My design", "Price"] <- my_price
    
    # 1) Calculate Utility for each row
    utilities <- apply(
      df,
      1,
      calc_utility_excel_style,
      partworths = partworths,
      minPrice   = minPrice,
      maxPrice   = maxPrice
    )
    df$Utility <- utilities
    
    # 2) Attractiveness = exp(Utility)
    df$Attractiveness <- exp(df$Utility)
    
    # 3) Market Share = each Attractiveness / sum(Attractiveness)
    total_attract <- sum(df$Attractiveness)
    df$Market_Share <- (df$Attractiveness / total_attract) #check if this needs to be round off till 2 decimal point
    
    # 4) Sales = Market Share * market_size
    df$Sales <- df$Market_Share * market_size
    
    # 5) Margin = Price - net_cost
    df$Margin <- df$Price - net_cost
    
    # 6) Profit = Margin * Sales
    df$Profit <- df$Margin * df$Sales
    
    # Let's return a clean table (you can keep more columns if you want).
    df_out <- df %>%
      select(
        Price, Utility, Attractiveness, Market_Share,
        Sales, Margin, Profit
      )
    
    return(df_out)
  }
  
  #############################
  # Iterate through price range
  #############################
  all_scenarios <- list()
  
  for (p in price_range) {
    scenario_table <- recompute_scenario(
      my_price    = p,
      base_df     = base_df,          # baseline
      partworths  = partworths,       # partworths
      market_size = market_size,      # set your desired market size
      net_cost    = net_cost,         # net cost for My design
      minPrice    = minPrice,         # for the Excel price-scaling formula
      maxPrice    = maxPrice
    )
    
    # For clarity, add a column indicating the tested "My design" price
    scenario_table$MyDesignPrice <- p
    
    # Also add a column for Brand (row name)
    scenario_table$Brand <- row.names(scenario_table)
    
    # Store the resulting table in our list
    all_scenarios[[as.character(p)]] <- scenario_table
  }
  
  #############################
  # Combine Results
  #############################
  combined_df <- bind_rows(all_scenarios, .id = "ScenarioID")
  # "ScenarioID" is just the list index. 
  # We'll keep "MyDesignPrice" as the actual numeric price we tested.
  combined_df$ScenarioID <- as.numeric(combined_df$ScenarioID)
  
  #############################
  # Analyze "My design" Only
  #############################
  my_design_only <- filter(combined_df, Brand == "My design")
  
  # Identify the Price that yields max Profit
  max_profit <- max(my_design_only$Profit)
  best_index <- which.max(my_design_only$Profit)
  best_price <- my_design_only$MyDesignPrice[best_index]
  
  #############################
  # Extract Best Row and Metrics
  #############################
  best_row <- my_design_only[best_index, ]
  optimal_price <- best_row$MyDesignPrice
  optimal_profit <- best_row$Profit
  optimal_share <- best_row$Market_Share
  
  # Create Optimal Result Data Frame
  optimal_result <- data.frame(
    Price = optimal_price,
    Profit = optimal_profit,
    Market_Share = optimal_share
  )
  
  #############################
  # Plot Results
  #############################
  library(ggplot2)
  
  # 1) Plot Profit vs. Price
  profit_plot <- ggplot(my_design_only, aes(x = MyDesignPrice, y = Profit)) +
    geom_line() +
    geom_point() +
    # Highlight the max-profit point
    geom_point(aes(x = best_price, y = max_profit), 
               color = "red", size = 3) +
    geom_text(
      aes(x = best_price, y = max_profit, 
          label = paste0("Max Profit = ", round(max_profit, 2), 
                         "\nPrice = ", best_price)),
      vjust = -0.8,      # position text above the point
      color = "red"
    ) +
    labs(
      title = "Profit vs. Price for My Design",
      x = "My Design Price",
      y = "Profit"
    )
  
  # 2) Plot Sales vs. Price
  sales_plot <- ggplot(my_design_only, aes(x = MyDesignPrice, y = Sales)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Sales vs. Price for My Design",
      x = "My Design Price",
      y = "Sales (units)"
    )
  
  #############################
  # Return All Results
  #############################
  return(list(
    combined_df     = combined_df,
    my_design_only  = my_design_only,
    max_profit      = max_profit,
    best_price      = best_price,
    best_row        = best_row,
    optimal_result  = optimal_result,
    profit_plot     = profit_plot,
    sales_plot      = sales_plot
  ))
}


results <- analyze_design(
  base_df = profiles_baseline,
  partworths = partworths
)

combined_df <- results$combined_df
my_design_only <- results$my_design_only
max_profit <- results$max_profit
best_price <- results$best_price
best_row <- results$best_row
optimal_result <- results$optimal_result


cat("Combined DF \n")
print(combined_df)

cat("My Design Only \n")
print(my_design_only)

cat("4. Optimal price for our design is: \n")
cat(best_price)

cat("\n5. Maximum Profit: \n")
cat(max_profit)

cat("\n 6. Market share associated with optimal price \n")
print(optimal_result)

cat("\n 7. Plot market shares as a function of prices \n ")
results$sales_plot

cat("\n 8. Plot profit as a function of prices \n ")
results$profit_plot

