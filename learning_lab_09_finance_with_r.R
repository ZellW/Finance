# LEARNING LAB 09 ----
# FINANCE WITH R

# Capital Asset Pricing Model
# 1. Stock Price
# 2. Stock Returns:  Can be daily, weekly, monthly. . . simply a ratio of price / price -1
#    Mean RETURN = Reward, standard deviation of return = RISK (volatility)
# 3. Sharpe Ratio: ratio of RETURN to RISK (RETURN/RISK).  RETURN is reduced by the cost of money (risk free rate - Fed Rate)
#    Higher is better (higher return or lower risk or both)
# 4. Portfoliio: Aggregating can reduce risk while preserving return.  By evaluating different percentages of stock, 
#    random portfolios can be selected to max Sharpe Ration

# Not covered:  Backtesting - important topic.  Using previous data to evalaute portfolio
# Methods outside CAPM (widely used):  Factor Analysis, Technical Traung Indicators and much more

# tidyquant workflow:
# 1. Import:  tq_get() to get financial and economic data
# 2. Manipulate:  tq_tranmute():  Convert to returns
# 3. Aggregate:  tq_portfoliio() combines returns of multiple stocks into 1 or more portfolios
# 4. Calculate performance:  tq_performance calculates Sharpe Ratio

# Libraries ----
library(tidyquant)
library(furrr)
library(plotly)

# 1.0 IMPORT DATA ----

# Stock Prices
tq_get("AAPL", from = "2018-01-01", to = "2018-12-31")

# FRED Economic Data
tibble(symbol = c("MKTGDPCNA646NWDB"), name   = c("GDP China")) %>% tq_get(get = "economic.data", from = "1960-01-01") 


# 2.0 TRANSFORM TO RETURNS ----
end   <- "2018-12-31" %>% ymd()
start <- end - years(5) + days(1)

# 2.1 Components ----
returns_m_components_tbl <- c("AAPL", "GOOG", "NFLX") %>%
    tq_get(get  = "stock.prices", from = start, to = end) %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly") %>%
    ungroup()

returns_m_components_tbl

# 2.2 Benchmark ----
returns_m_benchmark_tbl <- "XLK" %>%
    tq_get(get  = "stock.prices", from = start, to   = end) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly") %>%
    add_column(symbol = "XLK", .before = 1) 
# when getting one symbol, the symbol column is not included so add it manually
# Could have gotten this with the individual stocks above (and probably should have)

returns_m_benchmark_tbl

# 3.0 PERFORMANCE ANALYSIS - COMPONENTS -----

# This provides the baseline performance but portfolio creation

returns_m_tbl <- returns_m_components_tbl %>% bind_rows(returns_m_benchmark_tbl)

returns_m_tbl %>%
    group_by(symbol) %>%
    tq_performance(Ra = monthly.returns, 
                   performance_fun = SharpeRatio.annualized, 
                   scale = 12, 
                   Rf    = 0) %>% #Rf = risk free rate
    ungroup() %>% mutate(symbol = as_factor(symbol)) %>%
    
    ggplot(aes(symbol, `AnnualizedSharpeRatio(Rf=0%)`)) +
    geom_col(fill = "#2c3e50") +
    geom_text(aes(label = `AnnualizedSharpeRatio(Rf=0%)` %>% round(2)), 
              color = "white", nudge_y = -0.05) +
    theme_tq() + labs(title = "Sharpe Ratio: Stocks vs Benchmark",
                      x = "", y = "Annualized Sharpe Ratio (Rf = 0%)")

# 4.0 PORTFOLIO AGGREGATION ----

wts_tbl <- returns_m_components_tbl %>% distinct(symbol) %>% mutate(weights = c(0.25, 0.25, 0.5))

wts_tbl

returns_m_portfolio_tbl <- returns_m_components_tbl %>%
    tq_portfolio(symbol, monthly.returns, 
                 weights = wts_tbl,
                 rebalance_on = "quarters",
                 col_rename   = "monthly.returns")

returns_m_portfolio_tbl

# 5.0 PERFORMANCE ANALYSIS - PORTFOLIO ----

returns_m_portfolio_merged_m_tbl <- returns_m_portfolio_tbl %>%
    add_column(symbol = "Portfolio", .before = 1) %>%
    bind_rows(returns_m_benchmark_tbl)

returns_m_portfolio_merged_m_tbl %>%
    group_by(symbol) %>%
    tq_performance(Ra = monthly.returns, 
                   performance_fun = SharpeRatio.annualized,
                   scale = 12)

# 6.0 OPTIMIZATION (Advanced) ----

# 6.1 Random Portfolio Weights ----
weight_iterator <- function(assets, iter = 100, seed = NULL) {
    
    n <- length(assets)
    
    if (!is.null(seed)) set.seed(seed)
    mtx <- matrix(runif(n = iter*n, min = 0, max = 1), nrow = 3)
    
    mtx_normalized <- mtx %*% diag(1/colSums(mtx))
    
    vectorized_output <- as.vector(mtx_normalized)
    
    return(vectorized_output)}

# Inputs
assets <- c("AAPL", "GOOG", "NFLX")
iter  <- 250

# Generate Random Portfolios - pretty cool
weights_tbl <- tibble(
        portfolio_id = rep(1:iter, each = length(assets)),
        symbol  = rep(assets, times = iter),
        weights = weight_iterator(assets, iter = iter, seed = 123) 
    ) %>%
    group_by(portfolio_id)

# 6.2 Calculate Performance for Every Portfolio ----
plan("multiprocess") # furrr
portfolio_optim_tbl <- weights_tbl %>% nest(.key = portfolio_weights) %>%
    
    # Map tq_portfolio() to nested weights
    mutate(portfolio_agg = future_map(portfolio_weights, ~ tq_portfolio(
        data = returns_m_components_tbl,
        assets_col  = symbol, 
        returns_col = monthly.returns,
        weights     = .x, # using data in the nested tibbles
        rebalance_on = "quarters"
    ))) %>%
    
    # Map tq_performance() to nested portfolio aggregations
    mutate(sharp_ratio = map(portfolio_agg, ~ tq_performance(
        data = .x,
        Ra = portfolio.returns,
        performance_fun = SharpeRatio.annualized,
        scale = 12
    ))) 

portfolio_optim_tbl

best_portfolio_tbl <- portfolio_optim_tbl %>% unnest(sharp_ratio) %>%
    filter(`AnnualizedSharpeRatio(Rf=0%)` == max(`AnnualizedSharpeRatio(Rf=0%)`)) 

best_portfolio_tbl %>% select(portfolio_id, `AnnualizedSharpeRatio(Rf=0%)`)

best_portfolio_tbl %>% pull(portfolio_weights) %>% pluck(1)


# 6.3 3D Plot: Visualize the Performance of All Portfolios ----
portfolio_optim_flattened_tbl <- portfolio_optim_tbl %>%
    select(-portfolio_agg) %>%
    unnest(sharp_ratio) %>%
    unnest(portfolio_weights) %>%
    spread(symbol, weights) %>%
    rename(SharpeRatio = `AnnualizedSharpeRatio(Rf=0%)`)

portfolio_optim_flattened_tbl

p <- portfolio_optim_flattened_tbl %>%
    plot_ly(x = ~AAPL, y = ~GOOG, z = ~NFLX, color = ~SharpeRatio,
            size = ~SharpeRatio, sizes = c(1, 30),
            marker = list(symbol = 'circle', sizemode = 'diameter'),
            text = ~str_glue("Sharpe Ratio: {SharpeRatio %>% round(3)}
                             AAPL: {AAPL %>% scales::percent()}
                             GOOG: {GOOG %>% scales::percent()}
                             NFLX: {NFLX %>% scales::percent()}"))

p
