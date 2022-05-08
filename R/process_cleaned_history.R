#' This script takes in the cleaned transaction history and
#' joins in the price data.
#'
#'
nb.cols <- 10
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(nb.cols)

balance_df_old <-
  readr::read_csv('data/balancehistoryold.csv') %>%
  mutate(stock = as.factor(stock))

balance_df_new <-
  readr::read_csv('data/balancehistorynew.csv') %>%
  mutate(stock = as.factor(stock)) %>% 
  mutate(cost_basis = round(cost_basis, digits = 5),
         count = round(count, digits = 5))

# Check correctness
all_taxable_stocks = 
  unique(balance_df_new[balance_df_new$account == 'account2', ]$stock)
all_nontaxable_stocks = 
  unique(balance_df_new[balance_df_new$account == 'account1', ]$stock)
for (ticker in all_taxable_stocks) {
  df1 = balance_df_new %>% filter(stock == ticker, account == 'account2')
  df2 = balance_df_old %>% filter(stock == ticker, account == 'account2')
  if (all_equal(df1, df2) != T) {
    print(all_equal(df1, df2))
    print(nrow(df2))
    print(nrow(df1))
    print(ticker)
  }
}

start_date <- min(balance_df$date)
end_date <- max(balance_df$date)
all_stocks <- levels(balance_df$stock)

ticker_df <-
  BatchGetSymbols::BatchGetSymbols(
    tickers = all_stocks,
    first.date = start_date,
    last.date = end_date,
    freq.data = 'daily'
  )$df.tickers %>% select(price.close, ref.date, stock = ticker)


price_history <-
  balance_df %>% left_join(ticker_df,
                           by = c("stock" = "stock", "date" = "ref.date")) %>%
  mutate(stock = as.factor(stock),
         market_value = price.close * count) %>%
  filter(!is.na(price.close))


price_history %>%
  filter(account == 'account1') %>%
  group_by(stock) %>%
  mutate(final_value = last(market_value)) %>%
  ungroup() %>%
  mutate(stock = fct_lump_n(stock, n = 10, w = final_value)) %>%
  group_by(stock, date) %>%
  ungroup() %>%
  filter(stock != 'Other') %>%
  filter(cost_basis > 0) %>%
  ggplot(aes(x = date, y = market_value, group = stock)) +
  geom_line(aes(color = stock), size = 1.5, alpha = 0.6) +
  geom_vline(
    xintercept = as.Date('2022-01-01'),
    alpha = 0.5,
    linetype = 'dotted'
  ) +
  labs(
    x = "",
    y = "Cost Basis",
    title = "Top 10 Asset Balances Over Time",
    color = 'Stock'
  ) +
  scale_y_log10() +
  theme_light() +
  scale_color_manual(values = mycolors) +
  scale_x_date(labels = scales::date_format("%b %y"),
               date_breaks = "1 month")


aggregate_daily_history <-
  price_history %>% group_by(account, date) %>% summarize(
    daily_market_value = sum(market_value),
    daily_cost_basis = sum(cost_basis),
    .groups = "drop"
  ) %>%
  mutate(profit = daily_market_value - daily_cost_basis) %>%
  filter(daily_cost_basis > 0) %>%
  pivot_longer(cols = daily_market_value:profit,
               names_to = "metric",
               values_to = 'amount') 

modified_dietz <- function(aggregate_data, account_type) {
  account_aggregated_df = aggregate_data %>% 
    filter(account == account_type) %>% 
    mutate(days_since_start = as.numeric(date - first(date)))

  market_values = account_aggregated_df %>% 
    filter(metric == 'daily_market_value') %>% 
    .$amount
  
  start_value = first(market_values)
  last_value = last(market_values)
  
  contribution_history = account_aggregated_df %>%
    filter(metric == 'daily_cost_basis') %>%
    mutate(lagged_amount = lag(amount, default = 0)) %>% 
    mutate(contribution = amount - lagged_amount) %>% 
    filter(contribution != 0)
  
  contribution_days = contribution_history$days_since_start[-1]
  contribution_amounts = contribution_history$contribution[-1]

  total_days = last(account_aggregated_df$days_since_start) - 
    first(account_aggregated_df$days_since_start) + 1

  dietz_return = mdietz(start_value,
                        last_value,
                        contribution_amounts,
                        contribution_days,
                        total_days)
  
  output = list(full_portfolio_df = account_aggregated_df,
                portfolio_value = market_values,
                contribution_amounts = contribution_amounts,
                contribution_day = contribution_days,
                portfolio_start_value = start_value,
                portfolio_last_value = last_value,
                portfolio_age = total_days,
                dietz_return = dietz_return)
  
  return(output)
}
