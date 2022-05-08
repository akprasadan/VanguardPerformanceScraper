#' Merge in price data into complete portfolio history.
#'
#' @description
#' Once the user has generated the complete balance history using
#' [clean_transaction_export()] followed by [complete_history()],
#' we obtain a dataset with the holdings for every single day between
#' the first and last transaction in the raw dataset.
#'
#' This function scrapes the price of each stock using the BatchGetSymbols
#' library and then computes the balance value for each ticker in each account.
#'
#' @param input_balance_df The complete balance_history df.
#' @export
#' @return The full price history of each holdings in the portfolios.
#' @examples
#' \dontrun{}
#' # Vanguard exports the file 'ofxdownload.csv' which we save as
#' # 'raw_data.xlsx.'
#'
#' cleaned_df = clean_transaction_export('raw_data.xlsx')
#' complete_df = complete_history(cleaned_df)
#'
#' price_history_df = prepare_price_history(complete_df)
prepare_price_history <- function(input_balance_df) {
  balance_df <- input_balance_df %>%
    mutate(stock = as.factor(stock))
  start_date <- min(balance_df$date)
  end_date <- max(balance_df$date)
  all_stocks <- levels(balance_df$stock)
  cache_folder_path = 'cache_folder'
  cache_home = '.'
  dir.create(file.path(cache_home, cache_folder_path), showWarnings = FALSE)
  ticker_df <-
    BatchGetSymbols::BatchGetSymbols(
      tickers = all_stocks,
      first.date = start_date,
      last.date = end_date,
      freq.data = 'daily',
      cache.folder = cache_folder_path
    )$df.tickers %>% select(price.close, ref.date, stock = ticker)


  price_history <-
    balance_df %>% left_join(ticker_df,
                             by = c("stock" = "stock", "date" = "ref.date")) %>%
    mutate(stock = as.factor(stock),
           market_value = price.close * count) %>%
    filter(!is.na(price.close))

  return(price_history)
}

#' Compute portfolio wide performance history for each account
#'
#' @description
#' Once the user has generated the complete price history
#' for each holding in each account, using [prepare_price_history()],
#' this function aggregates each day's holdings to compute total
#' portfolio value and cost basis over time.
#'
#' @param processed_price_df The tibble of complete price history
#' @export
#' @return The full portfolio-level history of for each account.
#' @examples
#' \dontrun{}
#' # Vanguard exports the file 'ofxdownload.csv' which we save as
#' # 'raw_data.xlsx.'
#'
#' cleaned_df = clean_transaction_export('raw_data.xlsx')
#' complete_df = complete_history(cleaned_df)
#'
#' price_history_df = prepare_price_history(complete_df)
#' prepare_return_data(price_history_df)
prepare_return_data <- function(processed_price_df) {

  aggregate_daily_history <-
    processed_price_df %>% group_by(account, date) %>% summarize(
      daily_market_value = sum(market_value),
      daily_cost_basis = sum(cost_basis),
      .groups = "drop"
    ) %>%
    mutate(profit = daily_market_value - daily_cost_basis) %>%
    filter(daily_cost_basis > 0) %>%
    pivot_longer(cols = daily_market_value:profit,
                 names_to = "metric",
                 values_to = 'amount')

  return(aggregate_daily_history)
}

generate_ticker_plots <- function(){
  nb.cols <- 10
  mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(nb.cols)


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
}

#' Compute the Modified Dietz return of a portfolio.
#'
#' @description
#' The Modified Dietz return is a measure of performance for a portfolio.
#' It takes an approach in between a purely money-weighted and purely
#' time-weighted rate of return. In the former, one ignores *when* contributions
#' are made and only analyzes the net gains, while a
#' time-weighted rate of returns
#' takes into accounts the timing of investments. The implementation is based on
#' that used by the package 'spudr.'
#'
#' @param aggregate_data The portfolio level daily balance.
#' @param account_type Which account to analyze (account1, account2, etc.)
#' @param start_date The start date from which to calculate performance
#' @param end_date The date up to which performance is desired.
#' @export
#' @return The Modified Dietz Return
modified_dietz <- function(aggregate_data, account_type,
                           start_date, end_date) {
  account_aggregated_df = aggregate_data %>%
    filter(account == account_type,
           date >= start_date,
           date <= end_date) %>%
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
