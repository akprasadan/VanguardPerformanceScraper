#' Generate complete historical data from cleaned transaction history.
#'
#' @description
#' Once the exported Vanguard data has been initially cleaned with
#' [clean_transaction_export()], we must interpolate the
#' holdings for all days, even if no transactions were recorded.
#' This function forms the complete time series for each stock and
#' Vanguard account. This will allow us to calculate the daily
#' portfolio balance once we merge in price data.
#'
#' @param cleaned_raw_df A cleaned transaction history tibble.
#' @return Complete portfolio history
#' @export
#' @examples
#' \dontrun{}
#' # Vanguard exports the file 'ofxdownload.csv' which we save as
#' # 'raw_data.xlsx.'
#'
#' cleaned_df = clean_transaction_export('raw_data.xlsx')
#' complete_df = complete_history(cleaned_df)
complete_history <- function(cleaned_raw_df) {
  processed_transaction = cleaned_raw_df
  all_stocks <- levels(processed_transaction$symbol)
  all_accounts <- levels(processed_transaction$account_number)

  # Store each day's tibble of data in a list, before combining.
  daily_list <- list()

  start_date <-
    min(processed_transaction$trade_date)
  end_date <-
    max(processed_transaction$trade_date)

  all_dates <-  seq(as.Date(start_date),
                    to = as.Date(end_date),
                    by = "day")

  combined_holdings <- processed_transaction %>%
    group_by(account_number) %>%
    complete(nesting(account_number, symbol),
             trade_date = seq(start_date, end_date, by = "day")) %>%
    ungroup() %>%
    group_by(account_number, symbol, trade_date) %>%
    slice(n()) %>%
    ungroup() %>%
    group_by(account_number, symbol) %>%
    fill(share_sum, share_cumulative_cost_basis, .direction = "down") %>%
    ungroup() %>%
    replace_na(list(
      share_sum = 0,
      share_cumulative_cost_basis = 0
    )) %>%
    ungroup() %>%
    select(symbol,
           trade_date,
           account_number,
           share_sum,
           share_cumulative_cost_basis) %>%
    rename(
      stock = symbol,
      date = trade_date,
      account = account_number,
      count = share_sum,
      cost_basis = share_cumulative_cost_basis
    )

  return(combined_holdings)
}
