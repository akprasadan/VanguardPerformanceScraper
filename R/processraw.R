#' This script cleans the exported Vanguard data and exports it to a csv file.
#' It may take a minute or two to run.


#' Import, clean, and export transaction data for analysis.
#'
#' @param filename A string specifying the path to the exported data
#' from Vanguard (.xlsx file format).
main <- function(filename) {
  processed_transaction = clean_transaction_export(filename)
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
    replace_na(list(share_sum = 0, share_cumulative_cost_basis = 0)) %>%
    ungroup() %>%
    select(symbol, trade_date, account_number, share_sum, share_cumulative_cost_basis) %>%
    rename(stock = symbol, date = trade_date, account = account_number, count = share_sum,
           cost_basis = share_cumulative_cost_basis)

  readr::write_csv(test_combined_holdings, 'data/balancehistory.csv')
}
main('data/ofxdownload.xlsx')

