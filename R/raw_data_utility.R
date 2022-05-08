

#' Cleans the raw Vanguard transaction history.
#'
#' @description
#' Vanguard permits a user to download the transaction history for one or more
#' of the accounts that user owns. This function cleans up this file by removing
#' redundant information and calculating cumulative quantities over time.
#' To do so, this function removes the cash holdings and transactions between
#' banks and the settlement fund--only transactions of mutual funds
#' (excluding money market accounts), ETFs, stocks, and bonds
#' are tracked. Dividends and capital gains that get reinvested are also
#' retained. But the raw transaction history records both the accumulation of the
#' dividend and its reinvestment, so we retain only the reinvestment
#' (recorded with a minus sign, like is done when buying a share of a stock).
#'
#' No edits should be made to the contents of the excel file Vanguard exports.
#' If it is in .csv format, convert to .xlsx first.
#'
#' @param filename A string corresponding to the filepath of the
#'                exported Vanguard data (.xlsx format).
#' @export
#' @return The cleaned transaction history tibble
#' @examples
#' \dontrun{}
#' # Vanguard exports the file 'ofxdownload.csv' which we save as
#' # 'raw_data.xlsx.'
#'
#' clean_transaction_export('ofxdownload.xlsx')
clean_transaction_export <- function(filename) {
  # Read in the data to identify extraneous lines
  raw = readxl::read_excel(filename)
  header_skip = which(raw['Account Number'] == 'Account Number')

  raw_df =
    readxl::read_excel(filename, skip = header_skip) %>%
    janitor::clean_names() %>%
    select(account_number:transaction_type,
           symbol:share_price,
           net_amount) %>%
    mutate(
      account_number = as.factor(account_number),
      transaction_type = as.factor(transaction_type),
      symbol = as.factor(symbol)
    ) %>%
    # Generate arbitrary integer numberings to replace the account numbers
    mutate(account_number = fct_anon(account_number, prefix = "account")) %>%
    # Remove the bank transfers, which have no symbol
    filter(!is.na(symbol)) %>%
    # Omit duplicate or irrelevant transaction types
    filter(!(
      transaction_type %in% c(
        "Sweep in",
        "Sweep out",
        "Funds Received",
        "Dividend",
        "Contribution",
        "Capital gain (LT)",
        "Capital gain (ST)"
      )
    )) %>%
    mutate(
      trade_date = lubridate::date(trade_date),
      settlement_date = lubridate::date(settlement_date)
    ) %>%
    # Now view investments as positive quantities (assets)
    mutate(net_amount = -net_amount) %>%
    group_by(symbol, account_number) %>%
    arrange(trade_date) %>%
    mutate(
      # Total number of shares at this date
      share_sum = cumsum(shares),

      # Total cost basis at this date
      share_cumulative_cost_basis = cumsum(net_amount)
    ) %>%
    ungroup()

  return(raw_df)
}
