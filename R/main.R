

#' Generate all output tibbles produced from raw import data.
#'
#' @description Runs all the processing scripts at once for quick data generation.
#'
#' @param input_filename The path to the exported Vanguard data
#' @export
#' @return A list of tibbles used for analysis and plotting.
#'  \itemize{
#'   \item processed_raw - A cleaned tibble of all transactions.
#'   \item balance_df - A day level history of each holding's share count and
#'                       cost basis.
#'   \item price_history - A daily history of each individual holding's share count,
#'                        current value, and cost_basis.
#'   \item portfolio_history - A portfolio-level daily balance history.
#' }
generate_portfolio <- function(input_filename) {
  processed_raw = clean_transaction_export(input_filename)
  balance_df = complete_history(processed_raw)
  price_history = prepare_price_history(balance_df)
  portfolio_history = prepare_return_data(price_history)

  result_dfs = list(processed_raw = processed_raw,
                    balance_df = balance_df,
                    price_history = price_history,
                    portfolio_history = portfolio_history)

  return(result_dfs)
}
