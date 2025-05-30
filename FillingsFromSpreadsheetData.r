# Install required packages if not already installed
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("XBRL", quietly = TRUE)) install.packages("XBRL")
if (!requireNamespace("edgar", quietly = TRUE)) install.packages("edgar")
if (!requireNamespace("edgarWebR", quietly = TRUE)) install.packages("edgarWebR")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")



library(httr)
library(dplyr, quietly = TRUE)
library(purrr, quietly = TRUE)
library(ggplot2)
library(xml2)
library(edgarWebR)
library(edgar)
library(tidyverse)
library(httr)
library(XBRL)
library(readxl)


data <- read_csv("alltickers.csv")


# View the first few rows of the data
head(data)

# Extract the first column
first_column <- data[[1]]



# Set user agent globally for httr
user_agent_string <- "peterqhou@gmail.com"            ## <- USE YOUR EMAIL HERE

## SET UP THE FUNCTION TO EXTRACT THE CIK NUMBER FROM TICKER
httr::set_config(httr::user_agent(user_agent_string))

# Function to get CIK from ticker
get_cik_from_ticker <- function(ticker) {
  url <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?CIK=", ticker, "&action=getcompany&output=atom")
  response <- httr::GET(url, httr::user_agent(user_agent_string))
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch CIK for ticker: ", ticker)
  }
  content <- httr::content(response, as = "text")
  cik <- stringr::str_extract(content, "(?<=CIK=)\\d+")
  if (is.na(cik)) {
    stop("CIK not found for ticker: ", ticker)
  }
  return(cik)
}


# Loop to download first 10 elements of the first column
for (i in 2:10) {
  ticker <- first_column[i]
# Get CIKs for the tickers
  ciks <- ticker %>% map_chr(~ get_cik_from_ticker(.x)) %>% na.omit()
  # Convert to numeric (Important to use later in the function)
  CIK <- as.numeric(ciks)
  print(CIK)
  print(ticker)
  # Check if we have valid CIKs
  if (length(ciks) == 0) stop("No valid CIKs found for the provided tickers.")
  
  ## GET FILINGS  Using EdgarWeb (First is for Text, second is for HTML Format)
  # Suppress messages and warnings from the functions
  suppressMessages({
    suppressWarnings({
      output <- getFilings(cik.no = c(CIK), form.type = c('497K', '485BPOS'),
                           filing.year = c(2024, 2023), quarter = c(1, 2, 3, 4))
      
      output2 <- getFilingsHTML(cik.no = c(CIK), form.type = c('497K', '485BPOS'),
                                filing.year = c(2024, 2023), quarter = c(1, 2, 3, 4))
    })
  })
  
}
