# Install necessary packages if not already installed
install.packages(c("httr", "rvest", "jsonlite"))

# Load the packages
library(httr)
library(rvest)
library(jsonlite)

useragent = "ndriu9711@gmail.com"

# Function to retrieve EDGAR documents
get_edgar_documents <- function(ticker, form_types = c("497", "485BPOS")) {
  # Base URL for the EDGAR full-text search
  base_url <- "https://www.sec.gov/cgi-bin/browse-edgar"
  
  # Construct the search URL
  search_url <- paste0(base_url, "?action=getcompany&CIK=", ticker, "&type=&dateb=&owner=exclude&start=0&count=100&output=atom")
  
  # Make the request to the EDGAR search API
  response <- httr::GET(search_url)
  
  # Debug: Inspect search URL and response
  print(search_url)
  print(response)
  
  # Parse the response
  content <- httr::content(response, as = "text")
  parsed_content <- read_html(content)
  
  # Debug: Inspect parsed content
  print(parsed_content)
  
  # Extract the document links and their types
  entries <- parsed_content %>% html_nodes("entry")
  
  # Debug: Inspect entries
  print(entries)
  
  documents <- lapply(entries, function(entry) {
    form_type <- entry %>% html_node("category") %>% html_attr("term")
    if (form_type %in% form_types) {
      link <- entry %>% html_node("link") %>% html_attr("href")
      list(form_type = form_type, link = link)
    } else {
      NULL
    }
  })
  
  documents <- Filter(Negate(is.null), documents)
  
  # Debug: Inspect documents
  print(documents)
  
  return(documents)
}

# Function to extract benchmark information from the document
extract_benchmark <- function(url) {
  # Fetch the document content
  response <- httr::GET(url)
  content <- httr::content(response, as = "text")
  parsed_content <- read_html(content)
  
  # Debug: Inspect document URL and parsed content
  print(url)
  print(parsed_content)
  
  # Example of parsing logic to find benchmark information
  # (This needs to be tailored to the specific structure of the forms)
  benchmark_info <- parsed_content %>%
    html_nodes(xpath = "//text()[contains(., 'benchmark')]/following-sibling::text()") %>%
    html_text() %>%
    trimws()
  
  # Debug: Inspect extracted benchmark info
  print(benchmark_info)
  
  return(benchmark_info)
}

# Main function to get the benchmark of the mutual fund
get_fund_benchmark <- function(ticker) {
  documents <- get_edgar_documents(ticker)
  
  # Debug: Inspect documents list
  print(documents)
  
  benchmarks <- sapply(documents, function(doc) {
    extract_benchmark(doc$link)
  })
  
  # Debug: Inspect extracted benchmarks
  print(benchmarks)
  
  # Filter out empty results and return unique benchmark names
  benchmarks <- unique(Filter(nzchar, benchmarks))
  
  # Debug: Inspect final benchmarks
  print(benchmarks)
  
  return(benchmarks)
}

# Use the function to get the benchmark for the ticker FCNTX
ticker <- "FCNTX"
benchmarks <- get_fund_benchmark(ticker)
print(benchmarks)
