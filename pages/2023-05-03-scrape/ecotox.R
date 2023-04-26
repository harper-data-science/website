## HEADER ####
## who: EdH
## what: scraping ecotox for ben and joe
## when: last edited 2023-04-25

## CONTENTS ####
## 00 Setup
## 01 Stuff

# 00 Setup ####

# install.packages("rvest")
# install.packages("httr")
library(rvest)
library(httr)

# 01 Stuff ####

# Replace with the actual URL
url <- "https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/373.htm"  

response <- httr::GET(url)

html_content <- as.character(page)

# Parse the HTML content
page <- read_html(response$content)

# Extract the Terrestrial ecotoxicology table
ecotoxicology_table1 <- page %>%
  html_nodes(xpath = '//td[contains(text(), "Terrestrial ecotoxicology")]/ancestor::table[1]/following-sibling::table[1]') %>%
  html_table()

# Print the extracted table
print(ecotoxicology_table1[[1]])

# Extract the Aquatic ecotoxicology table
ecotoxicology_table2 <- page %>%
  html_nodes(xpath = '//td[contains(text(), "Aquatic ecotoxicology")]/ancestor::table[1]/following-sibling::table[1]') %>%
  html_table()

# Print the extracted table
print(ecotoxicology_table2[[1]])


