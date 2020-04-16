library(rvest)

pg <- read_html("https://www.usms.org/longdist/ldnats00/1hrtoc.php")

links <- html_attr(html_nodes(pg, "a"), "href")
links_good <- links %>%
  .[map_lgl(., str_detect, "1hr")] %>%
  .[!map_lgl(., str_detect, "1hrr")] %>%
  .[!map_lgl(., str_detect, "1hrclub")]

links_good <- links_good[is.na(links_good) == FALSE]
links_good <- paste0("https://www.usms.org/longdist/ldnats00/", links_good)

Scrape_2000 <- function(link) {
  webpage <- read_html(link)
  html <- rvest::html_nodes(webpage, css = "td")
  results <- rvest::html_text(html)
  
  results <- str_replace(results, "\\\r\\\n", "")
  results <- results[results != ""]
  results <- results[str_detect(results, "^\\s$") == FALSE]
  results <- results[results != "\r\n"]
  
  First_Name <- results[seq(5, length(results), 5)]
  Last_Name <- results[seq(6, length(results), 5)]
  Age <- results[seq(7, length(results), 5)]
  Distance <- results[seq(8, length(results), 5)]
  Club <- results[seq(9, length(results), 5)]
  
  df_results <-
    data.frame(First_Name, Last_Name, Age, Distance, Club, stringsAsFactors = FALSE)
  
  return(df_results)
}




df_1 <- map(links_good, Scrape_2000)
Scrape_2000(links_good[5])

names(df_1) <- links_good
df_2000 <- data.table::rbindlist(df_1, idcol = "Source")

df_2000 %>% 
  mutate(Year = "2000",
         Gender = case_when(str_detect(Source, "1hrm") == TRUE ~ "Men",
                            str_detect(Source, "1hrf") == TRUE ~ "Women"),
         Source = NULL) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2000.csv", row.names = FALSE)

# webpage <- read_html(links_good[5])
# html <- rvest::html_nodes(webpage, css = "td")
# results <- rvest::html_text(html)
# 
# results <- str_replace(results, "\\\r\\\n", "")
# results <- results[results != ""]
# results <- results[results != space_string]
# results <- results[results != "\r\n"]
# 
# First_Name <- results[seq(5, length(results), 5)]
# Last_Name <- results[seq(6, length(results), 5)]
# Age <- results[seq(7, length(results), 5)]
# Distance <- results[seq(8, length(results), 5)]
# Club <- results[seq(9, length(results), 5)]
# 
# df_results <-
#   data.frame(First_Name, Last_Name, Age, Distance, Club, stringsAsFactors = FALSE)
# 
# 
# links_good[12]
# 
# results[605] == " "
