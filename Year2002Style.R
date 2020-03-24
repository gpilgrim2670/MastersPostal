library(tidyverse)

Style_2002_Read <- function(x) {
  df <- readr::read_delim(
    x,
    "\t",
    escape_double = FALSE,
    col_names = FALSE,
    trim_ws = TRUE
  )
  df <- df %>%
    mutate(Place = str_extract(X1, "^\\d+"),
           X1 = str_replace(X1, Place, ""),
           X1 = trimws(X1)) %>%
    mutate(Name = trimws(str_extract(X1, "[A-Za-z [:punct:]]+")),
           X1 = str_replace(X1, Name, ""),
           X1 = trimws(X1)) %>%
    mutate(Age = str_extract(X1, "\\d{2,3}"),
           X1 = str_replace(X1, Age, ""),
           X1 = trimws(X1)) %>%
    mutate(Distance = str_extract(X1, "[\\d,]+"),
           X1 = str_replace(X1, Distance, ""),
           X1 = trimws(X1)) %>%
    mutate(Club = X1,
           Club = trimws(Club)) %>%
    select(Place,
           Name,
           Age,
           Distance,
           Club,
           Gender = X2,
           Year = X3) %>%
    dplyr::na_if("") %>%
    tidyr::fill(Year, .direction = "down") %>%
    tidyr::fill(Gender, .direction = "down")
  
  return(df)
  
}

data_dir <- "C:/Users/gpilgrim/Documents/MastersPostal/inst/extdata/2002_Style"
files <- fs::dir_ls(data_dir, regexp = "\\.txt$")

df_2002 <- map(files, Style_2002_Read)

df_2002 <- bind_rows(df_2002)

Style_2002_Read("C:/Users/gpilgrim/Documents/MastersPostal/inst/extdata/2002_Style/2007 OHEP Womens Results.txt")
