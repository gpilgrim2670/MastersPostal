library(tidyverse)

X2005 <- readr::read_delim("inst/extdata/2010 OHEP Womens Results.txt",
                           "\t", escape_double = FALSE, col_names = FALSE,
                           trim_ws = TRUE)
df_2005 <- X2005 %>% 
  mutate(X1 = str_replace(X1, "(\\d{1,3})\\.\\s", "\\1 ")) %>% 
  mutate(National_Record = case_when(str_detect(X1, "\\*NR") == TRUE ~ "Y",
                                     TRUE ~ "N"),
         X1 = str_replace(X1, "\\*NR", ""),
         X1 = trimws(X1)) %>% 
  mutate(Place = str_extract(X1, "^\\d+"),
         X1 = str_replace(X1, Place, ""),
         X1 = trimws(X1)) %>% 
  mutate(Distance = sapply(str_split(X1, " "), tail, 1),
         X1 = str_replace(X1, Distance, ""),
         X1 = trimws(X1)) %>% 
  mutate(Age = sapply(str_split(X1, " "), tail, 1),
         X1 = str_replace(X1, Age, ""),
         X1 = trimws(X1)) %>% 
  mutate(Club = sapply(str_split(X1, " "), tail, 1),
         X1 = str_replace(X1, Club, ""),
         X1 = trimws(X1)) %>% 
  mutate(Name = X1) %>% 
  select(Place,
         Name,
         Age,
         Distance,
         Club,
         Gender = X2,
         Year = X3,
         National_Record) %>% 
  dplyr::na_if("") %>% 
  tidyr::fill(Year, .direction = "down") %>% 
  tidyr::fill(Gender, .direction = "down")

Style_2004_Read <- function (x) {
  df <- readr::read_delim(x,
                             "\t", escape_double = FALSE, col_names = FALSE,
                             trim_ws = TRUE)
  df <- df %>% 
    mutate(X1 = str_replace(X1, "(\\d{1,3})\\.\\s", "\\1 ")) %>% 
    mutate(National_Record = case_when(str_detect(X1, "\\*NR") == TRUE ~ "Y",
                                       TRUE ~ "N"),
           X1 = str_replace(X1, "\\*NR", ""),
           X1 = trimws(X1)) %>% 
    mutate(Place = str_extract(X1, "^\\d+"),
           X1 = str_replace(X1, Place, ""),
           X1 = trimws(X1)) %>% 
    mutate(Distance = sapply(str_split(X1, " "), tail, 1),
           X1 = str_replace(X1, Distance, ""),
           X1 = trimws(X1)) %>% 
    mutate(Age = sapply(str_split(X1, " "), tail, 1),
           X1 = str_replace(X1, Age, ""),
           X1 = trimws(X1)) %>% 
    mutate(Club = sapply(str_split(X1, " "), tail, 1),
           X1 = str_replace(X1, Club, ""),
           X1 = trimws(X1)) %>% 
    mutate(Name = X1) %>% 
    select(Place,
           Name,
           Age,
           Distance,
           Club,
           Gender = X2,
           Year = X3,
           National_Record) %>% 
    dplyr::na_if("") %>% 
    tidyr::fill(Year, .direction = "down") %>% 
    tidyr::fill(Gender, .direction = "down")
  
  return(df)
}

data_dir <- "C:/Users/gpilgrim/Documents/MastersPostal/inst/extdata/2004_Style"
files <- fs::dir_ls(data_dir, regexp = "\\.txt$")

df_2004 <- map(files, Style_2004_Read)

df_2004 <- bind_rows(df_2004)

Style_2004_Read("inst/extdata/2004_Style/2007 OHEP Womens Results.txt")
