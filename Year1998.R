library(tidyverse)

X1998_2 <- readr::read_delim("inst/extdata/1998 OHEP Results.txt",
"\t", escape_double = FALSE, col_names = FALSE,
trim_ws = TRUE)


df <- X1998_2 %>%
  dplyr::mutate(Name = stringr::str_extract(X2, "[A-Za-z [:punct:]]*"),
                Name = trimws(Name)) %>%
  dplyr::mutate(Age = dplyr::case_when(
    stringr::str_detect(X2, "\\d+") == TRUE ~ stringr::str_extract(X2, "\\d+"),
    stringr::str_detect(X3, "\\d{2}") == TRUE ~ stringr::str_extract(X3, "\\d{2}")
  )) %>%
  dplyr::mutate(Distance = case_when(
    stringr::str_detect(X3, "\\d{3,}") == TRUE ~ stringr::str_extract(X3, "\\d{3,}"),
    stringr::str_detect(X4, "\\d{3,}") == TRUE ~ stringr::str_extract(X4, "\\d{3,}")
  )) %>%
  dplyr::mutate(Club = case_when(stringr::str_detect(X4, "^[:alpha:]") == TRUE ~ X4,
                                 stringr::str_length(X5) >= 2 ~ X5)) %>%
  dplyr::mutate(Gender = case_when(stringr::str_detect(X5, "^[:upper:]$") == TRUE ~ X5,
                                   stringr::str_detect(X6, "^[:upper:]$") == TRUE ~ X6,
                                   TRUE ~ "")) %>%
  dplyr::mutate(Year = stringr::str_extract(X6, "\\d{4}")) %>% 
  dplyr::select(Place = X1,
                Name,
                Age,
                Distance,
                Club,
                Gender,
                Year) %>% 
  dplyr::na_if("") %>% 
  tidyr::fill(Year, .direction = "down") %>% 
  tidyr::fill(Gender, .direction = "down")

X1999 <- readr::read_delim("inst/extdata/1999 OHEP Results.txt",
                             "\t", escape_double = FALSE, col_names = FALSE,
                             trim_ws = TRUE)


df_1999 <- X1999 %>%
  dplyr::mutate(Name = stringr::str_extract(X2, "[A-Za-z [:punct:]]*"),
                Name = trimws(Name)) %>%
  dplyr::mutate(Age = dplyr::case_when(
    stringr::str_detect(X2, "\\d+") == TRUE ~ stringr::str_extract(X2, "\\d+"),
    stringr::str_detect(X3, "\\d{2}") == TRUE ~ stringr::str_extract(X3, "\\d{2}")
  )) %>%
  dplyr::mutate(Distance = case_when(
    stringr::str_detect(X3, "\\d{3,}") == TRUE ~ stringr::str_extract(X3, "\\d{3,}"),
    stringr::str_detect(X4, "\\d{3,}") == TRUE ~ stringr::str_extract(X4, "\\d{3,}"),
    stringr::str_detect(X5, "\\d{3,}") == TRUE ~ stringr::str_extract(X5, "\\d{3,}"),
  )) %>%
  dplyr::mutate(Club = X4) %>%
  dplyr::mutate(Gender = X6) %>%
  dplyr::mutate(Year = X7) %>% 
  dplyr::select(Place = X1,
                Name,
                Age,
                Distance,
                Club,
                Gender,
                Year) %>% 
  dplyr::na_if("") %>% 
  tidyr::fill(Year, .direction = "down") %>% 
  tidyr::fill(Gender, .direction = "down")
