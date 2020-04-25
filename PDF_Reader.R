library(pdftools)
library(tidyverse)

ePostal_Reader <- function(file_path) {
  Year <- stringr::str_extract(file_path, "\\d{4}")
  
  file <- pdftools::pdf_text(file_path)
  
  as_lines <- stringr::str_extract_all(file, "\n.*")
  as_lines_list_2 <- unlist(as_lines, recursive = TRUE)
  
  row_numbs <- seq(1, length(as_lines_list_2), 1)
  as_lines_list_2 <- paste(as_lines_list_2, row_numbs, sep = "  ")
  
  Gender = ifelse(any(stringr::str_detect(as_lines_list_2, "Men")), "M", "W")
  
  data_1 <- as_lines_list_2 %>%
    stringr::str_extract_all("\n\\s*\\d{1,3}.*") %>%
    unlist() %>%
    .[!purrr::map_lgl(., stringr::str_detect, "ePostal")] %>%
    .[!purrr::map_lgl(., stringr::str_detect, "First Name")] %>%
    .[!purrr::map_lgl(., stringr::str_detect, "Club")] %>%
    .[!purrr::map_lgl(., stringr::str_detect, "record")] %>%
    .[!purrr::map_lgl(., stringr::str_detect, "Record")] %>%
    stringr::str_replace_all("\n", "") %>%
    stringr::str_replace_all("1776", "1776 ") %>%
    stringr::str_replace_all("SHARK", "SHARK ") %>%
    trimws()
  
  data_1 <-
    unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
           recursive = FALSE)
  
  # unique(map(data_1, length))
  
  data_length_2 <- data_1[purrr::map(data_1, length) == 2]
  data_length_7 <- data_1[purrr::map(data_1, length) == 7]
  data_length_8 <- data_1[purrr::map(data_1, length) == 8]
  data_length_9 <- data_1[purrr::map(data_1, length) == 9]
  data_length_10 <- data_1[purrr::map(data_1, length) == 10]
  
  if (length(data_length_10) > 0) {
    df_10 <-
      as.data.frame(t(as.data.frame(data_length_10)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      dplyr::select(
        Age_Group = V1,
        Place = V2,
        First_Name = V3,
        Last_Name = V4,
        Age = V5,
        Club = V6,
        USMS_ID = V7,
        Distance = V8,
        National_Record = V9,
        Row_Numb = V10
      )
  } else {
    df_10 <- data.frame(
      Age_Group = character(),
      Place = character(),
      First_Name = character(),
      Last_Name = character(),
      Age = character(),
      Club = character(),
      USMS_ID = character(),
      Distance = character(),
      National_Record = character(),
      Row_Numb = character(),
      stringsAsFactors = FALSE
    )
  }
  
  if (length(data_length_9) > 0) {
    df_9 <-
      as.data.frame(t(as.data.frame(data_length_9)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      dplyr::select(
        Age_Group = V1,
        Place = V2,
        First_Name = V3,
        Last_Name = V4,
        Age = V5,
        Club = V6,
        USMS_ID = V7,
        Distance = V8,
        Row_Numb = V9
      )
    
  } else {
    df_9 <- data.frame(
      Age_Group = character(),
      Place = character(),
      First_Name = character(),
      Last_Name = character(),
      Age = character(),
      Club = character(),
      USMS_ID = character(),
      Distance = character(),
      National_Record = character(),
      Row_Numb = character(),
      stringsAsFactors = FALSE
    )
  }
  
  if (length(data_length_8) > 0) {
    df_8 <-
      as.data.frame(t(as.data.frame(data_length_8)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      dplyr::select(
        Place = V1,
        First_Name = V2,
        Last_Name = V3,
        Age = V4,
        Club = V5,
        USMS_ID = V6,
        Distance = V7,
        Row_Numb = V8
      )
    
  } else {
    df_8 <- data.frame(
      Age_Group = character(),
      Place = character(),
      First_Name = character(),
      Last_Name = character(),
      Age = character(),
      Club = character(),
      USMS_ID = character(),
      Distance = character(),
      National_Record = character(),
      Row_Numb = character(),
      stringsAsFactors = FALSE
    )
  }
  
  if (length(data_length_7) > 0) {
    df_7 <-
      as.data.frame(t(as.data.frame(data_length_7)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      dplyr::mutate(
        Club = stringr::str_split_fixed(V5, " ", n = 2)[, 1],
        USMS_ID = stringr::str_split_fixed(V5, " ", n = 2)[, 2]
      ) %>%
      dplyr::select(
        Place = V1,
        First_Name = V2,
        Last_Name = V3,
        Age = V4,
        Club,
        USMS_ID,
        Distance = V6,
        Row_Numb = V7
      )
    
  } else {
    df_7 <- data.frame(
      Age_Group = character(),
      Place = character(),
      First_Name = character(),
      Last_Name = character(),
      Age = character(),
      Club = character(),
      USMS_ID = character(),
      Distance = character(),
      National_Record = character(),
      Row_Numb = character(),
      stringsAsFactors = FALSE
    )
  }
  
  if (length(data_length_2) > 0) {
    df_2 <-
      as.data.frame(t(as.data.frame(data_length_2)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      dplyr::select(ID_Cut = V1,
                    Row_Numb = V2)
    
  } else {
    df_2 <- data.frame(
      Age_Group = character(),
      Place = character(),
      First_Name = character(),
      Last_Name = character(),
      Age = character(),
      Club = character(),
      USMS_ID = character(),
      Distance = character(),
      National_Record = character(),
      Row_Numb = character(),
      stringsAsFactors = FALSE
    )
  }
  
  data <- dplyr::full_join(df_10, df_9) %>%
    dplyr::full_join(df_8) %>%
    dplyr::full_join(df_7) %>%
    dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
    dplyr::arrange(Row_Numb) %>%
    dplyr::mutate(
      USMS_ID = dplyr::case_when(
        stringr::str_detect(USMS_ID, "-") == FALSE &
          stringr::str_detect(USMS_ID, "[:lower:]") == FALSE ~ paste(df_2$ID_Cut, USMS_ID, sep = ""),
        TRUE ~ USMS_ID
      )
    ) %>%
    tidyr::fill(Age_Group, .direction = "down") %>%
    dplyr::mutate(National_Record = dplyr::case_when(is.na(National_Record) == TRUE ~ "N",
                                                     TRUE ~ National_Record)) %>%
    dplyr::mutate(
      Place = as.numeric(Place),
      Age = as.numeric(Age),
      Distance = stringr::str_replace(Distance, ",", ""),
      Distance = as.numeric(Distance),
      Gender = Gender,
      Year = as.numeric(Year)
    ) %>%
    dplyr::select(-Row_Numb)
  
  return(data)
}

Mens_2020 <- ePostal_Reader("inst/extdata/raw_data/2020 OHEP Mens Final Results.pdf")
Womens_2020 <- ePostal_Reader("inst/extdata/raw_data/2020 OHEP Womens Final Results.pdf")

df_2020 <- bind_rows(Mens_2020, Womens_2020) %>% 
  mutate(Name = paste(First_Name, Last_Name, sep = " ")) %>% 
  select(Place, Name, Age, Club, Distance, USMS_ID, Gender, Year, National_Record)


write.csv(df_2020, row.names = FALSE, "Postal_2020.csv")
