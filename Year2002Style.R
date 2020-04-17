library(tidyverse)
# 
# Style_2002_Read <- function(x) {
#   # df <- readr::read_delim(
#   #   x,
#   #   "\t",
#   #   escape_double = FALSE,
#   #   col_names = FALSE,
#   #   trim_ws = TRUE
#   # )
#   df <- df %>%
#     mutate(Place = str_extract(X1, "^\\d+"),
#            X1 = str_replace(X1, Place, ""),
#            X1 = trimws(X1)) %>%
#     mutate(Name = trimws(str_extract(X1, "[A-Za-z [:punct:]]+")),
#            X1 = str_replace(X1, Name, ""),
#            X1 = trimws(X1)) %>%
#     mutate(Age = str_extract(X1, "\\d{2,3}"),
#            X1 = str_replace(X1, Age, ""),
#            X1 = trimws(X1)) %>%
#     mutate(Distance = str_extract(X1, "[\\d,]+"),
#            X1 = str_replace(X1, Distance, ""),
#            X1 = trimws(X1)) %>%
#     mutate(Club = X1,
#            Club = trimws(Club)) %>%
#     select(Place,
#            Name,
#            Age,
#            Distance,
#            Club,
#            Gender = X2,
#            Year = X3) %>%
#     dplyr::na_if("") %>%
#     tidyr::fill(Year, .direction = "down") %>%
#     tidyr::fill(Gender, .direction = "down") %>% 
#     tidyr::fill(Place, .direction = "down")
#   
#   return(df)
#   
# }
# 
# data_dir <- "inst/extdata/2002_Style"
# files <- fs::dir_ls(data_dir, regexp = "\\.txt$")

### 2001 ###

X2001 <- readr::read_delim("inst/extdata/2001 OHEP Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                           trim_ws = TRUE)

X2001 %>% 
  mutate(X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2001 OHEP Results.csv", col_names = FALSE)

X2001 <- readr::read_csv("inst/extdata/2002_Style/2001 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2001 %>% 
  separate(X1, into = , c("Name", "Age", "Distance", "Club", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  tidyr::fill(Gender, .direction = "down") %>% 
  # tidyr::fill(Place, .direction = "down") %>% 
  mutate(Age_Group = case_when(
    Age <= 24 ~ "18-24",
    Age > 24 & Age < 30 ~ "25-29",
    Age > 29 & Age < 35 ~ "30-34",
    Age > 34 & Age < 40 ~ "35-39",
    Age > 39 & Age < 45 ~ "40-44",
    Age > 44 & Age < 50 ~ "45-49",
    Age > 49 & Age < 55 ~ "50-54",
    Age > 54 & Age < 60 ~ "55-59",
    Age > 59 & Age < 65 ~ "60-64",
    Age > 64 & Age < 70 ~ "65-69",
    Age > 69 & Age < 75 ~ "70-74",
    Age > 74 & Age < 80 ~ "75-79",
    Age > 79 & Age < 85 ~ "80-84",
    Age > 84 & Age < 90 ~ "85-89",
    Age > 89 & Age < 95 ~ "90-94",
    Age > 94 & Age < 100 ~ "95-99",
    Age > 99 & Age < 105 ~ "100-104"
  )) %>% 
  group_by(Age_Group, Gender) %>% 
  arrange(desc(Distance)) %>% 
  mutate(Place = row_number()) %>% 
  ungroup() %>% 
  mutate(Age_Group = NULL) %>% 
  select(Place, Name, Age, Distance, Club, Gender, Year) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2001.csv", row.names = FALSE)

### 2002 ###

X2002 <- readr::read_delim("inst/extdata/2002_Style/2002 OHEP Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                           trim_ws = TRUE)

X2002 %>% 
  mutate(X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2002 OHEP Results.csv", col_names = FALSE)

X2002 <- readr::read_csv("inst/extdata/2002_Style/2002 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2002 %>% 
  separate(X1, into = , c("Place", "Name", "Age", "Distance", "Club", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2002.csv", row.names = FALSE)

### 2003 ###

X2003 <- readr::read_delim("inst/extdata/2002_Style/2003 OHEP Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                           trim_ws = TRUE)

X2003 %>% 
  mutate(X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2003 OHEP Results.csv", col_names = FALSE)

X2003 <- readr::read_csv("inst/extdata/2002_Style/2003 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2003 %>% 
  separate(X1, into = , c("Place", "Name", "Age", "Distance", "Club", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2003.csv", row.names = FALSE)

### 2004 ###
X2004 <- readr::read_delim("inst/extdata/2004 OHEP Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2004 %>% 
  mutate(X1 = str_replace_all(X1, "\\.", ""),
         X1 = str_replace_all(X1, "\\* [:alpha:]{2,}", " UNAT"),
         X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) ([A-Z|\\d\\*]{2,})", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:alpha:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:punct:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, "SMMM(\\d)", "SMMM  \\1"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2004 OHEP Results.csv", col_names = FALSE)

X2004 <- readr::read_csv("inst/extdata/2002_Style/2004 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2004 %>% 
  separate(X1, into = , c("Place", "Name", "Club", "Age", "Distance", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  mutate(Gender = case_when(str_detect(Gender, "NR") ~ "",
                            TRUE ~ Gender)) %>% 
  na_if("") %>% 
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  mutate(Gender = trimws(Gender)) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2004.csv", row.names = FALSE)

### 2005 ###
X2005m <- readr::read_delim("inst/extdata/2005 OHEP Mens Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2005w <- readr::read_delim("inst/extdata/2005 OHEP Womens Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2005m %>% 
  bind_rows(X2005w) %>% 
  mutate(X1 = str_replace_all(X1, "\\.", ""),
         X1 = str_replace_all(X1, "- ", ""),
         X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) ([A-Z|\\d\\*]{2,})", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:alpha:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:punct:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, "SMMM(\\d)", "SMMM  \\1"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2005 OHEP Results.csv", col_names = FALSE)

X2005 <- readr::read_csv("inst/extdata/2002_Style/2005 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2005 %>% 
  separate(X1, into = , c("Place", "Name", "Club", "Age", "Distance", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  mutate(Gender = case_when(str_detect(Gender, "NR") ~ "",
                            TRUE ~ Gender)) %>% 
  na_if("") %>% 
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  mutate(Gender = trimws(Gender)) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2005.csv", row.names = FALSE)

### 2006 ###
X2006 <- readr::read_delim("inst/extdata/2006 OHEP Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)


X2006 %>% 
  mutate(X1 = str_replace_all(X1, "\\.", ""),
         X1 = str_replace_all(X1, "- ", ""),
         X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) ([A-Z|\\d\\*]{2,})", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:alpha:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:punct:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, "SMMM(\\d)", "SMMM  \\1"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2006 OHEP Results.csv", col_names = FALSE)

X2006 <- readr::read_csv("inst/extdata/2002_Style/2006 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2006 %>% 
  separate(X1, into = , c("Place", "Name", "Age", "Club", "Distance", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  mutate(Gender = case_when(str_detect(Gender, "NR") ~ "",
                            TRUE ~ Gender)) %>% 
  na_if("") %>% 
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  mutate(Gender = trimws(Gender)) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2006.csv", row.names = FALSE)



### 2007 ###
X2007m <- readr::read_delim("inst/extdata/2002_Style/2007 OHEP Mens Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                                     trim_ws = TRUE)

X2007w <- readr::read_delim("inst/extdata/2002_Style/2007 OHEP Womens Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2007m %>% 
  bind_rows(X2007w) %>% 
  mutate(X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2007 OHEP Results.csv", col_names = FALSE)

X2007 <- readr::read_csv("inst/extdata/2002_Style/2007 OHEP Results.csv", col_names = FALSE,
                            trim_ws = TRUE)

X2007 %>% 
  separate(X1, into = , c("Place", "Name", "Age", "Distance", "Club", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2007.csv", row.names = FALSE)

### 2008 ###
X2008 <- readr::read_delim("inst/extdata/2008 OHEP Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2008 %>%
  mutate(
    X1 = str_replace_all(X1, "\\.", ""),
    X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
    X1 = str_replace_all(X1, "([:alpha:]) (\\d)", "\\1  \\2"),
    X1 = str_replace_all(X1, ",", ""),
  ) %>%
  mutate(X1 = str_replace_all(X1, "\\s{2,}", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2008 OHEP Results.csv", col_names = FALSE)

X2008 <- readr::read_csv("inst/extdata/2002_Style/2008 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2008 %>% 
  separate(X1, into = , c("Place", "Name", "Age", "Distance", "Club", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  na_if("X") %>% 
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  mutate(Gender = trimws(Gender)) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2008.csv", row.names = FALSE)

### 2009 ###
X2009m <- readr::read_delim("inst/extdata/2009 OHEP Mens Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2009w <- readr::read_delim("inst/extdata/2009 OHEP Womens Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2009m %>% 
  bind_rows(X2009w) %>% 
  mutate(X1 = str_replace_all(X1, "\\.", ""),
         X1 = str_replace_all(X1, "- ", ""),
         X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) ([A-Z|\\d\\*]{2,})", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:alpha:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:punct:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, "SMMM(\\d)", "SMMM  \\1"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2009 OHEP Results.csv", col_names = FALSE)

X2009 <- readr::read_csv("inst/extdata/2002_Style/2009 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2009 %>% 
  separate(X1, into = , c("Place", "Name", "Club", "Age", "Distance", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  mutate(Gender = trimws(Gender)) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2009.csv", row.names = FALSE)

### 2010 ###
X2010m <- readr::read_delim("inst/extdata/2010 OHEP Mens Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2010w <- readr::read_delim("inst/extdata/2010 OHEP Womens Results.txt", "\t", escape_double = FALSE, col_names = FALSE,
                            trim_ws = TRUE)

X2010m %>% 
  bind_rows(X2010w) %>% 
  mutate(X1 = str_replace_all(X1, "\\.", ""),
         X1 = str_replace_all(X1, "(\\d) ", "\\1  "),
         X1 = str_replace_all(X1, "([:lower:]) ([A-Z|\\d\\*]{2,})", "\\1  \\2"),
         X1 = str_replace_all(X1, "([:alpha:]) (\\d)", "\\1  \\2"),
         X1 = str_replace_all(X1, ",", "")) %>% 
  mutate(X1 = str_replace_all(X1, "  ", ",")) %>%
  write_delim(path = "inst/extdata/2002_Style/2010 OHEP Results.csv", col_names = FALSE)

X2010 <- readr::read_csv("inst/extdata/2002_Style/2010 OHEP Results.csv", col_names = FALSE,
                         trim_ws = TRUE)

X2010 %>% 
  separate(X1, into = , c("Place", "Name", "Club", "Age", "Distance", "Gender", "Year"), sep = ",", remove = TRUE) %>% 
  tidyr::fill(Year, .direction = "down") %>%
  tidyr::fill(Gender, .direction = "down") %>% 
  tidyr::fill(Place, .direction = "down") %>% 
  mutate(Gender = trimws(Gender)) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2010.csv", row.names = FALSE)

