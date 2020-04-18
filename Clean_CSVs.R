library(tidyverse)
library(readxl)
## CSV Cleaning

#Want Place, Gender, Name, Age, Distance, Club, Year, USMS_ID (if possible) National_Record (if possible)

Postal_1998 <- read_csv("inst/extdata/cleaned_data/Postal_1998.csv")
Postal_1998 %>%
  mutate(Gender = case_when(Gender == "Men" ~ "M",
                            Gender == "Women" ~ "F")) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_1998.csv", row.names = FALSE)

Postal_1999 <- read_csv("inst/extdata/cleaned_data/Postal_1999.csv")
Postal_1999 %>%
  tidyr::fill(X7, .direction = "down") %>% 
  tidyr::fill(X6, .direction = "down") %>% 
  tidyr::fill(X1, .direction = "down") %>% 
  select(Place = X1,
         Name = X2,
         Age = X3,
         Club = X4,
         Distance = X5,
         Gender = X6,
         Year = X7) %>% 
  mutate(Gender = case_when(Gender == "Men" ~ "M",
                            Gender == "Women" ~ "F")) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_1999.csv", row.names = FALSE)

Postal_2000 <- read_csv("inst/extdata/cleaned_data/Postal_2000.csv")
Postal_2000 %>%
  mutate(Gender = case_when(Gender == "Men" ~ "M",
                            Gender == "Women" ~ "F"),
         Name = paste(First_Name, Last_Name, sep = " "),
         First_Name = NULL,
         Last_Name = NULL) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2000.csv", row.names = FALSE)

Postal_2001 <- read_csv("inst/extdata/cleaned_data/Postal_2001.csv")
Postal_2001 %>%
  write.csv(file = "inst/extdata/cleaned_data/Postal_2001.csv", row.names = FALSE)

Postal_2002 <- read_csv("inst/extdata/cleaned_data/Postal_2002.csv")
Postal_2002 %>%
  mutate(Gender = case_when(Gender == "Men" ~ "M",
                            Gender == "Women" ~ "F")) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2002.csv", row.names = FALSE)

Postal_2003 <- read_csv("inst/extdata/cleaned_data/Postal_2003.csv")
Postal_2003 %>%
  mutate(Gender = case_when(Gender == "Men" ~ "M",
                            Gender == "Women" ~ "F")) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2003.csv", row.names = FALSE)

Postal_2004 <- read_csv("inst/extdata/cleaned_data/Postal_2004.csv")
Postal_2004 %>%
  mutate(Gender = case_when(Gender == "Men" ~ "M",
                            Gender == "Women" ~ "F")) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2004.csv", row.names = FALSE)

Postal_2005 <- read_csv("inst/extdata/cleaned_data/Postal_2005.csv")
Postal_2005 %>%
  mutate(Gender = case_when(Gender == "Men" ~ "M",
                            Gender == "Women" ~ "F")) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2005.csv", row.names = FALSE)


Postal_2006 <- read_csv("inst/extdata/cleaned_data/Postal_2006.csv")
Postal_2006 %>%
  write.csv(file = "inst/extdata/cleaned_data/Postal_2006.csv", row.names = FALSE)

### 2011 ###

Postal_2011 <- read_excel("inst/extdata/2011_Men_x2.xlsx", col_names = FALSE) %>% 
  mutate(Gender = "M") %>% 
  bind_rows(read_excel("inst/extdata/2011_Women_x.xlsx", col_names = FALSE) %>% 
              mutate(Gender = "F")) %>% 
  select(Place = `...2`,
         First_Name = `...3`,
         Last_Name = `...4`,
         Age = `...6`,
         Club = `...7`,
         USMS_ID = `...8`,
         Distance = `...9`,
         Gender) %>% 
  mutate(Year = 2011) %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2011.csv", row.names = FALSE)

### 2012 ###

Postal_2012m <- read_excel("inst/extdata/2012_Men_x.xlsx", col_names = FALSE) %>% 
  select(Place = `...2`,
         First_Name = `...3`,
         Last_Name = `...4`,
         Age = `...6`,
         Club = `...7`,
         USMS_ID = `...8`,
         Distance = `...9`) %>% 
  mutate(Gender = "M")
  
Postal_2012w <- read_csv("inst/extdata/2012_Women_x.csv", col_names = FALSE) %>% 
  select(Place = X2,
         First_Name = X3,
         Last_Name = X4,
         Age = X6,
         Club = X7,
         USMS_ID = X8,
         Distance = X9) %>% 
  mutate(Gender = "F")

Postal_2012m %>% 
  bind_rows(Postal_2012w) %>% 
  mutate(Year = 2012,
         Club = case_when(is.na(Club) ~ "UNAT",
                          TRUE ~ Club)) %>%
  na_if("-") %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2012.csv", row.names = FALSE)

### 2013 ###
Postal_2013m <- read_csv("inst/extdata/2013_Men_x.csv", col_names = FALSE) %>% 
  select(Place = X2,
         First_Name = X3,
         Last_Name = X4,
         Age = X6,
         Club = X7,
         USMS_ID = X8,
         Distance = X9) %>% 
  mutate(Gender = "M")

Postal_2013w <- read_csv("inst/extdata/2013_Women_x.csv", col_names = FALSE) %>% 
  select(Place = X2,
         First_Name = X3,
         Last_Name = X4,
         Age = X6,
         Club = X7,
         USMS_ID = X8,
         Distance = X9) %>% 
  mutate(Gender = "F")

Postal_2013m %>% 
  bind_rows(Postal_2013w) %>% 
  mutate(Year = 2013,
         Club = case_when(is.na(Club) ~ "UNAT",
                          TRUE ~ Club)) %>%
  na_if("-") %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2013.csv", row.names = FALSE)

### 2014 ###
Postal_2014m <- read_excel("inst/extdata/2014_Men_x.xlsx", col_names = FALSE) %>% 
  select(Place = `...1`,
         First_Name = `...2`,
         Last_Name = `...3`,
         Age = `...5`,
         Club =`...6`,
         USMS_ID = `...7`,
         Distance = `...8`) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(Gender = "M")

Postal_2014w <- read_excel("inst/extdata/2014_Women_x.xlsx", col_names = FALSE) %>% 
  select(Place = `...1`,
         First_Name = `...2`,
         Last_Name = `...3`,
         Age = `...5`,
         Club =`...6`,
         USMS_ID = `...9`,
         Distance = `...11`) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(Gender = "F")

Postal_2014m %>% 
  bind_rows(Postal_2014w) %>% 
  mutate(Year = 2014,
         Club = case_when(is.na(Club) ~ "UNAT",
                          TRUE ~ Club)) %>%
  na_if("-") %>% 
  write.csv(file = "inst/extdata/cleaned_data/Postal_2014.csv", row.names = FALSE)

