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
