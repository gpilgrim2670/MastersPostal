# library(tidyverse)
# library(readxl)
# ## CSV Cleaning
# 
# #Want Place, Gender, Name, Age, Distance, Club, Year, USMS_ID (if possible) National_Record (if possible)
# 
# Postal_1998 <- read_csv("inst/extdata/cleaned_data/Postal_1998.csv")
# Postal_1998 %>%
#   tidyr::fill(Gender, .direction = "down") %>% 
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W",
#                             TRUE ~ Gender)) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_1998.csv", row.names = FALSE)
# 
# Postal_1999 <- read_csv("inst/extdata/cleaned_data/Postal_1999.csv")
# Postal_1999 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_1999.csv", row.names = FALSE)
# 
# Postal_2000 <- read_csv("inst/extdata/cleaned_data/Postal_2000.csv")
# Postal_2000 %>%
#   tidyr::fill(Gender, .direction = "down") %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          Gender,
#          Year) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2000.csv", row.names = FALSE)
# 
# Postal_2001 <- read_csv("inst/extdata/cleaned_data/Postal_2001.csv")
# Postal_2001 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2001.csv", row.names = FALSE)
# 
# Postal_2002 <- read_csv("inst/extdata/cleaned_data/Postal_2002.csv")
# Postal_2002 %>%
#   tidyr::fill(Gender, .direction = "down") %>% 
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W",
#                             TRUE ~ Gender)) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2002.csv", row.names = FALSE)
# 
# Postal_2003 <- read_csv("inst/extdata/cleaned_data/Postal_2003.csv")
# Postal_2003 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W",
#                             TRUE ~ Gender)) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2003.csv", row.names = FALSE)
# 
# Postal_2004 <- read_csv("inst/extdata/cleaned_data/Postal_2004.csv")
# Postal_2004 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W",
#                             TRUE ~ Gender)) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2004.csv", row.names = FALSE)
# 
# Postal_2005 <- read_csv("inst/extdata/cleaned_data/Postal_2005.csv")
# Postal_2005 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2005.csv", row.names = FALSE)
# 
# 
# Postal_2006 <- read_csv("inst/extdata/cleaned_data/Postal_2006.csv")
# Postal_2006 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2006.csv", row.names = FALSE)
# 
# Postal_2007 <- read_csv("inst/extdata/cleaned_data/Postal_2007.csv")
# Postal_2007 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2007.csv", row.names = FALSE)
# 
# Postal_2008 <- read_csv("inst/extdata/cleaned_data/Postal_2008.csv")
# Postal_2008 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W",
#                             TRUE ~ Gender)) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2008.csv", row.names = FALSE)
# 
# Postal_2009 <- read_csv("inst/extdata/cleaned_data/Postal_2009.csv")
# Postal_2009 %>%
#   mutate(Gender = case_when(Gender == "Men" ~ "M",
#                             Gender == "Women" ~ "W")) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2009.csv", row.names = FALSE)
# 
# Postal_2010 <- read_csv("inst/extdata/cleaned_data/Postal_2010.csv")
# Postal_2010 %>%
#   mutate(Gender = case_when(Gender == "Men" ~ "M",
#                             Gender == "Women" ~ "W")) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2010.csv", row.names = FALSE)
# 
# Postal_2011 <- read_csv("inst/extdata/cleaned_data/Postal_2011.csv")
# Postal_2011 %>%
#   mutate(Name = paste(First_Name, Last_Name, sep = " "),
#          First_Name = NULL,
#          Last_Name = NULL) %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID,
#          Gender,
#          Year) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2011.csv", row.names = FALSE)
# 
# Postal_2012 <- read_csv("inst/extdata/cleaned_data/Postal_2012.csv")
# Postal_2012 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID,
#          Gender,
#          Year) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2012.csv", row.names = FALSE)
# 
# Postal_2013 <- read_csv("inst/extdata/cleaned_data/Postal_2013.csv")
# Postal_2013 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID,
#          Gender,
#          Year) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2013.csv", row.names = FALSE)
# 
# Postal_2014 <- read_csv("inst/extdata/cleaned_data/Postal_2014.csv")
# Postal_2014 %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W")) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID,
#          Gender,
#          Year) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2014.csv", row.names = FALSE)
# 
# Postal_2015 <- read_csv("inst/extdata/cleaned_data/Postal_2015.csv")
# Postal_2015 %>%
#   mutate(Name = paste(First_Name, Last_Name, sep = " "),
#          First_Name = NULL,
#          Last_Name = NULL) %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W"),
#          Year = 2015) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID = USMS,
#          Gender,
#          Year,
#          National_Record) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2015.csv", row.names = FALSE)
# 
# Postal_2016 <- read_csv("inst/extdata/cleaned_data/Postal_2016.csv")
# Postal_2016 %>%
#   mutate(Name = paste(First_Name, Last_Name, sep = " "),
#          First_Name = NULL,
#          Last_Name = NULL) %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W"),
#          Year = 2016) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID = USMS,
#          Gender,
#          Year,
#          National_Record) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2016.csv", row.names = FALSE)
# 
# Postal_2017 <- read_csv("inst/extdata/cleaned_data/Postal_2017.csv")
# Postal_2017 %>%
#   mutate(Name = paste(First_Name, Last_Name, sep = " "),
#          First_Name = NULL,
#          Last_Name = NULL) %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W"),
#          Year = 2017) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID = USMS,
#          Gender,
#          Year,
#          National_Record) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2017.csv", row.names = FALSE)
# 
# Postal_2018 <- read_csv("inst/extdata/cleaned_data/Postal_2018.csv")
# Postal_2018 %>%
#   # mutate(Name = paste(First_Name, Last_Name, sep = " "),
#   #        First_Name = NULL,
#   #        Last_Name = NULL) %>%
#   mutate(
#     # Gender = case_when(Gender == "M" ~ "M",
#     #                         Gender == "F" ~ "W"),
#          Year = 2018) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID,
#          Gender,
#          Year,
#          National_Record) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2018.csv", row.names = FALSE)
# 
# Postal_2019 <- read_csv("inst/extdata/cleaned_data/Postal_2019.csv")
# Postal_2019 %>%
#   # mutate(Name = paste(First_Name, Last_Name, sep = " "),
#   #        First_Name = NULL,
#   #        Last_Name = NULL) %>%
#   mutate(
#     # Gender = case_when(Gender == "M" ~ "M",
#     #                         Gender == "F" ~ "W"),
#          Year = 2019) %>% 
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID,
#          Gender,
#          Year,
#          National_Record) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2019.csv", row.names = FALSE)
# 
# Postal_2020 <- read_csv("inst/extdata/cleaned_data/Postal_2020.csv")
# Postal_2020 %>%
#   # mutate(Name = paste(First_Name, Last_Name, sep = " "),
#   #        First_Name = NULL,
#   #        Last_Name = NULL) %>%
#   mutate(Gender = case_when(Gender == "M" ~ "M",
#                             Gender == "F" ~ "W",
#                             is.na(Gender) ~ "W"),
#          Year = 2020) %>%
#   select(Place,
#          Name,
#          Age,
#          Club,
#          Distance,
#          USMS_ID,
#          Gender,
#          Year,
#          National_Record) %>%
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2020.csv", row.names = FALSE)

# ### 2011 ###
# 
# Postal_2011 <- read_excel("inst/extdata/raw_data/2011_Men_x2.xlsx", col_names = FALSE) %>% 
#   mutate(Gender = "M") %>% 
#   bind_rows(read_excel("inst/extdata/raw_data/2011_Women_x.xlsx", col_names = FALSE) %>% 
#               mutate(Gender = "W")) %>% 
#   select(Place = `...2`,
#          First_Name = `...3`,
#          Last_Name = `...4`,
#          Age = `...6`,
#          Club = `...7`,
#          USMS_ID = `...8`,
#          Distance = `...9`,
#          Gender) %>% 
#   mutate(Year = 2011) %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2011.csv", row.names = FALSE)
# 
# ### 2012 ###
# 
# Postal_2012m <- read_excel("inst/extdata/2012_Men_x.xlsx", col_names = FALSE) %>% 
#   select(Place = `...2`,
#          First_Name = `...3`,
#          Last_Name = `...4`,
#          Age = `...6`,
#          Club = `...7`,
#          USMS_ID = `...8`,
#          Distance = `...9`) %>% 
#   mutate(Gender = "M")
#   
# Postal_2012w <- read_csv("inst/extdata/2012_Women_x.csv", col_names = FALSE) %>% 
#   select(Place = X2,
#          First_Name = X3,
#          Last_Name = X4,
#          Age = X6,
#          Club = X7,
#          USMS_ID = X8,
#          Distance = X9) %>% 
#   mutate(Gender = "W")
# 
# Postal_2012m %>% 
#   bind_rows(Postal_2012w) %>% 
#   mutate(Year = 2012,
#          Club = case_when(is.na(Club) ~ "UNAT",
#                           TRUE ~ Club)) %>%
#   na_if("-") %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2012.csv", row.names = FALSE)
# 
# ### 2013 ###
# Postal_2013m <- read_csv("inst/extdata/2013_Men_x.csv", col_names = FALSE) %>% 
#   select(Place = X2,
#          First_Name = X3,
#          Last_Name = X4,
#          Age = X6,
#          Club = X7,
#          USMS_ID = X8,
#          Distance = X9) %>% 
#   mutate(Gender = "M")
# 
# Postal_2013w <- read_csv("inst/extdata/2013_Women_x.csv", col_names = FALSE) %>% 
#   select(Place = X2,
#          First_Name = X3,
#          Last_Name = X4,
#          Age = X6,
#          Club = X7,
#          USMS_ID = X8,
#          Distance = X9) %>% 
#   mutate(Gender = "W")
# 
# Postal_2013m %>% 
#   bind_rows(Postal_2013w) %>% 
#   mutate(Year = 2013,
#          Club = case_when(is.na(Club) ~ "UNAT",
#                           TRUE ~ Club)) %>%
#   na_if("-") %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2013.csv", row.names = FALSE)
# 
# ### 2014 ###
# Postal_2014m <- read_excel("inst/extdata/2014_Men_x.xlsx", col_names = FALSE) %>% 
#   select(Place = `...1`,
#          First_Name = `...2`,
#          Last_Name = `...3`,
#          Age = `...5`,
#          Club =`...6`,
#          USMS_ID = `...7`,
#          Distance = `...8`) %>% 
#   filter_all(any_vars(!is.na(.))) %>% 
#   mutate(Gender = "M")
# 
# Postal_2014w <- read_excel("inst/extdata/2014_Women_x.xlsx", col_names = FALSE) %>% 
#   select(Place = `...1`,
#          First_Name = `...2`,
#          Last_Name = `...3`,
#          Age = `...5`,
#          Club =`...6`,
#          USMS_ID = `...9`,
#          Distance = `...11`) %>% 
#   filter_all(any_vars(!is.na(.))) %>% 
#   mutate(Gender = "W")
# 
# Postal_2014m %>% 
#   bind_rows(Postal_2014w) %>% 
#   mutate(Year = 2014,
#          Club = case_when(is.na(Club) ~ "UNAT",
#                           TRUE ~ Club)) %>%
#   na_if("-") %>% 
#   write.csv(file = "inst/extdata/cleaned_data/Postal_2014.csv", row.names = FALSE)
# 
