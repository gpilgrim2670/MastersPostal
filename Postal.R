library(readxl)
library(dplyr)
library(stringr)
#library(purrr)

Postal_2015 <- read_excel("Postal_2015.xlsx")
Postal_2016 <- read_excel("Postal_2016.xlsx")
Postal_2017 <- read_excel("Postal_2017.xlsx")
Postal_2018 <- read_excel("Postal_2018.xlsx")
Postal_2019 <- read_excel("Postal_2019.xlsx")

Postals <- list(Postal_2015, Postal_2016, Postal_2017, Postal_2018, Postal_2019)

Postalize <- function(x){
DFName <- as.character(substitute(x))
DFName <- as.numeric(str_replace(DFName, "Postal_", ""))
x <- x %>% 
 mutate(Year = DFName)

x <- x %>%
  mutate(Perm_ID = str_sub(USMS, -5))

x <- x %>% 
  mutate(Name = paste(Last_Name, First_Name, sep = ", "))


x <- x %>% 
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
  ))

# x <- x %>% 
#   filter(is.na(National_Record) == TRUE) %>% 
#   mutate(National_Record = "N")

# x <- x %>% 
#   filter(is.na(Club) == TRUE) %>% 
#   mutate(Club = "Unattached")

x <- x %>%
group_by(Club) %>%
mutate(Club_Count = n())

x <- x %>%
  group_by(Club) %>%
  mutate(Club_Count_Male = sum(Gender == "M"))

x <- x %>%
  group_by(Club) %>%
  mutate(Club_Count_Female = sum(Gender == "F"))


x <- x %>%
  mutate(Club_Size_Combined = case_when(
    Club_Count < 26 ~ "S",
    Club_Count < 50 ~ "M",
    Club_Count <= 100 ~ "L",
    TRUE ~ "XL"
  ))

x <- x %>%
  mutate(Club_Size_Male = case_when(
    Club_Count_Male < 26 ~ "S",
    Club_Count_Male < 50 ~ "M",
    Club_Count_Male <= 100 ~ "L",
    TRUE ~ "XL"
  ))

x <- x %>%
  mutate(Club_Size_Female = case_when(
    Club_Count_Female < 26 ~ "S",
    Club_Count_Female < 50 ~ "M",
    Club_Count_Female <= 100 ~ "L",
    TRUE ~ "XL"
  ))

x <- x %>%
  group_by(Club) %>%
  mutate(Total_Distance_Combined = sum(Distance))

x <- x %>%
  group_by(Club) %>%
  mutate(Total_Distance_Male = sum(Distance[Gender == "M"]))

x <- x %>%
  group_by(Club) %>%
  mutate(Total_Distance_Female = sum(Distance[Gender == "F"]))

x <- x %>%
  group_by(Club) %>%
  mutate(Avg_Age_Club = mean(Age))
x$Avg_Age_Club <- format(round(x$Avg_Age_Club, 2), nsmall = 2)
x$Avg_Age_Club <- as.numeric(x$Avg_Age_Club)

x <- x %>%
  group_by(Club) %>%
  mutate(Avg_Age_Club_Male = mean(Age[Gender == "M"]))
x$Avg_Age_Club_Male <- format(round(x$Avg_Age_Club_Male, 2), nsmall = 2)
x$Avg_Age_Club_Male <- as.numeric(x$Avg_Age_Club_Male)

x <- x %>%
  group_by(Club) %>%
  mutate(Avg_Age_Club_Female = mean(Age[Gender == "F"]))
x$Avg_Age_Club_Female <- format(round(x$Avg_Age_Club_Female, 2), nsmall = 2)
x$Avg_Age_Club_Female <- as.numeric(x$Avg_Age_Club_Female)

x <- x %>%
group_by(Club_Size_Combined) %>%
mutate(Combined_Rank = dense_rank(desc(Total_Distance_Combined)))

x <- x %>%
  group_by(Club_Size_Male) %>%
  mutate(Male_Rank = dense_rank(desc(Total_Distance_Male)))

x <- x %>%
  group_by(Club_Size_Female) %>%
  mutate(Female_Rank = dense_rank(desc(Total_Distance_Female)))

x <- x %>%
  mutate(Avg_Speed_50 = (1/Distance)*60*60*50)
x$Avg_Speed_50 <- format(round(x$Avg_Speed_50, 2), nsmall = 2)

x <- x %>%
  mutate(Avg_Distance_Male = Total_Distance_Male/Club_Count_Male)
x$Avg_Distance_Male <- format(round(x$Avg_Distance_Male, 0), nsmall = 0)
x$Avg_Distance_Male <- as.numeric(x$Avg_Distance_Male)

x <- x %>%
  mutate(Avg_Distance_Female = Total_Distance_Female/Club_Count_Female)
x$Avg_Distance_Female <- format(round(x$Avg_Distance_Female, 0), nsmall = 0)
x$Avg_Distance_Female <- as.numeric(x$Avg_Distance_Female)

x <- x %>%
  mutate(Avg_Distance_Combined = Total_Distance_Combined/Club_Count)
x$Avg_Distance_Combined <- format(round(x$Avg_Distance_Combined, 0), nsmall = 0)
x$Avg_Distance_Combined <- as.numeric(x$Avg_Distance_Combined)

x <- x %>%
group_by(Gender, Age_Group) %>%
mutate(Relative_Place = paste(Place, max(Place), sep = " of "))

x <- x %>%
  mutate(Avg_Speed_50_Club_Combined = (1/Avg_Distance_Combined)*60*60*50)
x$Avg_Speed_50_Club_Combined <- format(round(x$Avg_Speed_50_Club_Combined, 2), nsmall = 2)

x <- x %>%
  mutate(Avg_Speed_50_Club_Male = (1/Avg_Distance_Male)*60*60*50)
x$Avg_Speed_50_Club_Male <- format(round(x$Avg_Speed_50_Club_Male, 2), nsmall = 2)

x <- x %>%
  mutate(Avg_Speed_50_Club_Female = (1/Avg_Distance_Female)*60*60*50)
x$Avg_Speed_50_Club_Female <- format(round(x$Avg_Speed_50_Club_Female, 2), nsmall = 2)


x$Age_Group <- factor(x$Age_Group, levels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"))
x$National_Record <- factor(x$National_Record, levels  = c("N", "Y"))
x$Club_Size_Combined <- factor(x$Club_Size_Combined, levels = c("S", "M", "L", "XL"))
x$Club_Size_Male <- factor(x$Club_Size_Male, levels = c("S", "M", "L", "XL"))
x$Club_Size_Female <- factor(x$Club_Size_Female, levels = c("S", "M", "L", "XL"))
x$Club <- factor(x$Club)
x$Name <- factor(x$Name)

return(as.data.frame(x))
}

Postal_2015 <- Postalize(Postal_2015)
Postal_2016 <- Postalize(Postal_2016)
Postal_2017 <- Postalize(Postal_2017)
Postal_2018 <- Postalize(Postal_2018)
Postal_2019 <- Postalize(Postal_2019)

Postal <- rbind(Postal_2018, Postal_2019)
Postal <- rbind(Postal, Postal_2017)
Postal <- rbind(Postal, Postal_2016)
Postal <- rbind(Postal, Postal_2015)

Postal <- Postal %>% 
  group_by(Perm_ID) %>% 
  mutate(First_Name = max(First_Name)) %>% 
  mutate(Name = paste(Last_Name, First_Name, sep = ", "))

write.csv(Postal, file = "Postal.csv")
