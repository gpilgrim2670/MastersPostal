# Masters Postal

This project deals with the [United States Masters Swimming (USMS) ePostal championships](https://www.usms.org/events/national-championships/epostal-national-championships).  Held every year, ePostal competitors swim for one hour, aiming to travel the furthest distance.  Results are ranked by age group and gender.

## Raw Results

As reported results since 1998 are [available as a .csv file](https://github.com/gpilgrim2670/MastersPostal/blob/master/Postal_Raw.csv) in this repo.

## Value Added Results

Results with [additional calculated statistics](https://github.com/gpilgrim2670/MastersPostal/blob/master/Postal_All.csv) (split times, athlete and club ranking etc.) are also available in this repo.

### Glossary of Column Values

#### The following values refer to the athlete listed in 'Name'

**Place:** Order of finish for an athlete in a given age group, gender and year

**Name:** Athlete name as First Last

**Age:** Athlete age in years

**Distance:** Yards athlete swam in one hour

**Club:** Name of organization/team athlete represents

**Gender:** Athlete gender, 'M' for men, 'W' for women.  Competition is within age group and gender

**Year:** When event took place

**Age_Group:** USMS age categories.  Competition is within age group and gender

**Avg_Split_50:** Average time to swim 50 yards, in seconds

**Relative_Place:** Athlete's place within their gender and age group for a given year.  E.g. '4 of 23'.

**USMS_ID:** An athlete's United States Masters Swimming identification number for a given year.  Changes year to year, not present prior to 2011.

**Perm_ID:** A 5 character alphanumeric string that is the permanent portion of an athlete's USMS_ID.  Used to identify athletes across years in the event of name changes.  If no USMS_ID is present a randomly generated 6 character alphabetic string is used instead.

**National_Record:** Did the athlete set a national record for their age group and gender with this particular swim?  Either (Y)es or (N)o.

#### The following values refer to the Club listed in 'Club'

**Club_Count:** Number of athletes in Club

**Club_Count_Male:** Number of male athletes in Club

**Club_Count_Female:** Number of female athletes in Club

**Club_Size_Combined:** USMS category for club size based on total number of athletes competing.  Values are (s)mall, (M)edium, (L)arge and e(x)tra (L)arge

**Club_Size_Male:** USMS category for club size based on total number of male athletes competing. Values are (s)mall, (M)edium, (L)arge and e(x)tra (L)arge

**Club_Size_Female:** USMS category for club size based on total number of female athletes competing. Values are (s)mall, (M)edium, (L)arge and e(x)tra (L)arge

**Total_Distance_Combined:** Number of yards swam by all members of a club competing in the ePostal in a given year

**Total_Distance_Male:** Number of yards swam by all male members of a club competing in the ePostal in a given year

**Total_Distance_Female:** Number of yards swam by all female members of a club competing in the ePostal in a given year

**Avg_Age_Club:** Average age in years of all club members competing in the ePostal in a given year

**Avg_Age_Male:** Average age in years of all male club members competing in the ePostal in a given year

**Avg_Age_Female:** Average age in years of all female club members competing in the ePostal in a given year

**Combined_Rank:** Order of finish based on distance swam in a given year for a club within its appropriate club size

**Male_Rank:** Order of finish based on distance swam by men in a given year for a club within its appropriate club size

**Female_Rank:** Order of finish based on distance swam by women in a given year for a club within its appropriate club size

**Avg_Distance_Combined:** Average number of yards swam by all athletes in a club in a given year

**Avg_Distance_Male:** Average number of yards swam by all male athletes in a club in a given year

**Avg_Distance_Female:** Average number of yards swam by all female athletes in a club in a given year

**Avg_Split_50_Club_Combined:** Average number of seconds per 50 yards for all athletes in a club in a given year

**Avg_Split_50_Club_Male:** Average number of seconds per 50 yards for all male athletes in a club in a given year

**Avg_Split_50_Club_Female:** Average number of seconds per 50 yards for all female athletes in a club in a given year

## Shiny App

A shiny app based on the value added results has been built.  It can be used [here](https://gpilgrim.shinyapps.io/MastersPostalProject/?_ga=2.155632699.1406177177.1619535238-1657862835.1619017959), and the [source code](https://github.com/gpilgrim2670/MastersPostal/blob/master/Postal_App.R) is in this repo.

## Demonstrations

There are several articles on the blog [Swimming + Data Science](https://pilgrim.netlify.app/) that deal with the ePostal and specifically draw on the data from this repo.  They include:

1. [The USMS ePostal Over the Last 20+ Years](https://pilgrim.netlify.app/post/the-usms-epostal-over-the-last-20-years/)
2. [Analyzing the US Masters 2020 1-Hour ePostal Results](https://pilgrim.netlify.app/post/analyzing-the-us-masters-2020-epostal-results/)

### Questions?

Please don't hesitate to get in touch, either here on Github or via the [Swimming + Data Science Contact Form](https://pilgrim.netlify.app/contact/)