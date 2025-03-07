Global Shark Attacks Analysis
Natalie Avila

2025-03-07

#Introduction
library(tidyverse)
 ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
 ✔ dplyr     1.1.4     ✔ readr     2.1.5
 ✔ forcats   1.0.0     ✔ stringr   1.5.1
 ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
 ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
 ✔ purrr     1.0.2     
 ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
 ✖ dplyr::filter() masks stats::filter()
 ✖ dplyr::lag()    masks stats::lag()
 Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
setwd("~/Downloads/")
shark_attacks <- read.csv("~/Downloads/gshark_attacks.csv")
head(shark_attacks)

     date year       type   country               area
 1 5/13/23 2023 Unprovoked AUSTRALIA    South Australia
 2 4/29/23 2023 Unprovoked AUSTRALIA  Western Australia
 3 10/7/22 2022 Unprovoked AUSTRALIA Western  Australia
 4 10/4/21 2021 Unprovoked       USA            Florida
 5 10/3/21 2021 Unprovoked       USA            Florida
 6 5/23/21 2021 Unprovoked       USA     South Carolina
                                     location     activity               name
 1                                   Elliston      Surfing   Simon Baccanello
 2                       Yallingup, Busselton     Swimming               male
 3                               Port Hedland Spearfishing        Robbie Peck
 4   Fort Pierce State Park, St. Lucie County      Surfing Truman Van Patrick
 5                Jensen Beach, Martin County     Swimming               male
 6 Burkes Beach, Hilton Head, Beaufort County     Swimming       Wyatt Bowman
   sex age fatal_y_n  time     species
 1   M  46         Y 10h10 White shark
 2   M             N 11h20    1m shark
 3   M  38         N 11h30  Bull shark
 4   M  25         N                  
 5   M             N 12h00            
 6   M  26         N          5' shark
Link to my dataset
https://www.kaggle.com/datasets/gauravkumar2525/shark-attacks?select=global_shark_attacks.csv

Description of Dataset
This dataset analyzes the shark attack interaction worldwide. It includes information about the incident’s date, location, type of attack, the kind of activity the person was involved in, the sharks species, and the outcome of the attack.

Description of Motivation
I’ve always enjoyed the water and going to the beach. I love getting into the water and going deep enough to the point where I’m swimming. However each time, I would have a little fear that a shark would come up. So, ultimately, I hope to find more information on whether factors like the location or the time of year influences the likelihood of an attack.

Basic summary statistics
      date                year          type             country         
  Length:368         Min.   :1903   Length:368         Length:368        
  Class :character   1st Qu.:1984   Class :character   Class :character  
  Mode  :character   Median :2004   Mode  :character   Mode  :character  
                     Mean   :1994                                        
                     3rd Qu.:2013                                        
##                     Max.   :2023                                        
##      area             location           activity             name          
##  Length:368         Length:368         Length:368         Length:368        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##      sex                age             fatal_y_n             time          
##  Length:368         Length:368         Length:368         Length:368        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##    species         
##  Length:368        
##  Class :character  
##  Mode  :character  
##                    
##                    
## 
Visualizations
One plot showing the relationship between two quantitative variables
My Interpretation

Based on the data, there seems to be a weak positive relationship between the age of the person and the year the shark attack occurred. In which I would infer that the year does not explain much on why people are certain ages get attacked.

ggplot(shark_attacks, aes(x = year, y = age, group = 1)) + 
  geom_jitter(alpha = 0.6, width = 0.2, color = "blue") +  
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.5) +
  labs(title = "Age vs Year of Shark Attacks",
       x = "Year of Attack",
       y = "Age of Individual") +
  theme_minimal()
## `geom_smooth()` using formula = 'y ~ x'


One plot showing the relationship between a categorical and quantitative variable
My Interpretation

USA and Australia are the top two countries for the events of where shark attacks occurred. Based on this I can tell that 25% of the shark attacks occurred before 1991 and 75% happened before 2014. While in Australia 25% of attacks occurred before 1962 and 75% of the attacks happened before 2015. Although countries such as Brazil and South Africa have had shark attacks that happened in specific periods. So some countries have shark attacks more spread out within time and some have more attacks during specific times.

country_order <- shark_attacks %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  pull(country)

shark_attacks$country <- factor(shark_attacks$country, levels = country_order)

ggplot(shark_attacks, aes(x = country, y = year)) +
  geom_boxplot() +
  labs(title = "Distribution of Shark Attack Years by Country",
       x = "Country",
       y = "Year of Attack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


Choose the variables and plot type that are best adapted for showing an interesting relationship in your dataset.
The variables I selected for analysis are country, age, and year, and I created plots to explore the relationships between age and year, as well as country and year

Dataset Manipulations
At least one plot based on a dplyr manipulation of your dataset
How can data manipulations reveal a new insight into your dataset?

Data manipulation helps me clean up the dataset by focusing on important details, like the top 10 countries, and organizing it to show how shark attacks change over time. By making the data simpler, I can spot patterns that aren’t obvious at first. This helps me understand where shark attacks happen most, how they change, and which countries are most affected.

top_countries <- shark_attacks %>%
  filter(!is.na(country), !is.na(year)) %>%
  group_by(country) %>%
  summarize(num_attacks = n(), .groups = 'drop') %>%
  arrange(desc(num_attacks)) %>%
  head(10)

top_country_attacks <- shark_attacks %>%
  filter(country %in% top_countries$country)

ggplot(top_country_attacks, aes(x = year, color = country)) +
  geom_line(stat = "count", aes(y = after_stat(count))) + 
  labs(title = "Shark Attacks Over Time by Top 10 Countries",
       x = "Year", 
       y = "Number of Attacks") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "bottom")


Statistical Analyses
At least one correlation between two quantitative variables in your dataset and your interpretation.
My Interpretation

The correlation coefficient showed that there was a weak positive correlation of 0.296 between age and year, which indicates there is some type of relationship but it’s not strong enough to explain the other variables.

#correlation_result <- cor(shark_attacks$age, shark_attacks$year, use = "complete.obs")
#print(correlation_result)
summary(shark_attacks)
##      date                year          type                    country   
##  Length:368         Min.   :1903   Length:368         USA          :155  
##  Class :character   1st Qu.:1984   Class :character   AUSTRALIA    : 71  
##  Mode  :character   Median :2004   Mode  :character   SOUTH AFRICA : 43  
##                     Mean   :1994                      BAHAMAS      : 10  
##                     3rd Qu.:2013                      BRAZIL       :  8  
##                     Max.   :2023                      NEW CALEDONIA:  6  
##                                                       (Other)      : 75  
##      area             location           activity             name          
##  Length:368         Length:368         Length:368         Length:368        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##      sex                age             fatal_y_n             time          
##  Length:368         Length:368         Length:368         Length:368        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##    species         
##  Length:368        
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
At least one linear regression between two quantitative variable in your dataset, and your interpretation.
My Interpretation

So based off of this the coefficients show that for each additional year, the age increases by 0.2 years, so as the year goes up, the average age of the person increases but very slightly. Since the p-value is extremely small this shows that there is a relationship, but the R-squared being around 8.8% indicates the relationship between the two are not strong.

model <- lm(age ~ year, data = shark_attacks)
summary(model)
###At least one t-test between a categorical and quantitative variable in your dataset and your interpretation.
###My Interpretation

The t-test results show no significant difference in the average years of shark attacks between Australia and the USA. The t-statistic of -1.35 and p-value of 0.1808, which is greater than the typical 0.05 threshold, suggest that the difference in means is not statistically significant. The 95% confidence interval (-13.85 to 2.64) includes zero, further supporting the lack of a significant difference. On average, shark attacks in Australia occurred in 1991.87, while in the USA, they occurred in 1997.48, but this difference is likely due to random variation rather than a true effect.

shark_attacks_subset <- shark_attacks %>%
  filter(country %in% c("USA", "AUSTRALIA"), !is.na(year))

t_test_result <- t.test(year ~ country, data = shark_attacks_subset)
print(t_test_result)
## 
##  Welch Two Sample t-test
## 
## data:  year by country
## t = 1.3469, df = 110.84, p-value = 0.1808
## alternative hypothesis: true difference in means between group USA and group AUSTRALIA is not equal to 0
## 95 percent confidence interval:
##  -2.640977 13.849337
## sample estimates:
##       mean in group USA mean in group AUSTRALIA 
##                1997.477                1991.873
How can you use this dataset to draw conclusions about a larger population?
By doing these analysis, I can draw conclusions about broader trends in shark attacks, such as how age, year, and location impact the frequency and nature of shark incidents. This helps with better understanding and predicting future risks in different populations and regions.

##Conclusion
###Overarching conclusions from your analysis
In the Age vs Year of Shark Attacks analysis, the scatter plot and linear regression indicate a weak positive relationship between the age of individuals and the year of the attack. The weak slope of the regression model suggests that while older individuals may be involved in shark attacks in later years, the relationship between age and year of attack is minimal. In the Year by Country analysis, there is no statistically significant difference in the average year of shark attacks between countries like Australia and the USA. While countries with more recorded attacks, such as the USA and Australia, show a wider range of attack years, no noticeable trend or significant relationship between the year of attack and country is evident.
