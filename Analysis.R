###SUMMARY OF DATASET###
#installing packages
install.packages("readr")
install.packages("dplyr")
remove.packages("ggplot2")
remove.packages("rlang")
install.packages("rlang", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)


#libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rlang)


#reading dataset
netflix <- read_csv("Data/netflix_titles.csv")

#show first rows
head(netflix)

#summary
summary(netflix)
#shows 8807 rows and 12 columns

#****************************************#
###EXPLORING DATA TO START ANALYSIS###

#First 10 rows
head(netflix, 10)

#Last 10 rows
tail(netflix, 10)

#structure of data (types)
str(netflix)

#unique data types
unique(netflix$type)
unique(netflix$country)
unique(netflix$rating)
unique(netflix$listed_in)

#count titles by types
table(netflix$type)
#6131 movies & 2676 TV Shows

#Analysis Questions
# 1. Which countries produce the most titles on Netflix?
# 2. How has the number of titles added to Netflix changed over the years?
# 3. Which types or categories of movies and TV shows are most produced?
# 4. Which countries produce the most adult content (TV-MA and R)?

#*******************************************#
###DATA CLEANING###

#identify how many NAs are per column
#sapply = apply function to every column
#is.na = returns true if there is NA, false if there is not
sapply(netflix, function(x) sum(is.na(x)))

#creating a clean version of the dataset for analysis
#filter(!is.na(column)) deletes rows where NA is present
#%>% transfer the data it has in the left and transfer it to the right as an argument
netflix_clean <- netflix %>%
    filter(!is.na(country), !is.na(rating), !is.na(date_added))

#verifying data is cleaned with no NAs
head(netflix_clean)
sapply(netflix_clean, function(x) sum(is.na(x)))

#verifying if there are spaces in cells
char_cols <- sapply(netflix_clean, is.character)

#reviewing if there are spaces at the beginning or at the end
#^\\s search spaces at the beginning of the cell
#^\\s$ search spaces at the end of the cell
#grepl() returns true if it finds something
sapply(netflix_clean[, char_cols], function(x) sum(grepl("^\\s|\\s$", x)))

#REVIEWING DATA FORMAT
str(netflix_clean)
#change date_added to date format
netflix_clean$date_added <- as.Date(netflix_clean$date_added, format="%B %d, %Y")
#%B = month
#%d = day
#%Y = year

#REVIEWING RATING DATA AND FIND ROWS THEY ARE IN
#Remove 74 min, 84 min, 66 min as they are not part of the column

netflix_clean %>% filter(rating %in% c("74 min", "84 min", "66 min"))

#Define valid rating
valid_ratings <- c("G", "PG", "PG-13", "R", "NC-17", "TV-Y", "TV-Y7", "TV-Y7-FV", "TV-G", "TV-PG", "TV-14", "TV-MA", "NR", "UR")

#Replace incorrect values to NA
netflix_clean$rating[!netflix_clean$rating %in% valid_ratings] <-NA

#verifying NA
sapply(netflix_clean, function(x) sum(is.na(x)))

#Removing NA from rating
netflix_clean <- netflix_clean %>% filter (!is.na(rating) & !(rating %in% c("74 min", "84 min", "66 min")))

###REVIEWING DUPLICATES###
sum(duplicated(netflix_clean))

#************************************#

#Analysis Question
# 1. Which countries produce the most titles on Netflix?

netflix_clean %>%
    group_by(country) %>%
    summarise(count =n()) %>%
    arrange(desc(count))

#group_by(country) group by country
#summarise(count=n()) count titles by group
#arrange(desc(count)) arrange from top amounts to less amounts

#graphic
netflix_clean %>%
    group_by(country) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice(1:10) %>% #take first 10
    ggplot(aes(x = reorder(country, count), y = count)) + 
    geom_bar(stat = "identity", fill = "steelblue") + #arrange bars from lowest to highest
    coord_flip() + #turn around the graphic to be horizontal
    labs(title = "Top 10 Countries by Netflix Titles", 
        x = "Country",
        y = "Number of Titles")
# 2. How has the number of titles added to Netflix changed over the years?

#Review if date_added is clean in the dataset
str(netflix_clean$date_added)

#Get year from date_added
netflix_clean <- netflix_clean %>%
    mutate(year_added = as.numeric(format(date_added, "%Y")))

#count title by the year they were added
titles_per_year <- netflix_clean %>%
    filter(!is.na(year_added)) %>% #ignore rows with na
    group_by(year_added) %>%
    summarise(count = n()) %>%
    arrange(year_added)

#review first results
head(titles_per_year, 10)

#graphic

ggplot(titles_per_year, aes(x=year_added, y= count)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "red") + 
    labs(title = "Number of Netflix Titles Added by Year",
        x = "Year",
        y = "Number of Titles")

# 3. Which types or categories of movies and TV shows are most produced?

#Separate categories into individuals rows
netflix_categories <- netflix_clean %>%
    separate_rows(listed_in, sep = ", ")

#count quantity of titles by category
category_counts <- netflix_categories %>%
    group_by(listed_in) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

print(category_counts)

#graphic
category_counts %>%
    slice(1:10) %>%
    ggplot(aes(x = reorder(listed_in, count), y = count)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    coord_flip() +
    labs(title = "Top 10 Netflix Categories by Number of Titles",
        x = "Category",
        y = "Number of Titles") +
    theme_minimal()


# 4. Which countries produce the most adult content (TV-MA and R)?

#filter titles by adult content
adult_content <- netflix_clean %>%
    filter(rating %in% c("TV-MA", "R"))

#Count titles by country
adult_counts <- adult_content %>%
    group_by(country) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
print(adult_counts)

#graphic
adult_counts %>%
    slice(1:10) %>%
    ggplot(aes(x = reorder(country, count), y = count)) + 
    geom_bar(stat = "identity", fill = "tomato") + 
    coord_flip() + 
    labs(title = "Top 10 Countries by Adult Netflix Content",
        x = "Country",
        y = "Number of Titles") +
    theme_minimal()
