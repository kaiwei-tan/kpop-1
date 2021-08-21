# 1. SETUP
library(dplyr)
library(rvest)
library(tidyverse)
library(lubridate)

# 2. IMPORTING DATA
url <- "https://kprofiles.com/kpop-birthdays/"

text_raw <- read_html(url) %>% html_nodes('p') %>% html_text()

# Must unlist after every split
text <-
  text_raw[2:371] %>% # Remove other body text (not needed)
  strsplit(., '\\\n') %>% # Remove \n
  unlist() %>%
  strsplit('(?<=\\d{4})(?=[[:alpha:]])', perl=TRUE) %>% # Split e.g. '1993NextPersonName'
  unlist() %>%
  strsplit('(?<=\\d{4})\\s+(?=[[:alpha:]])', perl=TRUE) %>% # Split e.g. '1993 NextPersonName'
  unlist() %>%
  strsplit('(?<=\\))(?=[[:alpha:]])', perl=TRUE) %>% # Split e.g. ')NextPersonName'
  unlist() %>%
  str_squish() # Remove unnecessary spaces

# 3. DATA CLEANING
# 3.1 TIDYING UP THE FIELDS
idol_group_birthday <- function(text) {
  i_g_b <-
    text %>%
    strsplit('\\s[–-]\\s') %>% # Split to 'Name (Group)' 'Birthday'
    unlist() %>%
    strsplit('\\s\\(', perl=TRUE) %>% # Split to 'Name' 'Group)' 'Birthday'
    unlist() %>%
    gsub('\\)$', '', .) %>% # Remove extra close bracket ')'
    str_squish()
  
  return(i_g_b)
}

idol_birthdays <- list(name = rep(NA, length(text)),
                       group = rep(NA, length(text)),
                       birthday = rep(NA, length(text)))

for (i in 1:length(text)) {
  i_g_b <- idol_group_birthday(text[i])
  idol_birthdays$name[i] <- i_g_b[1]
  idol_birthdays$group[i] <- i_g_b[2]
  idol_birthdays$birthday[i] <- i_g_b[3]
}

# Convert list to dataframe
idol_birthdays <- idol_birthdays %>% as.data.frame()

rm(i, i_g_b)

months <- c(month.name, month.abb)

idol_birthdays <-
  idol_birthdays[!(idol_birthdays$name %in% months
                   & is.na(idol_birthdays$birthday)), ]

# Separate group name and birthday
group_birthday <- function(text) {
  for (month in months) {
    pos <- 
      text %>%
      gregexpr(month, .) %>%
      unlist()
    
    if (pos > 0) {
      break
    }
  }
  
  if (pos == 1) {
    g_b <- list(group = 'Soloist',
                birthday = substr(text, pos, nchar(text)))
  } else if (pos > 1) {
    g_b <- list(group = substr(text, 1, pos-2),
                birthday = substr(text, pos, nchar(text)))
  }
  
  return(g_b)
}

# Apply function to clean up
for (id in which(!complete.cases(idol_birthdays))) {
  g_b <- group_birthday(idol_birthdays$group[id])
  
  idol_birthdays$group[id] <- g_b$group
  idol_birthdays$birthday[id] <- g_b$birthday
}

rm(id, g_b)

# 3.2 TIDYING UP THE TEXT
# Preliminary processing
idol_birthdays$group <-
  idol_birthdays$group %>%
  gsub('\\/DBSK', '', .)  %>%
  gsub('\\)[-;]', '', .) %>%
  gsub('\\–', '', .) %>%
  gsub('\\)', '', .) %>%
  gsub('f\\(x', 'f(x)', .) %>%
  gsub('GI-DLE', '(G)I-DLE', .)

# Manual correction
idol_birthdays[171,]$group <- 'DIA'
idol_birthdays[1565,]$name <- 'Seoyul'
idol_birthdays[1565,]$group <- 'Berry Good'

# Multiple groups
multiple_groups <- function(df) {
  df <- filter(df, grepl(',', group) | grepl('/', group))
  
  df_output <- data.frame()
  
  for (i in 1:nrow(df)) {
    groups <- df$group[i] %>% strsplit('/') %>% unlist() %>% strsplit(',') %>% unlist()
    idol_df <- data.frame(name = rep(df$name[i], length(groups)),
                          group = groups,
                          birthday = rep(df$birthday[i], length(groups)))
    
    df_output <- rbind(df_output, idol_df)
  }
  
  return(df_output)
}

# Perform function for those rows, append to original dataframe, remove original rows
idol_birthdays <-
  rbind(idol_birthdays[-which(grepl(',', idol_birthdays$group) | grepl('/', idol_birthdays$group)), ],
        multiple_groups(idol_birthdays))

# Removing soloists and actors
idol_birthdays <-
  idol_birthdays[-which(idol_birthdays$group == 'Solo' |
                          idol_birthdays$group == 'Soloist' |
                          idol_birthdays$group == 'Solo Singer' |
                          idol_birthdays$group == 'Solist/' |
                          idol_birthdays$group == 'Actor' |
                          idol_birthdays$group == 'Actress'), ]

# Removing former/ex members
idol_birthdays$group <-
  idol_birthdays$group %>%
  gsub('[fF]ormer.*', '', .) %>%
  gsub('[eE]x.*', '', .)

idol_birthdays <- 
  idol_birthdays[!idol_birthdays$group == '', ]

# Manual correction
idol_birthdays$group <-
  idol_birthdays$group %>%
  gsub('also known as PUNCH', '1PUNCH', .) %>%
  gsub('A-Peace ‘Jade‘', 'A-Peace ‘Lapis’', .) %>%
  gsub('A-Peace ‘Lapis$', 'A-Peace ‘Lapis’', .) %>%
  gsub('A-Peace ‘Lapis‘', 'A-Peace ‘Lapis’', .) %>%
  gsub('B2st', 'Beast', .) %>%
  gsub('Bigflo', 'BIGFLO', .) %>%
  gsub('BigFlo', 'BIGFLO', .) %>%
  gsub('Boyfriendist', 'Boyfriend', .) %>%
  gsub('Dalshabet', 'Dal Shabet', .) %>%
  gsub('F.Cuz', 'F.CUZ', .) %>%
  gsub('FT Island', 'FT. Island', .) %>%
  gsub('Kara', 'KARA', .) %>%
  gsub('miss A', 'Miss A', .) %>%
  gsub('Moxine', 'MOXIE', .) %>%
  gsub('MOXINE', 'MOXIE', .) %>%
  gsub('NU-EST', 'NU’EST', .) %>%
  gsub('RaNia', 'Rania', .) %>%
  gsub('Satruday', 'Saturday', .) %>%
  gsub('SKARF', 'SKarf', .) %>%
  gsub('Touch', 'TOUCH', .) %>%
  gsub('Varsity', 'VARSITY', .) %>%
  str_squish()

idol_birthdays$birthday[which(idol_birthdays$name == 'Samuel' & idol_birthdays$group == '1PUNCH')] <- 'Jan 17, 2002'
idol_birthdays$group[which(idol_birthdays$name == 'Aoora')] <- 'AA'
idol_birthdays$birthday[which(idol_birthdays$name == 'Aoora')] <- 'Jan 10, 1986'

# Filter out groups 
idol_birthdays <-
  idol_birthdays %>%
  group_by(group) %>%
  summarize(group_size = n()) %>%
  right_join(idol_birthdays, by='group') %>%
  filter(group_size > 1) %>%
  select(name, group, birthday)

# 3.3 TIDYING UP THE DATES
# Standardize the dates
standard_date <- function(text) {
  b <- 
    text %>%
    gsub('\\,', ' ', .) %>% # Remove commas
    gsub('th', '', .) %>% # Remove 'th' (not recognized)
    gsub('Sept', 'Sep', .) %>% # Change 'Sept' (not recognized) to 'Sep' 
    str_squish() %>%
    as.Date(format('%b %d %Y'))
  
  return(b)
}

idol_birthdays <-
  mutate(idol_birthdays, birthday = standard_date(birthday))

# Sort by date and tidy up row numbers
idol_birthdays <-
  idol_birthdays %>%
  mutate(month = lubridate::month(birthday),
         day = lubridate::day(birthday)) %>%
  arrange(month, day, name) %>%
  select(name, group, birthday)

# 4. DATA VISUALIZATION
# Distribution of Group Sizes
idol_birthdays %>%
  group_by(group) %>%
  summarize(group_size = n()) %>%
  group_by(group_size) %>%
  summarize(n = n()) %>%
  ggplot(aes(x=group_size, y=n, fill=group_size)) +
  geom_bar(stat='identity') +
  theme(legend.position = 'none') +
  labs(title='Distribution of Group Sizes', x='Group Size', y='No. of Groups') 

# No. of Idols by Birth Year
idol_birthdays %>%
  mutate(year = format(birthday, format='%Y')) %>%
  group_by(year) %>%
  summarize(number = n()) %>%
  ggplot(aes(x=year, y=number, fill=year)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = 'none') +
  labs(title='No. of Idols by Birth Year', x='Year', y=NULL) 

# No. of Idols by Birth Month
idol_birthdays %>%
  mutate(month = factor(format(birthday, format='%b'), levels = month.abb)) %>%
  group_by(month) %>%
  summarize(number = n()) %>%
  ggplot(aes(x=month, y=number, fill=month)) +
  geom_bar(stat='identity') +
  theme(legend.position = 'none') +
  labs(title='No. of Idols by Birth Month', x='Month', y=NULL) 

# 5. BACK TO THE QUESTION
# 5.1 Birthday Range

# Since year is irrelevant, standardize all years to a specified non-leap year (1997)
idol_birthdays2 <- idol_birthdays
lubridate::year(idol_birthdays2$birthday) <- 1997

birthday_range <- function(birthdays, show_start=FALSE) {
  gaps <- vector()
  
  for (i in 1:length(birthdays)) {
    if (i == length(birthdays)) {
      gaps <- c(gaps, 365 + birthdays[1] - birthdays[i])
    } else {
      gaps <- c(gaps, birthdays[i+1] - birthdays[i])
    }
  }
  
  largest_gap_index <- which.max(gaps)
  
  if (largest_gap_index == length(birthdays)) {
    start_index = 1
    last_index = length(birthdays)
  } else {
    start_index = largest_gap_index + 1
    last_index = largest_gap_index
  }
  
  # 365 - largest gap in days + 1 to include last person's birthday
  range = 366 - gaps[last_index]
  
  if (show_start) {
    return(format(birthdays[start_index], '%m/%d'))
  }
  
  return(range)
}

# Apply function on one group
twice_birthdays <- filter(idol_birthdays2, group == 'Twice')

paste(twice_birthdays$group[1],
      'has a range of',
      birthday_range(twice_birthdays$birthday),
      'and we start counting from',
      birthday_range(twice_birthdays$birthday, show_start=TRUE))

# Apply function
idol_birthday_ranges <-
  idol_birthdays2 %>%
  group_by(group) %>%
  summarize(range = birthday_range(birthday)) %>%
  arrange(range)

# 5.2 Function to Cover Birthdays

# start_date: string in the format 'dd/mm'
# end_date: string in the format 'dd/mm'
# groups: character vector
which_idols <- function(start_date, end_date, groups, df) {
  range <-
    c(start_date, end_date) %>%
    paste0('/1997') %>%
    as.Date(format='%d/%m/%Y')
  
  if (range[2] < range[1]) {
    range <- c(range, range[2] + 365)
    
    celebrations <- 
      df %>%
      filter(group %in% groups &
               ((birthday >= range[1] & birthday <= range[3]) | birthday <= range[2]))
  } else {
    celebrations <- 
      df %>%
      filter(group %in% groups & birthday >= range[1] & birthday <= range[2])
  }
  
  celebrations <-
    celebrations %>%
    arrange(birthday) %>%
    select(name, group) %>%
    left_join(idol_birthdays, by=c('name', 'group')) 
  
  return(celebrations)
}