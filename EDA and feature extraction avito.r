


# --------------------import packages------------------------------------


library(tidyverse)
library(translate)
library(tm)
library(stringr)
library(ggplot2)
library(ggthemes)


# load data

train <- read_csv("train.csv",locale = locale(encoding = 'utf-8'))

test <- read_csv("test.csv",locale = locale(encoding = 'utf-8'))





# --------------------------------EDA----------------------------------------


##compute missing data percentage per feature in train set

propmiss <- function(dataframe)
  lapply(dataframe,function(x)
    data.frame(nmiss=sum(is.na(x)),
               n=length(x), 
               perc_miss= sum(is.na(x))/length(x))
  )

missingperc<- bind_rows(propmiss(train))

missingperc['variable_name'] <- colnames(train)

missingperc <- missingperc %>% 
  arrange(desc(perc_miss)) %>% 
  select(variable_name,perc_miss,nmiss,n)

missingperc

ggplot(missingperc, aes(x= variable_name,y = perc_miss))+ 
  geom_col(fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept= 0.05,linetype="dashed", color = "red")


# it seems that parameter 2 and 3 have over 50% data missing - might need to be removed if they
#  don't have any correlation high level of correlation with other features or the target variables.
#  parameter 1 is under 5% with description, image, image_top_1 and price just a little over 5%.






## EDA of all variables with target feature
range(train$activation_date) 


get_unique_count <- function(dataframe){
  unique_count<- data.frame()
  for (i in (1:ncol(dataframe))){
    unique_count[i,'variable_name'] <- colnames(dataframe[i])
    unique_count[i,'unique_records_count'] <- count(unique(dataframe[i]))
  }
  unique_count <- unique_count %>% 
    arrange(unique_records_count)
  
  unique_count
}

unique_count <- get_unique_count(train)

# for visualization purpose,it seems that the top 4 variables user type, parent_category_name,
# activation_date, dow_activation can be converted to categorical variables for visualization





ggplot(train, aes(x =user_type, y = deal_probability)) + 
  geom_boxplot()
#the deal probability does not seem to be drastically different among the different user type


ggplot(train, aes(x = parent_category_name, y = deal_probability)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# the deal probability seem to be quite different across different parent category name
ggplot(train) + 
  geom_boxplot(aes(x = activation_date, y = deal_probability, group = activation_date)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#plot activation_date as time series data per day of activation. it seems that certain days 
# such as March29th,april 1st, 3rd etc have much higher deal probabilities. 

ggplot(train, aes(x = region, y = deal_probability)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# doesn not seem to be anything significant different across regions in deals probabilities

ggplot(train, aes(x = category_name, y = deal_probability)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# it seems that the different category name has very different levels of deals probabilities
ggplot(train,aes(x = price, y = deal_probability)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_jitter() 


nodealperc <- sum(train$deal_probability == 0)/nrow(train)
ggplot(train, aes(deal_probability)) + 
  geom_histogram(aes(y = ..density../30))+
  ylab("percentage distribution")
# it seems that over 64% of theh deal probabilities are 0

#----------------------------- imputing missing values -----------------------------------------------








# ----------------------------------feature engineering----------------------------------------------------



#join train and test data for feature engineering
test["deal_probability"]= NA
tdata <- bind_rows(list(train,test))


# 1) dow_week: given that we only have one month of data for training, it might be more beneficial to use day
# of the week as a feature
tdata <- tdata %>% mutate(dow_activation = weekdays(activation_date))

ggplot(tdata, aes(x = dow_activation, y = deal_probability)) +
  geom_boxplot()
# the deal probability does not seem to be drastically different among the day of activation


# 2) new_para_1 and new_para_2: considering a large amount of parameter 1 and parameter 2 are
# missing values. Creating a new feature for each feature based on whether the data is missing
# then do EDA on these two new variables against target variables and see if there are any 
# correlation between missing values and the target feature

#3) features based on word count, field length, and similarities between features;
tdata <- tdata %>% 
  mutate(new_para_1 = ifelse(is.na(param_1), "missing", "hasvalue")) %>%
  mutate(new_para_2 = ifelse(is.na(param_2), "missing", "hasvalue")) %>%
  mutate(desc_word_count = str_count(description,pattern = "\\w+")) %>%
  mutate(title_word_count = str_count(title,pattern = "\\w+")) %>%
  mutate(desc_length = str_count(description)) %>%
  mutate(title_length = str_count(title)) %>%
  mutate(has_image = !is.na(image)) %>%
  mutate(sim_desc_title = levenshteinDist(description, title)) %>%
  mutate(sim_p1_p2) = levenshteinDist(param_1,param_2) %>%
  mutate(sim_p2_p3) = levenshteinDist(param_2,param_3) %>%
  mutate(sim_p1_p3) = levenshteinDist(param_3, param_1)

ggplot(tdata, aes(x = new_para_1, y = deal_probability)) +
  geom_boxplot()

ggplot(tdata, aes(x = new_para_2, y = deal_probability)) +
  geom_boxplot()

# 4) features based on deeper text analysis:

# creating corpus of description:
stopwords = stopwords("ru")

## extracting features based on term document matrix of description variable and title variable:

get_term_features <- function(dataframe, variable_name){









}
description_corpus <- Corpus(VectorSource(tdata$description))
inspect(description_corpus[1:4])


description_corpus <- description_corpus %>%
  tm_map(removeWords,stopwords) %>%
  tm_map(tolower) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)

## perform stemming -note that this should always be performed after text doc conversion

description_corpus <- tm_map(description_corpus, stemDocument, language = 'ru')

## convert to document term matrix

docterm_desc_corpus <- DocumentTermMatrix(description_corpus)

dim(docterm_desc_corpus) #check the size of the term document matrix

new_description_corpus <- removeSparseTerms(docterm_desc_corpus, sparse = 0.95)
## above is to remove variables that are 95% or more sparse.

dim(new_description_corpus)

## find frequent terms
column_sum <- colSums(as.matrix(new_description_corpus))
length(column_sum)
desc_features <- data.table(name = attributes(column_sum)$names, count = column_sum)

## most frequent and least frequent words
doc_features[order(-count)][1:10] # top 10 most frequent words
doc_features[order(count)][1:10] # least 10 frequent words


## plot features that occuring more than 20,000
ggplot(doc_features[count>20000],aes(name, count)) + 
	geom_bar(stat = "identity",fill='lightblue',color='black')+ 
	` `theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
	` `theme_economist()+` `scale_color_economist()` <br/>
	
	
# check association of terms of top features
findAssocs(new_description_corpus,"street",corlimit = 0.5)
findAssocs(new_description_corpus,"new",corlimit = 0.5)






