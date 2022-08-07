###############################################
#### MSBA 1 HULT 2021-2022 ###################
#### Business Insight Report of 10-K #########
#### Text Analytics & NLP ####################
#### CREATED BY: NIVEDITA Venkatramanan ######
#### nvenkatramanan@student.hult.edu #########
#### Submission Date: December 5, 2021  ######
###############################################


## Library packages required for the analysis 

library(magrittr)
library(tidyverse)
library(tidytext)
library(textdata)
library(pdftools)
library(tm)
library(wordcloud)
library(scales)
library(ggplot2)
library(igraph)
library(ggraph)
library(topicmodels)
library(widyr)

########### xxxx ###############

### Importing all PDF files from the same folder which is in my C drive. 

# Using the below code as there are multiple files (3 in this case) inside a folder that needs to be put together

getwd()
setwd("C:/MBAN HULT/Text Analytics & NLP/Business Insight Report/PDF for project")
nm <- list.files(path="C:/MBAN HULT/Text Analytics & NLP/Business Insight Report/PDF for project")

my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x))) ## Binding the files together by rows
my_pdf_text <- as.data.frame(my_pdf_text) # Converting to a dataframe so that we don't have errors


## We have 116 columns, so we need to combine them together so that we have only 1 column 
## Renaming as annual_reports
annual_reports <- unite(my_pdf_text,text, 1:116, sep=" ", remove = T, na.rm = F)

## Adding a column to name each row for the respective company & renaming the variable as company
annual_reports <- annual_reports %>%
  bind_cols(c("Columbia","Starbucks","UnderArmour"))
colnames(annual_reports)[2] <- "company"

## The dataframe called annual_reports is now ready to start work with. 


####### Tidying the data ##

## Calling stop words data so that we can filter out the stop words to create a tidy format for each company's annual report. 
data("stop_words")

### Creating a tidy format for starbucks
tidy_star <- annual_reports %>% 
              filter(company == "Starbucks") %>% 
              unnest_tokens(word,text) %>%
              anti_join(stop_words)

### Creating a tidy format for columbia
tidy_columbia <- annual_reports %>% 
  filter(company == "Columbia") %>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

### Creating a tidy format for under armour
tidy_ua <- annual_reports %>% 
  filter(company == "UnderArmour") %>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

### Creating a consolidated tidy format of the entire dataset 
tidy_report <- annual_reports %>% 
            unnest_tokens(word,text) %>% 
            anti_join(stop_words)
  

#############################################
### Framework 1 - Correlograms 
#############################################

##Combining all the datasets and counting frequencies - which will be used to plot the correlograms

frequency <- bind_rows(mutate(tidy_star, company="Starbucks"),
                       mutate(tidy_columbia, company= "Columbia"), 
                      mutate(tidy_ua, company="UnderArmour")
                      ) %>% #closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(company, word) %>%
  group_by(company) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, `Starbucks`,`Columbia`)


### Plotting the frequencies as correlograms:

ggplot(frequency, aes(x=proportion, y=`UnderArmour`, 
                      color = abs(`UnderArmour`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~company, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "UnderArmour", x=NULL)

#### Additionally - performing a correlation test between the 3 companies 

cor.test(data=frequency[frequency$company == "Starbucks",],
         ~proportion + `UnderArmour`)

cor.test(data=frequency[frequency$company == "Columbia",],
         ~proportion + `UnderArmour`)

#############################################
### Framework 2 - TF IDF 
#############################################

annual_token <- annual_reports %>%   ## finding the number of each token company wise  
  unnest_tokens(word, text) %>%
  count(company, word, sort=TRUE) %>%
  ungroup()


total_words <- annual_token %>%  ## finding the total sum of tokens per company
  group_by(company) %>%
  summarize(total=sum(n))


## combining the 2 sets giving us word count of each token company wise and total number of words company wise
report_words <- left_join(annual_token, total_words) 

## using the tf idf function to get the tf, if and tf-idf values 
company_words <- report_words %>%
  bind_tf_idf(word, company, n)  ## we want to see the word, company(document)


## arrange in descending order of tf-idf so that we can see the unique words in each company
uniqueness <- company_words %>%
  arrange(desc(tf_idf))


## plotting the unique words to get a visual representation of the top 15 words they use in their reports
uniqueness %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(company) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=company))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~company, ncol=2, scales="free")+
  coord_flip()


#############################################
### Framework 3 - N-grams 
#############################################

######### Creating a bigram for Columbia ##############

columbia_bigrams <- annual_reports %>%       ## splitting into bigrams and removing stopwords
                  filter(company == "Columbia") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  separate(bigram,c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%   
  filter(!word2 %in% stop_words$word)

columbia_bigrams_count <- columbia_bigrams %>%  ## sorting the bigrams 
  count(word1, word2, sort = TRUE)


bigram_graph_columbia <- columbia_bigrams_count %>% ## filtering the bigrams with count greater than 20 for plotting them
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph_columbia

## Using ggraph to plot the bigram network
ggraph(bigram_graph_columbia, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  labs(title = "Bigrams - Columbia Sportswear")


######### Creating a bigram for Starbucks ##############

starbucks_bigrams <- annual_reports %>% 
  filter(company == "Starbucks") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  separate(bigram,c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%   
  filter(!word2 %in% stop_words$word)

starbucks_bigrams_count <- starbucks_bigrams %>%
  count(word1, word2, sort = TRUE)


bigram_graph_starbucks <- starbucks_bigrams_count %>%  ## filtering the bigrams with count greater than 15
  filter(n>15) %>%
  graph_from_data_frame()

bigram_graph_starbucks

ggraph(bigram_graph_starbucks, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) +
  labs(title = "Bigrams - Starbucks")



######### Creating a bigram for UnderArmour ##############

ua_bigrams <- annual_reports %>% 
  filter(company == "UnderArmour") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  separate(bigram,c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%   
  filter(!word2 %in% stop_words$word)


ua_bigrams_count <- ua_bigrams %>%
  count(word1, word2, sort = TRUE)


bigram_graph_ua <- ua_bigrams_count %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph_ua

ggraph(bigram_graph_ua, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) + 
  labs(title = "Bigrams - UnderArmour")


#############################################
### Framework 4 - Sentiment Analysis 
#############################################

#### Sentiment Analysis - Columbia 

bing_columbia <- tidy_columbia %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_columbia %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(title = "Sentiment Analysis - Columbia Sportswear",y="Contribution to sentiment", x=NULL)+
  coord_flip()

afinn_score_columbia <- tidy_columbia %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


##calculating the maximum negative value 

afinn_neg_columbia <- tidy_columbia %>%
  inner_join(get_sentiments("afinn"))%>%
  filter(value<1) %>% 
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

#### Sentiment Analysis - Starbucks 

bing_starbucks <- tidy_star %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_starbucks %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(title = "Sentiment Analysis - Starbucks Corporation",y="Contribution to sentiment", x=NULL)+
  coord_flip()

afinn_score_starbucks <- tidy_star %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_neg_star <- tidy_star %>%
  inner_join(get_sentiments("afinn"))%>%
  filter(value<1) %>% 
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


#### Sentiment Analysis - UnderArmour 

bing_ua <- tidy_ua %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_ua %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(title = "Sentiment Analysis - Under Armour",y="Contribution to sentiment", x=NULL)+
  coord_flip()

afinn_score_ua <- tidy_ua %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_neg_ua <- tidy_ua %>%
  inner_join(get_sentiments("afinn"))%>%
  filter(value<1) %>% 
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

##########################################################################
