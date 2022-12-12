### scraping reviews and other variables form amazon.com for sony, bose & sennheiser headphones one at a time ###
#### using the unique product key and the applicable number of reviews available ####

#### Sennheiser (the below code was repeated for sony and bose respectively) #####
#Install pacman. It reduces typing in package management actions. The function names in pacman package follow the format p_xxx
install.packages("pacman")
library("pacman")
#p_load allows the user to load one or more packages as a substitute for the library function
pacman::p_load(XML, dplyr, stringr, rvest, audio)

# Creating a trim function to remove all white spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Initialising the product code of the product you want to examine 
#View the product in Amazon, get the product code from its URL. It looks something like this: B073HCFZM7
prod_code = "B07XYLPPSL"

#Amazon's URL is easy to build. Just concatenate the product code at the end
url <- paste0("https://www.amazon.com/dp/", prod_code)
#Read the HTML source of the URL and store it in doc
doc <- read_html(url)

#Obtain the product name. The product name is stored in the 'doc' html. '#productTitle' gives the product name
#Also trim the spaces and new lines
prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()

#Now we have got the Product name!
prod

#Source funtion is used to parse Amazon html pages for data
#The source has the amazon_scraper function in it
#It parses the data and gives the following values - prod, title, author, date, ver.purchase, format, stars, comments and helpful for each product



#Parse Amazon html pages for data
amazon_scraper <- function(doc, reviewer = T, delay = 0){
  
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load_gh("trinker/sentimentr")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)
  
  sec = 0
  if(delay < 0) warning("delay was less than 0: set to 0")
  if(delay > 0) sec = max(0, delay + runif(1, -1, 1))
  
  #Remove all white space
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text()
  
  # author <- doc %>%
  #  html_nodes(".review-byline .author") %>%
  #  html_text()
  
  author <- doc %>%
    html_nodes("#cm_cr-review_list .a-profile-name") %>%
    html_text()
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  ver.purchase <- doc%>%
    html_nodes(".review-data.a-spacing-mini") %>%
    html_text() %>%
    grepl("Verified Purchase", .) %>%
    as.numeric()
  
  format <- doc %>% 
    html_nodes(".review-data.a-spacing-mini") %>% 
    html_text() %>%
    gsub("Color: |\\|.*|Verified.*", "", .)
  #if(length(format) == 0) format <- NA
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  #helpful <- doc %>%
  #  html_nodes(".cr-vote-buttons .a-color-secondary") %>%
  #  html_text() %>%
  #  str_extract("[:digit:]+|One") %>%
  #  gsub("One", "1", .) %>%
  #  as.numeric()
  
  # Helpful votes number cannot be extracted from reviews with no helpful votes cast
  helpful <- doc %>%
    html_nodes("#cm_cr-review_list  .cr-vote-text") %>%
    html_text() %>%
    str_extract("[:digit:]+|One") %>%
    gsub("One", "1", .) %>%
    as.numeric() 
  
  if(reviewer == T){
    
    rver_url <- doc %>%
      html_nodes(".review-byline .author") %>%
      html_attr("href") %>%
      gsub("/ref=cm_cr_othr_d_pdp\\?ie=UTF8", "", .) %>%
      gsub("/gp/pdp/profile/", "", .) %>%
      paste0("https://www.amazon.com/gp/cdp/member-reviews/",.) 
    
    #average rating of past 10 reviews
    rver_avgrating_10 <- rver_url %>%
      sapply(., function(x) {
        read_html(x) %>%
          html_nodes(".small span img") %>%
          html_attr("title") %>%
          gsub("out of.*|stars", "", .) %>%
          as.numeric() %>%
          mean(na.rm = T)
      }) %>% as.numeric()
    
    rver_prof <- rver_url %>%
      sapply(., function(x) 
        read_html(x) %>%
          html_nodes("div.small, td td td .tiny") %>%
          html_text()
      )
    
    rver_numrev <- rver_prof %>%
      lapply(., function(x)
        gsub("\n  Customer Reviews: |\n", "", x[1])
      ) %>% as.numeric()
    
    rver_numhelpful <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Helpful Votes:|\n", "", x[2]) %>%
          trim()
      ) %>% as.numeric()
    
    rver_rank <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Top Reviewer Ranking:|Helpful Votes:.*|\n", "", x[2]) %>%
          removePunctuation() %>%
          trim()
      ) %>% as.numeric()
    
    df <- data.frame(title, date, ver.purchase, format, stars, comments, helpful,
                     rver_url, rver_avgrating_10, rver_numrev, rver_numhelpful, rver_rank, stringsAsFactors = F)
    
  } #else df <- data.frame(title, author, date, ver.purchase, format, stars, comments, helpful, stringsAsFactors = F)
  # Removing 'author', 'helpful' from the dataframe. (Resolving the open issue: Error in data.frame(title, author, date, ver.purchase, format, stars,  arguments imply differing number of rows) 
  # Amazon.com had changed the HTML code of the reviews page. Due to different HTML nodes, this script to extract author, and helpful votes did not work
  # I added the right HTML tags in the code, but still removed them from the data frame
  else df <- data.frame(title, date, ver.purchase, format, stars, comments, stringsAsFactors = F)
  
  return(df)
}


#Give the number of pages of reviews you want to extract 
pages <- 50

#Initialising the sennheiser_reviews_all variable as NULL
sennheiser_reviews_all <- NULL

#Extracting the pages of reviews and storing it in 'sennheiser_reviews_all' list variable
for(page_num in 1:pages){
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  doc <- read_html(url)  
  reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
  sennheiser_reviews_all <- rbind(sennheiser_reviews_all, cbind(prod, reviews))
}

#Check the structure of 'sennheiser_reviews_all' list
str(sennheiser_reviews_all)

#changing column variables to sony, bose e.t.c
sennheiser_reviews_all$prod <- paste("sennheiser")

#binding all three dataframes together
headphones <- rbind(bose_reviews_all, sony_reviews_all, sennheiser_reviews_all)

#renaming comment variable to 'text'
colnames(headphones)[7] <- "text"

## tokenising 

library(tidytext)
tidy_headphones <- headphones %>%
  unnest_tokens(word, text)
print(tidy_headphones)

#removing stop words
data(stop_words)
headphones_no_stop <- tidy_headphones %>%
  anti_join(stop_words)
print(headphones_no_stop)
#printing the count frequencies for each token without stop words
headphones_no_stop %>%
  count(word, sort=TRUE)

install.packages("tidyverse")
library(tidyverse)

#plotting the token frequencies:
install.packages("ggplot")
library(ggplot2)
freq_hist <-headphones_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>1500) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(freq_hist)


#############################################
### creating a tidy format for products: bose headphones
bose <- headphones %>%
  filter(prod == "bose")  

tidy_bose <- bose %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_bose)

### creating a tidy format for sony headphones
sony <- headphones %>%
  filter(prod == "sony")

tidy_sony <- sony %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_sony)


### creating a tidy format for sennheiser
sennheiser <- headphones %>%
  filter(prod == "sennheiser")

tidy_sennheiser <- sennheiser %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_sennheiser)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################  
library(tidyr)
library(tidyverse)
library(dplyr)
frequency <- bind_rows(mutate(tidy_sony, author="bose"),
                       mutate(tidy_bose, author= "sony"),
                       mutate(tidy_sennheiser, author="sennheiser")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%  # % of freq of token compared to sum of freq
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `sony`, `sennheiser`) # by exclusion, bose headphone is the benchmark

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`bose`, 
                      color = abs(`bose`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "bose", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$author == "sony",],  ##58.7%
         ~proportion + `bose`)

cor.test(data=frequency[frequency$author == "sennheiser",],  #88.8%
         ~proportion + `bose`)


#dtm headphones
headphones_dtm <- tidy_headphones %>%
  count(prod, word) %>%
  cast_dtm(prod, word, n)
headphones_dtm

headphones_dtm <- tidy_headphones %>%
  count(stars, word) %>%
  cast_dtm(stars, word, n)
headphones_dtm
## Sparsity for stars is 65% and the sparsity for the prod is 53%
#### so for the classification it is better with stars


### Sentiment analysis with Headphones ###
###############################################################

library(tidytext)

library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)

tidy_headphones <- headphones %>%
  unnest_tokens(word, text)   #tidy format for sentiment
print(tidy_headphones)

nrcpositive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")  #loads the nrc positive sentiments

#inner joining the headphones and the positive sentiments
tidy_headphones %>%
  filter(prod == "bose") %>%
  inner_join(nrcpositive) %>%
  count(word, sort=TRUE)

########################################################
##### Comparing different sentiment libraries on headphones ####
########################################################

bose_headphones <- tidy_headphones %>%
  filter(prod == "bose")

afinn <- bose_headphones %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiments=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  bose_headphones %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  bose_headphones %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(bing_and_nrc, afinn) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

# from the tidy format for "sony"
bing_counts_sony <- tidy_sony %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_sony

bing_counts_sony %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# from the tidy format for "bose"
bing_counts_bose <- tidy_bose %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_bose

bing_counts_bose %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# from the tidy format for "sennheiser"
bing_counts_sennheiser <- tidy_sennheiser %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_sennheiser

bing_counts_sennheiser %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


####### TF-IDF framework in Headphones #######
######################################################


library(dplyr)
library(stringr)
library(tidytext)
library(tidytuesdayR)

# grouping by the product: prod
tidy_headphones <- headphones %>%
  unnest_tokens(word, text) %>%
  count(prod, word, sort=TRUE) %>%
  ungroup()

total_words <- tidy_headphones %>%
  group_by(prod) %>%
  summarize(total=sum(n))


headphone_words <- left_join(tidy_headphones, total_words)%>%
  filter(prod %in% c("bose", "sony", "sennheiser"))

print(headphone_words)
install.packages("ggplot2")
library(ggplot2)
ggplot(headphone_words, aes(n/total, fill = prod))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~prod, ncol=2, scales="free_y")
#the tails represent extremely common words which we are not really 
# interested in. we want the not so common words. 

######################################
########## ZIPF's law ################
######################################

freq_by_rank <- headphone_words %>%
  group_by(prod) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=prod))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################

prod_words <- headphone_words %>%
  bind_tf_idf(word, prod, n)

prod_words # we get all the zeros because we are looking at stop words ... too common

prod_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
prod_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(prod) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=prod))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~prod, ncol=2, scales="free")+
  coord_flip()


###### N-grams and tokenizing ###############
#############################################

library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)


headphone_bigrams <- headphones %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

headphone_bigrams #We want to see the bigrams (words that appear together, "pairs")

headphone_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- headphone_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

## Visualising a bigram network ##
install.packages("igraph")
library(igraph)  

bigram_graph <- bigram_counts %>%
  filter(n>45) %>%
  graph_from_data_frame()

bigram_graph

#install package
install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") + # fr is frequency
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust=1, hjust=1)


######## adding sentiment from affin

bigrams_filtered_sentimenet <- bigrams_filtered %>%
  left_join(get_sentiments("afinn"), by=c("word1"="word"))%>%
  left_join(get_sentiments("afinn"), by=c("word2"="word")) %>%
  mutate_all( ~replace(., is.na(.), 1))%>% # we need to replace the NAs with 1 so that it's neutral for multiplication
  mutate(total_sentiment = value.x*value.y) %>% #here we multiple the sentiment in bigrams
  mutate(clean_sentiment = ifelse(total_sentiment==1, 0, total_sentiment))# we need to remove those 1-s from the multiplication
# ordering the bigrams filter sentiment 

most_negative <- bigrams_filtered_sentimenet %>%
  arrange(clean_sentiment)

highest_sentiment <- bigrams_filtered_sentimenet %>%
  arrange(desc(clean_sentiment))

# getting the total combined afinn sentiment 
sum(bigrams_filtered_sentimenet$clean_sentiment)                


################################################
###Pairwise correlations between words #########
################################################

#install.packages("widyr")
library(widyr)
library(tidyr)
library(dplyr)
library(ggraph)
library(igraph)
library(tidytuesdayR)

my_tidy_headphones <- headphones %>%
  filter(ver.purchase == 1) %>%   #can i change this to stars or another product
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

my_tidy_headphones
#taking out the least common words
word_cors <- my_tidy_headphones %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, title, sort=TRUE)
#pairwise_cor() check correlation based on how ofter words appear in the same section

word_cors %>%
  filter(item1 == "entertain")
########################################################
####### creating barcharts for correlations ############
########################################################

word_cors %>%
  filter(item1 %in% c("quality", "music", "app", "price")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

########################################################
####### creating a correlation network #################
########################################################

#this will take some time to run, we will need to wait for the result
# feel free to adjust the geom_node_point to somehting smaller

word_cors %>%
  filter(correlation >.93) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=4)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()



#calling the Latent Dirichlet Allocation algorithm
## convert data frame to dtm
library(tm)
my_corpus <- Corpus(VectorSource(headphones))
dtm <- DocumentTermMatrix(my_corpus)
dtm


ap_lda <- LDA(dtm, k=2, control=list(seed=123))
ap_lda

#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"
library(tidytext)
ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics
library(ggplot2)
library(dplyr)
library(tidyr)

top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

beta_spread <- ap_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic2/topic1))

beta_spread






