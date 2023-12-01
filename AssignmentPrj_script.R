### ASSIGNMENT MILESTONE 1 ###

# INSTALL NECESSARY PACKAGES ----

install.packages("remotes")

# install GitHub version of vosonSML 0.32.10
install_github("vosonlab/vosonSML")

# install GitHub version of rtweet 1.1.0.9001
install_github("ropensci/rtweet")

# Load packages required for this session into library
library(remotes)
library(vosonSML)
library(magrittr)
library(igraph)
library(tidyr)
library(tidytext)
library(stopwords)
library(dplyr)

# get twitter App name, API key, API secret, Access token, Access token secret
my_app_name <- "Class7230ICT"
my_api_key <- "5k06tQOIdyTsT9xR914kY3epo"
my_api_secret <- "FsLe2v57PVk1L8Pt9ez95KQ0ROPMrOJh5SySyJRlwOwDfMfRum"
my_access_token <- "1632183185256878080-DgxyxOrMCJcKZ38xXb1gQSs8svlQBo"
my_access_token_secret <- "euv2yxyqjLlDP8CNWttAPGqWJ07nOArFNPRmZsLJlOaPh"

# ----------===============================================================================================---------- #

# QUESTION 1 - COLLECT DATA ----



# Search terms topic 1 - FAN ----
twitter_fan_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "Justin Bieber OR Justin Bieber tour",
          searchType = "recent",
          numTweets = 1000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE)
View(twitter_fan_data$tweets)


# Search terms topic 2 - MUSIC PRODUCT ----
twitter_musicProduct_data <- Authenticate("twitter",
                                 appName = my_app_name,
                                 apiKey = my_api_key,
                                 apiSecret = my_api_secret,
                                 accessToken = my_access_token,
                                 accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "Justin Bieber album OR Justin Bieber hit",
          searchType = "recent",
          numTweets = 1000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE)
View(twitter_musicProduct_data$tweets)


# Search terms topic 3 - INTERESTED USERS ----
twitter_relatedFan_data <- Authenticate("twitter",
                                 appName = my_app_name,
                                 apiKey = my_api_key,
                                 apiSecret = my_api_secret,
                                 accessToken = my_access_token,
                                 accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "Justin Bieber collab OR Justin Bieber ft",
          searchType = "recent",
          numTweets = 1000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE)
View(twitter_relatedFan_data$tweets)

# ----------===============================================================================================---------- #

# QUESTION 2 - ACTOR NETWORK ----



# Actor Network 1 - FAN ----

# Create actor network and graph from the data
fan_network <- twitter_fan_data %>% Create("actor")
fan_graph <- fan_network %>% Graph()

# Write graph to file
write.graph(fan_graph, file = "FanNetwork.graphml", format = "graphml")

# Overwrite the 'name' attribute in the graph with the 'screen name' attribute
V(fan_graph)$name <- V(fan_graph)$screen_name

# Run Page Rank algorithm to find important users
rank_fan <- sort(page_rank(fan_graph)$vector, decreasing = TRUE)
head(rank_fan, n=5)

# Actor Network 2 - MUSIC PRODUCT ----

musicProduct_network <- twitter_musicProduct_data %>% Create("actor")
musicProduct_graph <- musicProduct_network %>% Graph()

write.graph(musicProduct_graph, file = "MusicProduct.graphml", format = "graphml")

V(musicProduct_graph)$name <- V(musicProduct_graph)$screen_name
rank_musicProduct <- sort(page_rank(musicProduct_graph)$vector, decreasing = TRUE)
head(rank_musicProduct, n=5)

# Actor Network 3 - INTERESTED USERS ----

relatedFan_network <- twitter_relatedFan_data %>% Create("actor")
relatedFan_graph <- relatedFan_network %>% Graph()

write.graph(relatedFan_graph, file = "RelatedFan.graphml", format = "graphml")

V(relatedFan_graph)$name <- V(relatedFan_graph)$screen_name
rank_relatedFan <- sort(page_rank(relatedFan_graph)$vector, decreasing = TRUE)
head(rank_relatedFan, n=5)

# ----------===============================================================================================---------- #

# QUESTION 3 - SEMANTIC NETWORK ----



# Semantic Network 1 - FAN ----
semantic_fan_network <- twitter_fan_data %>% Create("semantic")
semantic_fan_graph <- semantic_fan_network %>% Graph()

write.graph(semantic_fan_graph, file = "FanSemantic.graphml", format = "graphml")

rank_semantic_fan <- sort(page_rank(semantic_fan_graph)$vector, decreasing = TRUE)
head(rank_semantic_fan, n=10)

# Semantic Network 2 - MUSIC PRODUCT ----
semantic_musicProduct_network <- twitter_musicProduct_data %>% Create("semantic")
semantic_musicProduct_graph <- semantic_musicProduct_network %>% Graph()

write.graph(semantic_musicProduct_graph, file = "ProductSemantic.graphml", format = "graphml")

rank_semantic_musicProduct <- sort(page_rank(semantic_musicProduct_graph)$vector, decreasing = TRUE)
head(rank_semantic_musicProduct, n=10)

# Semantic Network 3 - INTERESTED USERS ----
semantic_relatedFan_network <- twitter_relatedFan_data %>% Create("semantic")
semantic_relatedFan_graph <- semantic_relatedFan_network %>% Graph()

write.graph(semantic_relatedFan_graph, file = "RelatedSemantic.graphml", format = "graphml")

rank_semantic_relatedFan <- sort(page_rank(semantic_relatedFan_graph)$vector, decreasing = TRUE)
head(rank_semantic_relatedFan, n=10)

# ----------===============================================================================================---------- #

# QUESTION 4 - CALCULATE UNIQUE USERS ----



# DATASET 1 - FAN ----

# Create a dataframe for the data set
dataFrame_twitter_fan <- data.frame(twitter_fan_data)
View (dataFrame_twitter_fan)

# Calculate unique users in the new dataframe just created using n_distinct() function from dplyr package
n_distinct(dataFrame_twitter_fan$tweets.user_id)



# DATASET 2 - PRODUCT ----

# Create a dataframe for the data set
dataFrame_twitter_musicProduct <- data.frame(twitter_musicProduct_data)
View (dataFrame_twitter_musicProduct)

# Calculate unique users in the new dataframe just created using n_distinct() function from dplyr package
n_distinct(dataFrame_twitter_musicProduct$tweets.user_id)



# DATASET 3 - INTERESTED USERS ----

# Create a dataframe for the data set
dataFrame_twitter_relatedFan <- data.frame(twitter_relatedFan_data)
View (dataFrame_twitter_relatedFan)

# Calculate unique users in the new dataframe just created using n_distinct() function from dplyr package
n_distinct(dataFrame_twitter_relatedFan$tweets.user_id)







