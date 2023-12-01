# Part 1: DATA SELECTION & EXPLORATION ----

# QUESTION 2.1: Spotify artist analysis ----

# Load packages required for this session into library
library(Rspotify)
library(spotifyr)
library(magrittr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)
library(httpuv)

# Store Spotify authentication data in cache
options(httr_oauth_cache = TRUE)

# Authentication variables
app_id <- "1bd7fc8390cd4061af4b2b72e8f82580"
app_secret <- "aa667070d1df4be1a6eb71ea0a6d16b6"
token <- "1"

# Authentication for Rspotify package:
keys <- spotifyOAuth(token, app_id, app_secret) 

# ----------===============================================================================================---------- #


# ARTIST

# Get Spotify data on 'Justin Bieber'
find_my_artist <- searchArtist("Justin Bieber", token = keys)
View(find_my_artist)

my_artist <- getArtist("1uNFoZAHBGtllmzznpCI3s", token = keys)
View(my_artist)


# ----------===============================================================================================---------- #


# ALBUMS

# Retrieving info about Justin Bieber's published albums
artist_albums <- getAlbums("1uNFoZAHBGtllmzznpCI3s", token = keys)
View(artist_albums)

# -> Not show enough albums

# Missing albums
getAlbumInfo("0sTWDNDdByvpadJqLYTR2U", token = keys)
getAlbumInfo("3BmcYMh0KYsimWL6p2gPa9", token = keys)


# ----------===============================================================================================---------- #


# Authentication for spotifyr package, use this package to visualise audio features for all songs:
Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# ----------===============================================================================================---------- #


# ARTIST AUDIO FEATURES (by all tracks)


# Get audio features for 'Justin Bieber'
audio_features <- get_artist_audio_features("Justin Bieber")

# Remove duplicate tracks
audio_features <- audio_features[!duplicated(audio_features$track_name), ]
View(audio_features)



# Plot all features scores for each album

danceability <- ggplot(audio_features, aes(x = danceability, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Danceability in Justin Bieber Albums",
          subtitle = "Based on danceability from Spotify's Web API")

valence <- ggplot(audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Happiness in Justin Bieber Albums",
          subtitle = "Based on valence from Spotify's Web API")

energy <- ggplot(audio_features, aes(x = energy, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Energy in Justin Bieber Albums",
          subtitle = "Based on energy from Spotify's Web API")

loudness <- ggplot(audio_features, aes(x = loudness, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Loudness in Justin Bieber Albums",
          subtitle = "Based on loudness from Spotify's Web API")

acousticness <- ggplot(audio_features, aes(x = acousticness, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Acousticness in Justin Bieber Albums",
          subtitle = "Based on acousticness from Spotify's Web API")

instrumentalness <- ggplot(audio_features, aes(x = instrumentalness, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Instrumentalness in Justin Bieber Albums",
          subtitle = "Based on instrumentalness from Spotify's Web API")

speechiness <- ggplot(audio_features, aes(x = speechiness, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Speechiness in Justin Bieber Albums",
          subtitle = "Based on speechiness from Spotify's Web API")

tempo <- ggplot(audio_features, aes(x = tempo, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Tempo in Justin Bieber Albums",
          subtitle = "Based on tempo from Spotify's Web API")

liveness <- ggplot(audio_features, aes(x = liveness, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Liveness in Justin Bieber Albums",
          subtitle = "Based on liveness from Spotify's Web API")



# ----------===============================================================================================---------- #



# QUESTION 2.2: Youtube artist analysis ----



# Load additional packages required for this session into library
library(remotes)
library(tuber)
library(vosonSML)
library(sqldf)


# Set up YouTube authentication variables 
api_key <- "AIzaSyA0sJVuxd9UhksHyoz6k8oMIV8rzUIytdI"
client_id <- "153711118078-0954h0hhk6cd9amu23u3mkgd3k727pts.apps.googleusercontent.com"
client_secret <- "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"


# Authenticate to YouTube using the tuber package
yt_oauth (
  app_id = client_id, 
  app_secret = client_secret
)

# ----------===============================================================================================---------- #

# SEARCH FOR VIDEOS AND GET STATISTICS


# Use search function in tuber package
artist_video <- yt_search("Justin Bieber")
View(artist_video)


# Get information about the artist channel
get_channel_stats(channel_id = "UCIwFjwMjI0y7PDBVEO9-bkQ")
artist_videos_stats <- get_all_channel_video_stats (channel_id = "UCIwFjwMjI0y7PDBVEO9-bkQ")

#typeof(artist_videos_stats$viewCount)
#typeof(artist_videos_stats$likeCount)

# ----------===============================================================================================---------- #

# TOP 20 - To investigate Views & Likes correlation

top_20 <- sqldf("SELECT id,title,viewCount,likeCount,url FROM artist_videos_stats 
                ORDER BY CAST(viewCount AS numeric) DESC
                LIMIT 20", row.names = TRUE)

df_top20 <- as.data.frame(top_20)
View(df_top20)

ggplot(df_top20, aes(x = title, y = viewCount)) +
  geom_col(fill = "mediumpurple3", alpha = 0.8, width = 0.5) +
  geom_col(aes(y = likeCount), fill = "lightpink2", alpha = 0.8, width = 0.3) +
  labs(x = "Video Title", y = "Count") +
  ggtitle("Top 20 Videos: View Count and Like Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# TOP 10 - To compare with top tracks on Spotify

top_10 <- sqldf("SELECT id,title,viewCount,likeCount,url FROM artist_videos_stats 
                ORDER BY CAST(viewCount AS numeric) DESC
                LIMIT 10", row.names = TRUE)

# TOP TRACKS from Spotify

# keys <- spotifyOAuth(token, app_id, app_secret)
artist_topTracks <- getTopTracks("1uNFoZAHBGtllmzznpCI3s", token = keys, country = "us")
View(artist_topTracks)



# ----------===============================================================================================---------- #



# Part 2: TEXT PRE-PROCESSING ----

# QUESTION 2.3: ----

# Load additional packages required for this session into library
library(rtweet)
library(tidyr)
library(tidytext)
library(stopwords)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)
library(wordcloud)

# Load the pre-collected data (10,000 tweets) from Twitter to this environment
View(twitter_data$tweets)

# Clean Text
cleanText <- twitter_data$tweets$text %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_emoji() %>% 
  replace_emoticon()
View(cleanText)

# ----------===============================================================================================---------- #

# Document Corpus

# convert clean_text vector into a document corpus
tweet_corpus <- VCorpus(VectorSource(cleanText))
# fan_tweet_corpus[[1]]$content

# further pre-processing
tweet_corpus <- tweet_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords(kind = "SMART")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

# ----------===============================================================================================---------- #

# Term Frequency Matrix

# transform corpus into a Term Frequency Matrix (Document Term Matrix)
term_matrix <- DocumentTermMatrix(tweet_corpus)

# convert Document Term Matrix into a data frame
term_df <- as.data.frame(as.matrix(term_matrix))

# calculate term frequency
freq_term <- sort(colSums(term_df), decreasing = TRUE)

head(freq_term, n = 10)

# ----------===============================================================================================---------- #

# Visualize Term Frequency

# make a new data frame with 2 cols (one stores each word, another stores that word's frequency)
word_df <- data.frame(word = names(freq_term), freq_term)
View(word_df)

# use ggplot() and wordcloud() functions to visualise top word frequency

ggplot(subset(word_df, freq_term > 700), aes(x = reorder(word, -freq_term), y = freq_term)) +
  geom_bar(stat = "identity", fill = "mediumpurple3", width = 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  labs(title = "Top 10 Word Frequency in Tweets about Justin Bieber",
       x = "Words",
       y = "Frequency") +
  coord_flip()

wordcloud(word_df$word, word_df$freq_term, max.words = 200, colors = brewer.pal(8,"Dark2"))

# ----------===============================================================================================---------- #


# PART 3: SOCIAL NETWORK ANALYSIS ----

# Question 2.4: Centrality Analysis ----

# 2.4.1.  Justin Bieber's Network Centrality Analysis ----

# Load the pre-collected data for this part to this environment
View(justin_bieber_tweet$tweets)

# CREATE TWOMODE (BIDOMAL) NETWORK

twomode_network <- justin_bieber_tweet %>% Create("twomode", removeTermsOrHashtags = c("justin bieber"))
twomode_graph <-twomode_network %>% Graph()
#length(V(twomode_graph))
#V(twomode_graph)$name

write.graph(twomode_graph, file = "JustinBieber_TwoModeNetwork.graphml", format = "graphml")

# ------------------=============================================================------------------ #

# SEPERATE GRAPH INTO COMPONENTS

twomode_comps <- components(twomode_graph, mode = c("weak"))

twomode_comps$no # number of connect components
twomode_comps$csize # number of nodes in each component

# ------------------=============================================================------------------ #

# CREATE SUB-GRAPH FOR THE LARGEST COMPONENT

# identify the largest component & make subgraph for it
largest_comp <- which.max(twomode_comps$csize)
twomode_subgraph <- twomode_graph %>%
  induced_subgraph(vids = which(twomode_comps$membership == largest_comp))

# ------------------=============================================================------------------ #

# ANALYZE SUB-GRAPH

# Degree Centrality
sort(degree(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]

# Closeness Centrality
sort(closeness(twomode_subgraph, mode = "in"), decreasing = FALSE)[1:20]
sort(closeness(twomode_subgraph, mode = "out"), decreasing = FALSE)[1:20]
sort(closeness(twomode_subgraph, mode = "total"), decreasing = FALSE)[1:20]

# Betweenness Centrality
sort(betweenness(twomode_subgraph, directed = FALSE), decreasing = TRUE)[1:20]

# Data Frame
a <- sort(degree(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
data.frame(Vertex = names(a), In_Degree = a, row.names = NULL)
b <- sort(degree(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
data.frame(Vertex = names(b), Out_Degree = b, row.names = NULL)
c <- sort(degree(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]
data.frame(Vertex = names(c), Total_Degree = c, row.names = NULL)

d <- sort(closeness(twomode_subgraph, mode = "in"), decreasing = FALSE)[1:20]
data.frame(Vertex = names(d), In_Degree = d, row.names = NULL)
e <- sort(closeness(twomode_subgraph, mode = "out"), decreasing = FALSE)[1:20]
data.frame(Vertex = names(e), Out_Degree = e, row.names = NULL)

f <- sort(betweenness(twomode_subgraph, directed = FALSE), decreasing = TRUE)[1:20]
data.frame(Vertex = names(f), Betweenness_Degree = f, row.names = NULL)

# ------------------=============================================================------------------ #


# 2.4.2.  Centrality Analysis Comparision (Justin Bieber vs. DJ Khaled) ----

# Create twomode network for related artist

# Load related artist's data from external file
View(DJ_Khaled_twitter$tweets)

twomode_network_2 <- DJ_Khaled_twitter %>% Create("twomode", removeTermsOrHashtags = c("dj khaled"))
twomode_graph_2 <-twomode_network_2 %>% Graph()

write.graph(twomode_graph_2, file = "DJKhaled_TwoModeNetwork.graphml", format = "graphml")

length(V(twomode_graph_2))
V(twomode_graph_2)$name

twomode_comps_2 <- components(twomode_graph_2, mode = c("weak"))

twomode_comps_2$no # number of connect components
twomode_comps_2$csize # number of nodes in each component
head(twomode_comps_2$membership, n=10)

largest_comp_2 <- which.max(twomode_comps_2$csize)
twomode_subgraph_2 <- twomode_graph_2 %>%
  induced_subgraph(vids = which(twomode_comps_2$membership == largest_comp_2))

# ----------===============================================================================================---------- #

# Overall centrality analysis for related artist network

# Degree Centrality
sort(degree(twomode_subgraph_2, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_2, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_2, mode = "total"), decreasing = TRUE)[1:20]

# Closeness Centrality
sort(closeness(twomode_subgraph_2, mode = "in"), decreasing = FALSE)[1:20]
sort(closeness(twomode_subgraph_2, mode = "out"), decreasing = FALSE)[1:20]
sort(closeness(twomode_subgraph_2, mode = "total"), decreasing = FALSE)[1:20]

# Betweenness Centrality
sort(betweenness(twomode_subgraph_2, directed = FALSE), decreasing = TRUE)[1:20]

# ----------===============================================================================================---------- #

# Degree Centrality Comparision

# Justin Bieber

#sort(degree(twomode_graph, mode = "total"), decreasing = TRUE)[1:30]

justinB_degreeCentrality <- sort(degree(twomode_subgraph, mode = "total"), decreasing = TRUE)[10:20]

top_vertices_1 <- names(justinB_degreeCentrality)

plot(justinB_degreeCentrality, 1:length(top_vertices_1), pch = 16, cex = justinB_degreeCentrality*0.26,
     col = "dodgerblue3",
     xlab = "Degree Centrality", ylab = "Vertices",
     main = "Degree Centrality - Justin Bieber")

par(mar = c(5, 5, 4, 5)) # Customize the margin of the graph

text(justinB_degreeCentrality, 1:length(top_vertices_1), labels = top_vertices_1,
     pos = 4, offset = 0.4) # Add text labels next to each data point

# DJ Khaled

#sort(degree(twomode_graph_2, mode = "total"), decreasing = TRUE)[1:20]

djKhaled_degreeCentrality <- sort(degree(twomode_subgraph_2, mode = "total"), decreasing = TRUE)[1:10]

top_vertices_2 <- names(djKhaled_degreeCentrality)

plot(djKhaled_degreeCentrality, 1:length(top_vertices_2), pch = 16, cex = djKhaled_degreeCentrality*0.5,
     col = "dodgerblue3",
     xlab = "Degree Centrality", ylab = "Vertices",
     main = "Degree Centrality - DJ Khaled")

par(mar = c(5, 5, 4, 4)) # Customize the margin of the graph

text(djKhaled_degreeCentrality, 1:length(top_vertices_2), labels = top_vertices_2,
     pos = 4, offset = 0.4) # Add text labels next to each data point

# ----------===============================================================================================---------- #

# Closeness Centrality Comparision

# Justin Bieber
justinB_closenessCentrality <- sort(closeness(twomode_subgraph, mode = "total"), decreasing = TRUE)[37:50]
top_vertices_3 <- names(justinB_closenessCentrality)

plot(justinB_closenessCentrality, 1:length(top_vertices_3), pch = 16, cex = justinB_closenessCentrality*4000,
     col = "deeppink3",
     xlab = "Closeness Centrality", ylab = "Vertices",
     main = "Closeness Centrality - Justin Bieber")

par(mar = c(5, 5, 4, 7)) # Customize the margin of the graph

text(justinB_closenessCentrality, 1:length(top_vertices_3), labels = top_vertices_3,
     pos = 4, offset = 0.4) # Add text labels next to each data point

# DJ Khaled
djKhaled_closenessCentrality <- sort(closeness(twomode_subgraph_2, mode = "total"), decreasing = TRUE)[1:20]
top_vertices_4 <- names(djKhaled_closenessCentrality)
top_data <- data.frame(Vertices = top_vertices_4, Closeness = djKhaled_closenessCentrality)
write.csv(top_data, "top_closeness.csv", row.names = FALSE)

plot(djKhaled_closenessCentrality, 1:length(top_vertices_4), pch = 16, cex = djKhaled_closenessCentrality*500,
     col = "deeppink3",
     xlab = "Closeness Centrality", ylab = "Vertices",
     main = "Closeness Centrality - DJ Khaled")

par(mar = c(5, 5, 4, 2)) # Customize the margin of the graph

text(djKhaled_closenessCentrality, 1:length(top_vertices_4), labels = top_vertices_4,
     pos = 4, offset = 0.4) # Add text labels next to each data point

# ----------===============================================================================================---------- #

# Betweenness Centrality Comparision

# Justin Bieber
justinB_betweennessCentrality <- sort(betweenness(twomode_subgraph, directed = FALSE), decreasing = TRUE)[10:20]
top_vertices_5 <- names(justinB_betweennessCentrality)

plot(justinB_betweennessCentrality, 1:length(top_vertices_5), pch = 16, cex = justinB_betweennessCentrality*0.0007,
     col = "forestgreen",
     xlab = "Betweenness Centrality", ylab = "Vertices",
     main = "Betweenness Centrality - Justin Bieber")

par(mar = c(5, 5, 4, 4)) # Customize the margin of the graph

text(justinB_betweennessCentrality, 1:length(top_vertices_5), labels = top_vertices_5,
     pos = 4, offset = 0.4) # Add text labels next to each data point

# DJ Khaled
djKhaled_betweennessCentrality <- sort(betweenness(twomode_subgraph_2, directed = FALSE), decreasing = TRUE)[1:10]
top_vertices_6 <- names(djKhaled_betweennessCentrality)

plot(djKhaled_betweennessCentrality, 1:length(top_vertices_6), pch = 16, cex = djKhaled_betweennessCentrality*0.005,
     col = "forestgreen",
     xlab = "Betweenness Centrality", ylab = "Vertices",
     main = "Betweenness Centrality - DJ Khaled")

par(mar = c(5, 5, 4, 7)) # Customize the margin of the graph

text(djKhaled_betweennessCentrality, 1:length(top_vertices_6), labels = top_vertices_6,
     pos = 4, offset = 0.4) # Add text labels next to each data point


# ----------===============================================================================================---------- #




# Question 2.5: Community Analysis ----

# YouTube Authorization
yt_oauth (
  app_id = client_id, 
  app_secret = client_secret
)

# SEARCH FOR VIDEOS AND GET STATISTICS on Justin Bieber

# Load additional packages
library(scales)

# Get information about the artist channel
get_channel_stats(channel_id = "UCIwFjwMjI0y7PDBVEO9-bkQ")
artist_videos_stats <- get_all_channel_video_stats (channel_id = "UCIwFjwMjI0y7PDBVEO9-bkQ")

#typeof(artist_videos_stats$viewCount) # -> 'character' type
#typeof(artist_videos_stats$likeCount) # -> 'character' type

# TOP 10 VIDEOS WITH MOST VIEWS

top_view <- sqldf("SELECT id,title,viewCount,likeCount,url FROM artist_videos_stats 
                  ORDER BY CAST(viewCount AS numeric) DESC
                  LIMIT 10", row.names = TRUE)

# ----------===============================================================================================---------- #

# SEARCH FOR VIDEOS AND GET STATISTICS about DJ Khaled

# Get information about the artist channel
get_channel_stats(channel_id = "UCrFB54bqp8sda4udJyNswlA")
djKhaled_videos_stats <- get_all_channel_video_stats (channel_id = "UCrFB54bqp8sda4udJyNswlA")

#typeof(djKhaled_videos_stats$viewCount) # -> 'character' type
#typeof(djKhaled_videos_stats$likeCount) # -> 'character' type

# TOP 10 VIDEOS WITH MOST VIEWS

top_view_djKhaled <- sqldf("SELECT id,title,viewCount,likeCount,url FROM djKhaled_videos_stats 
                  ORDER BY CAST(viewCount AS numeric) DESC
                  LIMIT 10", row.names = TRUE)

# ----------===============================================================================================---------- #

# COLLECT COMMENTS AND CREATE ACTOR NETWORK for Justin Bieber


# STEP 1: Store selected video IDs
video_ids <- as.vector(top_view$id)

# STEP 2: Authenticate to collect data
videos_data_1 <- Authenticate("youtube", apiKey = api_key) %>%
  Collect (videoIDs = video_ids,
           writeToFile = TRUE,
           maxComments = 500,
           verbose = TRUE)
View(videos_data_1)

# STEP 3: Create actor network and graph
justinB_actor_network <- videos_data_1 %>% Create("actor")
justinB_actor_graph <- Graph(justinB_actor_network)

# Make the graph undirected
undirected_justinB_actor_graph <- as.undirected(justinB_actor_graph, mode = "collapse")

# ----------===============================================================================================---------- #

# COLLECT COMMENTS AND CREATE ACTOR NETWORK for DJ Khaled


# STEP 1: Store selected video IDs
video_ids_djKhaled <- as.vector(top_view_djKhaled$id)

# STEP 2: Authenticate to collect data
videos_data_2 <- Authenticate("youtube", apiKey = api_key) %>%
  Collect (videoIDs = video_ids_djKhaled ,
           writeToFile = TRUE,
           maxComments = 500,
           verbose = TRUE)
View(videos_data_2)

# STEP 3: Create actor network and graph
djKhaled_actor_network <- videos_data_2 %>% Create("actor")
djKhaled_actor_graph <- Graph(djKhaled_actor_network)

# Make the graph undirected
undirected_djKhaled_actor_graph <- as.undirected(djKhaled_actor_graph, mode = "collapse")

# ----------===============================================================================================---------- #

# LOUVAIN ALGORITHMS (Justin Bieber)

lovain_justinB_actor <- cluster_louvain(undirected_justinB_actor_graph)
sizes(lovain_justinB_actor)

# calculate the degree centrality of each node (for node size)
degree_centrality_1 <- degree(undirected_justinB_actor_graph, mode = "all")
node_size_1 <- rescale(degree_centrality_1, to = c(2,15))

# hide screen_name if the node size is small
vertex_labels <- ifelse(node_size_1 > 2.2, V(undirected_justinB_actor_graph)$screen_name, NA)

# Visualise the communities created using Kamada-Kawai algorithm
plot(lovain_justinB_actor,
     undirected_justinB_actor_graph,
     layout = layout_with_kk,
     vertex.label = vertex_labels,
     vertex.label.dist = 1,
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.size = node_size_1,
     vertex.label.cex = node_size_1,
     edge.arrow.size = 0.5)

# Visualise the communities created using Fruchterman-Reingold algorithm

plot(lovain_justinB_actor,
     undirected_justinB_actor_graph,
     layout = layout_with_fr,
     vertex.label = vertex_labels,
     vertex.label.dist = 1,
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.size = node_size_1,
     vertex.label.cex = node_size_1,
     edge.arrow.size = 0.5)

# ----------===============================================================================================---------- #

# LOUVAIN ALGORITHMS (DJ Khaled)

lovain_djKhaled_actor <- cluster_louvain(undirected_djKhaled_actor_graph)
sizes(lovain_djKhaled_actor)

# calculate the degree centrality of each node (for node size)
degree_centrality_2 <- degree(undirected_djKhaled_actor_graph, mode = "all")
node_size_2 <- rescale(degree_centrality_2, to = c(2,15))

# hide screen_name if the node size is small
vertex_labels_2 <- ifelse(node_size_2 > 2.2, V(undirected_djKhaled_actor_graph)$screen_name, NA)

# Visualise the communities created using Kamada-Kawai algorithm
plot(lovain_djKhaled_actor,
     undirected_djKhaled_actor_graph,
     layout = layout_with_kk,
     vertex.label = vertex_labels_2,
     vertex.label.dist = 2,
     vertex.label.color = "white",
     vertex.label.font = 0.5,
     vertex.size = node_size_2,
     vertex.label.cex = node_size_2*0.5,
     edge.arrow.size = 0.5)

# Visualise the communities created using Fruchterman-Reingold algorithm

plot(lovain_djKhaled_actor,
     undirected_djKhaled_actor_graph,
     layout = layout_with_fr,
     vertex.label = vertex_labels_2,
     vertex.label.dist = 2,
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.size = node_size_2,
     vertex.label.cex = node_size_2*0.6,
     edge.arrow.size = 0.5)



# ----------===============================================================================================---------- #


# GIRVAN-NEWMAN ALGORITHMS (Justin Bieber)

newman_justinB_actor <- cluster_edge_betweenness(undirected_justinB_actor_graph)
sizes(newman_justinB_actor)

# Visualise the communities created using Kamada-Kawai algorithm
plot(newman_justinB_actor,
     undirected_justinB_actor_graph,
     layout = layout_with_kk,
     vertex.label = vertex_labels,
     vertex.label.dist = 1,
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.size = node_size_1,
     vertex.label.cex = node_size_1,
     edge.arrow.size = 0.5)

# Visualise the communities created using Fruchterman-Reingold algorithm

plot(newman_justinB_actor,
     undirected_justinB_actor_graph,
     layout = layout_with_fr,
     vertex.label = vertex_labels,
     vertex.label.dist = 1,
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.size = node_size_1,
     vertex.label.cex = node_size_1,
     edge.arrow.size = 0.5)

# ----------===============================================================================================---------- #

# GIRVAN-NEWMAN ALGORITHMS (DJ Khaled)

newman_djKhaled_actor <- cluster_edge_betweenness(undirected_djKhaled_actor_graph)
sizes(newman_djKhaled_actor)

# Visualise the communities created using Kamada-Kawai algorithm
plot(newman_djKhaled_actor,
     undirected_djKhaled_actor_graph,
     layout = layout_with_kk,
     vertex.label = vertex_labels_2,
     vertex.label.dist = 1,
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.size = node_size_2,
     vertex.label.cex = node_size_2*0.5,
     edge.arrow.size = 0.5)

# Visualise the communities created using Fruchterman-Reingold algorithm

plot(newman_djKhaled_actor,
     undirected_djKhaled_actor_graph,
     layout = layout_with_fr,
     vertex.label = vertex_labels_2,
     vertex.label.dist = 1,
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.size = node_size_2,
     vertex.label.cex = node_size_2,
     edge.arrow.size = 0.5)

# ----------===============================================================================================---------- #







# PART 4: MACHINE LEARNING MODELS ----

# Question 2.6: Sentiment Analysis ----

library(syuzhet)

# Clean the tweet text

clean_text <- twitter_data$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()
#View(clean_text)

# ----------===============================================================================================---------- #

# SENTIMENT CLASSIFICATION

# Assign sentiment scores to tweets

sentiment_scores <- get_sentiment(clean_text, method = "afinn") %>% sign()

sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
#View(sentiment_df)

# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
#View(sentiment_df)

# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets about Justin Bieber")

# ----------===============================================================================================---------- #

# EMOTION SCORES

# Assign emotion scores to tweets

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)


# Calculate proportion of emotions across all tweets

emo_sums <- emo_scores_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)


# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of Tweets about Justin Bieber")


# ----------===============================================================================================---------- #


# Question 2.7: Decision Tree ----

# Load additional packages required for this part
library(C50)
library(caret)
library(e1071)

# Authenticate to Spotify using the spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get songs from Justin Bieber and his audio features

justinB_features <- get_artist_audio_features("Justin Bieber")
#View(justinB_features)

data.frame(colnames(justinB_features))

justinB_features_subset <- justinB_features[ , 9:20]
#View(justinB_features_subset)

# ----------===============================================================================================---------- #

# Get top 100 data

# Get top 100 songs and their audio features

top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
#View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset <- top100_features[ , 6:17]
#View(top100_features_subset)

top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)

# ----------===============================================================================================---------- #

# Prepare data for training

# Add the 'isJustin' column (class variable) to each data frame
# to indicate which songs are by Justin Bieber and which are not

top100_features_subset["isJustin"] <- 0
justinB_features_subset["isJustin"] <- 1


# Remove any songs by Justin Bieber that appear in the top 100
# and combine the two data frames into one dataset

top100_features_nojustin <- anti_join(top100_features_subset,
                                      justinB_features_subset,
                                      by = "track_id")
comb_data <- rbind(top100_features_nojustin, justinB_features_subset)

# Format the dataset so that we can give it as input to a model:
# change the 'isJustin' column into a factor
# and remove the 'track_id' column


comb_data$isJustin <- factor(comb_data$isJustin)
comb_data <- select(comb_data, -track_id)

# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]

# ----------===============================================================================================---------- #

# Making decision tree model

# Train the decision tree model

dt_model <- train(isJustin~ ., data = training_set, method = "C5.0")

prediction_row <- 7

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}

#print (predict(dt_model, testing_set[prediction_row, ]))

# ----------===============================================================================================---------- #

# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isJustin)



# ----------===============================================================================================---------- #



# Question 2.8: LDA Topic Modelling ----

# Load additional packages required for this part
library(tm)
library(topicmodels)
library(slam)
library(Rmpfr)
library(reshape2)

# Convert clean tweet vector into a document corpus (collection of documents)
text_corpus <- VCorpus(VectorSource(clean_text))

# Remove stop words
text_corpus <- text_corpus %>%
  tm_map(removeWords, stopwords(kind = "SMART")) 

# Transform corpus into a Document Term Matrix and remove 0 entries
doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(doc_term_matrix$i)
dtm = doc_term_matrix[non_zero_entries,]


# Create LDA model with k topics
lda_model <- LDA(dtm, k = 6)


# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic

tweet_topics <- tidy(lda_model, matrix = "beta")


# Visualise the top 10 terms per topic
top_terms <- tweet_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# DATA PREPARATION FOR TABLEAU VISUALIZATION ----

# Load additional packages required for this part
library(jsonlite)
library(countrycode)

# Remove special characters from location field
twitter_data$users$location <- gsub("[^[:alnum:] ]", "", twitter_data$users$location)

# Use the countrycode package to guess the country based on the location string for each user
twitter_data$users$country_guess <- countrycode(twitter_data$users$location, "country.name", "country.name")

# Rename all columns in twitter_data$users with "fromuser_" prefix
twitter_data$users <- twitter_data$users %>%
  rename_with(~ paste0("fromuser_", .), everything())

# Merge the two data frames into a new one
merged_data <- bind_cols(twitter_data$tweets, twitter_data$users)
View(merged_data)

#To json, and save
json_data <- toJSON(merged_data)
write(json_data, file = "justinBieber_tweets.json")