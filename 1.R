library(ggplot2)
library(readr)

#This program allows users to import a dataset, visualize, and explore it to identify patterns. It leverages bar plots for data visualization, alongside employing subset, table, and tapply functions. The program features 10 distinct queries and 5 unique plots, all implemented in R.

#I will be using artists and collaborations datasets for this program. The artists dataset contains information about artists, including their name, country, and number of collaboration songs. The collaborations dataset contains information about collaborations between artists, including the names of the collaborating artists.

artists <- read_csv("/Users/yourdirectory/random.csv")
collaborations <- read_csv("/Users/yourdirectory/random.csv")


ggplot(data = artists, aes(x = reorder(artist, -collab_songs), y = collab_songs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Collaboration Songs by Artist",
       x = "Artist",
       y = "Number of Collaboration Songs") +
  theme_minimal()


# 1. tapply: to find the average number of collaboration songs by gender
average_collabs_by_gender <- tapply(artists$collab_songs, artists$type, mean)
print(average_collabs_by_gender)

# 2. table: to count the number of collaborations between artist pairs
collaboration_counts <- table(collaborations$artist1, collaborations$artist2)
print(collaboration_counts)

# 3. subset: filters artists from the United States and analyze their collaboration patterns
us_artists <- subset(artists, country == "United States")

average_collabs_us_artists <- mean(us_artists$collab_songs)
print(average_collabs_us_artists)


#Query 1: Average number of collaboration songs by gender.
average_collabs_by_gender <- tapply(artists$collab_songs, artists$type, mean)

#Query 2: Total number of collaborations by country.
total_collabs_by_country <- aggregate(artists$collab_songs, by=list(artists$country), FUN=sum)

#Query 3: Count of artists by genre.
artists$primary_genre <- sapply(strsplit(as.character(artists$genres), ","), `[`, 1)
genre_counts <- table(artists$primary_genre)

#Query 4: Average number of collaborating individuals by country.
avg_collab_individuals_by_country <- tapply(artists$collab_individuals, artists$country, mean)

#Query 5: Number of unique collaboration pairs.
unique_collab_pairs <- length(unique(paste(collaborations$artist1, collaborations$artist2, sep="-")))

#Query 6: Top 5 artists with the most collaboration songs.
top_artists_by_collabs <- head(artists[order(-artists$collab_songs),], 5)

#Query 7: Frequency of collaboration song counts.
collab_song_frequency <- table(artists$collab_songs)

#Query 8: Subset of artists with more than 20 collaboration songs.
artists_more_than_20_collabs <- subset(artists, collab_songs > 20)

#Query 9: Artists from the United States with at least 10 collaborations.
us_artists_min_10_collabs <- subset(artists, country == "United States" & collab_songs >= 10)

#Query 10: # The number of artists by gender
artists_by_gender <- table(artists$type)
print(artists_by_gender)



#1 Barplot of average collaboration songs by gender.
barplot(average_collabs_by_gender, main="Average Collaboration Songs by Gender", xlab="Gender", ylab="Average Number of Songs")

#2 Histogram of collaboration songs across all artists.
hist(artists$collab_songs, breaks=20, main="Distribution of Collaboration Songs", xlab="Number of Collaboration Songs")

#3 Barplot of total collaborations by country.
barplot(total_collabs_by_country$x, names.arg=total_collabs_by_country$Group.1, las=2, main="Total Collaborations by Country", xlab="Country", ylab="Total Collaborations", cex.names=0.7)

#4 Pie chart of genre distribution among artists.
pie(genre_counts, main="Genre Distribution Among Artists")

#5 Boxplot of collaborating individuals by country.
boxplot(collab_individuals~country, data=artists, main="Collaborating Individuals by Country", xlab="Country", ylab="Number of Collaborating Individuals", las=2, cex.axis=0.7)



# Predictor of a categorical variable 

artists$country_binary <- ifelse(artists$country == "United States", 1, 0)  
model <- glm(country_binary ~ collab_songs + collab_individuals, data=artists, family=binomial)
artists$predicted_country_binary <- ifelse(predict(model, type="response") > 0.5, 1, 0)

actual <- artists$country_binary
predicted <- artists$predicted_country_binary
accuracy <- sum(actual == predicted) / length(actual)
print(paste("Accuracy:", accuracy))






