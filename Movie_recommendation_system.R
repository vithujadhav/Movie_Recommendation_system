  # Build a recommendation engine that recommends movies to users.
  # Item Based Collaborative Filter recommendation system
  # Importing Essential libraries
  library(recommenderlab)
  library(ggplot2)                       
  library(data.table)
  library(reshape2)

  # Retrieve and display data
  movie_data <- read.csv("C:\\Users\\hp\\Desktop\\Rproject\\movies.csv",stringsAsFactors=FALSE)
  rating_data <- read.csv("C:\\Users\\hp\\Desktop\\Rproject\\ratings.csv")
  str(movie_data)
  
  # Overview the summary 
  summary(movie_data)
  head(movie_data)
  summary(rating_data)
  head(rating_data)
  
  # Data pre-processing
  # From the above table, we observe that the userId column, as well 
  #as the movieId column,consist of integers. Furthermore, we need to 
  #convert the genres present in the movie_data dataframe into a more 
  #usable format by the users. In order to do so, we will first create a 
  #one-hot encoding to create a matrix that comprises of corresponding 
  #genres for each of the films.
  movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
  library(data.table)
  movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                          type.convert=TRUE), 
                                stringsAsFactors=FALSE) 
  colnames(movie_genre2) <- c(1:10)
  list_genre <- c("Action", "Adventure", "Animation", "Children", 
                  "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                  "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                  "Sci-Fi", "Thriller", "War", "Western")
  genre_mat1 <- matrix(0,10330,18)
  genre_mat1[1,] <- list_genre
  colnames(genre_mat1) <- list_genre
  for (index in 1:nrow(movie_genre2)) {
    for (col in 1:ncol(movie_genre2)) {
      gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) 
      genre_mat1[index+1,gen_col] <- 1
    }
  }
  genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
  for (col in 1:ncol(genre_mat2)) {
    genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
  } 
  str(genre_mat2)
  
  #In the next step of Data Pre-processing of R project,
  #we will create a 'search matrix' that will allow us to perform 
  #an easy search of the films by specifying the genre present in our list.
  
  SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])
  head(SearchMatrix)
  ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
  ratingMatrix <- as.matrix(ratingMatrix[,-1])

  #There are movies that have several genres, for example,Toy Story,
  #which is an animated film also falls under the genres of Comedy,
  #Fantasy, and Children. This applies to the majority of the films.
  #For our movie recommendation system to make sense of our ratings 
  #through recommenderlabs, we have to convert our matrix into a sparse 
  #matrix one. This new matrix is of the class 'realRatingMatrix'. 
  #This is performed as follows:
  ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
  ratingMatrix
 
  #Let us now overview some of the important parameters that provide us
  #various options for building recommendation systems for movies
  recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
  names(recommendation_model)
  lapply(recommendation_model, "[[", "description")
  
  # Implementing a single model in the R project - Item Based Collaborative Filtering
  recommendation_model$IBCF_realRatingMatrix$parameters
#Exploring Similar Data
  # Collaborative Filtering involves suggesting movies to the users 
  #that are based on collecting preferences from many other users.
  # With the help of recommenderlab, we can compute similarities between 
  #users as follows:
  similarity_mat <- similarity(ratingMatrix[1:4, ],
                               method = "cosine",
                               which = "users")
  as.matrix(similarity_mat)
  image(as.matrix(similarity_mat), main = "User's Similarities")
  #In the above matrix, each row and column represents a user.We have 
  #taken four users and each cell in this matrix represents the similarity 
  #that is shared between the two users.
 
  #Now, we delineate the similarity that is shared between the films
  movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                   "cosine", which = "items")
  as.matrix(movie_similarity)
  image(as.matrix(movie_similarity), main = "Movies similarity")
 
   #Here we had extracted most unique ratings
  rating_values <- as.vector(ratingMatrix@data)
  unique(rating_values) # extracting unique ratings
 
  #Now we will create a table of ratings that will display
  #the most unique ratings.
  Table_of_Ratings <- table(rating_values) # creating a count of movie ratings
  Table_of_Ratings
  
  # Most viewed movies visualization
  #In this section of the R project, we will explore
  #the most viewed movies in our dataset. We will first count the 
  #number of views in a film and then organize them in a table that
  #would group them in descending order.
  library(ggplot2)
  movie_views <- colCounts(ratingMatrix) # count views for each movie
  table_views <- data.frame(movie = names(movie_views),
                            views = movie_views) # create dataframe of views
  table_views <- table_views[order(table_views$views,
                                   decreasing = TRUE), ] # sort by number of views
  table_views$title <- NA
  for (index in 1:10325){
    table_views[index,3] <- as.character(subset(movie_data,
                                                movie_data$movieId == table_views[index,1])$title)
  }
  table_views[1:6,]
 
  #Now, we will visualize a bar plot for the total number of views
  #of the top films. We will carry this out using ggplot2.
  ggplot(table_views[1:6, ], aes(x = title, y = views)) +
    geom_bar(stat="identity", fill = 'steelblue') +
    geom_text(aes(label=views), vjust=-0.3, size=3.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Total Views of the Top Films")
 
  # Heatmap of Movie Ratings
  # Now, in this R project of Recommendation system,we
  #will visualize a heatmap of the movie ratings. This heatmap will
  #contain first 25 rows and 25 columns as follows -
  image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")
 
  # Data Preparation
  #We will conduct data preparation in the following three steps -
  #Selecting useful data.
  # Normalizing data.
  # Binarizing the data.
  # For finding useful data in our dataset, we have set the threshold
  #for the minimum number of users who have rated a film as 50. 
  #This is also same for minimum number of views that are per film. 
  #This way, we have filtered a list of watched films from least-watched ones.
  movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                                colCounts(ratingMatrix) > 50]
  movie_ratings

  #From the above output of 'movie_ratings', we observe that there
  #are 420 users and 447 films as opposed to the previous 668 users 
  #and 10325 films. We can now delineate our matrix of relevant users as follows
  minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
  minimum_users <- quantile(colCounts(movie_ratings), 0.98)
  image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                      colCounts(movie_ratings) > minimum_users],
        main = "Heatmap of the top users and movies")
 
  # Now,we will visualize the distribution of the average ratings per user.
  average_ratings <- rowMeans(movie_ratings)
  qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
    ggtitle("Distribution of the average rating per user")
 
  # Data Normalization
  #In the case of some users,there can be high ratings or low 
  #ratings provided to all of the watched films.This will act as a bias 
  #while implementing our model.In order to remove this,we normalize our
  #data.Normalization is a data preparation procedure to standardize the
  #numerical values in a column to a common scale value. This is done in 
  #such a way that there is no distortion in the range of values. 
  #Normalization transforms the average value of our ratings column to 0. 
  #We then plot a heatmap that delineates our normalized ratings.
  normalized_ratings <- normalize(movie_ratings)
  sum(rowMeans(normalized_ratings) > 0.00001)
  image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                           colCounts(normalized_ratings) > minimum_users],
        main = "Normalized Ratings of the Top Users")
  
  # Data Binarization
  #In the final step of our data preparation in this R project,
  #we will binarize our data. Binarizing the data means that we have
  #two discrete values 1 and 0, which will allow our recommendation 
  #systems to work more efficiently. We will define a matrix that will 
  #consist of 1 if the rating is above 3 and otherwise it will be 0.
  binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
  binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
 
  movies_watched <- binarize(movie_ratings, minRating = 1)
  good_rated_films <- binarize(movie_ratings, minRating = 3)
  image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                         colCounts(movie_ratings) > binary_minimum_users],
        main = "Heatmap of the top users and movies")
 
  # Collaborative Filtering System
  # We will build this filtering system by splitting the dataset 
  #into 80% training set and 20% test set.
  sampled_data<- sample(x = c(TRUE, FALSE),
                        size = nrow(movie_ratings),
                        replace = TRUE,
                        prob = c(0.8, 0.2))
  training_data <- movie_ratings[sampled_data, ]
  testing_data <- movie_ratings[!sampled_data, ]

  # Building the Recommendation System
  #We will now explore the various parameters of our Item Based Collaborative 
  #Filter. These parameters are default in nature. In the first step, k denotes
  #the number of items for computing their similarities. Here, k is equal 
  #to 30. Therefore, the algorithm will now identify the k most similar items
  #and store their number. We use the cosine method which is the default one 
  #but you can also use pearson method.
  recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
  recommendation_system$IBCF_realRatingMatrix$parameters
  recommen_model <- Recommender(data = training_data,
                                method = "IBCF",
                                parameter = list(k = 30))
  recommen_model
  class(recommen_model)

  #Let us now explore our data science recommendation system model as follows -
  #Using the getModel() function, we will retrieve the recommen_model.
  #We will then find the class and dimensions of our similarity matrix 
  #that is contained within model_info. Finally, we will generate a 
  #heatmap, that will contain the top 20 items and visualize the 
  #similarity shared between them.
  model_info <- getModel(recommen_model)
  class(model_info$sim)
  dim(model_info$sim)
  top_items <- 20
  image(model_info$sim[1:top_items, 1:top_items],
        main = "Heatmap of the first rows and columns")
 
  #In the next step of R project, we will carry out the
  #sum of rows and columns with the similarity of the objects 
  #above 0. We will visualize the sum of columns through a distribution 
  #as follows
  sum_rows <- rowSums(model_info$sim > 0)
  table(sum_rows)
  sum_cols <- colSums(model_info$sim > 0)
  qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ 
      ggtitle("Distribution of the column count")

  #Recommender system on dataset using R
  #We will create a top_recommendations variable which will be
  #initialized to 10, specifying the number of films to each user.
  #We will then use the predict() function that will identify
  #similar items and will rank them appropriately. Here,
  #each rating is used as a weight. Each weight is multiplied
  #with related similarities. Finally, everything is added in
  #the end.
  top_recommendations <- 10 
  predicted_recommendations <- predict(object = recommen_model,
                                       newdata = testing_data,
                                       n = top_recommendations)
  predicted_recommendations

  # recommendation for the first user
  user1 <- predicted_recommendations@items[[1]] 
  movies_user1 <- predicted_recommendations@itemLabels[user1]
  movies_user2 <- movies_user1
  for (index in 1:10){
    movies_user2[index] <- as.character(subset(movie_data,
                                               movie_data$movieId == movies_user1[index])$title)
  }
  movies_user2
  
  # matrix with the recommendations for each user
  recommendation_matrix <- sapply(predicted_recommendations@items,
                                  function(x){ as.integer(colnames(movie_ratings)[x]) }) 
  
  #dim(recc_matrix)
  recommendation_matrix[,1:4]
 
   # Distribution of the Number of Items for IBCF
  number_of_items <- factor(table(recommendation_matrix))
  chart_title <- "Distribution of the Number of Items for IBCF"
  qplot(number_of_items, fill=I("steelblue"), col=I("red")) + ggtitle(chart_title)
  number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
  number_of_items_top <- head(number_of_items_sorted, n = 4)
  table_top <- data.frame(as.integer(names(number_of_items_top)),
                          number_of_items_top)
  for(i in 1:4) {
    table_top[i,1] <- as.character(subset(movie_data,
                                          movie_data$movieId == table_top[i,1])$title)
  }
  colnames(table_top) <- c("Movie Title", "No. of Items")
  head(table_top)

  