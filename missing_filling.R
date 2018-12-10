ratings <- read.table('house-votes-84.data.txt', sep=',')

# find number of na for each col
numNA <- rep(0,ncol(ratings))
for (i in 1:length(numNA))
  numNA[i] <- sum(ratings[,i] == '?')

# output missing value count for each column"
names(numNA) <- sprintf("col%s",seq(1:length(numNA)))
print(numNA)


# create testing and training data
train_df <- data.frame(matrix(ncol = 4, nrow = 0))
test_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(train_df) <- c("userID", "itemID", "rating", "class")
colnames(test_df) <- c("userID", "itemID", "rating", "class")
for (rowID in 1:nrow(ratings)) {
  row <- ratings[rowID,]
  for (i in 2:17) {
    if (sum(row=="?") == 16)
      train_df <- rbind(train_df, data.frame(userID=rowID, itemID=NA, rating=NA, 
                                             class=as.numeric(row[1])))
    if (row[i] == '?')
      test_df <- rbind(test_df, data.frame(userID=rowID, itemID=i, rating=NA, class=as.numeric(row[1])))
    else
      train_df <- rbind(train_df, data.frame(userID=rowID, itemID=i, rating=as.numeric(row[i]), 
                                             class=as.numeric(row[1])))
  }
}

library('rectools')
#---------------------
# KNN Approach
#---------------------
# create train and test data
# set the seed to make train test split reproducible
set.seed(123)
trainIdx <- sample(seq_len(nrow(train_df)), size=floor(0.80 * nrow(train_df)))
train <- train_df[trainIdx,]
test <- train_df[-trainIdx,]
ud <- formUserData(train[,1:3])

# predict the training data and find the MAPE
Ks <- seq(1, 300, by=3)
predictions <- apply(test, 1, function(row) round(predict(ud, ud[[row[1]]], row[2], Ks)))
preds<- vector()
for (row in predictions) preds<- rbind(preds, row)
MAPEs <- apply(preds, 2, function(pred) mean(abs((pred - test[,3])), na.rm=T))

# output first minimum MAPE and the k for that
paste("Min MAPE:", min(MAPEs))
k_loc <- which(MAPEs==min(MAPEs))
optimal_k <- Ks[k_loc[1]]
print("Best k found:")
print(optimal_k)

# using ggplot2 to graph the MAPE vs k line plot
library('ggplot2')
print("- Creating graphs for MAPE vs K...")
mape_vs_k <- data.frame(k=Ks, mape=MAPEs)
qplot(x=Ks, y=MAPEs, data=mape_vs_k, geom="line")
ggsave('mape_vs_k.png')

#--------------------------------------------------------------------------------------------------
# predict votes for missing data
ud <- formUserData(train_df[,1:3]) # train the whole dataset after evaluation
test_df[,3] <- apply(test_df, 1, function(row) round(predict(ud, ud[[row[1]]], row[2], optimal_k)))

# fill the missing votes
completeRating <- ratings
# for (i in 1:nrow(test_df)) {
#   voterID <- test_df[i,1]
#   billID <- test_df[i,2]
#   vote <- test_df[i,3]
#   completeRating[voterID, billID] <- ifelse(vote==2, 'n', 'y')
# }
apply(test_df, 1, function(row) {
  completeRating[test_df[i,1], test_df[i,2]] <- ifelse(test_df[i,3]==2, 'n', 'y')
})
