ratings <- read.table('house-votes-84.data.txt', sep=',')

# find number of na for each col
numNA <- rep(0,ncol(ratings))
for (i in 1:length(numNA))
  numNA[i] <- sum(ratings[,i] == '?')

# output missing value count for each column"
names(numNA) <- sprintf("col%s",seq(1:length(numNA)))
print(numNA)

library('rectools')

#---------------------
# KNN Approach
#---------------------
# create testing and training dataframe
train_df <- data.frame(matrix(ncol = 3, nrow = 0))
test_df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(train_df) <- c("userID", "itemID", "rating")
colnames(test_df) <- c("userID", "itemID", "rating")
for (rowID in 1:nrow(ratings)) {
  row <- ratings[rowID,]
  for (i in 2:17) {
    if (sum(row=="?") == 16)
      train_df <- rbind(train_df, data.frame(userID=rowID, itemID=NA, rating=NA))
    if (row[i] == '?')
      test_df <- rbind(test_df, data.frame(userID=rowID, itemID=i, rating=NA))
    else
      train_df <- rbind(train_df, data.frame(userID=rowID, itemID=i, rating=as.numeric(row[i])))
  }
}

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
paste("Min MAPE:", min(MAPEs)) # 0.432699619771863
k_loc <- which(MAPEs==min(MAPEs))
optimal_k <- Ks[k_loc[1]]
print("Best k found:")
print(optimal_k)

# using ggplot2 to graph the MAPE vs k line plot
library('ggplot2')
print("- Creating graphs for MAPE vs K...")
mape_vs_k <- data.frame(k=Ks, mape=MAPEs)
qplot(x=Ks, y=MAPEs, data=mape_vs_k, geom="smooth")
ggsave('mape_vs_k.png')

# predict votes for missing data
ud <- formUserData(train_df[,1:3]) # train the whole dataset after evaluation
test_df[,3] <- apply(test_df, 1, function(row) round(predict(ud, ud[[row[1]]], row[2], optimal_k)))

# fill the missing votes using optimal-knn prediction
knn_filled_data <- ratings
for (i in 1:nrow(test_df)) {
  voterID <- test_df[i,1]
  billID <- test_df[i,2]
  vote <- test_df[i,3]
  knn_filled_data[voterID, billID] <- ifelse(vote==2, 'n', 'y')
}
write.table(knn_filled_data, "knn-filled-data.txt", sep=",")

#-------------------------------
# Matrix Factoriazation Approach
#-------------------------------
# create testing and training dataframe
train_df <- data.frame(matrix(ncol = 3, nrow = 0))
test_df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(train_df) <- c("userID", "itemID", "rating")
colnames(test_df) <- c("userID", "itemID", "rating")
for (rowID in 1:nrow(ratings)) {
  row <- ratings[rowID,]
  for (i in 2:17) {
    if (row[i] == '?')
      test_df <- rbind(test_df, data.frame(userID=rowID, itemID=i, rating=NA))
    else
      train_df <- rbind(train_df, data.frame(userID=rowID, itemID=i, rating=as.numeric(row[i])))
  }
}

# 80/20 train test split 
set.seed(123)
trainIdx <- sample(seq_len(nrow(train_df)), size=floor(0.80 * nrow(train_df)))
train <- train_df[trainIdx,]
train <- train[order(train[,1], train[,2]),]
test <- train_df[-trainIdx,]
test <- test[order(test[,1], test[,2]),]

# train NMF model and predict
Ranks <- seq(1, 100, by=1)
test_mapes <- vector(length=length(Ranks))
for (rank in Ranks) {
  preds_test <- round(predict(trainReco(train, rnk=rank, nmf=T), test[,-3]))
  test_mapes <- c(test_mapes, mean(abs(preds_test - test[,3]), na.rm = T))
}

# output first minimum MAPE and the rank for that
preds.mf <- round(predict(trainReco(train, rnk=27, nmf=T), test[,-3]))
paste("MAPE:", mean(abs(preds.mf - test[,3]), na.rm = T))

# using ggplot2 to graph the MAPE vs k line plot
library('ggplot2')
print("- Creating graphs for MAPE vs Rank...")
mapes <- data.frame(rnk=Ranks, test_mape=test_mapes)
qplot(x=rnk, y=test_mape, data=mapes, geom="smooth")
ggsave('mape_vs_rank.png')

# predict votes for missing data
ud <- formUserData(train_df[,1:3]) # train the whole dataset after evaluation
test_df[,3] <- round(predict(trainReco(train, rnk=27, nmf=T), test_df[,-3]))

# fill the missing votes using optimal-knn prediction
knn_filled_data <- ratings
for (i in 1:nrow(test_df)) {
  voterID <- test_df[i,1]
  billID <- test_df[i,2]
  vote <- test_df[i,3]
  knn_filled_data[voterID, billID] <- ifelse(vote==2, 'n', 'y')
}
write.table(knn_filled_data, "knn-filled-data.txt", sep=",")

#-------------------------------
# Method of Moments Approach
#-------------------------------
# create testing and training dataframe
mmout <- trainMM(train)
preds.mm <- predict(mmout, test[, -3])
paste("MAPE:" ,mean(abs(preds.mm - test[ ,3]), na.rm=T)) # 0.499323611050705