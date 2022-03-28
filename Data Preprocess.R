rm(list=ls())
graphics.off()

library(tidyverse)

i <- 0
while (i <= 10) {
  tmpque <- read.csv(paste(sprintf("%d",i*1000000),'to',sprintf("%d",i*1000000 + 999999),'_Block_queMinerReward.csv', sep=''))
  tmpbu <- read.csv(paste(sprintf("%d",i*1000000),'to',sprintf("%d",i*1000000 + 999999),'_Block_buMinerReward.csv', sep=''))
  
  # merge que and bu
  merged_df <- tmpbu %>% 
    select(blockNumber, reward) %>% 
    right_join(tmpque, by="blockNumber") %>% 
    mutate(reward=reward) 
  # distinct(blockNumber, .keep_all = T) # delete duplicated 
  
  # recompute avgGasPrice
  merged_df$avgGasPrice <- merged_df$txFee / merged_df$gasUsed
  
  # drop unwanted cols 
  drops <- c("minerAddress", "minerExtra", "minGasPrice", "maxGasPrice")
  merged_df <- merged_df[ ,!(names(merged_df) %in% drops)]
  
  # drop None
  merged_df <- merged_df[!(merged_df$transactionCount == 0),]
  
  # final file
  if(i){
    total <- rbind(total, merged_df, by="blockNumber")
  }
  else {
    total <- merged_df
  }
  
  i = i + 1
}

# remove column X
#total <- total[ ,!(names(total) %in% c("X"))]

# remove NA
# total <- head(total, -1)
total <- total %>% 
  filter(blockNumber != "blockNumber")

total <- transform(total, 
                   blockNumber = as.numeric(blockNumber), 
                   reward = as.numeric(reward), 
                   timestamp = as.numeric(timestamp), 
                   size = as.numeric(size),
                   difficulty = as.numeric(difficulty),
                   transactionCount = as.numeric(transactionCount),
                   gasLimit = as.numeric(gasLimit),
                   gasUsed = as.numeric(gasUsed),
                   avgGasPrice = as.numeric(avgGasPrice),
                   txFee = as.numeric(txFee)
                   )

View(total)
summary(total)
sapply(total, class)
names(total)

# write out file
write.csv(total, file="0to10999999_Block.csv")

