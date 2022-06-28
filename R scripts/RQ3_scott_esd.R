library(ScottKnottESD)
library(ggplot2)
ORG <-'Libreoffice'
DATA_PATH = '../results/RQ3_results'
MODEL <- 'ExtraTrees'
rank_features <- function(data,features_to_rank) {
  features_scores <- NULL 
  for (feature in features_to_rank){
    features_scores[[feature]] <- unlist(subset(data,feature_name==feature)['importance_value'],use.names = FALSE)
  }
  sk <- sk_esd(features_scores)
  return(sk)
  
} 
# main 
data <- read.csv(paste(DATA_PATH,'/',paste(ORG,'_',MODEL,'_features_ranking.csv',sep=''),sep=''))
features <- unique(data$feature_name)
sk_ranks <- rank_features(data,features)
sk_ranks$groups
plot(sk_ranks,las=3,cex.lab=0.75)
data['group'] <- -1 
for (feature in sk_ranks$nms) {
  print(feature)
  group <- sk_ranks$groups[[feature]]
  print(group)
  data$group[data$feature_name==feature] <-group
}
write.csv(data,paste(DATA_PATH,'/',paste(ORG,'_features_ranking_results.csv',sep=''),sep=''))