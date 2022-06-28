library(Hmisc)
library(rms)

ORGS <- c('Android','Qt','Eclipse','Openstack','Libreoffice')
PATH <- '../datasets'

#helpers 
sampleWithoutSurprises <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}
FEATURES = c(
  #date features 
  'created_weekday_utc',
  'created_weekday_owner_tz',
  'created_hours_utc',
  'created_hours_owner_tz',
  
  #collaboration graph features
  'degree_centrality',
  'closeness_centrality',
  'betweenness_centrality',
  'eigenvector_centrality',
  'clustering_coefficient',
  'core_number',
  #change features 
  'insertions',
  'deletions',
  'num_files',
  'num_files_type',
  'num_directory',
  'num_programming_languages',
  'modify_entropy',
  'code_churn',
  'sum_loc',
  'sum_complexity',
  'num_binary_files',
  #text features 
  'subject_length',
  'subject_word_count',
  'msg_length',
  'msg_word_count',
  'is_non_fonctional',
  'is_refactoring',
  'is_corrective',
  'is_preventive',
  'has_feature_addition',
  'is_merge',
  #files metrics
  'avg_num_dev_modified_files',
  'num_files_changes_sum',
  'num_files_changes_avg',
  'files_changes_duration_avg',
  'files_revisions_duration_avg',
  'files_num_recent_branch_changes',
  'num_file_changes_for_revrs_avg_30_days',
  'num_file_changes_for_revrs_max_30_days',
  'file_changes_time_revrs_avg_30_days',
  'file_changes_time_revrs_max_30_days',
  #Owner 
  'num_owner_prior_changes',
  'num_owner_open_changes_30_days',
  'owner_age',
  'prior_owner_rate',
  'owner_changes_messages_sum',
  'owner_changes_messages_avg',
  'owner_time_between_message_avg',
  'owner_merged_ratio',
  'num_prior_owner_project_changes',
  'prior_owner_project_changes_ratio',
  
  
 
  #history dimension
  'num_prior_changes',
  'num_prior_project_changes',
  'num_open_changes_30_days',
  'num_project_open_changes_30_days',
  'subsystem_age',
  #Reviewers
  'revrs_changes_sum_30_days',
  'revrs_changes_avg_30_days',
  'revrs_merged_changes_sum_30_days',
  'revrs_merged_changes_avg_30_days',
  'revrs_abandoned_changes_sum_30_days',
  'revrs_abandoned_changes_avg_30_days',
  'revrs_open_changes_sum_30_days',
  'revrs_open_changes_avg_30_days',
  'revrs_num_review_sum_30_days',
  'revrs_num_review_avg_30_days',
  'revrs_previous_msgs_sum_30_days',
  'revrs_previous_msgs_avg_30_days',
  'owner_revrs_commons_changes_sum_30_days',
  'owner_revrs_commons_msgs_avg_30_days'
)
OUTCOME = 'date_updated_date_created_diff'
clusterings = NULL


remove_filtered_features <- function(data,clustering_threshold = 0.7){
  

  cat('Step1: Clustering variables with varclus and with threshold',clustering_threshold,'...\n')
  clusterings<- varclus(data.matrix(data[FEATURES]),method = 'complete')
  plot(clusterings)
  abline(h=clustering_threshold,col="red")
  cut <- cutree(clusterings$hclust, h = clustering_threshold)
  
  selected_features_after_corrolation <- c()
  for (group in unique(cut)){
    a <- which(cut %in% group)
    sample <-sampleWithoutSurprises(a)
    feature <- names(cut)[sample]
    selected_features_after_corrolation <- append(selected_features_after_corrolation,feature)
  } 
  cat('kept features after step 1\n')
  print(selected_features_after_corrolation)
  cat('Step2: removing redunandant features with redun...\n')
  ready_columns <- selected_features_after_corrolation
  data_for_redun <-data[ready_columns]
  data_for_redun<- data_for_redun[complete.cases(data_for_redun),]
  V <- redun(~., data = data_for_redun, r2 = 0.8,nk=8,tlinear=FALSE)
  to_remove_with_redun <- V$Out
  final_columns_set <- c()
  for (col in selected_features_after_corrolation) {
    if (!(col %in% to_remove_with_redun))
    {
      final_columns_set <- append(final_columns_set,col)
    }
  }
  return(final_columns_set)
}
#main 
for (org in ORGS){
  cat('Processing', org)
  data <- read.csv(paste(PATH,'/',org,'.csv',sep=''))
  final_columns_set<-remove_filtered_features(data = data)
  cat('final features set\n')
  print(final_columns_set)
}
