library(ScottKnottESD)
library(dplyr)

ORGS = c('Qt','Openstack','Eclipse','Libreoffice','Android','All')
DATA_PATH = '../results/RQ1_results'
ML_ALGO_LIST = c('AdaBoost', 'DecisionTree', 'Mean', 'Median','ExtraTrees', 'RandomForest', 'XGBOOST','RandomGuess')
METRICS = c('Test.MAE','Test.MAPE','Test.SAE')

compare_algorithm <- function(orgs,data,metric,algos,greater_is_better = TRUE){
  results <- data.frame(Organization = character(),model = character(),group = double(),mean = double(),median = double())
  for (org_name in orgs) {
    cat('working on org',org_name,'\n')
    org_data = subset(data,Organization == org_name)
    
    cat('working on metric',metric,'\n')
      
    algos_list = NULL
    for (algo in algos) {
      cat('Working on model',algo,'\n')
      x <- unlist(subset(org_data,model==algo)[metric],use.names = FALSE)
      if (greater_is_better == FALSE){
        x <- x*-1
      }
      algos_list[[algo]] <- x
    }
    metric_df =  as.data.frame(algos_list)
    sk <- sk_esd(metric_df)
    for (algo in algos) {
      x <- unlist(subset(org_data,model==algo)[metric],use.names = FALSE)
      new_entry <- list(Organizatation= org_name,group=sk$groups[[algo]],model=algo,mean= mean(x),median = median(x))
      results <- rbind(results,new_entry)
    }
    print(sk)
    plot(sk)
  }
  return(results)
}
#main 
ALL_ORGS_DATA <- NULL
for (org in ORGS){
  org_data = read.csv(paste(DATA_PATH,'/',org,'_RQ1.csv',sep=''))
  if (is.null(ALL_ORGS_DATA)){
    ALL_ORGS_DATA <- org_data 
  }
  else {
    ALL_ORGS_DATA<- rbind(ALL_ORGS_DATA,org_data)
  }
}
ALL_ORGS_DATA_CAT_ALL <- data.frame(ALL_ORGS_DATA)
ALL_ORGS_DATA_CAT_ALL$Organization <- 'All'

ALL_ORGS_DATA <- rbind(ALL_ORGS_DATA,ALL_ORGS_DATA_CAT_ALL)
results_SAE <- compare_algorithm(orgs=ORGS,data=ALL_ORGS_DATA,metric='Test.SAE',algos=ML_ALGO_LIST,greater_is_better = TRUE)
results_MAE <- compare_algorithm(orgs=ORGS,data=ALL_ORGS_DATA,metric='Test.MAE',algos=ML_ALGO_LIST,greater_is_better = FALSE)
results_MAPE <- compare_algorithm(orgs=ORGS,data=ALL_ORGS_DATA,metric='Test.MAPE',algos=ML_ALGO_LIST,greater_is_better = FALSE)
write.csv(results_SAE,paste(DATA_PATH,'/','SAE_RQ1.csv',sep=''), row.names = FALSE)
write.csv(results_MAE,paste(DATA_PATH,'/','MAE_RQ1.csv',sep=''), row.names = FALSE)
write.csv(results_MAPE,paste(DATA_PATH,'/','MAPE_RQ1.csv',sep=''), row.names = FALSE)