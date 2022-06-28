library(ScottKnottESD)
library(dplyr)

ORGS = c('Qt','Openstack','Eclipse','Libreoffice','Android')
DATA_PATH = '../results/RQ2_results'
METRICS = c('Test.MAE','Test.MAPE','Test.SAE')
DIMS = c("all","time","collaboration_graph","change","text","files","Owner","history","reviewers")
compare_algorithm <- function(orgs,data,metric,dims,greater_is_better = TRUE){
  results <- data.frame(Organization = character(),model = character(),group = double(),mean = double(),median = double())
  for (org_name in orgs) {
    cat('working on org',org_name,'\n')
    org_data = subset(data,Organization == org_name)
    
    cat('working on metric',metric,'\n')
    
    dims_list = NULL
    for (d in dims) {
      cat('Working on dim',d,'\n')
      print(subset(org_data,dim==d))
      x <- unlist(subset(org_data,org_data$dim==d)[metric],use.names = FALSE)
      if (greater_is_better == FALSE){
        x <- x*-1
      }
      dims_list[[d]] <- x
    }
    metric_df =  as.data.frame(dims_list)
    sk <- sk_esd(metric_df)
    for (d in dims) {
      x <- unlist(subset(org_data,org_data$dim==d)[metric],use.names = FALSE)
      new_entry <- list(Organizatation= org_name,group=sk$groups[[d]],Dimension=d,mean= mean(x),median = median(x))
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
  org_data = read.csv(paste(DATA_PATH,'/',org,'_RQ2.csv',sep=''))
  if (is.null(ALL_ORGS_DATA)){
    ALL_ORGS_DATA <- org_data 
  }
  else {
    ALL_ORGS_DATA<- rbind(ALL_ORGS_DATA,org_data)
  }
}
results_SAE <- compare_algorithm(orgs=ORGS,data=ALL_ORGS_DATA,metric='Test.SAE',dims=DIMS,greater_is_better = TRUE)
results_MAE <- compare_algorithm(orgs=ORGS,data=ALL_ORGS_DATA,metric='Test.MAE',dims=DIMS,greater_is_better = FALSE)
results_MAPE <- compare_algorithm(orgs=ORGS,data=ALL_ORGS_DATA,metric='Test.MAPE',dims=DIMS,greater_is_better = FALSE)
write.csv(results_SAE,paste(DATA_PATH,'/','SAE_RQ2.csv',sep=''), row.names = FALSE)
write.csv(results_MAE,paste(DATA_PATH,'/','MAE_RQ2.csv',sep=''), row.names = FALSE)
write.csv(results_MAPE,paste(DATA_PATH,'/','MAPE_RQ2.csv',sep=''), row.names = FALSE)