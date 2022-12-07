wd <- getwd()
setwd(wd)

data_name = "XOR"

# import data
data_path = "./data/XOR04.csv"
original_data=read.csv(data_path)
summary(original_data)


# cleaning

# target class
id_target = 3
dim(original_data)



for (i in 1:ncol(original_data)) 
{
    original_data[,i]=as.factor(original_data[,i])
    levels(original_data[,i])=gsub("_","-",levels(original_data[,i]))
}

str(original_data)

# 
original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)
original_data[,id_target]= as.factor(original_data[,id_target])

if (original_data_summary$ncat_att >=1)
{
  for (i in eval(parse(text=paste0("c(",original_data_summary$cat,")"))))
  {
    original_data[,i]=as.factor(original_data[,i])
    levels(original_data[,i])=gsub("_","-",levels(original_data[,i]))
  }
}

if (original_data_summary$ncont_att >=1)
{
  for (i in eval(parse(text=paste0("c(",original_data_summary$cont,")"))))
  {
    original_data[,i]=as.numeric(original_data[,i])
    
  }
}

original_data=na.roughfix(original_data)
if (original_data_summary$ncont_att >=1)
{
  
  discretize_data = ForestDisc(data=original_data, id_target=id_target,max_splits = 4)
  original_data = discretize_data$data_disc
  
}
str(original_data)


vec_seed=c(012,123,234,345,456,567,678,789,890,901)

exe_time_add_classifiers=data.frame(matrix(nrow=10,ncol=5))
colnames(exe_time_add_classifiers)=c("data", "iter","CBA","RIPPER", "SBRL" )

rules_count_add_classifiers=data.frame(matrix(nrow=10,ncol=5))
colnames(rules_count_add_classifiers)=c("data", "iter", "CBA","RIPPER", "SBRL")


prediction_perf_add_classifiers=data.frame()

var_used_stat_add_classifiers=data.frame()

# Set default parameters
Nbr_tree = 100 # number of Random Forest Trees
min_R_class_support=0.05 # min support per target class


Testsbrl = FALSE
if (original_data_summary$nclass == 2) 
{
  Testsbrl = TRUE
  tab_target = table(original_data[, id_target])
  pos=which.min(tab_target)
  positive_class= names(tab_target)[pos]
  negative_class= names(tab_target)[-pos]
  
}


I=1
  iteration=I
  
  print(paste0(Sys.time()," Iter ",I," in progress"))
  set_seed=vec_seed[I]
  # splitting into training and testing set
  Data=split_dataset(XDataset=original_data[,-id_target],
                     YDataset=original_data[,id_target],
                     SplitRatio=0.7,seed = set_seed )
  
  
  DF=Data$training_set
  
  if (Testsbrl == TRUE) 
  {
    DF_sbrl_train=Data$training_set
    DF_sbrl_test=Data$test_set
    names(DF_sbrl_train)[ncol(DF_sbrl_train)]="label"
    names(DF_sbrl_test)[ncol(DF_sbrl_test)]="label"
  }
  
  
  # DF=Data$training_set
  
  # computing Random Forest prediction/results
  min_rule_support = min(table(Data$YTrain)*min_R_class_support/length(Data$YTrain))
  RF_Selected_TREES=RF2Selectedtrees(X=Data$XTrain,Y=Data$YTrain,Ntree=Nbr_tree,
                                     min_RuleSupport=min_rule_support)
  RF_fit = RF_Selected_TREES$rf
  RFpredtrain=predict(RF_fit,Data$XTrain,type='class', predict.all = FALSE)
  RFpredtest=predict(RF_fit,Data$XTest,type='class', predict.all = FALSE)
  RF_PredTrain=Perf_indicators(Y_pred=Data$YTrain,Y_predicted=RFpredtrain)
  RF_PredTest=Perf_indicators(Y_pred=Data$YTest,Y_predicted=RFpredtest)
  RF_error_train=1-RF_PredTrain$Global_perf$accuracy
  
  # -----------------------------------------------------------------------------------------------------------------
  # CBA
  # -----------------------------------------------------------------------------------------------------------------
  print("processing Classifier CBA in progress")
  start_time_CBA = Sys.time()
  classifier <- arulesCBA::CBA(Y ~ ., data = DF)
  end_time_CBA = Sys.time()
  time_taken_CBA = difftime(end_time_CBA, start_time_CBA,units="secs")
  classifier$model
  # classifier_rules = inspect(classifier$rules)
  classifier_rules = DATAFRAME(classifier$rules)
  classifier_rules_CBA=classifier_rules
  classifier_rules_CBA[,c(3,4,5,6)]= round(classifier_rules_CBA[,c(3,4,5,6)],2)
  XOR_CBA_rules_metrics_path= ".\\benchmark_results\\rules_results\\XOR_CBA_rules_metrics.csv"
  write.csv(classifier_rules_CBA,XOR_CBA_rules_metrics_path, row.names = FALSE)

  # -----------------------------------------------------------------------------------------------------------------
  # RIPPER
  # -----------------------------------------------------------------------------------------------------------------
  
  print("processing Classifier RIPPER in progress")
  start_time_RIPPER = Sys.time()
  
  # classifier <- arulesCBA::RIPPER_CBA(Y ~ ., data = DF)
  classifier <- RWeka::JRip(Y ~ ., data = DF)
  end_time_RIPPER = Sys.time()
  time_taken_RIPPER = difftime(end_time_RIPPER, start_time_RIPPER,units="secs")
  
  classifier_output <- capture.output(classifier)
  
  classifier_predictions_train = predict(classifier, Data$XTrain)
  classifier_predictions_test = predict(classifier, Data$XTest)
  
  classifier_rules=data.frame(matrix(nrow=length(classifier_output)-6,ncol=3 ))
  colnames(classifier_rules)= c("rule","cond","pred")
  
  for (row in (1:nrow(classifier_rules))) 
  {
    rule =  classifier_output[row+3]
    classifier_rules$rule[row] = rule
    splitrule= strsplit(rule,split= "=>", fixed=TRUE)
    classifier_rules$cond[row] = splitrule[[1]][1]
    classifier_rules$pred[row] = splitrule[[1]][2]
    
  }
  
  classifier_rules_RIPPER=classifier_rules[,2:3]
  
  XOR_RIPPER_rules_metrics_path= ".\\benchmark_results\\rules_results\\XOR_RIPPER_rules_metrics.csv"
  write.csv(classifier_rules_RIPPER,XOR_RIPPER_rules_metrics_path, row.names = FALSE)
  
  
  ##SBRL (binary)-----------------------------------------------------------------------------
  # if (Testsbrl == TRUE) 
  # {
    print("processing Classifier SBRL in progress")
    
    start_time_SBRL = Sys.time()
    # Run the sbrl algorithm on the training set
    sbrl_model = sbrl(DF_sbrl_train, iters=20000, pos_sign=positive_class,
                      neg_sign=negative_class, rule_minlen=1, rule_maxlen=3, 
                      minsupport_pos=0.10, minsupport_neg=0.10, 
                      lambda=10.0, eta=1.0, nchain=25)
    end_time_SBRL = Sys.time()
    time_taken_SBRL = difftime(end_time_SBRL, start_time_SBRL,units="secs")
    
    
    classifier_rules=sbrl_model$rs
    
    SBRL_rules_nbr = nrow(classifier_rules)-1
    # the last row is reserved to the default rule
    
    classifier_predictions_train_prob = predict(sbrl_model, DF_sbrl_train)
    classifier_predictions_train_prob_pos = classifier_predictions_train_prob$V2
    classifier_predictions_test_prob = predict(sbrl_model, DF_sbrl_test)
    classifier_predictions_test_prob_pos = classifier_predictions_test_prob$V2
    
    classifier_predictions_train=rep(positive_class,length(classifier_predictions_train_prob_pos))
    classifier_predictions_train[which(classifier_predictions_train_prob_pos<=0.5)] = negative_class
    classifier_predictions_test=rep(positive_class,length(classifier_predictions_test_prob_pos))
    classifier_predictions_test[which(classifier_predictions_test_prob_pos<=0.5)] = negative_class
    
    classifier_rules_SBRL= classifier_rules
    names(classifier_rules_SBRL)= c("id_rule","positive proba")
    classifier_rules_SBRL$cond=""
    classifier_rules_SBRL$cond[-nrow(classifier_rules_SBRL)]= sbrl_model$rulenames[classifier_rules_SBRL$id_rule[-nrow(classifier_rules_SBRL)]]
    classifier_rules_SBRL=classifier_rules_SBRL[,c(1,3,2)]
    classifier_rules_SBRL[,3]= round(classifier_rules_SBRL[,3],3)
    XOR_SBRL_rules_metrics_path= ".\\benchmark_results\\rules_results\\XOR_SBRL_rules_metrics.csv"
    write.csv(classifier_rules_SBRL,XOR_SBRL_rules_metrics_path, row.names = FALSE)
    
    
#   