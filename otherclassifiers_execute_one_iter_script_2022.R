
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

classifier_rules = inspect(classifier$rules)
CBA_rules_nbr = nrow(classifier_rules)-1 # the last row is reserved to the default rule


cond_variables = colnames(DF)[1:ncol(DF)-1] 
target_name = Data$variables$YDataset
classifier_predictions_train = predict(classifier, Data$XTrain)
classifier_predictions_test = predict(classifier, Data$XTest)


classifier_rules_DF=extract_classifier_rules(classifier_rules = classifier_rules,
                                             rules_nbr = CBA_rules_nbr,
                                             cond_vars = cond_variables,
                                             cond_target =target_name)

classifier_rules_DF$countvar = str_count(classifier_rules_DF$cond, fixed("X[,"))
CBA_var_used_count=classifier_rules_DF$countvar
classifier_rules_cov_train = extract_conditions_coverage(X = Data$XTrain,
                                                         Y = Data$YTrain,
                                                         conditions = classifier_rules_DF$cond,
                                                         predictions = classifier_predictions_train)
classifier_rules_cov_test = extract_conditions_coverage(X = Data$XTest,
                                                        Y = Data$YTest,
                                                        conditions = classifier_rules_DF$cond,
                                                        predictions = classifier_predictions_train)
# instances covered by the classifier rule set

CBA_Xi_supported_train = classifier_rules_cov_train$Xis_covered
CBA_Xi_supported_test = classifier_rules_cov_test$Xis_covered


# Classifier perf. on predicting data
classifier_Pred_Train = Perf_indicators(Y_pred=Data$YTrain,Y_predicted=classifier_predictions_train)
classifier_Pred_Test = Perf_indicators(Y_pred=Data$YTest,Y_predicted=classifier_predictions_test)

classifier_Pred_Train_supported = Perf_indicators(Y_pred=as.character(Data$YTrain[CBA_Xi_supported_train]),
                                                  Y_predicted=as.character(classifier_predictions_train[CBA_Xi_supported_train]))
classifier_Pred_Test_supported = Perf_indicators(Y_pred=as.character(Data$YTest[CBA_Xi_supported_test]),
                                                 Y_predicted=as.character(classifier_predictions_test[CBA_Xi_supported_test]))

classifier_Pred_Train_notsupported = Perf_indicators(Y_pred=as.character(Data$YTrain[-CBA_Xi_supported_train]),
                                                     Y_predicted=as.character(classifier_predictions_train[-CBA_Xi_supported_train]))
classifier_Pred_Test_notsupported = Perf_indicators(Y_pred=as.character(Data$YTest[-CBA_Xi_supported_test]),
                                                    Y_predicted=as.character(classifier_predictions_test[-CBA_Xi_supported_test]))


# classifier fidelity metrics
classifier_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                               Y_surrogate=classifier_predictions_train,
                                                               Y_real=Data$YTrain,
                                                               covered_ids=CBA_Xi_supported_train,
                                                               predict_set="train",
                                                               learner_name="CBA",
                                                               iter=I)


classifier_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                              Y_surrogate=classifier_predictions_test,
                                                              Y_real=Data$YTest,
                                                              covered_ids=CBA_Xi_supported_test,
                                                              predict_set="test",
                                                              learner_name="CBA",
                                                              iter=I)

CBA_Fidelity_scores = rbind(classifier_PredTrain_Fidelity_scores,classifier_PredTest_Fidelity_scores)




prediction_perf_iter_CBA = get_prediction_perf_N3(PredTrain=classifier_Pred_Train, 
                                                  PredTest=classifier_Pred_Test,
                                                  PredTrain_supported=classifier_Pred_Train_supported ,
                                                  PredTest_supported=classifier_Pred_Test_supported, 
                                                  PredTrain_notsupported=classifier_Pred_Train_notsupported, 
                                                  PredTest_notsupported=classifier_Pred_Test_notsupported,
                                                  cov_train=classifier_rules_cov_train$prop_Xis_covered,
                                                  cov_test=classifier_rules_cov_test$prop_Xis_covered,
                                                  nbr_rules=CBA_rules_nbr, iter=I, learner_name="CBA")


prediction_perf_iter_CBA= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_CBA d1 LEFT JOIN CBA_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')

prediction_perf_add_classifiers=rbind(prediction_perf_add_classifiers , prediction_perf_iter_CBA)

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


# classifier_rules = inspect(classifier$rules)
RIPPER_rules_nbr = nrow(classifier_rules)-1 # the last row is reserved to the default rule


cond_variables = colnames(DF)[1:ncol(DF)-1] 
target_name = Data$variables$YDataset
classifier_predictions_train = predict(classifier, Data$XTrain)
classifier_predictions_test = predict(classifier, Data$XTest)

classifier_rules_DF=extract_classifier_rules_weka(classifier_rules = classifier_rules,
                                             rules_nbr = RIPPER_rules_nbr,
                                             cond_vars = cond_variables,
                                             cond_target =target_name)
classifier_rules_DF$countvar = str_count(classifier_rules_DF$cond, fixed("X[,"))
RIPPER_var_used_count=classifier_rules_DF$countvar
classifier_rules_cov_train = extract_conditions_coverage(X = Data$XTrain,
                                                         Y = Data$YTrain,
                                                         conditions = classifier_rules_DF$cond,
                                                         predictions = classifier_predictions_train)
classifier_rules_cov_test = extract_conditions_coverage(X = Data$XTest,
                                                        Y = Data$YTest,
                                                        conditions = classifier_rules_DF$cond,
                                                        predictions = classifier_predictions_train)
# instances covered by the classifier rule set

RIPPER_Xi_supported_train = classifier_rules_cov_train$Xis_covered
RIPPER_Xi_supported_test = classifier_rules_cov_test$Xis_covered


# Classifier perf. on predicting data
classifier_Pred_Train = Perf_indicators(Y_pred=Data$YTrain,Y_predicted=classifier_predictions_train)
classifier_Pred_Test = Perf_indicators(Y_pred=Data$YTest,Y_predicted=classifier_predictions_test)

classifier_Pred_Train_supported = Perf_indicators(Y_pred=as.character(Data$YTrain[RIPPER_Xi_supported_train]),
                                                  Y_predicted=as.character(classifier_predictions_train[RIPPER_Xi_supported_train]))
classifier_Pred_Test_supported = Perf_indicators(Y_pred=as.character(Data$YTest[RIPPER_Xi_supported_test]),
                                                 Y_predicted=as.character(classifier_predictions_test[RIPPER_Xi_supported_test]))

classifier_Pred_Train_notsupported = Perf_indicators(Y_pred=as.character(Data$YTrain[-RIPPER_Xi_supported_train]),
                                                     Y_predicted=as.character(classifier_predictions_train[-RIPPER_Xi_supported_train]))
classifier_Pred_Test_notsupported = Perf_indicators(Y_pred=as.character(Data$YTest[-RIPPER_Xi_supported_test]),
                                                    Y_predicted=as.character(classifier_predictions_test[-RIPPER_Xi_supported_test]))


# classifier fidelity metrics
classifier_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                               Y_surrogate=classifier_predictions_train,
                                                               Y_real=Data$YTrain,
                                                               covered_ids=RIPPER_Xi_supported_train,
                                                               predict_set="train",
                                                               learner_name="RIPPER",
                                                               iter=I)


classifier_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                              Y_surrogate=classifier_predictions_test,
                                                              Y_real=Data$YTest,
                                                              covered_ids=RIPPER_Xi_supported_test,
                                                              predict_set="test",
                                                              learner_name="RIPPER",
                                                              iter=I)

RIPPER_Fidelity_scores = rbind(classifier_PredTrain_Fidelity_scores,classifier_PredTest_Fidelity_scores)




prediction_perf_iter_RIPPER = get_prediction_perf_N3(PredTrain=classifier_Pred_Train, 
                                                     PredTest=classifier_Pred_Test,
                                                     PredTrain_supported=classifier_Pred_Train_supported ,
                                                     PredTest_supported=classifier_Pred_Test_supported, 
                                                     PredTrain_notsupported=classifier_Pred_Train_notsupported, 
                                                     PredTest_notsupported=classifier_Pred_Test_notsupported,
                                                     cov_train=classifier_rules_cov_train$prop_Xis_covered,
                                                     cov_test=classifier_rules_cov_test$prop_Xis_covered,
                                                     nbr_rules=RIPPER_rules_nbr, iter=I, learner_name="RIPPER")


prediction_perf_iter_RIPPER= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_RIPPER d1 LEFT JOIN RIPPER_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')

prediction_perf_otherclassifiers=rbind(prediction_perf_otherclassifiers , prediction_perf_iter_RIPPER)

# -----------------------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# RCAR
# -----------------------------------------------------------------------------------------------------------------
# install.packages("arulesCBA")

print("processing Classifier RCAR in progress")
start_time_RCAR = Sys.time()
classifier <- arulesCBA::RCAR(Y~., DF)
end_time_RCAR = Sys.time()
time_taken_RCAR = difftime(end_time_RCAR, start_time_RCAR,units="secs")

classifier_rules = inspect(classifier$rules)
is.null(dim(classifier_rules))
classifier_predictions_train = predict(classifier, Data$XTrain)
classifier_predictions_test = predict(classifier, Data$XTest)
if (!is.null(dim(classifier_rules))) 
{
  RCAR_rules_nbr = nrow(classifier_rules) # the last row is reserved to the default rule
  
  
  cond_variables = colnames(DF)[1:ncol(DF)-1] 
  target_name = Data$variables$YDataset
  
  
  
  classifier_rules_DF=extract_classifier_rules(classifier_rules = classifier_rules,
                                               rules_nbr = RCAR_rules_nbr,
                                               cond_vars = cond_variables,
                                               cond_target =target_name)
  
  classifier_rules_DF$countvar = str_count(classifier_rules_DF$cond, fixed("X[,"))
  RCAR_var_used_count=classifier_rules_DF$countvar
  classifier_rules_cov_train = extract_conditions_coverage(X = Data$XTrain,
                                                           Y = Data$YTrain,
                                                           conditions = classifier_rules_DF$cond,
                                                           predictions = classifier_predictions_train)
  classifier_rules_cov_test = extract_conditions_coverage(X = Data$XTest,
                                                          Y = Data$YTest,
                                                          conditions = classifier_rules_DF$cond,
                                                          predictions = classifier_predictions_train)
  # instances covered by the classifier rule set
  
  RCAR_Xi_supported_train = classifier_rules_cov_train$Xis_covered
  RCAR_Xi_supported_test = classifier_rules_cov_test$Xis_covered 
  classifier_Pred_Train_notsupported = Perf_indicators(Y_pred=as.character(Data$YTrain[-RCAR_Xi_supported_train]),
                                                       Y_predicted=as.character(classifier_predictions_train[-RCAR_Xi_supported_train]))
  classifier_Pred_Test_notsupported = Perf_indicators(Y_pred=as.character(Data$YTest[-RCAR_Xi_supported_test]),
                                                      Y_predicted=as.character(classifier_predictions_test[-RCAR_Xi_supported_test]))
  
}else
{
  RCAR_rules_nbr = 0
  RCAR_var_used_count = NULL
  RCAR_Xi_supported_train = NULL
  RCAR_Xi_supported_test =  NULL
  classifier_Pred_Train_notsupported = Perf_indicators(Y_pred=as.character(Data$YTrain),
                                                       Y_predicted=as.character(classifier_predictions_train))
  classifier_Pred_Test_notsupported = Perf_indicators(Y_pred=as.character(Data$YTest),
                                                      Y_predicted=as.character(classifier_predictions_test))
  
}



# Classifier perf. on predicting data
classifier_Pred_Train = Perf_indicators(Y_pred=Data$YTrain,Y_predicted=classifier_predictions_train)
classifier_Pred_Test = Perf_indicators(Y_pred=Data$YTest,Y_predicted=classifier_predictions_test)

classifier_Pred_Train_supported = Perf_indicators(Y_pred=as.character(Data$YTrain[RCAR_Xi_supported_train]),
                                                  Y_predicted=as.character(classifier_predictions_train[RCAR_Xi_supported_train]))
classifier_Pred_Test_supported = Perf_indicators(Y_pred=as.character(Data$YTest[RCAR_Xi_supported_test]),
                                                 Y_predicted=as.character(classifier_predictions_test[RCAR_Xi_supported_test]))


# classifier fidelity metrics
classifier_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                               Y_surrogate=classifier_predictions_train,
                                                               Y_real=Data$YTrain,
                                                               covered_ids=RCAR_Xi_supported_train,
                                                               predict_set="train",
                                                               learner_name="RCAR",
                                                               iter=I)


classifier_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                              Y_surrogate=classifier_predictions_test,
                                                              Y_real=Data$YTest,
                                                              covered_ids=RCAR_Xi_supported_test,
                                                              predict_set="test",
                                                              learner_name="RCAR",
                                                              iter=I)

RCAR_Fidelity_scores = rbind(classifier_PredTrain_Fidelity_scores,classifier_PredTest_Fidelity_scores)


if (!is.null(dim(classifier_rules))) 
{
  prediction_perf_iter_RCAR = get_prediction_perf_N3(PredTrain=classifier_Pred_Train, 
                                                     PredTest=classifier_Pred_Test,
                                                     PredTrain_supported=classifier_Pred_Train_supported ,
                                                     PredTest_supported=classifier_Pred_Test_supported, 
                                                     PredTrain_notsupported=classifier_Pred_Train_notsupported, 
                                                     PredTest_notsupported=classifier_Pred_Test_notsupported,
                                                     cov_train=classifier_rules_cov_train$prop_Xis_covered,
                                                     cov_test=classifier_rules_cov_test$prop_Xis_covered,
                                                     nbr_rules=RCAR_rules_nbr, iter=I, learner_name="RCAR") 
}else 
{
  prediction_perf_iter_RCAR = get_prediction_perf_N3(PredTrain=classifier_Pred_Train, 
                                                     PredTest=classifier_Pred_Test,
                                                     PredTrain_supported=classifier_Pred_Train_supported ,
                                                     PredTest_supported=classifier_Pred_Test_supported, 
                                                     PredTrain_notsupported=classifier_Pred_Train_notsupported, 
                                                     PredTest_notsupported=classifier_Pred_Test_notsupported,
                                                     cov_train=0,
                                                     cov_test=0,
                                                     nbr_rules=RCAR_rules_nbr, iter=I, learner_name="RCAR")
}

prediction_perf_iter_RCAR= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_RCAR d1 LEFT JOIN RCAR_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')

prediction_perf_otherclassifiers=rbind(prediction_perf_otherclassifiers , prediction_perf_iter_RCAR)
# -------------------------------------------------------------------------------------------------

##SBRL (binary)-----------------------------------------------------------------------------
if (Testsbrl == TRUE) 
{
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
  
  classifier_rules=classifier_rules[-nrow(classifier_rules),]
  names(classifier_rules)= c("id_rule","proba")
  classifier_rules$cond=sbrl_model$rulenames[classifier_rules$id_rule]
  cond_variables = colnames(DF_sbrl_train)[1:ncol(DF_sbrl_train)-1] 
  
  classifier_rules_DF =  extract_classifier_rules_sbrl(classifier_rules = classifier_rules,
                                                       rules_nbr = SBRL_rules_nbr,
                                                       cond_vars = cond_variables)
  
  
  
  classifier_rules_DF$countvar = str_count(classifier_rules_DF$cond, fixed("X[,"))
  SBRL_var_used_count=classifier_rules_DF$countvar
  
  classifier_predictions_train_prob = predict(sbrl_model, DF_sbrl_train)
  classifier_predictions_train_prob_pos = classifier_predictions_train_prob$V2
  classifier_predictions_test_prob = predict(sbrl_model, DF_sbrl_test)
  classifier_predictions_test_prob_pos = classifier_predictions_test_prob$V2
  
  classifier_predictions_train=rep(positive_class,length(classifier_predictions_train_prob_pos))
  classifier_predictions_train[which(classifier_predictions_train_prob_pos<=0.5)] = negative_class
  classifier_predictions_test=rep(positive_class,length(classifier_predictions_test_prob_pos))
  classifier_predictions_test[which(classifier_predictions_test_prob_pos<=0.5)] = negative_class
  
  classifier_rules_cov_train = extract_conditions_coverage(X = Data$XTrain,
                                                           Y = Data$YTrain,
                                                           conditions = classifier_rules_DF$cond,
                                                           predictions = classifier_predictions_train)
  classifier_rules_cov_test = extract_conditions_coverage(X = Data$XTest,
                                                          Y = Data$YTest,
                                                          conditions = classifier_rules_DF$cond,
                                                          predictions = classifier_predictions_train)
  # instances covered by the classifier rule set
  
  SBRL_Xi_supported_train = classifier_rules_cov_train$Xis_covered
  SBRL_Xi_supported_test = classifier_rules_cov_test$Xis_covered
  
  
  # Classifier perf. on predicting data
  classifier_Pred_Train = Perf_indicators(Y_pred=Data$YTrain,Y_predicted=classifier_predictions_train)
  classifier_Pred_Test = Perf_indicators(Y_pred=Data$YTest,Y_predicted=classifier_predictions_test)
  
  classifier_Pred_Train_supported = Perf_indicators(Y_pred=as.character(Data$YTrain[SBRL_Xi_supported_train]),
                                                    Y_predicted=as.character(classifier_predictions_train[SBRL_Xi_supported_train]))
  classifier_Pred_Test_supported = Perf_indicators(Y_pred=as.character(Data$YTest[SBRL_Xi_supported_test]),
                                                   Y_predicted=as.character(classifier_predictions_test[SBRL_Xi_supported_test]))
  
  classifier_Pred_Train_notsupported = Perf_indicators(Y_pred=as.character(Data$YTrain[-SBRL_Xi_supported_train]),
                                                       Y_predicted=as.character(classifier_predictions_train[-SBRL_Xi_supported_train]))
  classifier_Pred_Test_notsupported = Perf_indicators(Y_pred=as.character(Data$YTest[-SBRL_Xi_supported_test]),
                                                      Y_predicted=as.character(classifier_predictions_test[-SBRL_Xi_supported_test]))
  
  
  # classifier fidelity metrics
  classifier_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                                 Y_surrogate=classifier_predictions_train,
                                                                 Y_real=Data$YTrain,
                                                                 covered_ids=SBRL_Xi_supported_train,
                                                                 predict_set="train",
                                                                 learner_name="SBRL",
                                                                 iter=I)
  
  
  classifier_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                                Y_surrogate=classifier_predictions_test,
                                                                Y_real=Data$YTest,
                                                                covered_ids=SBRL_Xi_supported_test,
                                                                predict_set="test",
                                                                learner_name="SBRL",
                                                                iter=I)
  
  SBRL_Fidelity_scores = rbind(classifier_PredTrain_Fidelity_scores,classifier_PredTest_Fidelity_scores)
  
  
  
  
  prediction_perf_iter_SBRL = get_prediction_perf_N3(PredTrain=classifier_Pred_Train, 
                                                     PredTest=classifier_Pred_Test,
                                                     PredTrain_supported=classifier_Pred_Train_supported ,
                                                     PredTest_supported=classifier_Pred_Test_supported, 
                                                     PredTrain_notsupported=classifier_Pred_Train_notsupported, 
                                                     PredTest_notsupported=classifier_Pred_Test_notsupported,
                                                     cov_train=classifier_rules_cov_train$prop_Xis_covered,
                                                     cov_test=classifier_rules_cov_test$prop_Xis_covered,
                                                     nbr_rules=SBRL_rules_nbr, iter=I, learner_name="SBRL")
  
  
  prediction_perf_iter_SBRL= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_SBRL d1 LEFT JOIN SBRL_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  
  prediction_perf_otherclassifiers=rbind(prediction_perf_otherclassifiers , prediction_perf_iter_SBRL)
}




# --------------------------------------------------------------------------
# update prediction perfs

prediction_perf_add_classifiers = rbind(prediction_perf_add_classifiers , prediction_perf_otherclassifiers)

# update exe_time data frame

exe_time_add_classifiers$CBA[I] = time_taken_CBA
exe_time_add_classifiers$RIPPER[I] = time_taken_RIPPER
# exe_time$C4.5[I] = time_taken_C45
exe_time_add_classifiers$RCAR[I] = time_taken_RCAR

if (Testsbrl == TRUE) 
{
  exe_time_add_classifiers$SBRL[I] = time_taken_SBRL
}


# update trees/rules count data frame

rules_count_add_classifiers$CBA[I] = CBA_rules_nbr
rules_count_add_classifiers$RIPPER[I] = RIPPER_rules_nbr
# rules_count$C4.5[I] = C45_rules_nbr
rules_count_add_classifiers$RCAR[I] = RCAR_rules_nbr
if (Testsbrl == TRUE) 
{
  rules_count_add_classifiers$SBRL[I] = SBRL_rules_nbr
}


# update stat related to the number of variables used in each rule
# CBA
CBA_var_used_stat = stat_nbr_var_used(CBA_var_used_count,iter=I,learner_name= "CBA")
var_used_stat_add_classifiers = rbind(var_used_stat_add_classifiers,CBA_var_used_stat)

RIPPER_var_used_stat = stat_nbr_var_used(RIPPER_var_used_count,iter=I,learner_name= "RIPPER")
var_used_stat_add_classifiers = rbind(var_used_stat_add_classifiers,RIPPER_var_used_stat)

# C45_var_used_stat = stat_nbr_var_used(C45_var_used_count,iter=I,learner_name= "C45")
# var_used_stat = rbind(var_used_stat,C45_var_used_stat)

RCAR_var_used_stat = stat_nbr_var_used(RCAR_var_used_count,iter=I,learner_name= "RCAR")
var_used_stat_add_classifiers = rbind(var_used_stat_add_classifiers,RCAR_var_used_stat)


if (Testsbrl == TRUE) 
{
  SBRL_var_used_stat = stat_nbr_var_used(SBRL_var_used_count,iter=I,learner_name= "SBRL")
  var_used_stat_add_classifiers = rbind(var_used_stat_add_classifiers,SBRL_var_used_stat)
}

