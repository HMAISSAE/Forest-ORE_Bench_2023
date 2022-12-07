
vec_seed=c(012,123,234,345,456,567,678,789,890,901)
# Set default parameters
Nbr_tree = 100 # number of Random Forest Trees
# min_R_class_support=0.01 # min support per target class
min_R_class_support=0.05 # min support per target class
min_R_confidence = 0.51 # min rule confidence
max_R_var_nbr = 6 # min number of descriptive variables used in each rule
R_similarity_treshold=0.95


#  prepare output dataframes
exe_time=data.frame(matrix(nrow=10,ncol=15))
colnames(exe_time)=c("data", "iter","extract_rules","preselect_rules", "prepare_opt_input", 
                     "run_opt","buildandrun_opt","RF","CART","Intrees","CBA","RIPPER", "C4.5", "RCAR", "SBRL" )

rules_count=data.frame(matrix(nrow=10,ncol=15))
colnames(rules_count)=c("data", "iter", "RFTree","RFRules", "PreSelectedRules", "OPTRules","OPTRulesSTEL", 
                        "IntreesRRFRules", "IntreesSTELRules", "CART","CBA","RIPPER", "C4.5", "RCAR", "SBRL")



MIP_summary=data.frame(matrix(nrow=10,ncol=13))
colnames(MIP_summary)=c("data", "iter","error_tolerance", "coverage_tolerance", "coverage_ratio", "init_error", 
                        "covered_error_ratio", "overlapping_ratio", "optimality", "opt_run_time", "run_time",
                        "init_Rules_nbr", "Rules_nbr") 

prediction_perf=data.frame()
fidelity_perf=data.frame()

OptRules_metrics_df= data.frame()

var_used_stat=data.frame()

yelse_df = data.frame(matrix(nrow=10,ncol=6))
colnames(yelse_df) = c("data", "iter","PS_rules","Opt_rules", "RRF_rules", "Intrees")

# Ablation results: initialization of output data frames 

id_abl = 0
exe_time_abl=data.frame(matrix(ncol=8,nrow=100))
colnames(exe_time_abl)=c("data", "iter","extract_rules","preselect_rules", "prepare_opt_input", 
                         "run_opt","buildandrun_opt","abl_model" )

rules_count_abl=data.frame(matrix(ncol=8,nrow=100))
colnames(rules_count_abl)=c("data", "iter", "RFTree","RFRules", "PreSelectedRules", "OPTRules","OPTRulesSTEL", 
                            "abl_model")


MIP_summary_abl=data.frame(matrix(ncol=14,nrow=100))
colnames(MIP_summary_abl)=c("data", "iter","error_tolerance", "coverage_tolerance", "coverage_ratio", "init_error", 
                            "covered_error_ratio", "overlapping_ratio", "optimality", "opt_run_time", "run_time",
                            "init_Rules_nbr", "Rules_nbr", "abl_model") 

prediction_perf_abl=data.frame()

fidelity_perf_abl=data.frame()

OptRules_metrics_df_abl= data.frame()

var_used_stat_abl=data.frame()

yelse_df_abl = data.frame(matrix(ncol=6,nrow=100))
colnames(yelse_df_abl) = c("data", "iter","PS_rules","Opt_rules", "abl_model")

# additional classifiers: initialization of output data frames 

# run Iterations
I=1
# for (I in (1:length(vec_seed)))
# for (I in (1:10))
start_time_10iter = Sys.time()

# for (I in (1:10))
# for (I in (1:10))
for (I in (1:10))
{
  iteration=I
  
  print(paste0(Sys.time()," Iter ",I," in progress"))
  set_seed=vec_seed[I]
  # splitting into training and testing set
  Data=split_dataset(XDataset=original_data[,-id_target],
                     YDataset=original_data[,id_target],
                     SplitRatio=0.7,seed = set_seed )
  
  # computing minimum rule support given minimum rule Class support: 
  # Note that the Minimum size for terminal nodes in the random forest model is set for classification to 1. 
  # Here we set this value to ceiling(min_rule_support*length(Data$YTrain)) which is the smallest integer not less than min_RuleSupport*length(Data$YTrain)
  min_rule_support = min(table(Data$YTrain)*min_R_class_support/length(Data$YTrain))
  
  
  # Extracting trees
  start_time_extract_rules = Sys.time()
  
  RF_Selected_TREES=RF2Selectedtrees(X=Data$XTrain,Y=Data$YTrain,Ntree=Nbr_tree,
                                     min_RuleSupport=min_rule_support)
  
  # Extracting rules
  RF_Selected_Rules=Trees2Rulelist(tree_list=RF_Selected_TREES)
  
  end_time_extract_rules = Sys.time()
  time_taken_extract_rules = difftime(end_time_extract_rules,start_time_extract_rules,units="secs")
  time_taken_RF= RF_Selected_TREES$time_taken_RF
  time_taken_extract_rules= time_taken_extract_rules - time_taken_RF
  
  # coumputing metrics retlated to the rules
  N_RFTree=RF_Selected_TREES$ntree
  N_RFRules = length(RF_Selected_Rules$Conds)
  RF_var_used= RF_Selected_Rules$condsdata
  
  
  # computing Random Forest prediction/results
  RF_fit = RF_Selected_TREES$rf
  RFpredtrain=predict(RF_fit,Data$XTrain,type='class', predict.all = FALSE)
  RFpredtest=predict(RF_fit,Data$XTest,type='class', predict.all = FALSE)
  RF_PredTrain=Perf_indicators(Y_pred=Data$YTrain,Y_predicted=RFpredtrain)
  RF_PredTest=Perf_indicators(Y_pred=Data$YTest,Y_predicted=RFpredtest)
  RF_error_train=1-RF_PredTrain$Global_perf$accuracy
  RF_error_train=round(RF_error_train,4)
  # set min_R_confidence parameter (optionel): the goal is to reduce the number of preselected rules
  min_R_confidence=max(min_R_confidence,(RF_PredTrain$Global_perf$accuracy-0.25))
  
  # Selecting a set of non redundant rules based on thresholds related to class_support,class_confidence, length, and similarity
  start_time_preselect_rules = Sys.time()
  RF_PreSelected_Rules=RulesToPreSelectRules(X=Data$XTrain, Y=Data$YTrain,RF_Selected_Rules=RF_Selected_Rules,
                                             min_rule_class_support=min_R_class_support, 
                                             min_rule_confidence=min_R_confidence, max_rule_var=max_R_var_nbr,
                                             similarity_treshold=R_similarity_treshold)
  end_time_preselect_rules = Sys.time()
  time_taken_preselect_rules = difftime(end_time_preselect_rules, start_time_preselect_rules,units="secs")
  N_RFPreSelectedRules=nrow(RF_PreSelected_Rules$Rules_selection)
  
  
  # Extracting metrics related to preselected rules
  start_time_prepare_opt_input = Sys.time()
  
  Rules_metrics = RF_PreSelected_Rules$perf_Rules
  Rules_DF = RF_PreSelected_Rules$RulesPrediction
  Rules_DF_0 = Rules_DF[,-ncol(Rules_DF)]
  
  # identify the instances not covered by the preselected rules
  initial_not_covered_rows = which(rowSums(is.na(Rules_DF_0)) == ncol(Rules_DF_0))
  
  # Compute Preselected rules performance
  # compute else rule for preselected rules
  Rules_DF_0 = Rules_DF[,-ncol(Rules_DF)]
  if (length(initial_not_covered_rows) > 0)
  {
    SR_Y_elserule = names(which.max(table(Data$YTrain[initial_not_covered_rows])))
  }else 
  {
    SR_Y_elserule = names(which.max(table(Data$YTrain)))   
  }
  
  PSRules_PredTrain= PREDICT_selected_rules(X=Data$XTrain,Y=Data$YTrain,Rules_metrics=Rules_metrics,
                                            Rules_ids=1:nrow(Rules_metrics),Y_elserule=SR_Y_elserule)
  
  
  
  PSRules_PredTest= PREDICT_selected_rules(X=Data$XTest,Y=Data$YTest,Rules_metrics=Rules_metrics,
                                           Rules_ids=1:nrow(Rules_metrics),Y_elserule=SR_Y_elserule)
  
  
  # fidelity metrics on the training and testing sets
  PSR_tr_Xis=PSRules_PredTrain$Xis_covered
  PSR_ts_Xis=PSRules_PredTest$Xis_covered
  PSRules_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                              Y_surrogate=PSRules_PredTrain$predictions$Ypred,
                                                              Y_real=Data$YTrain,
                                                              covered_ids=PSR_tr_Xis,
                                                              predict_set="train",
                                                              learner_name="PreSelectedRules",
                                                              iter=I)
  PSRules_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                             Y_surrogate=PSRules_PredTest$predictions$Ypred,
                                                             Y_real=Data$YTest,
                                                             covered_ids=PSR_ts_Xis,
                                                             predict_set="test",
                                                             learner_name="PreSelectedRules",
                                                             iter=I)
  
  PSRules_Fidelity_scores = rbind(PSRules_PredTrain_Fidelity_scores,PSRules_PredTest_Fidelity_scores)
  
  # remove not covered instances from the data that will be used in the optimization stage
  if (length(initial_not_covered_rows) > 0)
  {
    Rules_DF = Rules_DF[-initial_not_covered_rows, ] 
  }
  
  #  Compute Rules binary prediction. 
  Rules_cover_pred=Rules_BinaryPred(Rules_DF)
  
  # "Rules_pred_perf" :Rules prediction performance
  Rules_pred_perf= Rules_cover_pred$Prediction_Perf
  Rules_pred_error = round(1-Rules_pred_perf$accuracy,4)
  
  # Data frame of correctly covered lines : It is a binary data frame where each cell (i,j) equals 1 if rule j covers instance i 
  # and predicts it correctly, 0 otherwise.
  Rules_cover_ok= Rules_cover_pred$Rules_pred_ok
  
  
  # Data frame of incorrectly covered lines: It is a binary data frame where each cell (i,j) equals 1 if rule j covers instance i 
  # and does not predict it correctly, 0 otherwise.
  Rules_cover_nok = Rules_cover_pred$Rules_pred_nok
  
  
  # write the inputs that will be used in the optimization step into csv files
  Rules_metrics_Path = paste0(".\\Rules_files\\", data_name, "_Rules_metrics.csv")
  Rules_metrics_Path_py = paste0("./Rules_files/",data_name, "_Rules_metrics.csv")
  
  write.csv(Rules_metrics,Rules_metrics_Path, row.names = FALSE)
  
  Rules_cover_ok_Path = paste0(".\\Rules_files\\", data_name, "_Rules_cover_ok.csv")
  Rules_cover_ok_Path_py = paste0("./Rules_files/", data_name, "_Rules_cover_ok.csv")
  write.csv(Rules_cover_ok,Rules_cover_ok_Path, row.names = FALSE)
  
  Rules_cover_nok_Path = paste0(".\\Rules_files\\", data_name,"_Rules_cover_nok.csv")
  Rules_cover_nok_Path_py = paste0("./Rules_files/", data_name, "_Rules_cover_nok.csv")
  write.csv(Rules_cover_nok,Rules_cover_nok_Path, row.names = FALSE)
  
  Rules_pred_perf_Path = paste0(".\\Rules_files\\", data_name,"_Rules_pred_perf.csv")
  write.csv(Rules_pred_perf,Rules_pred_perf_Path, row.names = FALSE)
  
  end_time_prepare_opt_input = Sys.time()
  time_taken_prepare_opt_input = difftime(end_time_prepare_opt_input, start_time_prepare_opt_input,units="secs")
  
  ##Optimization step
  #-----------------------------------------------------------------------
  
  # Find the optimal number of rules needed to interpret Random Forest model while considering diverse 
  # individual and collaborative factors: predictive performance, coverage, complexity (attributes number 
  # and levels numbers), and overlapping. The optimization is formulated and solved as a Mixed Integer Programming pb.
  # w0, w1, w2, w3: Weights used in the objective function. Default parameters used are respectively:  1, 1, 0.1, 0.05
  # MaxCover: The upper bound for the number of rules to which an instance can belong to. 
  # MaxOverlap: The upper bound for the overall overlap ratio(ratio of the instances belonging to 2 rules or more). 
  
  # Alpha: The upper bound for the loss in overall accuracy compared to the accuracy of Random Forest classifier.
  
  # Beta: The upper bound for the loss in overall coverage compared to the coverage of the preselected rules
  # (initial coverage equals 1).
  
  
  source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
  
  # opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
  #                               RulesCoverOk_location = Rules_cover_ok_Path_py,
  #                               RulesCoverNok_location = Rules_cover_nok_Path_py,
  #                               InitError = RF_error_train,
  #                               W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
  #                               MaxOverlap = 0.25, Alpha= 0.01, Beta= 0.01)
  
  # opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
  #                               RulesCoverOk_location = Rules_cover_ok_Path_py,
  #                               RulesCoverNok_location = Rules_cover_nok_Path_py,
  #                               InitError = RF_error_train,
  #                               W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
  #                               MaxOverlap = 0.3, Alpha= 0.01, Beta= 0.01)
  
  opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                RulesCoverOk_location = Rules_cover_ok_Path_py,
                                RulesCoverNok_location = Rules_cover_nok_Path_py,
                                InitError = RF_error_train,
                                W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
                                MaxOverlap = 0.5, Alpha= 0.01, Beta= 0.025)
  
  
  # MaxOverlap = 0.3, Alpha= 0.025, Beta= 0.025
  source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  
  # optimal solution summary, rule list, uncoverd instances list, and incorrectly covered instances list
  OptSol_Summary=py_to_r(opt_solution$summary)
  OptSol_RuleList = py_to_r(opt_solution$Rules)
  OptSol_UncoveredList = py_to_r(opt_solution$not_covered_instances)
  OptSol_ErrorList =py_to_r(opt_solution$incorrectly_covered_instances)
  
  
  # once the optimization pb is solved, add not covered rows after optimization 
  # to initial not covered rules to compute else rule
  
  not_covered_rows= c(initial_not_covered_rows,OptSol_UncoveredList)
  
  if (length(not_covered_rows) > 0)
  {
    Y_elserule = names(which.max(table(Data$YTrain[not_covered_rows])))
  }else 
  {
    Y_elserule = names(which.max(table(Data$YTrain)))   
  }
  
  
  # Opt rules prediction on the traning set
  
  OptRules_PredTrain=PREDICT_selected_rules(X=Data$XTrain,Y=Data$YTrain,Rules_metrics=Rules_metrics,
                                            Rules_ids=OptSol_RuleList,Y_elserule=Y_elserule)
  
  
  # rules prediction on the testing set
  OptRules_PredTest=PREDICT_selected_rules(X=Data$XTest,Y=Data$YTest,Rules_metrics=Rules_metrics,
                                           Rules_ids=OptSol_RuleList,Y_elserule=Y_elserule)
  
  # fidelity metrics on the training and testing sets
  optR_tr_Xis=OptRules_PredTrain$Xis_covered
  optR_ts_Xis=OptRules_PredTest$Xis_covered
  
  OptRules_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                               Y_surrogate=OptRules_PredTrain$predictions$Ypred, 
                                                               Y_real=Data$YTrain, 
                                                               covered_ids=optR_tr_Xis,
                                                               predict_set="train",
                                                               learner_name="OptimalRules",
                                                               iter=I)
  
  
  OptRules_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                              Y_surrogate=OptRules_PredTest$predictions$Ypred,
                                                              Y_real=Data$YTest, 
                                                              covered_ids=optR_ts_Xis,
                                                              predict_set="test",
                                                              learner_name="OptimalRules",
                                                              iter=I)
  OptRules_Fidelity_scores=rbind(OptRules_PredTrain_Fidelity_scores,OptRules_PredTest_Fidelity_scores)
  
  
  # Use STEL approach to build a simplified tree ensemble learner (optionel)
  OptRulesMetric = Rules_metrics[OptSol_RuleList,c("var_length", "support","confidence","cond","Ypred")]
  names(OptRulesMetric) = c("len", "freq", "err", "condition", "pred")
  OptRulesMetric$err= 1- OptRulesMetric$err
  OptRulesMetric$pred= gsub("'","",OptRulesMetric$pred)
  # OptRulesMetric = pruneRule(OptRulesMetric,Data$XTrain,Data$YTrain) 
  OptRulesMetric= as.matrix(OptRulesMetric)
  
  learnerOptRules <- buildLearner(OptRulesMetric,Data$XTrain,Data$YTrain) #build the simplified tree ensemble learner
  
  OPTSTELpredtrain = applyLearner(learnerOptRules,Data$XTrain)
  OPTSTELpredtest = applyLearner(learnerOptRules,Data$XTest)
  
  OPTSTEL_Pred_Train = Perf_indicators(Y_pred=Data$YTrain,Y_predicted=OPTSTELpredtrain)
  OPTSTEL_Pred_Test = Perf_indicators(Y_pred=Data$YTest,Y_predicted=OPTSTELpredtest)
  
  OPTSTEL_else_train=get_elserows(X=Data$XTrain,Y=Data$YTrain,
                                  conditions=learnerOptRules[-nrow(learnerOptRules),"condition"],
                                  predictions =learnerOptRules[-nrow(learnerOptRules),"pred"] )
  OPTSTEL_else_test=get_elserows(X=Data$XTest,Y=Data$YTest,
                                 conditions=learnerOptRules[-nrow(learnerOptRules),"condition"],
                                 predictions =learnerOptRules[-nrow(learnerOptRules),"pred"] )
  
  OPTSTEL_Xi_supported_train=setdiff(1:nrow(Data$XTrain),OPTSTEL_else_train$not_covered_rows)
  OPTSTEL_Xi_supported_test=setdiff(1:nrow(Data$XTest),OPTSTEL_else_test$not_covered_rows)
  
  OPTSTEL_covered_train = length(OPTSTEL_Xi_supported_train)/nrow(Data$XTrain)
  OPTSTEL_covered_test = length(OPTSTEL_Xi_supported_test)/nrow(Data$XTest)
  OPTSTEL_size = nrow(learnerOptRules)-1
  
  OPTSTEL_Pred_Train_supported = Perf_indicators(Y_pred=as.character(Data$YTrain[OPTSTEL_Xi_supported_train]),
                                                 Y_predicted=as.character(OPTSTELpredtrain[OPTSTEL_Xi_supported_train]))
  OPTSTEL_Pred_Test_supported = Perf_indicators(Y_pred=as.character(Data$YTest[OPTSTEL_Xi_supported_test]),
                                                Y_predicted=as.character(OPTSTELpredtest[OPTSTEL_Xi_supported_test]))
  
  OPTSTEL_Pred_Train_supported = Perf_indicators(Y_pred=as.character(Data$YTrain[OPTSTEL_Xi_supported_train]),
                                                 Y_predicted=as.character(OPTSTELpredtrain[OPTSTEL_Xi_supported_train]))
  OPTSTEL_Pred_Test_supported = Perf_indicators(Y_pred=as.character(Data$YTest[OPTSTEL_Xi_supported_test]),
                                                Y_predicted=as.character(OPTSTELpredtest[OPTSTEL_Xi_supported_test]))
  OPTSTEL_Pred_Train_notsupported = Perf_indicators(Y_pred=as.character(Data$YTrain[-OPTSTEL_Xi_supported_train]),
                                                    Y_predicted=as.character(OPTSTELpredtrain[-OPTSTEL_Xi_supported_train]))
  
  OPTSTEL_Pred_Test_notsupported = Perf_indicators(Y_pred=as.character(Data$YTest[-OPTSTEL_Xi_supported_test]),
                                                   Y_predicted=as.character(OPTSTELpredtest[-OPTSTEL_Xi_supported_test]))
  
  # fidelity metrics on the training and testing sets
  OPTSTEL_tr_Xis=OPTSTEL_Xi_supported_train
  OPTSTEL_ts_Xis=OPTSTEL_Xi_supported_test
  
  OPTSTEL_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                              Y_surrogate=OPTSTELpredtrain,
                                                              Y_real=Data$YTrain, 
                                                              covered_ids=OPTSTEL_tr_Xis,
                                                              predict_set="train",
                                                              learner_name="OPTSTEL",
                                                              iter=I)
  
  
  OPTSTEL_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                             Y_surrogate=OPTSTELpredtest,
                                                             Y_real=Data$YTest,
                                                             covered_ids=OPTSTEL_ts_Xis,
                                                             predict_set="test",
                                                             learner_name="OPTSTEL",
                                                             iter=I)
  
  OPTSTEL_Fidelity_scores=rbind(OPTSTEL_PredTrain_Fidelity_scores,OPTSTEL_PredTest_Fidelity_scores)
  
  # CART
  start_time_CART = Sys.time()
  CART_fit=rpart(Y ~ ., data = Data$training_set,method="class")
  end_time_CART = Sys.time()
  time_taken_CART = difftime(end_time_CART, start_time_CART,units="secs")
  CARTpredtrain=predict(object = CART_fit, newdata = Data$training_set, type = "class")
  CARTpredtest=predict(object = CART_fit, newdata = Data$test_set, type = "class")
  CART_PredTrain=Perf_indicators(Y_pred=Data$YTrain,Y_predicted=CARTpredtrain)
  CART_PredTest=Perf_indicators(Y_pred=Data$YTest,Y_predicted=CARTpredtest)
  
  # extract CART rules
  CART_rules = rpart.plot::rpart.rules(CART_fit)
  names(CART_rules)
  col_rules_begining=which(CART_rules[1,]=="when")+1
  CART_rules_nbr = nrow(CART_rules)
  # select columns ids containing variables and compute the number of variable used in each cart rule
  multiplecolvar =(c(col_rules_begining, col_rules_begining:ncol(CART_rules) %% 4)==0)
  colvar= (1:ncol(CART_rules))[multiplecolvar]
  CART_var_used = as.data.frame(CART_rules[,colvar])
  
  
  # Intrees
  start_time_Intrees = Sys.time()
  X=Data$XTrain
  target=Data$YTrain
  ruleExec0 <- extractRules(RF2List(RF_Selected_TREES$rf),X) 
  ruleExec <- unique(ruleExec0)
  ruleMetric <- getRuleMetric(ruleExec,X,target)
  ruleMetric <- pruneRule(ruleMetric,X,target) # prune each rule
  ruleMetric <- selectRuleRRF(ruleMetric,X,target) # rule selection
  learner <- buildLearner(ruleMetric,X,target) #build the simplified tree ensemble learner
  end_time_Intrees = Sys.time()
  
  time_taken_Intrees = difftime(end_time_Intrees, start_time_Intrees,units="secs")
  Intrees_RRF_rules_size = nrow(ruleMetric)
  row_else=which(learner[,"condition"]==c("X[,1]==X[,1]"))
  
  intreesCond=learner[-row_else,4]
  Intreespred=learner[-row_else,5]
  intreeselse=learner[row_else,]
  intreessize=length(learner[,1])-1
  
  #  prediction of the rules selected by RRF method (intrees)
  RRFRules <- ruleMetric
  RRFRules_else = get_elserows(X=Data$XTrain,Y=Data$YTrain,
                               conditions=RRFRules[,"condition"],predictions =RRFRules[,"pred"] )
  
  RRF_Rules_PredTrain=PREDICT_intrees(X=Data$XTrain,Y=Data$YTrain,conditions=RRFRules[,"condition"],
                                      predictions=RRFRules[,"pred"],
                                      ids_Rules=1:nrow(RRFRules),
                                      elserule=RRFRules_else$Y_elserule)
  
  RRF_Rules_PredTest=PREDICT_intrees(X=Data$XTest,Y=Data$YTest,conditions=RRFRules[,"condition"],
                                     predictions=RRFRules[,"pred"],
                                     ids_Rules=1:nrow(RRFRules),
                                     elserule=RRFRules_else$Y_elserule)
  RRF_Rules_size = nrow(ruleMetric)
  
  # fidelity metrics on the training and testing sets
  RRFR_tr_Xis=RRF_Rules_PredTrain$Xis_covered
  RRFR_ts_Xis=RRF_Rules_PredTest$Xis_covered
  
  RRFR_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                           Y_surrogate=RRF_Rules_PredTrain$predictions$Ypred,
                                                           Y_real=Data$YTrain,
                                                           covered_ids=RRFR_tr_Xis,
                                                           predict_set="train",
                                                           learner_name="IntreesRRFVote",
                                                           iter=I)
  
  
  RRFR_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                          Y_surrogate=RRF_Rules_PredTest$predictions$Ypred, 
                                                          Y_real=Data$YTest,
                                                          covered_ids=RRFR_ts_Xis,
                                                          predict_set="test",
                                                          learner_name="IntreesRRFVote",
                                                          iter=I)
  RRFR_Fidelity_scores = rbind(RRFR_PredTrain_Fidelity_scores,RRFR_PredTest_Fidelity_scores)
  
  #  prediction based on intrees rules vote
  Intrees_vote_PredTrain=PREDICT_intrees(X=Data$XTrain,Y=Data$YTrain,conditions=intreesCond,
                                         predictions=Intreespred,
                                         ids_Rules=1:intreessize,
                                         elserule=intreeselse["pred"])
  
  Intrees_vote_PredTest=PREDICT_intrees(X=Data$XTest,Y=Data$YTest,conditions=intreesCond,
                                        predictions=Intreespred,
                                        ids_Rules=1:intreessize,
                                        elserule=intreeselse["pred"])
  
  
  Intrees_Xi_supported_train= which(Intrees_vote_PredTrain$predictions$X_Supported==1)
  Intrees_Xi_supported_test= which(Intrees_vote_PredTest$predictions$X_Supported==1)
  
  # fidelity metrics on the training and testing sets
  Intrees_vote_tr_Xis=Intrees_Xi_supported_train
  Intrees_vote_ts_Xis=Intrees_Xi_supported_test
  
  Intrees_vote_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                                   Y_surrogate=Intrees_vote_PredTrain$predictions$Ypred, 
                                                                   Y_real=Data$YTrain, 
                                                                   covered_ids=Intrees_vote_tr_Xis,
                                                                   predict_set="train",
                                                                   learner_name="IntreesVote",
                                                                   iter=I)
  
  
  Intrees_vote_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                                  Y_surrogate=Intrees_vote_PredTest$predictions$Ypred, 
                                                                  Y_real=Data$YTest, 
                                                                  covered_ids=Intrees_vote_ts_Xis,
                                                                  predict_set="test",
                                                                  learner_name="IntreesVote",
                                                                  iter=I)
  
  Intrees_vote_Fidelity_scores = rbind(Intrees_vote_PredTrain_Fidelity_scores,Intrees_vote_PredTest_Fidelity_scores)
  
  #  prediction based on intrees STEL rules (simplified tree ensemble learner)
  STELpredtrain = applyLearner(learner,Data$XTrain)
  STELpredtest = applyLearner(learner,Data$XTest)
  
  STEL_Pred_Train = Perf_indicators(Y_pred=Data$YTrain,Y_predicted=STELpredtrain)
  STEL_Pred_Test = Perf_indicators(Y_pred=Data$YTest,Y_predicted=STELpredtest)
  
  STEL_Pred_Train_supported = Perf_indicators(Y_pred=as.character(Data$YTrain[Intrees_Xi_supported_train]),
                                              Y_predicted=as.character(STELpredtrain[Intrees_Xi_supported_train]))
  STEL_Pred_Test_supported = Perf_indicators(Y_pred=as.character(Data$YTest[Intrees_Xi_supported_test]),
                                             Y_predicted=as.character(STELpredtest[Intrees_Xi_supported_test]))
  
  STEL_Pred_Train_notsupported = Perf_indicators(Y_pred=as.character(Data$YTrain[-Intrees_Xi_supported_train]),
                                                 Y_predicted=as.character(STELpredtrain[-Intrees_Xi_supported_train]))
  STEL_Pred_Test_notsupported = Perf_indicators(Y_pred=as.character(Data$YTest[-Intrees_Xi_supported_test]),
                                                Y_predicted=as.character(STELpredtest[-Intrees_Xi_supported_test]))
  
  # fidelity metrics on the training and testing sets
  STEL_tr_Xis=Intrees_Xi_supported_train
  STEL_vote_ts_Xis=Intrees_Xi_supported_test
  
  STEL_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                           Y_surrogate=STELpredtrain,
                                                           Y_real=Data$YTrain,
                                                           covered_ids=STEL_tr_Xis,
                                                           predict_set="train",
                                                           learner_name="IntreesSTEL",
                                                           iter=I)
  
  
  STEL_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                          Y_surrogate=STELpredtest,
                                                          Y_real=Data$YTest,
                                                          covered_ids=STEL_vote_ts_Xis,
                                                          predict_set="test",
                                                          learner_name="IntreesSTEL",
                                                          iter=I)
  
  STEL_Fidelity_scores = rbind(STEL_PredTrain_Fidelity_scores,STEL_PredTest_Fidelity_scores)
  # update exe_time data frame
  exe_time$iter[I] = I
  exe_time$extract_rules[I] = round(time_taken_extract_rules,4)
  exe_time$preselect_rules[I] = round(time_taken_preselect_rules,4)
  exe_time$prepare_opt_input[I] = round(time_taken_prepare_opt_input,4) 
  #prepare_opt_input is the required time for preparing the optimization pb input data.
  exe_time$buildandrun_opt[I] = round(OptSol_Summary$run_time,4) 
  # buildandrun_opt is the time used by Gurobi to build, solve, and generate the outputs of the optimization problem. 
  # model-building time includes adding the variables, constraints, etc.).
  exe_time$run_opt[I] = round(OptSol_Summary$opt_run_time,4) # 
  # run_opt is the required time for solving the optimization pb. It does not include the time needed 
  # for model-building time and results computation .  
  exe_time$RF[I] = round(time_taken_RF,4)
  exe_time$CART[I] = round(time_taken_CART,4)
  exe_time$Intrees[I] = round(time_taken_Intrees,4)
  
  
  # update optimization results data frame
  MIP_summary$iter[I] = I
  MIP_summary[I,3:13] = OptSol_Summary[1,]
  
  # update Y else data frame
  yelse_df$iter[I] = I
  yelse_df$PS_rules[I] = SR_Y_elserule
  yelse_df$Opt_rules[I] = Y_elserule
  yelse_df$RRF_rules[I] = RRFRules_else$Y_elserule
  yelse_df$Intrees[I] = intreeselse["pred"]
  
  #  update rule metrics data frame
  OptRules_metrics_df_iter= data.frame(matrix(ncol=13, nrow = nrow(Rules_metrics[OptSol_RuleList,]) ))
  colnames(OptRules_metrics_df_iter)= c("data", "iter", "id" ,"confidence", "support", "class_suppport", "var_length", 
                                        "levels_length", "var_nbr_scaled", "levels_nbr_scaled", "cond","Ypred" ,"var_used")         
  
  OptRules_metrics_df_iter[,3:13]=Rules_metrics[OptSol_RuleList,]
  OptRules_metrics_df_iter$iter = I
  
  
  OptRules_metrics_df = rbind(OptRules_metrics_df,OptRules_metrics_df_iter)
  
  
  
  # update prediction data frame
  # RF
  prediction_perf_iter_RF = get_prediction_perf_N1(PredTrain=RF_PredTrain, PredTest=RF_PredTest, 
                                                   nbr_rules=N_RFRules,iter=I,learner_name = "RF") 
  prediction_perf_iter_RF$fidelity= c(1,NA,1)
  prediction_perf_iter_RF$fidelity_correct= c(1,NA,1)
  prediction_perf_iter_RF$fidelity_incorrect= c(1,NA,1)
  prediction_perf = rbind(prediction_perf , prediction_perf_iter_RF)
  
  
  # Preselected rule
  prediction_perf_iter_PSRules = get_prediction_perf_N2(PredTrain=PSRules_PredTrain,PredTest=PSRules_PredTest, 
                                                        nbr_rules=N_RFPreSelectedRules,iter=I, learner_name="PreSelectedRules") 
  prediction_perf_iter_PSRules= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_PSRules d1 LEFT JOIN PSRules_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  prediction_perf = rbind(prediction_perf , prediction_perf_iter_PSRules)
  
  # Preselected rule_abl
  
  # opt rules
  prediction_perf_iter_OptRules = get_prediction_perf_N2(PredTrain=OptRules_PredTrain,PredTest=OptRules_PredTest, 
                                                         nbr_rules=OptSol_Summary$Rules_nbr,
                                                         iter=I, learner_name="OptimalRules") 
  prediction_perf_iter_OptRules= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_OptRules d1 LEFT JOIN OptRules_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  prediction_perf = rbind(prediction_perf , prediction_perf_iter_OptRules)
  
  # opt rules STEL
  prediction_perf_iter_OPTSTEL = get_prediction_perf_N3(PredTrain=OPTSTEL_Pred_Train, 
                                                        PredTest=OPTSTEL_Pred_Test,
                                                        PredTrain_supported=OPTSTEL_Pred_Train_supported ,
                                                        PredTest_supported=OPTSTEL_Pred_Test_supported, 
                                                        PredTrain_notsupported=OPTSTEL_Pred_Train_notsupported, 
                                                        PredTest_notsupported=OPTSTEL_Pred_Test_notsupported,
                                                        cov_train=OPTSTEL_covered_train,
                                                        cov_test=OPTSTEL_covered_test,
                                                        nbr_rules=OPTSTEL_size, iter=I, learner_name="OPTSTEL") 
  prediction_perf_iter_OPTSTEL= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_OPTSTEL d1 LEFT JOIN OPTSTEL_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  prediction_perf = rbind(prediction_perf , prediction_perf_iter_OPTSTEL)
  
  
  
  # CART
  prediction_perf_iter_CART= get_prediction_perf_N1(PredTrain=CART_PredTrain, PredTest=CART_PredTest, 
                                                    nbr_rules=CART_rules_nbr,iter=I,learner_name = "CART") 
  
  # classifier fidelity metrics
  CART_PredTrain_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtrain,
                                                           Y_surrogate=CARTpredtrain,
                                                           Y_real=Data$YTrain,
                                                           covered_ids=1:length(CARTpredtrain),
                                                           predict_set="train",
                                                           learner_name="CART",
                                                           iter=I)
  
  length(CARTpredtest[])
  CART_PredTest_Fidelity_scores = Compute_fidelity_scores(Y_base=RFpredtest,
                                                          Y_surrogate=CARTpredtest,
                                                          Y_real=Data$YTest,
                                                          covered_ids=1:length(CARTpredtest),
                                                          predict_set="test",
                                                          learner_name="CART",
                                                          iter=I)
  
  CART_Fidelity_scores = rbind(CART_PredTrain_Fidelity_scores,CART_PredTest_Fidelity_scores)
  
  prediction_perf_iter_CART= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_CART d1 LEFT JOIN CART_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  
  
  
  prediction_perf = rbind(prediction_perf , prediction_perf_iter_CART)
  
  
  
  # Intrees RRF vote
  prediction_perf_iter_IntreesRRFVote = get_prediction_perf_N2(PredTrain=RRF_Rules_PredTrain,
                                                               PredTest=RRF_Rules_PredTest, 
                                                               nbr_rules=RRF_Rules_size,
                                                               iter=I, learner_name="IntreesRRFVote") 
  prediction_perf_iter_IntreesRRFVote= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_IntreesRRFVote d1 LEFT JOIN RRFR_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  prediction_perf = rbind(prediction_perf , prediction_perf_iter_IntreesRRFVote)
  
  # Intrees vote
  prediction_perf_iter_IntreesVote = get_prediction_perf_N2(PredTrain=Intrees_vote_PredTrain,PredTest=Intrees_vote_PredTest, 
                                                            nbr_rules=intreessize,
                                                            iter=I, learner_name="IntreesVote") 
  prediction_perf_iter_IntreesVote= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_IntreesVote d1 LEFT JOIN Intrees_vote_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  prediction_perf = rbind(prediction_perf , prediction_perf_iter_IntreesVote)
  
  
  # IntreesSTEL
  prediction_perf_iter_IntreesSTEL = get_prediction_perf_N3(PredTrain=STEL_Pred_Train, 
                                                            PredTest=STEL_Pred_Test,
                                                            PredTrain_supported=STEL_Pred_Train_supported ,
                                                            PredTest_supported=STEL_Pred_Test_supported, 
                                                            PredTrain_notsupported=STEL_Pred_Train_notsupported, 
                                                            PredTest_notsupported=STEL_Pred_Test_notsupported,
                                                            cov_train=Intrees_vote_PredTrain$prop_Xis_covered,
                                                            cov_test=Intrees_vote_PredTest$prop_Xis_covered,
                                                            nbr_rules=intreessize, iter=I, learner_name="IntreesSTEL")
  prediction_perf_iter_IntreesSTEL= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_IntreesSTEL d1 LEFT JOIN STEL_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  prediction_perf = rbind(prediction_perf , prediction_perf_iter_IntreesSTEL)
  
  #  update fidelity data frame
  fidelity_perf=rbind(fidelity_perf,PSRules_Fidelity_scores)
  fidelity_perf=rbind(fidelity_perf,OptRules_Fidelity_scores)
  fidelity_perf=rbind(fidelity_perf,OPTSTEL_Fidelity_scores)
  fidelity_perf=rbind(fidelity_perf,RRFR_Fidelity_scores)
  fidelity_perf=rbind(fidelity_perf,Intrees_vote_Fidelity_scores)
  fidelity_perf=rbind(fidelity_perf,STEL_Fidelity_scores)
  
  # update trees/rules count data frame
  rules_count$iter[I] = I
  rules_count$RFTree[I] = N_RFTree
  rules_count$RFRules[I] = N_RFRules
  rules_count$PreSelectedRules[I] = N_RFPreSelectedRules
  rules_count$OPTRules[I] = OptSol_Summary$Rules_nbr
  rules_count$IntreesRRFRules[I]= Intrees_RRF_rules_size
  rules_count$IntreesSTELRules[I] = intreessize
  rules_count$OPTRulesSTEL[I] = OPTSTEL_size
  rules_count$CART[I] = CART_rules_nbr
  
  # update stat related to the number of variables used in each rule
  
  # RF
  RF_var_used_count =ncol(RF_var_used)- rowSums(is.na(RF_var_used) | RF_var_used == "" | RF_var_used == " ")
  RF_var_used_stat= stat_nbr_var_used(rules_var_used_count=RF_var_used_count,iter=I,learner_name= "RF")
  var_used_stat = rbind(var_used_stat,RF_var_used_stat)
  
  # CART
  CART_var_used_count =ncol(CART_var_used)- rowSums(is.na(CART_var_used) | CART_var_used == "" | CART_var_used == " ")
  CART_var_used_stat= stat_nbr_var_used(rules_var_used_count=CART_var_used_count,iter=I,learner_name= "CART")
  var_used_stat = rbind(var_used_stat,CART_var_used_stat)
  
  # Preselected rules
  PSR_var_used_count =RF_PreSelected_Rules$perf_Rules$var_length
  PSR_var_used_stat= stat_nbr_var_used(rules_var_used_count=PSR_var_used_count,iter=I,learner_name= "PreSelectedRules")
  var_used_stat = rbind(var_used_stat,PSR_var_used_stat)
  
  # OptimalRules
  OPTrules_var_used_count =OptRules_metrics_df_iter$var_length
  OPTrules_var_used_stat= stat_nbr_var_used(rules_var_used_count=OPTrules_var_used_count,iter=I,learner_name= "OptimalRules")
  var_used_stat = rbind(var_used_stat,OPTrules_var_used_stat)
  
  
  # OPTSTEL
  OPTSTEL_var_used_count =as.numeric(learnerOptRules[-nrow(learnerOptRules),"len"])
  
  OPTSTEL_var_used_stat= stat_nbr_var_used(rules_var_used_count=OPTSTEL_var_used_count,iter=I,learner_name= "OPTSTEL")
  var_used_stat = rbind(var_used_stat,OPTSTEL_var_used_stat)
  
  # IntreesRRFVote
  
  IntreesRRFVote_var_used_count =as.numeric(RRFRules[,"len"])
  IntreesRRFVote_var_used_stat= stat_nbr_var_used(rules_var_used_count=IntreesRRFVote_var_used_count,iter=I,learner_name= "IntreesRRFVote")
  var_used_stat = rbind(var_used_stat,IntreesRRFVote_var_used_stat)
  
  # IntreesVote
  IntreesVote_var_used_count =as.numeric(learner[-nrow(learner),"len"])
  IntreesVote_var_used_stat= stat_nbr_var_used(rules_var_used_count=IntreesVote_var_used_count,iter=I,learner_name= "IntreesVote")
  var_used_stat = rbind(var_used_stat,IntreesVote_var_used_stat)
  
  # IntreesSTEL
  IntreesSTEL_var_used_stat= IntreesVote_var_used_stat
  IntreesSTEL_var_used_stat$method = "IntreesSTEL"
  var_used_stat = rbind(var_used_stat,IntreesSTEL_var_used_stat)
  
  # ablation study
  # source("ablation_script_2022.R")
  
  # other classifiers
  # path_other_classifier= paste0(".\\R_code\\", data_name,"_other_rule_classifiers_script.R") 
  # source(path_other_classifier)
}
end_time_10iter = Sys.time()
time_taken_10iter = difftime(end_time_10iter, start_time_10iter,units="secs")
884/60
I
# ------------------------------------------------
exe_time[,"data"] = data_name
rules_count[,"data"] = data_name
MIP_summary[,"data"] = data_name
prediction_perf[,"data"] = data_name
yelse_df[,"data"] = data_name
OptRules_metrics_df[,"data"] = data_name
var_used_stat[,"data"] = data_name
fidelity_perf[,"data"] = data_name

# -----------------------------------------------------------------------------------
exe_time_Path = paste0(".\\Rules_results\\", data_name, "_exe_time.csv")
write.csv(exe_time,exe_time_Path, row.names = FALSE)

rules_count_Path = paste0(".\\Rules_results\\", data_name, "_rules_count.csv")
write.csv(rules_count, rules_count_Path, row.names = FALSE)

MIP_summary_Path = paste0(".\\Rules_results\\", data_name, "_MIP_summary.csv")
write.csv(MIP_summary,MIP_summary_Path, row.names = FALSE)

prediction_perf_Path = paste0(".\\Rules_results\\", data_name, "_prediction_perf.csv")
write.csv(prediction_perf,prediction_perf_Path, row.names = FALSE)
A= read.csv(prediction_perf_Path)

fidelity_perf_Path = paste0(".\\Rules_results\\", data_name, "_fidelity_perf.csv")
write.csv(fidelity_perf,fidelity_perf_Path, row.names = FALSE)

yelse_df_Path = paste0(".\\Rules_results\\", data_name, "_yelse.csv")
write.csv(yelse_df,yelse_df_Path, row.names = FALSE)

var_used_stat_Path = paste0(".\\Rules_results\\", data_name, "_var_used_stat.csv")
write.csv(var_used_stat,var_used_stat_Path, row.names = FALSE)

OptRules_metrics_df_Path = paste0(".\\Rules_results\\", data_name, "_OptRules_metrics_df.csv")
write.csv(OptRules_metrics_df,OptRules_metrics_df_Path, row.names = FALSE)


# ------------------------------------------------
# exe_time_abl[,"data"] = data_name
# rules_count_abl[,"data"] = data_name
# MIP_summary_abl[,"data"] = data_name
# prediction_perf_abl[,"data"] = data_name
# fidelity_perf_abl[,"data"] = data_name
# OptRules_metrics_df_abl[,"data"] = data_name
# var_used_stat_abl[,"data"] = data_name
# yelse_df_abl[,"data"] = data_name
# # ------------------------------------------------
# # ablation study files -----------------------------------------------------------------------------------
# exe_time_abl_Path = paste0(".\\Rules_results_abl\\", data_name, "_exe_time_abl.csv")
# write.csv(exe_time_abl,exe_time_abl_Path, row.names = FALSE)
# 
# rules_count_abl_Path = paste0(".\\Rules_results_abl\\", data_name, "_rules_count_abl.csv")
# write.csv(rules_count_abl, rules_count_abl_Path, row.names = FALSE)
# 
# MIP_summary_abl_Path = paste0(".\\Rules_results_abl\\", data_name, "_MIP_summary_abl.csv")
# write.csv(MIP_summary_abl,MIP_summary_abl_Path, row.names = FALSE)
# 
# prediction_perf_abl_Path = paste0(".\\Rules_results_abl\\", data_name, "_prediction_perf_abl.csv")
# write.csv(prediction_perf_abl,prediction_perf_abl_Path, row.names = FALSE)
# 
# fidelity_perf_abl_Path = paste0(".\\Rules_results_abl\\", data_name, "_fidelity_perf_abl.csv")
# write.csv(fidelity_perf_abl,fidelity_perf_abl_Path, row.names = FALSE)
# 
# yelse_df_abl_Path = paste0(".\\Rules_results_abl\\", data_name, "_yelse_abl.csv")
# write.csv(yelse_df_abl,yelse_df_abl_Path, row.names = FALSE)
# 
# var_used_stat_abl_Path = paste0(".\\Rules_results_abl\\", data_name, "_var_used_stat_abl.csv")
# write.csv(var_used_stat_abl,var_used_stat_abl_Path, row.names = FALSE)
# 
# OptRules_metrics_df_abl_Path = paste0(".\\Rules_results_abl\\", data_name, "_OptRules_metrics_df_abl.csv")
# write.csv(OptRules_metrics_df_abl,OptRules_metrics_df_abl_Path, row.names = FALSE)
# 
# # -----------------------------------------------------------------------------------
names(prediction_perf)
avg_prediction_perf = sqldf('select data, method, pred_set, pred_sub_set, 
                          avg(rules_nbr) as mean_rules_nbr, 
                          stdev(rules_nbr) as std_rules_nbr,
                          avg(coverage) as mean_coverage, 
                          stdev(coverage) as std_coverage,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          avg(macroPrecision) as mean_macroPrecision, 
                          stdev(macroPrecision) as std_macroPrecision,
                          avg(macroRecall) as mean_macroRecall, 
                          stdev(macroRecall) as std_macroRecall,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa,
                          avg(kappa_w) as mean_kappa_w, 
                          stdev(kappa_w) as std_kappa_w,
                          avg(fidelity) as mean_fidelity, 
                          stdev(fidelity) as std_fidelity
                          from prediction_perf group by method, pred_set, pred_sub_set')
avg_prediction_perf_Path = paste0(".\\Rules_results\\", data_name, "_avg_prediction_perf.csv")
write.csv(avg_prediction_perf,avg_prediction_perf_Path, row.names = FALSE)
B = read.csv(avg_prediction_perf_Path)
# avg_prediction_perf_abl = sqldf('select data, method, pred_set, pred_sub_set, abl_model,
#                           avg(rules_nbr) as mean_rules_nbr, 
#                           stdev(rules_nbr) as std_rules_nbr,
#                           avg(coverage) as mean_coverage, 
#                           stdev(coverage) as std_coverage,
#                           avg(accuracy) as mean_acc, 
#                           stdev(accuracy) as std_acc, 
#                           avg(macroPrecision) as mean_macroPrecision, 
#                           stdev(macroPrecision) as std_macroPrecision,
#                           avg(macroRecall) as mean_macroRecall, 
#                           stdev(macroRecall) as std_macroRecall,
#                           avg(macroF1) as mean_F1, 
#                           stdev(macroF1) as std_F1,
#                           avg(kappa) as mean_Kappa, 
#                           stdev(kappa) as std_Kappa,
#                           avg(kappa_w) as mean_kappa_w, 
#                           stdev(kappa_w) as std_kappa_w,
#                           avg(fidelity) as mean_fidelity, 
#                           stdev(fidelity) as std_fidelity
#                           from prediction_perf_abl group by method, pred_set, pred_sub_set, abl_model')

