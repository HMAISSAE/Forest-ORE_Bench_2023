# Ablation study
# We  analyze the impact of the different components of the obtimization problem on 
# the predictive performance and the interpretability of the resulting models 
# Similarly to the ablation methodology used in [Interpretable Decision Sets: A Joint Framework for Description and Prediction], 
# we analyze the results obtained by considering various ablations of our model by 
# excluding some components from the objective function or the contraints one at a time. 
# We consider 7 ablation models. 
# The first ablated model called No high confidence (W0=0) is obtained by excluding the term which encourages 
# rules with high confidence.  
# The second model called No high support (W1=0) excludes the term that encourages rules with high support. 
# The third model called No reduced length (W2=0) excludes the term which encourages rules with small lengths. 
# The fourth model called No reduced modalities (W3=0) excludes the term which encourages rules with few levels. 
# The fifth model called No Min accuracy (alpha/remove the constraint) excludes the constraints which establish 
# a lower bound for the overall accuracy of the rule ensemble.
# The sixth model called No Min coverage (beta / remove the constraint) excludes the constraints which establish
# a lower bound for the overall coverage of the rule ensemble.
# The seventh model called No Max cover (MaxCover / remove constraint) excludes the constraints which set an upper 
# bound for the number of rules that can explain each instance.
# The eighth model called No Max overlap (maxoverlap / remove constraint) excludes the constraints which set an upper 
# bound for the overall overlap between the rules.
# The ninetieth model called No preselect excludes the preselection stage


# _no_abl
# _abl_conf
# _abl_supp
# _abl_length
# _abl_mod
# _abl_acc
# _abl_min_cov
# _abl_max_cov
# _abl_overlap
# _abl_preselect


# ----------------------------------------------------------------------------------------------------
print(paste0(Sys.time()," Iter ",I," Ablation"," no_abl"," in progress"))
# update output data frames by adding the case without ablation
id_abl = id_abl+1

# update exe_time data frame
exe_time_abl$iter[id_abl] = I
exe_time_abl$extract_rules[id_abl] = round(time_taken_extract_rules,4)
exe_time_abl$preselect_rules[id_abl] = round(time_taken_preselect_rules,4)
exe_time_abl$prepare_opt_input[id_abl] = round(time_taken_prepare_opt_input,4) 
#prepare_opt_input is the required time for preparing the optimization pb input data.
exe_time_abl$buildandrun_opt[id_abl] = round(OptSol_Summary$run_time,4) 
# buildandrun_opt is the time used by Gurobi to build, solve, and generate the outputs of the optimization problem. 
# model-building time includes adding the variables, constraints, etc.).
exe_time_abl$run_opt[id_abl] = round(OptSol_Summary$opt_run_time,4) # 
# run_opt is the required time for solving the optimization pb. It does not include the time needed 
# for model-building time and results computation .  
exe_time_abl$abl_model[id_abl] = "no_abl"



# update optimization results data frame
MIP_summary_abl$iter[id_abl] = I
MIP_summary_abl[id_abl,3:13] = OptSol_Summary[1,]
MIP_summary_abl$abl_model[id_abl] = "no_abl"
# update Y else data frame
yelse_df_abl$iter[id_abl] = I
yelse_df_abl$PS_rules[id_abl] = SR_Y_elserule
yelse_df_abl$Opt_rules[id_abl] = Y_elserule
yelse_df_abl$abl_model[id_abl] = "no_abl"

#  update rule metrics data frame
OptRules_metrics_df_iter= data.frame(matrix(ncol=13, nrow = nrow(Rules_metrics[OptSol_RuleList,]) ))
colnames(OptRules_metrics_df_iter)= c("data", "iter", "id" ,"confidence", "support", "class_suppport", "var_length", 
                                      "levels_length", "var_nbr_scaled", "levels_nbr_scaled", "cond","Ypred" ,"var_used")         

OptRules_metrics_df_iter[,3:13]=Rules_metrics[OptSol_RuleList,]
OptRules_metrics_df_iter$iter = I
OptRules_metrics_df_iter$abl_model = "no_abl"

OptRules_metrics_df_abl = rbind(OptRules_metrics_df_abl,OptRules_metrics_df_iter)

# Update prediction perf
# opt rules
prediction_perf_iter_OptRules = get_prediction_perf_N2(PredTrain=OptRules_PredTrain,PredTest=OptRules_PredTest, 
                                                       nbr_rules=OptSol_Summary$Rules_nbr,
                                                       iter=I, learner_name="OptimalRules") 
prediction_perf_iter_OptRules= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_OptRules d1 LEFT JOIN OptRules_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
prediction_perf_iter_OptRules$abl_model = "no_abl"
prediction_perf_abl = rbind(prediction_perf_abl , prediction_perf_iter_OptRules)

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
prediction_perf_iter_OPTSTEL$abl_model = "no_abl"
prediction_perf_abl = rbind(prediction_perf_abl , prediction_perf_iter_OPTSTEL)

#  update fidelity data frame
OptRules_Fidelity_scores$abl_model = "no_abl"
OPTSTEL_Fidelity_scores$abl_model = "no_abl"
fidelity_perf_abl=rbind(fidelity_perf_abl,OptRules_Fidelity_scores)
fidelity_perf_abl=rbind(fidelity_perf_abl,OPTSTEL_Fidelity_scores)

# update trees/rules count data frame
rules_count_abl$iter[id_abl] = I
rules_count_abl$RFTree[id_abl] = N_RFTree
rules_count_abl$RFRules[id_abl] = N_RFRules
rules_count_abl$PreSelectedRules[id_abl] = N_RFPreSelectedRules
rules_count_abl$OPTRules[id_abl] = OptSol_Summary$Rules_nbr
rules_count_abl$OPTRulesSTEL[id_abl] = OPTSTEL_size
rules_count_abl$abl_model[id_abl] = "no_abl"

# update stat related to the number of variables used in each rule
# OptimalRules
OPTrules_var_used_count = OptRules_metrics_df_iter$var_length
OPTrules_var_used_stat = stat_nbr_var_used(rules_var_used_count=OPTrules_var_used_count,iter=I,learner_name= "OptimalRules")
OPTrules_var_used_stat$abl_model = "no_abl"
var_used_stat_abl = rbind(var_used_stat_abl,OPTrules_var_used_stat)

# OPTSTEL
OPTSTEL_var_used_count =as.numeric(learnerOptRules[-nrow(learnerOptRules),"len"])
OPTSTEL_var_used_stat= stat_nbr_var_used(rules_var_used_count=OPTSTEL_var_used_count,iter=I,learner_name= "OPTSTEL")
OPTSTEL_var_used_stat$abl_model = "no_abl"
var_used_stat_abl = rbind(var_used_stat_abl,OPTSTEL_var_used_stat)


# -------------------------------------------------------------------------------------------------------------
# c("no_abl", "abl_preselect", "abl_conf","abl_supp", "abl_length", "abl_mod", 
#                      "abl_acc", "abl_min_cov", "abl_max_cov", "abl_overlap", "abl_preselect")) 

for (abl_method in c("abl_conf","abl_supp", "abl_length", "abl_mod", 
                     "abl_acc", "abl_min_cov", "abl_max_cov", "abl_overlap")) 
# for (abl_method in c("abl_acc", "abl_min_cov", "abl_max_cov", "abl_overlap"))
{
  id_abl=id_abl+1
  print(paste0(Sys.time()," Iter ",I," Ablation ",abl_method," in progress"))
  if (abl_method=="abl_conf") 
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 0, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
                                  MaxOverlap = 0.5, Alpha= 0.01, Beta= 0.025)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }else if (abl_method=="abl_supp")
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 1, W1 = 0, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
                                  MaxOverlap = 0.5, Alpha= 0.01, Beta= 0.025)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }else if (abl_method=="abl_length")
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 1, W1 = 1, W2 = 0, W3 = 0.05, MaxCover = 3, 
                                  MaxOverlap = 0.5, Alpha= 0.01, Beta= 0.025)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }else if (abl_method=="abl_mod")
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 1, W1 = 1, W2 = 0.1, W3 = 0, MaxCover = 3, 
                                  MaxOverlap = 0.5, Alpha= 0.01, Beta= 0.025)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }else if (abl_method=="abl_acc")
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
                                  MaxOverlap = 0.5, Alpha= 0.999, Beta= 0.025)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }else if (abl_method=="abl_min_cov") 
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
                                  MaxOverlap = 0.5, Alpha= 0.01, Beta= 0.999)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }else if (abl_method=="abl_max_cov")   
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 100, 
                                  MaxOverlap = 0.5, Alpha= 0.01, Beta= 0.025)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }else if (abl_method=="abl_overlap")
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
                                  MaxOverlap = 1, Alpha= 0.01, Beta= 0.025)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }else if (abl_method=="abl_overlap")
  {
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE)
    opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                                  RulesCoverOk_location = Rules_cover_ok_Path_py,
                                  RulesCoverNok_location = Rules_cover_nok_Path_py,
                                  InitError = RF_error_train,
                                  W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
                                  MaxOverlap = 1, Alpha= 0.01, Beta= 0.025)
    source_python("./Find_OPT_Rules_2022.py", convert = FALSE) # for printing remaining output
  }
  
  # optimal solution summary, rule list, uncovered instances list, and incorrectly covered instances list
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
  
  # update exe_time data frame
  exe_time_abl$iter[id_abl] = I
  exe_time_abl$extract_rules[id_abl] = round(time_taken_extract_rules,4)
  exe_time_abl$preselect_rules[id_abl] = round(time_taken_preselect_rules,4)
  exe_time_abl$prepare_opt_input[id_abl] = round(time_taken_prepare_opt_input,4) 
  #prepare_opt_input is the required time for preparing the optimization pb input data.
  exe_time_abl$buildandrun_opt[id_abl] = round(OptSol_Summary$run_time,4) 
  # buildandrun_opt is the time used by Gurobi to build, solve, and generate the outputs of the optimization problem. 
  # model-building time includes adding the variables, constraints, etc.).
  exe_time_abl$run_opt[id_abl] = round(OptSol_Summary$opt_run_time,4) # 
  # run_opt is the required time for solving the optimization pb. It does not include the time needed 
  # for model-building time and results computation .  
  exe_time_abl$abl_model[id_abl] = abl_method

  
  
  # update optimization results data frame
  MIP_summary_abl$iter[id_abl] = I
  MIP_summary_abl[id_abl,3:13] = OptSol_Summary[1,]
  MIP_summary_abl$abl_model[id_abl] = abl_method
  # update Y else data frame
  yelse_df_abl$iter[id_abl] = I
  yelse_df_abl$PS_rules[id_abl] = SR_Y_elserule
  yelse_df_abl$Opt_rules[id_abl] = Y_elserule
  yelse_df_abl$abl_model[id_abl] = abl_method
  
  #  update rule metrics data frame
  OptRules_metrics_df_iter= data.frame(matrix(ncol=13, nrow = nrow(Rules_metrics[OptSol_RuleList,]) ))
  colnames(OptRules_metrics_df_iter)= c("data", "iter", "id" ,"confidence", "support", "class_suppport", "var_length", 
                                        "levels_length", "var_nbr_scaled", "levels_nbr_scaled", "cond","Ypred" ,"var_used")         
  
  OptRules_metrics_df_iter[,3:13]=Rules_metrics[OptSol_RuleList,]
  OptRules_metrics_df_iter$iter = I
  OptRules_metrics_df_iter$abl_model = abl_method
  
  OptRules_metrics_df_abl = rbind(OptRules_metrics_df_abl,OptRules_metrics_df_iter)
  
  # Update prediction perf
  # opt rules
  prediction_perf_iter_OptRules = get_prediction_perf_N2(PredTrain=OptRules_PredTrain,PredTest=OptRules_PredTest, 
                                                         nbr_rules=OptSol_Summary$Rules_nbr,
                                                         iter=I, learner_name="OptimalRules") 
  prediction_perf_iter_OptRules= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_OptRules d1 LEFT JOIN OptRules_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
  prediction_perf_iter_OptRules$abl_model = abl_method
  prediction_perf_abl = rbind(prediction_perf_abl , prediction_perf_iter_OptRules)
  
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
  prediction_perf_iter_OPTSTEL$abl_model = abl_method
  prediction_perf_abl = rbind(prediction_perf_abl , prediction_perf_iter_OPTSTEL)
  
  #  update fidelity data frame
  OptRules_Fidelity_scores$abl_model = abl_method
  OPTSTEL_Fidelity_scores$abl_model = abl_method
  fidelity_perf_abl=rbind(fidelity_perf_abl,OptRules_Fidelity_scores)
  fidelity_perf_abl=rbind(fidelity_perf_abl,OPTSTEL_Fidelity_scores)
 
  # update trees/rules count data frame
  rules_count_abl$iter[id_abl] = I
  rules_count_abl$RFTree[id_abl] = N_RFTree
  rules_count_abl$RFRules[id_abl] = N_RFRules
  rules_count_abl$PreSelectedRules[id_abl] = N_RFPreSelectedRules
  rules_count_abl$OPTRules[id_abl] = OptSol_Summary$Rules_nbr
  rules_count_abl$OPTRulesSTEL[id_abl] = OPTSTEL_size
  rules_count_abl$abl_model[id_abl] = abl_method
  
  # update stat related to the number of variables used in each rule
    # OptimalRules
  OPTrules_var_used_count = OptRules_metrics_df_iter$var_length
  OPTrules_var_used_stat = stat_nbr_var_used(rules_var_used_count=OPTrules_var_used_count,iter=I,learner_name= "OptimalRules")
  OPTrules_var_used_stat$abl_model = abl_method
  var_used_stat_abl = rbind(var_used_stat_abl,OPTrules_var_used_stat)
  
  # OPTSTEL
  OPTSTEL_var_used_count =as.numeric(learnerOptRules[-nrow(learnerOptRules),"len"])
  OPTSTEL_var_used_stat= stat_nbr_var_used(rules_var_used_count=OPTSTEL_var_used_count,iter=I,learner_name= "OPTSTEL")
  OPTSTEL_var_used_stat$abl_model = abl_method
  var_used_stat_abl = rbind(var_used_stat_abl,OPTSTEL_var_used_stat)
  
} 


# Ablation preselection step
id_abl = id_abl+1
print(paste0(Sys.time()," Iter ",I," Ablation"," abl_preselection"," in progress"))
start_time_rules_abl_preselect = Sys.time()

RF_Rules_abl_preselect=RulestoRulespred(X=Data$XTrain, Y=Data$YTrain,RF_Selected_Rules=RF_Selected_Rules,
                                        min_rule_class_support=min_R_class_support, 
                                        min_rule_confidence=min_R_confidence, max_rule_var=max_R_var_nbr,
                                        similarity_treshold=R_similarity_treshold)
end_time_rules_abl_preselect = Sys.time()
time_taken_preselect_rules = difftime(end_time_rules_abl_preselect, start_time_rules_abl_preselect,units="secs")
N_RF_abl_preselect=nrow(RF_Rules_abl_preselect$Rules_selection)


dim(RF_Rules_abl_preselect$Rules_selection)
# Extracting metrics related to preselected rules
start_time_prepare_opt_input = Sys.time()

Rules_metrics = RF_Rules_abl_preselect$perf_Rules
Rules_DF = RF_Rules_abl_preselect$RulesPrediction
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

source_python("./Find_OPT_Rules_2022.py", convert = FALSE)

opt_solution = find_opt_rules(RulesMetrics_location = Rules_metrics_Path_py,
                              RulesCoverOk_location = Rules_cover_ok_Path_py,
                              RulesCoverNok_location = Rules_cover_nok_Path_py,
                              InitError = round(RF_error_train,4),
                              W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3, 
                              MaxOverlap = 0.5, Alpha= 0.01, Beta= 0.025)


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


# update output data frames by adding the case without preselection stage
# update exe_time data frame
exe_time_abl$iter[id_abl] = I
exe_time_abl$extract_rules[id_abl] = round(time_taken_extract_rules,4)
exe_time_abl$preselect_rules[id_abl] = round(time_taken_preselect_rules,4)
exe_time_abl$prepare_opt_input[id_abl] = round(time_taken_prepare_opt_input,4) 
#prepare_opt_input is the required time for preparing the optimization pb input data.
exe_time_abl$buildandrun_opt[id_abl] = round(OptSol_Summary$run_time,4) 
# buildandrun_opt is the time used by Gurobi to build, solve, and generate the outputs of the optimization problem. 
# model-building time includes adding the variables, constraints, etc.).
exe_time_abl$run_opt[id_abl] = round(OptSol_Summary$opt_run_time,4) # 
# run_opt is the required time for solving the optimization pb. It does not include the time needed 
# for model-building time and results computation .  
exe_time_abl$abl_model[id_abl] = "abl_preselect"



# update optimization results data frame
MIP_summary_abl$iter[id_abl] = I
MIP_summary_abl[id_abl,3:13] = OptSol_Summary[1,]
MIP_summary_abl$abl_model[id_abl] = "abl_preselect"
# update Y else data frame
yelse_df_abl$iter[id_abl] = I
yelse_df_abl$PS_rules[id_abl] = SR_Y_elserule
yelse_df_abl$Opt_rules[id_abl] = Y_elserule
yelse_df_abl$abl_model[id_abl] = "abl_preselect"

#  update rule metrics data frame
OptRules_metrics_df_iter= data.frame(matrix(ncol=13, nrow = nrow(Rules_metrics[OptSol_RuleList,]) ))
colnames(OptRules_metrics_df_iter)= c("data", "iter", "id" ,"confidence", "support", "class_suppport", "var_length", 
                                      "levels_length", "var_nbr_scaled", "levels_nbr_scaled", "cond","Ypred" ,"var_used")         

OptRules_metrics_df_iter[,3:13]=Rules_metrics[OptSol_RuleList,]
OptRules_metrics_df_iter$iter = I
OptRules_metrics_df_iter$abl_model = "abl_preselect"

OptRules_metrics_df_abl = rbind(OptRules_metrics_df_abl,OptRules_metrics_df_iter)

# Update prediction perf
# opt rules
prediction_perf_iter_OptRules = get_prediction_perf_N2(PredTrain=OptRules_PredTrain,PredTest=OptRules_PredTest, 
                                                       nbr_rules=OptSol_Summary$Rules_nbr,
                                                       iter=I, learner_name="OptimalRules") 
prediction_perf_iter_OptRules= sqldf('select d1.*, d2.fidelity, d2.fidelity_correct, d2.fidelity_incorrect 
              from prediction_perf_iter_OptRules d1 LEFT JOIN OptRules_Fidelity_scores d2 
              ON (  d1.pred_set = d2.pred_set and d1.pred_sub_set = d2.pred_sub_set )')
prediction_perf_iter_OptRules$abl_model = "abl_preselect"
prediction_perf_abl = rbind(prediction_perf_abl , prediction_perf_iter_OptRules)

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
prediction_perf_iter_OPTSTEL$abl_model = "abl_preselect"
prediction_perf_abl = rbind(prediction_perf_abl , prediction_perf_iter_OPTSTEL)

#  update fidelity data frame
OptRules_Fidelity_scores$abl_model = "abl_preselect"
OPTSTEL_Fidelity_scores$abl_model = "abl_preselect"
fidelity_perf_abl=rbind(fidelity_perf_abl,OptRules_Fidelity_scores)
fidelity_perf_abl=rbind(fidelity_perf_abl,OPTSTEL_Fidelity_scores)

# update trees/rules count data frame
rules_count_abl$iter[id_abl] = I
rules_count_abl$RFTree[id_abl] = N_RFTree
rules_count_abl$RFRules[id_abl] = N_RFRules
rules_count_abl$PreSelectedRules[id_abl] = N_RF_abl_preselect
rules_count_abl$OPTRules[id_abl] = OptSol_Summary$Rules_nbr
rules_count_abl$OPTRulesSTEL[id_abl] = OPTSTEL_size
rules_count_abl$abl_model[id_abl] = "abl_preselect"

# update stat related to the number of variables used in each rule
# OptimalRules
OPTrules_var_used_count = OptRules_metrics_df_iter$var_length
OPTrules_var_used_stat = stat_nbr_var_used(rules_var_used_count=OPTrules_var_used_count,iter=I,learner_name= "OptimalRules")
OPTrules_var_used_stat$abl_model = "abl_preselect"
var_used_stat_abl = rbind(var_used_stat_abl,OPTrules_var_used_stat)

# OPTSTEL
OPTSTEL_var_used_count =as.numeric(learnerOptRules[-nrow(learnerOptRules),"len"])
OPTSTEL_var_used_stat= stat_nbr_var_used(rules_var_used_count=OPTSTEL_var_used_count,iter=I,learner_name= "OPTSTEL")
OPTSTEL_var_used_stat$abl_model = "abl_preselect"
var_used_stat_abl = rbind(var_used_stat_abl,OPTSTEL_var_used_stat)
table(prediction_perf_abl$iter, prediction_perf_abl$abl_model)
