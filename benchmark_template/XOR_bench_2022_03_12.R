wd <- getwd()
setwd(wd)

data_name = "XOR1"

# import data
data_path = "./data/XOR04.csv"
original_data=read.csv(data_path)
summary(original_data)


# cleaning

# target class
id_target = 3
dim(original_data)


original_data=na.roughfix(original_data)

for (i in 1:ncol(original_data)) 
{
    original_data[,i]=as.factor(original_data[,i])
    levels(original_data[,i])=gsub("_","-",levels(original_data[,i]))
}

str(original_data)




# 
original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)
# data_summary_Path = paste0(".\\data_summary\\", data_name, "_datasummary.csv")
# write.csv(original_data_summary,data_summary_Path, row.names = FALSE)
# read.csv(data_summary_Path)
# discretizing continuous attributes
# if (original_data_summary$ncont_att >=1) 
# {
#     discretize_data = ForestDisc(data=original_data, id_target=id_target)
#     original_data = discretize_data$data_disc
#     
# }

original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)
# data_summary_Path = paste0(".\\data_summary\\", data_name, "_datasummary.csv")
# write.csv(original_data_summary,data_summary_Path, row.names = FALSE)
# read.csv(data_summary_Path)
# discretizing continuous attributes
# if (original_data_summary$ncont_att >=1) 
# {
#     discretize_data = ForestDisc(data=original_data, id_target=id_target)
#     original_data = discretize_data$data_disc
#     
# }


if (original_data_summary$ncont_att >=1) 
{
    
    discretize_data = ForestDisc(data=original_data, id_target=id_target,max_splits = 4)
    original_data = discretize_data$data_disc
    
}
if (original_data_summary$ncat_att >=1) 
{
    for (i in eval(parse(text=paste0("c(",original_data_summary$cat,")")))) 
    {
        original_data[,i]=as.factor(original_data[,i])
        levels(original_data[,i])=gsub("_","-",levels(original_data[,i]))
    } 
}



str(original_data)

vec_seed=c(012,123,234,345,456,567,678,789,890,901)
# Set default parameters
Nbr_tree = 100 # number of Random Forest Trees
# min_R_class_support=0.01 # min support per target class
min_R_class_support=0.05 # min support per target class
min_R_confidence = 0.51 # min rule confidence
max_R_var_nbr = 6 # min number of descriptive variables used in each rule
R_similarity_treshold=0.95


#  prepare output dataframes

exe_time=data.frame(matrix(nrow=10,ncol=10))
colnames(exe_time)=c("data", "iter","extract_rules","preselect_rules", "prepare_opt_input", 
                     "run_opt","prepareandrun_opt","RF","CART","Intrees" )

rules_count=data.frame(matrix(nrow=10,ncol=10))
colnames(rules_count)=c("data", "iter", "RFTree","RFRules", "PreSelectedRules", "OPTRules","OPTRulesSTEL", 
                        "IntreesRRFRules", "IntreesSTELRules", "CART")

MIP_summary=data.frame(matrix(nrow=10,ncol=13))
colnames(MIP_summary)=c("data", "iter","error_tolerance", "coverage_tolerance", "coverage_ratio", "init_error", 
                        "covered_error_ratio", "overlapping_ratio", "optimality", "opt_run_time", "run_time",
                        "init_Rules_nbr", "Rules_nbr") 

prediction_perf=data.frame()

OptRules_metrics_df= data.frame()

var_used_stat=data.frame()

yelse_df = data.frame(matrix(nrow=10,ncol=6))
colnames(yelse_df) = c("data", "iter","PS_rules","Opt_rules", "RRF_rules", "Intrees")


# run Iterations

# for (I in (1:length(vec_seed)))
for (I in (1:1))
{
    iteration=I
    
    print(paste0(Sys.time()," Iter ",I," in progress"))
    set_seed=vec_seed[I]
    # splitting into training and testing set
    Data=split_dataset(XDataset=original_data[,-id_target],
                       YDataset=original_data[,id_target],
                       SplitRatio=0.7,seed = set_seed )
    
    # computing minimun rule support given minimum rule Class support: 
    # Note that the Minimum size of terminal nodes in random forest is set for classification to 1. 
    # Here we set this value to min(1 ,min_rule_support*length(Data$YTrain))
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
    
    # set min_R_confidence parameter (optionel): the goal is to reduce the number of preselected rules
    min_R_confidence=max(min_R_confidence,(RF_PredTrain$Global_perf$accuracy-0.25))
    
    # Selecting a set of rules based on tresholds related to support/class and confidence
    start_time_preselect_rules = Sys.time()
    
    RF_PreSelected_Rules=RulesToPreSelectRules(X=Data$XTrain, Y=Data$YTrain,RF_Selected_Rules=RF_Selected_Rules,
                                               min_rule_class_support=min_R_class_support, 
                                               min_rule_confidence=min_R_confidence, max_rule_var=max_R_var_nbr,
                                               similarity_treshold=R_similarity_treshold)
    end_time_preselect_rules = Sys.time()
    time_taken_preselect_rules = difftime(end_time_preselect_rules, start_time_preselect_rules,units="secs")
    N_RFPreSelectedRules=nrow(RF_PreSelected_Rules$Rules_selection)
    A=RF_PreSelected_Rules$perf_Rules_simil
    
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
    
    
    source_python("./Find_OPT_Rules23062021.py", convert = FALSE)
    
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
    source_python("./Find_OPT_Rules23062021.py", convert = FALSE) # for printing remaining output
    
    # optimal solution summary, rule list, incoverd instances list, and incorrectly covered instances list
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
    
    
    
    
    
    # Opt rules prediction on the traing set
    
    OptRules_PredTrain=PREDICT_selected_rules(X=Data$XTrain,Y=Data$YTrain,Rules_metrics=Rules_metrics,
                                              Rules_ids=OptSol_RuleList,Y_elserule=Y_elserule)
    
    
    # rules prediction on the testing set
    OptRules_PredTest=PREDICT_selected_rules(X=Data$XTest,Y=Data$YTest,Rules_metrics=Rules_metrics,
                                             Rules_ids=OptSol_RuleList,Y_elserule=Y_elserule)
    
    
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
    
    # RRF_Rules_PredTest$cover_pred_perf_indicators$Global_perf
    # RRF_Rules_PredTest$global_pred_perf_indicators$Global_perf
    # RRF_Rules_PredTest$notcover_pred_perf_indicators$Global_perf
    
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
    
    
    # update exe_time data frame
    exe_time$iter[I] = I
    exe_time$extract_rules[I] = round(time_taken_extract_rules,4)
    exe_time$preselect_rules[I] = round(time_taken_preselect_rules,4)
    exe_time$prepare_opt_input[I] = round(time_taken_prepare_opt_input,4)
    exe_time$prepareandrun_opt[I] = round(OptSol_Summary$run_time,4)
    exe_time$run_opt[I] = round(OptSol_Summary$opt_run_time,4)
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
    
    prediction_perf = rbind(prediction_perf , prediction_perf_iter_RF)
    
    
    # Preselected rule
    prediction_perf_iter_PSRules = get_prediction_perf_N2(PredTrain=PSRules_PredTrain,PredTest=PSRules_PredTest, 
                                                          nbr_rules=N_RFPreSelectedRules,iter=I, learner_name="PreSelectedRules") 
    
    prediction_perf = rbind(prediction_perf , prediction_perf_iter_PSRules)
    
    
    # opt rules
    prediction_perf_iter_OptRules = get_prediction_perf_N2(PredTrain=OptRules_PredTrain,PredTest=OptRules_PredTest, 
                                                           nbr_rules=OptSol_Summary$Rules_nbr,
                                                           iter=I, learner_name="OptimalRules") 
    
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
    prediction_perf = rbind(prediction_perf , prediction_perf_iter_OPTSTEL)
    
    
    
    # CART
    prediction_perf_iter_CART= get_prediction_perf_N1(PredTrain=CART_PredTrain, PredTest=CART_PredTest, 
                                                      nbr_rules=CART_rules_nbr,iter=I,learner_name = "CART") 
    
    prediction_perf = rbind(prediction_perf , prediction_perf_iter_CART)
    
    # Intrees RRF vote
    prediction_perf_iter_IntreesRRFVote = get_prediction_perf_N2(PredTrain=RRF_Rules_PredTrain,
                                                                 PredTest=RRF_Rules_PredTest, 
                                                                 nbr_rules=RRF_Rules_size,
                                                                 iter=I, learner_name="IntreesRRFVote") 
    
    prediction_perf = rbind(prediction_perf , prediction_perf_iter_IntreesRRFVote)
    
    # Intrees vote
    prediction_perf_iter_IntreesVote = get_prediction_perf_N2(PredTrain=Intrees_vote_PredTrain,PredTest=Intrees_vote_PredTest, 
                                                              nbr_rules=intreessize,
                                                              iter=I, learner_name="IntreesVote") 
    
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
    prediction_perf = rbind(prediction_perf , prediction_perf_iter_IntreesSTEL)
    
    
    
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
}
XOR_rules_metrics=OptRules_metrics_df_iter[,-c(1,2,9,10)]
XOR_rules_metrics$cond=gsub("%","",XOR_rules_metrics$cond)
XOR_rules_metrics$cond=gsub("c(","{",XOR_rules_metrics$cond,fixed = TRUE)
XOR_rules_metrics$cond=gsub(")","}",XOR_rules_metrics$cond,fixed = TRUE)
XOR_rules_metrics$cond=gsub("'","",XOR_rules_metrics$cond,fixed = TRUE)

XOR_ForestORE_rules_metrics_path= ".\\benchmark_results\\rules_results\\XOR_ForestORE_rules_metrics.csv"
write.csv(XOR_rules_metrics,XOR_ForestORE_rules_metrics_path, row.names = FALSE)
testF8=read.table(XOR_ForestORE_rules_metrics_path,sep=',',header= T, na.strings="?")

CART_rules

XOR_STEl=as.data.frame(learner)
XOR_STEl$condition=gsub("%","",XOR_STEl$condition)
XOR_STEl$condition=gsub("c(","{",XOR_STEl$condition,fixed = TRUE)
XOR_STEl$condition=gsub(")","}",XOR_STEl$condition,fixed = TRUE)
XOR_STEl$condition=gsub("'","",XOR_STEl$condition,fixed = TRUE)

XOR_STEl_rules_metrics_path= ".\\benchmark_results\\rules_results\\XOR_STEl_rules_metrics.csv"
write.csv(XOR_STEl,XOR_STEl_rules_metrics_path, row.names = FALSE)
testF81=read.table(XOR_STEl_rules_metrics_path,sep=',',header= T, na.strings="?")

CART_rules_DF=data.frame(matrix(nrow=nrow(CART_rules),ncol=2))
colnames(CART_rules_DF)=c("pred","condition")
L=ncol(CART_rules)
str(CART_rules[,1])
CART_rules_DF$pred=as.character(round(as.numeric(CART_rules[,1]),0))
for (i in 1:nrow(CART_rules)) 
{
    
    CART_rules_DF[i,2]= paste0(CART_rules[i,2:L], collapse=" ")
}  
XOR_CART_rules_metrics_path= ".\\benchmark_results\\rules_results\\XOR_CART_rules_metrics.csv"
write.csv(CART_rules_DF,XOR_CART_rules_metrics_path, row.names = FALSE)
testF82=read.table(XOR_CART_rules_metrics_path,sep=',',header= T, na.strings="?")


# Do some plots to analyze the sets covered by the optimal set of rules
Rules_cover_oknok=Rules_cover_ok+Rules_cover_nok
data_rules=Rules_cover_oknok[,OptSol_RuleList]
data_rules[,"id"]= 1:nrow(data_rules)
data_rules[,"else"]=0
data_rules[, "rowsum"]=rowSums(data_rules[,1:length(OptSol_RuleList)])
data_rules[which(data_rules[, "rowsum"]==0),"else"]=1
ncl=ncol(data_rules)
data_rules_opt=data_rules[,c((ncl-2),(1:(ncl-3)),(ncl-1))]
names(data_rules_opt)= gsub('X','R',names(data_rules_opt))
if (length(initial_not_covered_rows) > 0)
{
    Data_ypred = Data$YTrain[-initial_not_covered_rows] 
}else
{
    Data_ypred = Data$YTrain  
}

data_rules_opt[,"yclass"]=Data_ypred
# OSR_pred=Rules_metrics_all[OptSol_RuleList,c(1:4,6:7,10)]
data_rules_opt_XOR=data_rules_opt
XOR_rules_pred_matrix=data_rules_opt_XOR
XOR_rules_pred_matrix_path= ".\\benchmark_results\\rules_results\\XOR_rules_pred_matrix.csv"
write.csv(XOR_rules_pred_matrix,XOR_rules_pred_matrix_path, row.names = FALSE)
testF9=read.table(XOR_rules_pred_matrix_path,sep=',',header= T, na.strings="?")

# If we want to run Metarules on all preselected rules before removing similar rules
Rules_info_metrics_all= rbind(RF_PreSelected_Rules$perf_Rules,RF_PreSelected_Rules$perf_Rules_simil)
Rule_pred_DF_All= cbind(RF_PreSelected_Rules$RulesPrediction[,-ncol(RF_PreSelected_Rules$RulesPrediction)],
                        RF_PreSelected_Rules$RulesPrediction_simil[,-ncol(RF_PreSelected_Rules$RulesPrediction_simil)])


XOR_MetaRules=Rules_Inclusion(id_selected_rules = OptRules_metrics_df$id,
                              Rules_info_metrics = Rules_info_metrics_all,
                              Rule_pred_DF = Rule_pred_DF_All,
                              X = Data$XTrain,
                              Inclusion_support = 0.005 ,
                              Inclusion_confidence = 0.95)

# If we want to run Metarules on all preselected rules (after removing similar rules)
XOR_MetaRules=Rules_Inclusion(id_selected_rules = OptRules_metrics_df$id,
                              Rules_info_metrics = Rules_metrics,
                              Rule_pred_DF = Rules_DF_0,
                              X = Data$XTrain,
                              Inclusion_support = 0.005 ,
                              Inclusion_confidence = 0.95)


XOR_MetaRules_ALL=XOR_MetaRules$Perf_ALL_Rules_inclusion
XOR_MetaRules_SR=XOR_MetaRules$Perf_selected_Rules_Inclusion

XOR_MetaRules_ALL_path= ".\\benchmark_results\\rules_results\\XOR_MetaRules_ALL.csv"
write.csv(XOR_MetaRules_ALL,XOR_MetaRules_ALL_path, row.names = FALSE)
testF12=read.table(XOR_MetaRules_ALL_path,sep=',',header= T, na.strings="?")

XOR_MetaRules_SR_path= ".\\benchmark_results\\rules_results\\XOR_MetaRules_SR.csv"
write.csv(XOR_MetaRules_SR,XOR_MetaRules_SR_path, row.names = FALSE)
testF13=read.table(XOR_MetaRules_SR_path,sep=',',header= T, na.strings="?")
