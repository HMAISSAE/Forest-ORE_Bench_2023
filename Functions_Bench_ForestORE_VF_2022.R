library(caTools)
library(ModelMetrics)
library(randomForest)
library(reticulate)
library(rpart)
library(rpart.plot)
library(inTrees)
library(rpart.plot)
library(sqldf)
library(Mercator)
library(ForestDisc)
library(stringr)
library(arulesCBA)
library(RWeka)
library(sbrl)
# sbrl was installed from archive using 
# devtools package ->library(devtools)
# Rtools installed from https://cran.r-project.org/bin/windows/Rtools/
library(rJava)

# ********************************************
# set a seed and split the data set into traning and testing sets
split_dataset=function(XDataset,YDataset,SplitRatio,seed){
    Dataset=cbind(XDataset,YDataset)
    # library(caTools)
    set.seed(seed=seed)
    Variables=data.frame(matrix(nrow=1,ncol=length(Dataset)))
    rownames(Variables)=c('attribut')
    colnames(Variables)=names(Dataset)
    for (i in 1:(length(Dataset)-1))
    {
        Variables[1,i]=paste0("V",i)
    }
    Variables[1,length(Dataset)]="Y"
    for (i in 1:(length(Dataset)))
    {
        names(Dataset)[i]=Variables[1,i]
    }
    split = sample.split(Dataset[,length(Dataset)], SplitRatio = SplitRatio) # Split  preserving relative ratios of different classes
    training_set = subset(Dataset, split == TRUE)
    test_set = subset(Dataset, split == FALSE)
    X=Dataset[,-length(Dataset)]
    Y=Dataset[,length(Dataset)]
    XTrain=training_set[,-length(Dataset)]
    YTrain=training_set[,length(Dataset)]
    XTest=test_set[,-length(Dataset)]
    YTest=test_set[,length(Dataset)]
    
    return(list(X=X,Y=Y,training_set=training_set,test_set=test_set,XTrain=XTrain,YTrain=YTrain,XTest=XTest,YTest=YTest,variables=Variables))
}

# ********************************************
#Selection of trees from RF (Here all trees are extracted. We can add some code to choose 
# some trees by random or depending on some metrics)
# RF2Selectedtrees=function(X,Y,Ntree,nodesize_rate,maxnodes)

RF2Selectedtrees=function(X,Y,Ntree,max_TreeRules = 'default',min_RuleSupport = 'default')
{
    # library(randomForest)
    start_time_RF = Sys.time()
    if (max_TreeRules != 'default' & min_RuleSupport !='default') 
    {
        rf = randomForest(x=X,y=Y, ntree=Ntree,nodesize=ceiling(min_RuleSupport*nrow(X)), keep.forest=TRUE,norm.votes=FALSE,maxnodes=max_TreeRules)
    } else if (max_TreeRules != 'default' & min_RuleSupport =='default') 
    {
        rf = randomForest(x=X,y=Y, ntree=Ntree, keep.forest=TRUE,norm.votes=FALSE,maxnodes=max_TreeRules)
    } else if (max_TreeRules == 'default' & min_RuleSupport !='default')
    {
        rf = randomForest(x=X,y=Y, ntree=Ntree,nodesize=ceiling(min_RuleSupport*nrow(X)), keep.forest=TRUE,norm.votes=FALSE)
    } else {
        rf = randomForest(x=X,y=Y, ntree=Ntree, keep.forest=TRUE,norm.votes=FALSE)
    }
    
    end_time_RF = Sys.time()
    time_taken_RF = difftime(end_time_RF, start_time_RF,units="secs")
    
    ID.trees.total=1:Ntree
    Selectedtrees = NULL
    Selectedtrees$ntree = length(ID.trees.total) 
    Selectedtrees$list = vector("list",Selectedtrees$ntree)
    for(i in 1:Selectedtrees$ntree)
    {
        Selectedtrees$list[[i]] = getTree(rf,k=as.integer(ID.trees.total[i]),labelVar=FALSE)
    }
    Selectedtrees$rf=rf
    Selectedtrees$xlevels=rf$forest$xlevels
    Selectedtrees$classes=rf$classes
    rf.err=as.data.frame(rf$err.rate)
    Selectedtrees$IDSelected= ID.trees.total 
    Selectedtrees$err=rf.err[ID.trees.total,] 
    Selectedtrees$time_taken_RF=time_taken_RF
    return(Selectedtrees)
}

# ********************************************
#function extracting the rules of a set of Selected Trees
Trees2Rulelist=function(tree_list){
    list_var_levels=tree_list$xlevels # list of variables levels
    target_levels=tree_list$classes    # Target Classes
    All_Conds=list()#create list of conditions  per path 
    All_condsdata=data.frame(matrix(ncol = length(list_var_levels)))
    colnames(All_condsdata)=names(list_var_levels)
    All_predClassdata=data.frame(matrix(ncol = 1))
    colnames(All_predClassdata)="TreePred"
    All_PredClass=list()# stock prediction per path 
    for (i in 1:length(tree_list$list)){
        Tree_Conds=TreeConds(as.data.frame(tree_list$list[i]),list_var_levels,target_levels)
        All_Conds= c(All_Conds,Tree_Conds$Conds)
        All_PredClass=c(All_PredClass,Tree_Conds$PredClass)
        All_condsdata=rbind(All_condsdata,Tree_Conds$condsdata)
        All_predClassdata=rbind(All_predClassdata,Tree_Conds$predClassdata)
    }
    All_condsdata=All_condsdata[-1,] #to remove the first ligne of 0 values that was in initial All_condsdata
    rownames(All_condsdata)=1:nrow(All_condsdata)
    All_predClassdata=All_predClassdata[-1,]#to remove the first ligne of 0 values that was in initial All_condsdata
    
    return(list(Conds=All_Conds,condsdata=All_condsdata,PredClass=All_PredClass,predClassdata=All_predClassdata,xlevels=list_var_levels,ylevels=target_levels))
}

# ********************************************
#function finding the previous conditions of a tree node
prevCond=function(tree,i,list_var_levels,target_levels){
    if(i %in% tree$left.daughter){
        id=which(tree$left.daughter==i)
        splitpoint=names(list_var_levels)[tree$split.var[id]]
        ids.splitvalues=which(as.integer(intToBits(as.integer(tree$split.point[id])))>0)
        splitvalue=c(list_var_levels[tree$split.var[id]])[[1]][ids.splitvalues]
        if (length(splitvalue)==1){
            c_splitvalue=paste0("c('",splitvalue,"')")  
        }else{
            c_splitvalue=gsub("\"","'",paste0(list(splitvalue),sep=","))
        }
        cond=paste0("'",splitpoint,"'","%in%",c_splitvalue)
    }
    
    if(i %in% tree$right.daughter){
        id=which(tree$right.daughter==i)
        splitpoint=names(list_var_levels)[tree$split.var[id]]
        ids.splitvalues=which(as.integer(intToBits(as.integer(tree$split.point[id])))>0)
        splitvalue=c(list_var_levels[tree$split.var[id]])[[1]][-ids.splitvalues]
        if (length(splitvalue)==1){
            c_splitvalue=paste0("c('",splitvalue,"')")  
        }else{
            c_splitvalue=gsub("\"","'",paste0(list(splitvalue),sep=","))
        }
        cond=paste0("'",splitpoint,"'","%in%",c_splitvalue)
    }
    return(list(cond=cond,id=id,condvar=splitpoint,list_value=splitvalue,vectorvalue=c_splitvalue))
}


# ********************************************
#function extracting the rules of a single tree 
TreeConds=function(tree,list_var_levels,target_levels){
    tree=as.data.frame(tree)
    for (i in 1:length(list_var_levels))
    {
        for (j in 1:length(list_var_levels[[i]]))
        {
            list_var_levels[[i]][j]=paste0(names(list_var_levels[i]),"_",list_var_levels[[i]][j])
        }
    }
    Conds=list()#create list of conditions  per path 
    condsdata=data.frame(matrix(ncol = length(list_var_levels)))#create data.frame of conditions per path
    colnames(condsdata)=names(list_var_levels)
    predClassdata=data.frame(matrix(ncol = 1))
    colnames(predClassdata)="TreePred"
    PredClass=list()# stock prediction per path
    #start by the terminal nodes and find previous conditions
    id.leafs=which(tree$status==-1)
    j=0 
    #find conditions of each path whose leaf id = "id"
    for(id in id.leafs){
        j=j+1
        listvar=c()# to stock list of variable used in each path
        listconds=vector("list",length(list_var_levels))# to stock list of categories used for each variable per path
        names(listconds)=names(list_var_levels)
        Lconds=list()# to list  all conditions of a path in the form ' V6 ' %in% c('high', 'med')
        vconds=vector("list",length(listconds))#list of  conditions/variable of a path ( used to full data.farame of conditions)
        names(vconds)=names(listconds)
        # start by leaf id until to get the root of the path
        prevConds=prevCond(tree,id,list_var_levels,target_levels)
        listvar=c(listvar,prevConds$condvar)
        listconds[prevConds$condvar]=list(prevConds$list_value)
        #conds[[j]]=prevConds$cond
        if(prevConds$id==1){
            Conds[[j]]=prevConds$cond
            condsdata[j,]=NA
            condsdata[j,prevConds$condvar]=prevConds$vectorvalue
            PredClass[[j]]=target_levels[tree$prediction[id]]
            predClassdata[j,1]=target_levels[tree$prediction[id]]
        }else{
            while(prevConds$id>1){
                prevConds=prevCond(tree,prevConds$id,list_var_levels,target_levels)
                if (prevConds$condvar %in% listvar==FALSE){
                    listconds[prevConds$condvar]=list(prevConds$list_value)
                    listvar=c(listvar,prevConds$condvar)
                }else {
                    listconds[prevConds$condvar][[1]]= intersect(prevConds$list_value,listconds[prevConds$condvar][[1]])
                }
                if(prevConds$id==1){
                    for (i in 1:length(listconds)) {
                        if (is.null(listconds[[i]])==FALSE) {
                            if (length(listconds[[i]])>1){
                                Lconds[[i]]=paste0(gsub("\"","'", paste0("'",names(listconds[i]),"'","%in%",list(listconds[[i]]))))
                                vconds[[i]]=gsub("\"","'",list(listconds[[i]]))
                            }else{
                                Lconds[[i]]=paste0("'",names(listconds[i]),"'","%in%","c('",listconds[[i]],"')")
                                vconds[[i]]=paste0("c('",listconds[[i]],"')")
                            }
                        }
                    }
                    Lconds[sapply(Lconds, is.null)] = NULL #to remove empty elements from Lconds
                    Conds[[j]]=gsub("\"","'",paste0(Lconds,collapse = ' & '))
                    for (i in 1:length(vconds)){
                        if (is.null(vconds[[i]])==FALSE) {
                            condsdata[j,i]=vconds[[i]]
                        }
                    } 
                    PredClass[[j]]=target_levels[tree$prediction[id]]
                    predClassdata[j,1]=target_levels[tree$prediction[id]]
                    break()
                }
            }
        }
    }
    return(list(Conds=Conds,condsdata=condsdata,PredClass=PredClass,predClassdata=predClassdata))
}

# ********************************************
# function selecting Rules from Rf rules respecting tresholds related to support/class and confidence
RulesToPreSelectRules=function (X, Y,RF_Selected_Rules,min_rule_class_support=0.025, min_rule_confidence=0.51, min_rule_var=1, max_rule_var=10,similarity_treshold=0.95) 
{
    dataconds=RF_Selected_Rules$condsdata
    
    # Clean dataconds, conds and predictions
    for (j in 1: ncol(dataconds))
    {
        dataconds[,j]  = gsub(paste0(colnames(dataconds)[j],"_"),"",dataconds[,j])
        dataconds[,j]  = gsub("),",")",dataconds[,j])
    }
    
    conds=RF_Selected_Rules$Conds
    
    for (j in 1: ncol(dataconds))
    {
        conds  = gsub(paste0(colnames(dataconds)[j],"_"),"",conds)
        conds  = gsub(paste0("'",colnames(dataconds)[j],"'%in%"),paste0("X[,",j,"] %in% "),conds)
    }
    conds = gsub("),",")",conds)
    conds = gsub(", '",",'",conds)
    
    predictions = RF_Selected_Rules$PredClass
    dataconds$Ypred = paste0("'",c(predictions),"'")
    
    # Select unique rules
    dataconds = unique(dataconds)
    id_unique = as.numeric(rownames(dataconds))
    
    predictions = predictions[id_unique]
    conds = conds[id_unique]
   
    
    # Compute the number of variables used per rule
    
    datavar = dataconds[,-ncol(dataconds)]
    dataconds[,"var_used"] = NA
    dataconds[,"var_length"] = 0
    for (i in 1:nrow(dataconds)) 
    {
        var_used= colnames(datavar)[which(!is.na(datavar[i,]))] 
        dataconds[i,"var_used"] = paste0(var_used,collapse=",")
        dataconds[i, "var_length"] = length(var_used) 
    }

    
    # Select rules respecting min_rule_var and max_rule_var
    ids_var_length_ok = which(dataconds$var_length >= min_rule_var & dataconds$var_length<= max_rule_var)
    
    # Update data
    conds = conds[ids_var_length_ok]
    dataconds= dataconds[ids_var_length_ok,]
    predictions= predictions[ids_var_length_ok]
    
    
    
    # Compute the number of levels used per rule
    
    levels_length=dataconds[,!names(dataconds) %in% c("Ypred", "var_used","var_length")]
    for (j in 1: ncol(levels_length))
    {
        for (i in 1: nrow(levels_length))
        {
            levels_length[i,j]  = ifelse(is.na(levels_length[i,j]),0,length(eval(parse(text=levels_length[i,j]))))
        }
    }
    
    levels_length = as.data.frame(sapply(levels_length, as.numeric))
    levels_length$length=rowSums(levels_length)
    
    # add levels_length and conds to dataconds
    dataconds$level_length = levels_length$length
    dataconds$cond = conds
    
    # compute prediction metrics
   
    id.Rules=1:nrow(dataconds)
    
    # Prediction of the PreSelected Rules
    P.PS.R=data.frame(matrix(NA,nrow=nrow(X),ncol=length(id.Rules)+1))
    
    names(P.PS.R)=c(paste0("R",id.Rules),"Y")
    
    P.PS.R[,"Y"]=as.character(Y)
    
    j=1
    for (k in id.Rules)
    {
        P.PS.R[,j]=ifelse(eval(parse(text=dataconds[k,"cond"]))=="TRUE",gsub("'","",dataconds[k,"Ypred"]),NA)
        j=j+1
    }
    
    perf_Rules=data.frame(matrix(NA,ncol=11,nrow=length(id.Rules)),row.names =paste0("R",id.Rules))
    names(perf_Rules)=c("id","confidence","support", "class_suppport",
                        "var_length", "levels_length","var_nbr_scaled","levels_nbr_scaled",
                        "cond", "Ypred", "var_used")
   
    perf_Rules[,"Ypred"] = dataconds[,"Ypred"]
    for (k in (id.Rules))
    {
        perf_Rules[k,"id"] = k
        ids_covered = which(!P.PS.R[,k]=="NA")
        coverage_count =length(ids_covered)
        perf_Rules[k,"support"] =coverage_count/nrow(X)
        perf_Rules[k,"confidence"] =length (which(P.PS.R[ids_covered,k] == P.PS.R[ids_covered,"Y"]))/ coverage_count
    }
    
    
    # Compute class support
    perf_Rules$class_suppport = perf_Rules$support
    target_class_dist= table(Y)/length(Y)
    
    for (i in 1: dim(target_class_dist))
    {
        id_rows = which(perf_Rules[,"Ypred"] == paste0("'",names(target_class_dist[i]),"'"))
        perf_Rules[id_rows,"class_suppport"] = (perf_Rules[id_rows,"class_suppport"])/target_class_dist[i]
    }
    # NB: the class support can have a value greater than 1 in the case where the rule has a low convidence and a high cover
    
    # Select rules respecting prediction metrics tresholds: min_rule_class_support and min_rule_confidence
    perf_Rules=perf_Rules[which(perf_Rules[,"confidence"] > min_rule_confidence & perf_Rules[,"class_suppport"] >= min_rule_class_support),]
    
    ids_pred_ok= perf_Rules$id
    
    # Update data
    conds = conds[ids_pred_ok]
    dataconds= dataconds[ids_pred_ok,]
    predictions= predictions[ids_pred_ok]
    levels_length = levels_length[ids_pred_ok,]
    P.PS.R = P.PS.R[,c(ids_pred_ok,ncol(P.PS.R))]
    
    
    # complete filling perf_Rules columns
    
    perf_Rules[,"levels_length"] = levels_length[,"length"]
    perf_Rules[,"var_length"] = dataconds[,"var_length"]
    perf_Rules[,"var_used"] = dataconds[,"var_used"] 
    perf_Rules[,"cond"] = dataconds[,"cond"]
    
    
    # compute scaled values of levels_length and levels_length (will be used in the optimization step)
    perf_Rules["var_nbr_scaled"]=perf_Rules$var_length/max(perf_Rules$var_length)
    perf_Rules["levels_nbr_scaled"]=perf_Rules$levels_length /max(perf_Rules$levels_length)
    
    # reinitialise rules ids and names
    perf_Rules$id=1:nrow(perf_Rules)
    rownames(perf_Rules) = paste0("R",1:nrow(perf_Rules))
    names(P.PS.R)=c(paste0("R",1:nrow(perf_Rules)),"Y")
    
    
    # remove rules not respecting similarity tresholds:
    # -Compute matrice of rules pairwise jaccard distance
    # -Extract rules that have a similarity index greater that similarity_treshold
    # -Given similar rules, choose the rule with higher confidence, support , var_length ,levels_length. 
    # The remaing rules are those to remove.
    Rules_cover = P.PS.R[,-ncol(P.PS.R)]
    
    for (i in 1:nrow(Rules_cover))
    {
        Rules_cover[i,!is.na(Rules_cover[i,])]= 1
    }
    for (i in 1:nrow(Rules_cover))
    {
        Rules_cover[i,is.na(Rules_cover[i,])]= 0
    }
    Rules_cover=data.frame(lapply(Rules_cover,as.numeric))
    ids_to_remove=remove_similar_rules( Rules_cover=Rules_cover,
                                                  Rules_metrics=perf_Rules,
                                                  similarity_treshold=similarity_treshold)
    
    
    # Update data
    ids_to_retain=setdiff(1:nrow(perf_Rules),ids_to_remove)
    conds = conds[ids_to_retain]
    dataconds= dataconds[ids_to_retain,]
    predictions= predictions[ids_to_retain]
    levels_length = levels_length[ids_to_retain,]
    P.PS.R_0 = P.PS.R
    perf_Rules_0 = perf_Rules
    P.PS.R = P.PS.R[,c(ids_to_retain,ncol(P.PS.R))]
    perf_Rules = perf_Rules[ids_to_retain,]
    # save removed rules
    P.PS.R_similremoved = data.frame()
    perf_Rules_similremoved = data.frame()
    if (length(ids_to_remove) > 0) 
    {
        P.PS.R_similremoved = P.PS.R_0[,c(ids_to_remove,ncol(P.PS.R_0))]
        perf_Rules_similremoved = perf_Rules_0[ids_to_remove,]
    }
    
    # reinitialise rules ids and names
    perf_Rules$id=1:nrow(perf_Rules)
    rownames(perf_Rules) = paste0("R",1:nrow(perf_Rules))
    names(P.PS.R)=c(paste0("R",1:nrow(perf_Rules)),"Y")
    if (length(ids_to_remove) > 0)
    {
        new_ids=(nrow(perf_Rules) + 1):(nrow(perf_Rules) + nrow(perf_Rules_similremoved))
        perf_Rules_similremoved$id=new_ids
        rownames(perf_Rules_similremoved) = paste0("R",new_ids)
        names(P.PS.R_similremoved)=c(paste0("R",new_ids),"Y")
    }
   
   
    return(list(Rules_selection=dataconds, 
                perf_Rules = perf_Rules, RulesPrediction=P.PS.R, 
                perf_Rules_simil = perf_Rules_similremoved, RulesPrediction_simil=P.PS.R_similremoved, 
                Conds=conds,PredClass=predictions))
}



# ********************************************
# Rules prediction dataframe
# "Rules_pred_perf" :Rules prediction performance
# "Rules_pred_ok": data frame (n rows and m columns) of the preselected rules correct coverage. It is a binary 
# data frame where Rules_pred_ok[i,j] = 1 if rule j covers row i and predict it correctly, 0 otherwise.
# "Rules_pred_nok": data frame (n rows and m columns) of the preselected rules incorrect coverage. 
# It is a binary data frame where Rules_pred_nok[i,j] = 1 if rule j covers row i and does not predict it correctly,
# 0 otherwise.

Rules_pred = function(Rules_pred_DF)
{
    Rules_pred_ok=Rules_pred_DF[,-1]
    colnames(Rules_pred_ok)=c(1:ncol(Rules_pred_ok))
    Rules_pred_nok=Rules_pred_DF[,-1]
    colnames(Rules_pred_nok)=c(1:ncol(Rules_pred_nok))
    Rules_pred_all=Rules_pred_DF[,-1]
    colnames(Rules_pred_all)=c(1:ncol(Rules_pred_all))
    for (i in 1:nrow(Rules_DF)) 
    {
        for (j in 1:(ncol(Rules_DF)-1)) 
        {
            if (!is.na(Rules_DF[i,j]) )
            {
                Rules_pred_all[i,j]= ifelse (Rules_DF[i,j]==Rules_DF[i,"Y"],1,-1)
                Rules_pred_ok[i,j]= ifelse (Rules_DF[i,j]==Rules_DF[i,"Y"],1,0)
                Rules_pred_nok[i,j]= ifelse (Rules_DF[i,j]==Rules_DF[i,"Y"],0,1)
            }else
            {
                Rules_pred_all[i,j]= 0
                Rules_pred_ok[i,j]= 0
                Rules_pred_nok[i,j]= 0     
            }
                
        }
    }
   
    Ypredicted=apply(Rules_DF[,-ncol(Rules_DF)],1,function(x) names(which.max(table(x))))
    
    Rules_pred_perf=Perf_indicators(Y_pred=Rules_DF[,ncol(Rules_DF)],Y_predicted=Ypredicted)
    
    
    return(list(Rules_pred_all=Rules_pred_all,Rules_pred_ok=Rules_pred_ok,Rules_pred_nok=Rules_pred_nok, Prediction_Perf=Rules_pred_perf$Global_perf))
}

# ********************************************
Rules_BinaryPred = function(Rules_pred_DF)
{
    columns_size = ncol(Rules_pred_DF)-1
    rows_size= nrow(Rules_pred_DF)
    columns_names = c(1:columns_size)
    Rules_pred_ok=data.frame(matrix(0,ncol=columns_size,nrow=rows_size))
    Rules_pred_nok=Rules_pred_ok
    # Rules_pred_all=Rules_pred_ok
   
    for (i in 1:rows_size) 
    {
        for (j in 1:columns_size) 
        {
            if (!is.na(Rules_DF[i,j]) )
            {
               if(Rules_DF[i,j] == Rules_DF[i,"Y"]) 
               {
                   Rules_pred_ok[i,j]= 1
               }else 
               {
                   Rules_pred_nok[i,j]= 1  
               }
            }
        }
    }
    
    Ypredicted=apply(Rules_DF[,-ncol(Rules_DF)],1,function(x) names(which.max(table(x))))
    
    Rules_pred_perf=Perf_indicators(Y_pred=Rules_DF[,ncol(Rules_DF)],Y_predicted=Ypredicted)
    
    # return(list(Rules_pred_all=Rules_pred_all,Rules_pred_ok=Rules_pred_ok,Rules_pred_nok=Rules_pred_nok, Prediction_Perf=Rules_pred_perf$Global_perf))
    return(list(Rules_pred_ok=Rules_pred_ok,Rules_pred_nok=Rules_pred_nok, Prediction_Perf=Rules_pred_perf$Global_perf))
}


#Function computing perf indicators

Perf_indicators=function(Y_pred,Y_predicted){
    # library(ModelMetrics) #kappa
    if ((length(Y_pred)<1) | (length(Y_pred) != length(Y_predicted)))
    {
        error=NA
        cm=NA
        Global_perf=NA
        Class_perf=NA
    }else 
    {
        cm1=as.matrix(table(Y_pred,Y_predicted))
        
        # cm=matrix(0,nrow=nrow(cm1),ncol=nrow(cm1)) # necessary step to avoid errors when cm1 isn't a n*n matrix
        # rownames(cm)=rownames(cm1)
        # colnames(cm)=rownames(cm1)
        # 
        # for (j in 1:length(colnames(cm1)))
        # {
        #     cm[,colnames(cm1)[j]]=cm1[,colnames(cm1)[j]]
        # }
        
        
        nn_names=unique(c(rownames(cm1),colnames(cm1)))
        cm=matrix(0,nrow=length(nn_names),length(nn_names)) # necessary step to avoid errors when cm1 isn't a n*n matrix
        rownames(cm)=nn_names
        colnames(cm)=nn_names
        
        for (i in rownames(cm1))
        {
            for (j in colnames(cm1)) 
            {
                cm[i,j]=cm1[i,j]
            } 
        }
        
        n = sum(cm) # number of instances
        nc = nrow(cm) # number of classes
        diag = diag(cm) # number of correctly classified instances per class 
        rowsums = apply(cm, 1, sum) # number of instances per class
        colsums = apply(cm, 2, sum) # number of predictions per class
        p = rowsums / n # distribution of instances over the actual classes
        q = colsums / n # distribution of instances over the predicted classes
        accuracy = sum(diag) / n #accuracy
        error=1-accuracy
        # Precision: fraction of correct predictions for a certain class
        # recall:fraction of instances of a class that were correctly predicted. 
        # F-1 score: harmonic mean (or a weighted average) of precision and recall.
        precision = diag / colsums
        # precision[which(precision==c("NaN"))]=0
        recall = diag / rowsums 
        # recall[which(recall==c("NaN"))]=0
        f1 = 2 * precision * recall / (precision + recall) 
        # f1[which(f1==c("NaN"))]=0
       
        Class_perf=data.frame(precision, recall, f1) 
        
        # The per-class metrics can be averaged over all the classes resulting 
        # in macro-averaged precision, recall and F-1.
        macroPrecision = mean(precision, na.rm=TRUE)
        macroRecall = mean(recall, na.rm=TRUE) # macro recall is also known as balanced accuracy
        macroF1 = mean(f1, na.rm=TRUE)
        # kappa
        if (dim(cm)[1]==1) 
        {
            Kappa_metrics=1
            kappa = 1
            kappa_w = 1
        } else 
        {
            Kappa_metrics=vcd::Kappa(cm)
            kappa = Kappa_metrics$Unweighted[1]
            kappa_w = Kappa_metrics$Weighted[1]    
        }
        
        # k = (po – pe) / (1 – pe), where po:= Relative observed agreement and pe: Hypothetical probability of chance agreement.
        # In the case where the CM includes only one class, vcd::Kappa(cm) return NaN value.  
        # In this case, p0= 1 and pe=1∗1=1.Thus, we end up with 0/0=NaN.
        # In fact we have perfect agreement. In this case we replace NaN by 1.
        
        Global_perf=data.frame(accuracy,macroPrecision, macroRecall, macroF1,kappa,kappa_w)   
    }
    
    
    
    return(list(error=error,CM=cm,Global_perf=Global_perf,Class_perf=Class_perf))
}
#Function computing fidelity metrics
# How well does the explanation approximate the prediction of the black box model?
#  for each observation in the train/test set, we compute the prediction of the original(base) model and the prediction 
# of the (surrogate) explanation model and report the percentage of agreement.
#  we do that by considering 3 cases:
# "all_pred":all the observations are considered
# "base_correct_pred": only the observations that were correctly predicted by the original/base model are considered
# "base_incorrect_pred": only the observations that were not correctly predicted by the original/base model are considered
Fidelity_indicators=function(Y_base,Y_surrogate, Y_real){
    # homogenize the three vectors
    names(Y_base)=NULL
    names(Y_surrogate)=NULL
    names(Y_real)=NULL
    Y_surrogate = as.factor(Y_surrogate)
    # we will generate scores in 3 cases: We consider 1- all the instances,  2-instances that were correctly predicted by the origial model, 
    # 3- the instances that were not correctly predicted by the original model
    covered_items = c("all_pred", "base_correct_pred", "base_incorrect_pred")
    fidelity_perf=data.frame(matrix(NA,nrow=3,ncol=5))
    fidelity_class_perf= data.frame()
    names(fidelity_perf) = c("scenario","fidelity_score","fidelity_macroPrecision", "fidelity_macroRecall", "fidelity_macroF1")
    
    # verify that the three vectors are not null and that they have same length
    if ((length(Y_base)<1) | (length(Y_base) != length(Y_surrogate)))
    {
        error=NA
        cm=NA
        fidelity_class_perf=NA
        fidelity_perf[,1]=covered_items
        fidelity_scores= data.frame(matrix(NA,nrow=1,ncol=12))
        colnames(fidelity_scores)=c("fidelity", "fidelity_macroPrecision","fidelity_macroRecall","fidelity_macroF1",
                                    "fidelity_correct", "fidelity_macroPrecision_correct","fidelity_macroRecall_correct","fidelity_macroF1_correct",
                                    "fidelity_incorrect", "fidelity_macroPrecision_incorrect","fidelity_macroRecall_incorrect","fidelity_macroF1_incorrect")
        fidelity_scores[1,1:4]= fidelity_perf[1,2:5]
        fidelity_scores[1,5:8]=fidelity_perf[2,2:5]
        fidelity_scores[1,9:12]= fidelity_perf[3,2:5]
       
    }else if (length(Y_base) != length(Y_surrogate) | length(Y_base) != length(Y_real))
    {
        stop("Compared vectors have different lenghts") 
    }else
    {
        ids_Y_base_all = 1:length(Y_base)  
        ids_Y_base_correct = which(Y_base == Y_real)
        ids_Y_base_incorrect = which(Y_base != Y_real)
        # # we will generate scores in 3 cases: We consider 1- all the instances,  2-instances that were correctly predicted by the origial model, 
        # # 3- the instances that were not correctly predicted by the original model
        # covered_items = c("all_pred", "base_correct_pred", "base_incorrect_pred")
        # 
        # fidelity_perf=data.frame(matrix(NA,nrow=3,ncol=5))
        # fidelity_class_perf= data.frame()
        # names(fidelity_perf) = c("scenario","fidelity_score","fidelity_macroPrecision", "fidelity_macroRecall", "fidelity_macroF1")
        i = 0
        for (ids in list(ids_Y_base_all,ids_Y_base_correct, ids_Y_base_incorrect))
        { 
            if (length(ids) > 0) 
            {
                Y_base_0 = Y_base[ids]
                Y_surrogate_0 = Y_surrogate[ids]
                i = i+1
                fidelity_perf[i,"scenario"] = covered_items[i]
                
                cm0=as.matrix(table(Y_base_0,Y_surrogate_0))
                nn_names=unique(c(rownames(cm0),colnames(cm0)))
                cm=matrix(0,nrow=length(nn_names),length(nn_names)) # necessary step to avoid errors when cm0 isn't a n*n matrix
                rownames(cm)=nn_names
                colnames(cm)=nn_names
                
                for (r in rownames(cm0))
                {
                    for (c in colnames(cm0)) 
                    {
                        cm[r,c]=cm0[r,c]
                    } 
                }
                
                n = sum(cm) # number of instances
                nc = nrow(cm) # number of classes
                diag = diag(cm) # number of correctly matched instances per class (between the base model and the surrogate model) 
                rowsums = apply(cm, 1, sum) # number of instances per class
                colsums = apply(cm, 2, sum) # number of predictions per class
                p = rowsums / n # distribution of instances over the actual classes
                q = colsums / n # distribution of instances over the predicted classes
                fidelity = sum(diag) / n #correct matching
                error_match=1-fidelity
                # Precision: fraction of correct matching for a certain class
                # recall:fraction of instances of a class that were correctly matched. 
                # F-1 score: harmonic mean (or a weighted average) of precision and recall.
                fidelity_precision = diag / colsums
                fidelity_recall = diag / rowsums 
                fidelity_f1 = 2 * fidelity_precision * fidelity_recall / (fidelity_precision + fidelity_recall) 
                # f1[which(f1==c("NaN"))]=0
                classes = names(fidelity_precision)
                class_perf=data.frame(classes, fidelity_precision, fidelity_recall, fidelity_f1)
                class_perf$scenerio=covered_items[i]
                
                fidelity_class_perf= rbind(fidelity_class_perf,class_perf)
                
                
                # The per-class metrics can be averaged over all the classes resulting 
                # in macro-averaged precision, recall and F-1.
                fidelity_macroPrecision = mean(fidelity_precision, na.rm=TRUE)
                fidelity_macroRecall = mean(fidelity_recall, na.rm=TRUE) # macro recall is also known as balanced accuracy
                fidelity_macroF1 = mean(fidelity_f1, na.rm=TRUE)
                
                fidelity_perf[i,c("fidelity_score","fidelity_macroPrecision", "fidelity_macroRecall", "fidelity_macroF1")] = c(fidelity,fidelity_macroPrecision,fidelity_macroRecall,fidelity_macroF1)
            }else 
            {
                i=i+1
                fidelity_perf[i,"scenario"] = covered_items[i]
                classes= NA
                fidelity_precision= NA
                fidelity_recall= NA
                fidelity_f1= NA
                class_perf=data.frame(classes, fidelity_precision, fidelity_recall, fidelity_f1)
                
                class_perf$scenerio=covered_items[i]
                fidelity_class_perf= rbind(fidelity_class_perf,class_perf)
            } 
            
        }
        fidelity_class_perf=fidelity_class_perf[,c(5,1,2,3,4)]
        
        fidelity_scores= data.frame(matrix(NA,nrow=1,ncol=12))
        colnames(fidelity_scores)=c("fidelity", "fidelity_macroPrecision","fidelity_macroRecall","fidelity_macroF1",
                                       "fidelity_correct", "fidelity_macroPrecision_correct","fidelity_macroRecall_correct","fidelity_macroF1_correct",
                                       "fidelity_incorrect", "fidelity_macroPrecision_incorrect","fidelity_macroRecall_incorrect","fidelity_macroF1_incorrect")
        fidelity_scores[1,1:4]= fidelity_perf[1,2:5]
        fidelity_scores[1,5:8]=fidelity_perf[2,2:5]
        fidelity_scores[1,9:12]= fidelity_perf[3,2:5]
        
    }
    
    return(list(fidelity_class_perf= fidelity_class_perf, fidelity_perf=fidelity_perf, fidelity_scores=fidelity_scores))
}


Compute_fidelity_scores=function(Y_base,Y_surrogate, Y_real, covered_ids,predict_set,learner_name,iter){
    if (length(covered_ids)>0) 
    {
        scores_Fidelity_all= Fidelity_indicators(Y_base=Y_base,
                                                 Y_surrogate=Y_surrogate, 
                                                 Y_real=Y_real)
        scores_Fidelity_covered= Fidelity_indicators(Y_base=Y_base[covered_ids],
                                                     Y_surrogate=Y_surrogate[covered_ids],
                                                     Y_real=Y_real[covered_ids])
        scores_Fidelity_notcovered= Fidelity_indicators(Y_base=Y_base[-covered_ids],
                                                        Y_surrogate=Y_surrogate[-covered_ids], 
                                                        Y_real=Y_real[-covered_ids])
    }else 
    {
        scores_Fidelity_all= Fidelity_indicators(Y_base=Y_base,
                                                 Y_surrogate=Y_surrogate, 
                                                 Y_real=Y_real)
        scores_Fidelity_covered= Fidelity_indicators(Y_base=Y_base[covered_ids],
                                                     Y_surrogate=Y_surrogate[covered_ids],
                                                     Y_real=Y_real[covered_ids])
        scores_Fidelity_notcovered= Fidelity_indicators(Y_base=Y_base,
                                                        Y_surrogate=Y_surrogate, 
                                                        Y_real=Y_real)     
    }
    
    Fidelity_scores=rbind(scores_Fidelity_all$fidelity_scores,
                          scores_Fidelity_covered$fidelity_scores,
                          scores_Fidelity_notcovered$fidelity_scores)
    
   
    
    Fidelity_scores$data = NA
    Fidelity_scores$iter = iter
    Fidelity_scores$pred_set= predict_set
    Fidelity_scores$pred_sub_set= c("all", "covered", "notcovered")
    Fidelity_scores$method = learner_name
    Fidelity_scores=Fidelity_scores[,c(13:17,1:12)]
    return(Fidelity_scores)
}

# ********************************************
# function computing the rules prediction 

PREDICT_selected_rules=function(X,Y,Rules_metrics,Rules_ids,Y_elserule)
{
    # dfosr=df_rules[ids_Rules,]
    
    df_rules=Rules_metrics
    vote=levels(Y)
    Y_else=Y_elserule
    ids_Rules=Rules_ids
    conditions=df_rules[ids_Rules,"cond"]
    predictions=df_rules[ids_Rules,"Ypred"]
    # Prediction of Selected Rules via optimization
    P.S.R=data.frame(matrix(NA,nrow=nrow(X),ncol=length(ids_Rules)+4+length(levels(Y))))
    id_col_votes= (ncol(P.S.R)-length(vote)+1):ncol(P.S.R)
    names(P.S.R)=c(conditions,"Y","Ypred","X_Supported","Xcount_cond",c(vote))
    # for (j in ((ncol(P.S.R)-length(levels(Y))+1):ncol(P.S.R)))
    # {
    #   P.S.R[,j]=0
    # }
    
    P.S.R[1:nrow(P.S.R),"Y"]=as.character(Y)
    j=1
    # testr=ifelse(eval(parse(text=conditions[2]))=="TRUE",gsub("'","",predictions[2]),NA)
    for (k in (ids_Rules))
    {
        P.S.R[,j]=ifelse(eval(parse(text=conditions[j]))=="TRUE",gsub("'","",predictions[j]),NA)
        j=j+1
    }
    
    for (i in 1:nrow(X))
    {
        P.S.R[i,"Xcount_cond"]= length(which(!is.na(P.S.R[i,1:length(ids_Rules)])))
        P.S.R[i,"X_Supported"]= ifelse(as.integer(P.S.R[i,"Xcount_cond"])>0,1,0)
        if(P.S.R[i,"X_Supported"] == 1)
        {
            for (v in 1:length(vote))
            {
                P.S.R[i,vote[v]]= length(which(P.S.R[i,1:length(ids_Rules)]==vote[v]))
            }
            
            P.S.R[i,"Ypred"]=names(which.max(P.S.R[i,id_col_votes]))
        }
    }
    prop_Xis_covered=sum(P.S.R$X_Supported)/nrow(X)
    Xis_covered=which(P.S.R[,"X_Supported"] == 1)
    P.S.R_covered= P.S.R[which(P.S.R[,"X_Supported"] == 1),]
    cover_Pred_Perf_indicators=Perf_indicators(Y_pred=P.S.R_covered$Y,Y_predicted=P.S.R_covered$Ypred)
    
   
    if (prop_Xis_covered<1)
    {
        P.S.R_not_covered =  P.S.R[which(is.na(P.S.R[,"Ypred"])),]
        P.S.R[which(is.na(P.S.R[,"Ypred"])),"Ypred"]= gsub("'","",Y_else)
        P.S.R_not_covered[,"Ypred"] = gsub("'","",Y_else)
        notcover_Pred_Perf_indicators=Perf_indicators(Y_pred=P.S.R_not_covered$Y,
                                                      Y_predicted=P.S.R_not_covered$Ypred)
    }else
    {
        notcover_Pred_Perf_indicators= list(error=NA,CM=NA,Global_perf=NA,Class_perf=NA)      
    }
        
    global_Pred_Perf_indicators=Perf_indicators(Y_pred=P.S.R$Y,Y_predicted=P.S.R$Ypred)
    return(list(predictions=P.S.R,
                cover_pred_perf_indicators=cover_Pred_Perf_indicators,
                notcover_pred_perf_indicators=notcover_Pred_Perf_indicators,
                prop_Xis_covered=prop_Xis_covered,
                Xis_covered=Xis_covered,
                global_pred_perf_indicators=global_Pred_Perf_indicators))
}
PREDICT_selected_rules0=function(X,Y,Rules_metrics_all,opt_Rules_ids,Y_elserule)
{
    # dfosr=df_rules[ids_Rules,]
    
    df_rules=Rules_metrics_all
    vote=levels(Y)
    Y_else=Y_elserule
    ids_Rules=opt_Rules_ids
    conditions=df_rules[ids_Rules,"cond"]
    predictions=df_rules[ids_Rules,"Ypred"]
    # Prediction of Selected Rules via optimization
    P.S.R=data.frame(matrix(NA,nrow=nrow(X),ncol=length(ids_Rules)+4+length(levels(Y))))
    id_col_votes= (ncol(P.S.R)-length(vote)+1):ncol(P.S.R)
    names(P.S.R)=c(conditions,"Y","Ypred","X_Supported","Xcount_cond",c(vote))
    # for (j in ((ncol(P.S.R)-length(levels(Y))+1):ncol(P.S.R)))
    # {
    #   P.S.R[,j]=0
    # }
    
    P.S.R[1:nrow(P.S.R),"Y"]=as.character(Y)
    j=1
    # testr=ifelse(eval(parse(text=conditions[2]))=="TRUE",gsub("'","",predictions[2]),NA)
    for (k in (ids_Rules))
    {
        P.S.R[,j]=ifelse(eval(parse(text=conditions[j]))=="TRUE",gsub("'","",predictions[j]),NA)
        j=j+1
    }
    
    for (i in 1:nrow(X))
    {
        P.S.R[i,"Xcount_cond"]= length(which(!is.na(P.S.R[i,1:length(ids_Rules)])))
        P.S.R[i,"X_Supported"]= ifelse(as.integer(P.S.R[i,"Xcount_cond"])>0,1,0)
        if(P.S.R[i,"X_Supported"] == 1)
        {
            for (v in 1:length(vote))
            {
                P.S.R[i,vote[v]]= length(which(P.S.R[i,1:length(ids_Rules)]==vote[v]))
            }
            
            P.S.R[i,"Ypred"]=names(which.max(P.S.R[i,id_col_votes]))
        }
    }
    prop_Xis_covered=sum(P.S.R$X_Supported)/nrow(X)
    Xis_covered=which(P.S.R[,"X_Supported"] == 1)
    P.S.R_covered= P.S.R[which(P.S.R[,"X_Supported"] == 1),]
    cover_Pred_Perf_indicators=Perf_indicators(Y_pred=P.S.R_covered$Y,Y_predicted=P.S.R_covered$Ypred)
    
    
    if (prop_Xis_covered<1)
    {
        P.S.R_not_covered =  P.S.R[which(is.na(P.S.R[,"Ypred"])),]
        P.S.R[which(is.na(P.S.R[,"Ypred"])),"Ypred"]= gsub("'","",Y_else)
        P.S.R_not_covered[,"Ypred"] = gsub("'","",Y_else)
        notcover_Pred_Perf_indicators=Perf_indicators(Y_pred=P.S.R_not_covered$Y,
                                                      Y_predicted=P.S.R_not_covered$Ypred)
    }else
    {
        notcover_Pred_Perf_indicators= list(error=NA,CM=NA,Global_perf=NA,Class_perf=NA)      
    }
    
    global_Pred_Perf_indicators=Perf_indicators(Y_pred=P.S.R$Y,Y_predicted=P.S.R$Ypred)
    return(list(predictions=P.S.R,
                cover_pred_perf_indicators=cover_Pred_Perf_indicators,
                notcover_pred_perf_indicators=notcover_Pred_Perf_indicators,
                prop_Xis_covered=prop_Xis_covered,
                Xis_covered=Xis_covered,
                global_pred_perf_indicators=global_Pred_Perf_indicators))
}


PREDICT_intrees=function(X,Y,conditions,predictions,ids_Rules,elserule)
{
    id.Rules=ids_Rules
    vote=levels(Y)
    Y_else=elserule
    
    # prediction of Intrees rules 
    P.I.R=data.frame(matrix(NA,nrow=nrow(X),ncol=length(id.Rules)+4+length(levels(Y))))
    id_col_votes= (ncol(P.I.R)-length(vote)+1):ncol(P.I.R)
    names(P.I.R)=c(conditions,"Y","Ypred","X_Supported","Xcount_cond",c(vote))
    # for (j in ((ncol(P.I.R)-length(levels(Y))+1):ncol(P.I.R)))
    # {
    #   P.I.R[,j]=0
    # }
    
    P.I.R[1:nrow(P.I.R),"Y"]=as.character(Y)
    j=1
    for (k in (id.Rules))
    {
        P.I.R[,j]=ifelse(eval(parse(text=conditions[k]))=="TRUE",gsub("'","",predictions[k]),NA)
        j=j+1
    }
    
    for (i in 1:nrow(X))
    {
        P.I.R[i,"Xcount_cond"]= length(which(!is.na(P.I.R[i,1:length(id.Rules)])))
        P.I.R[i,"X_Supported"]= ifelse(as.integer(P.I.R[i,"Xcount_cond"])>0,1,0)
        if(P.I.R[i,"X_Supported"] == 1)
        {
            for (v in 1:length(vote))
            {
                P.I.R[i,vote[v]]= length(which(P.I.R[i,1:length(id.Rules)]==vote[v]))
            }
            
            P.I.R[i,"Ypred"]=names(which.max(P.I.R[i,id_col_votes]))
        }
    }
    prop_Xis_covered=sum(P.I.R$X_Supported)/nrow(X)
    Xis_covered=which(P.I.R[,"X_Supported"] == 1)
    P.I.R_covered= P.I.R[which(P.I.R[,"X_Supported"] == 1),]
    cover_Pred_Perf_indicators=Perf_indicators(Y_pred=P.I.R_covered$Y,Y_predicted=P.I.R_covered$Ypred)
    
    if (prop_Xis_covered<1)
    {
        P.I.R_not_covered =  P.I.R[which(is.na(P.I.R[,"Ypred"])),]
        P.I.R[which(is.na(P.I.R[,"Ypred"])),"Ypred"]= gsub("'","",Y_else)
        P.I.R_not_covered[,"Ypred"] = gsub("'","",Y_else)
        notcover_Pred_Perf_indicators=Perf_indicators(Y_pred=P.I.R_not_covered$Y,
                                                      Y_predicted=P.I.R_not_covered$Ypred)
    }else
    {
        notcover_Pred_Perf_indicators= list(error=NA,CM=NA,Global_perf=NA,Class_perf=NA)      
    }
    
    global_Pred_Perf_indicators=Perf_indicators(Y_pred=P.I.R$Y,Y_predicted=P.I.R$Ypred)
    return(list(predictions=P.I.R,
                cover_pred_perf_indicators=cover_Pred_Perf_indicators,
                notcover_pred_perf_indicators=notcover_Pred_Perf_indicators,
                prop_Xis_covered=prop_Xis_covered,
                Xis_covered=Xis_covered,
                global_pred_perf_indicators=global_Pred_Perf_indicators))
}

get_elserows=function(X,Y,conditions,predictions)
{
    if (length(conditions) == 0) 
    {
        Y_elserule = NA
        not_covered_rows = NA
         
    }else 
    {
        id.Rules=1:length(conditions)
        
        # prediction of Intrees rules 
        P.I.R=data.frame(matrix(NA,nrow=nrow(X),ncol=length(id.Rules)+4))
        names(P.I.R)=c(conditions,"Y","Ypred","X_Supported","Xcount_cond")
        
        P.I.R[1:nrow(P.I.R),"Y"]=as.character(Y)
        
        for (k in (id.Rules))
        {
            P.I.R[,k]=ifelse(eval(parse(text=conditions[k]))=="TRUE",gsub("'","",predictions[k]),NA)
            
        }
        
        for (i in 1:nrow(X))
        {
            P.I.R[i,"Xcount_cond"]= length(which(!is.na(P.I.R[i,1:length(id.Rules)])))
            P.I.R[i,"X_Supported"]= ifelse(as.integer(P.I.R[i,"Xcount_cond"])>0,1,0)
        }
        
        not_covered_rows = which(P.I.R[,"X_Supported"] == 0)
        if (length(not_covered_rows) > 0)
        {
            Y_elserule = names(which.max(table(Y[not_covered_rows])))
        }else 
        {
            Y_elserule = names(which.max(table(Y)))   
        }    
    }
    
   
    
    return(list(Y_elserule=Y_elserule,not_covered_rows=not_covered_rows))
}



summarize_data = function(DataSet,name,id_target) 
{
    class_attr=get_class_attribute(Data=DataSet,id_target=id_target)
    cont_attr=class_attr$continuous_att
    cat_attr=class_attr$categorical_att
    
    dataset_summary=data.frame(matrix(ncol=8,nrow=1))
    colnames(dataset_summary)=c("name","nrow","ncol","ncont_att","ncat_att","cont","cat","nclass")
    dataset_summary[1,]=c(name,nrow(DataSet),
                          ncol(DataSet),length(cont_attr),
                          length(cat_attr),paste0(as.character(cont_attr),collapse=",") ,
                          paste0(as.character(cat_attr),collapse=","),length(unique(DataSet[,id_target]))) 
    return(dataset_summary)
    
}

get_class_attribute =function(Data,id_target) 
{
    continuous_var=vector()
    categorical_var=vector() 
    id_target=id_target
    for (i in 1:ncol(Data))
    {
        if (is.numeric(Data[,i]))
        {
            continuous_var=c(continuous_var,i)
        }else
        {
            categorical_var=c(categorical_var,i)
        }
        continuous_var=setdiff(continuous_var,id_target)
        categorical_var=setdiff(categorical_var,id_target)
        # 
    }
    return(list(continuous_att=continuous_var,categorical_att=categorical_var,id_target=id_target))
}


get_prediction_perf_N1= function(PredTrain,PredTest, nbr_rules,iter, learner_name) 
{
    prediction_perf_iter=data.frame(matrix(nrow=6, ncol=13))
    colnames(prediction_perf_iter)=c("data", "iter", "pred_set", "pred_sub_set", "method","accuracy", "macroPrecision", "macroRecall",
                                     "macroF1", "kappa", "kappa_w","rules_nbr","coverage")
    prediction_perf_iter$iter = iter
    prediction_perf_iter$pred_set= c(rep("train",3), rep("test",3))
    prediction_perf_iter$pred_sub_set=rep(c("covered","notcovered","all"),2)
    prediction_perf_iter$method = learner_name
    prediction_perf_iter[1,6:11] = PredTrain$Global_perf
    prediction_perf_iter[2,6:11] = NA
    prediction_perf_iter[3,6:11] = PredTrain$Global_perf
    prediction_perf_iter[4,6:11] = PredTest$Global_perf
    prediction_perf_iter[5,6:11] = NA
    prediction_perf_iter[6,6:11] = PredTest$Global_perf
    prediction_perf_iter$coverage[c(1,4)] = 1
    prediction_perf_iter$rules_nbr[c(1,4)] = nbr_rules
    prediction_perf_iter$coverage[c(2,5)] = 0
    prediction_perf_iter$rules_nbr[c(2,5)] = 0
    prediction_perf_iter$coverage[c(3,6)] = 1
    prediction_perf_iter$rules_nbr[c(3,6)] = nbr_rules
    return(prediction_perf_iter)
}

get_prediction_perf_N2= function(PredTrain,PredTest, nbr_rules,iter, learner_name) 
{
    prediction_perf_iter=data.frame(matrix(nrow=6, ncol=13))
    colnames(prediction_perf_iter)=c("data", "iter", "pred_set", "pred_sub_set", "method","accuracy", "macroPrecision", "macroRecall",
                                     "macroF1", "kappa", "kappa_w","rules_nbr","coverage")
    prediction_perf_iter$iter = I
    prediction_perf_iter$pred_set= c(rep("train",3), rep("test",3))
    prediction_perf_iter$pred_sub_set=rep(c("covered","notcovered","all"),2)
    prediction_perf_iter$method = learner_name
    prediction_perf_iter[1,6:11] = PredTrain$cover_pred_perf_indicators$Global_perf
    prediction_perf_iter[2,6:11] = PredTrain$notcover_pred_perf_indicators$Global_perf
    prediction_perf_iter[3,6:11] = PredTrain$global_pred_perf_indicators$Global_perf
    prediction_perf_iter[4,6:11] = PredTest$cover_pred_perf_indicators$Global_perf
    prediction_perf_iter[5,6:11] = PredTest$notcover_pred_perf_indicators$Global_perf
    prediction_perf_iter[6,6:11] = PredTest$global_pred_perf_indicators$Global_perf
    prediction_perf_iter$coverage[1] = PredTrain$prop_Xis_covered
    prediction_perf_iter$coverage[2] = 1-prediction_perf_iter$coverage[1]
    prediction_perf_iter$coverage[c(3,6)] = 1
    prediction_perf_iter$coverage[4] = PredTest$prop_Xis_covered
    prediction_perf_iter$coverage[5] = 1-prediction_perf_iter$coverage[4]
    prediction_perf_iter$rules_nbr[c(1,4)] = nbr_rules
    prediction_perf_iter$rules_nbr[c(2,5)] = 1
    prediction_perf_iter$rules_nbr[c(3,6)] = nbr_rules +1
    return(prediction_perf_iter)
}

get_prediction_perf_N3 = function(PredTrain,PredTest,PredTrain_supported,PredTest_supported, 
                                 PredTrain_notsupported,PredTest_notsupported,
                                 cov_train,cov_test,
                                 nbr_rules,iter, learner_name) 
{
    prediction_perf_iter=data.frame(matrix(nrow=6, ncol=13))
    colnames(prediction_perf_iter)=c("data", "iter", "pred_set", "pred_sub_set", "method","accuracy", "macroPrecision", "macroRecall",
                                     "macroF1", "kappa", "kappa_w","rules_nbr","coverage")
    prediction_perf_iter$iter = I
    prediction_perf_iter$pred_set= c(rep("train",3), rep("test",3))
    prediction_perf_iter$pred_sub_set=rep(c("covered","notcovered","all"),2)
    prediction_perf_iter$method = learner_name
    prediction_perf_iter[1,6:11] = PredTrain_supported$Global_perf
    prediction_perf_iter[2,6:11] = PredTrain_notsupported$Global_perf
    prediction_perf_iter[3,6:11] = PredTrain$Global_perf
    prediction_perf_iter[4,6:11] = PredTest_supported$Global_perf
    prediction_perf_iter[5,6:11] = PredTest_notsupported$Global_perf
    prediction_perf_iter[6,6:11] = PredTest$Global_perf
    prediction_perf_iter$coverage[1] = cov_train
    prediction_perf_iter$coverage[2] = 1-prediction_perf_iter$coverage[1]
    prediction_perf_iter$coverage[c(3,6)] = 1
    prediction_perf_iter$coverage[4] = cov_test
    prediction_perf_iter$coverage[5] = 1-prediction_perf_iter$coverage[4]
    prediction_perf_iter$rules_nbr[c(1,4)] = nbr_rules 
    prediction_perf_iter$rules_nbr[c(2,5)] = 1
    prediction_perf_iter$rules_nbr[c(3,6)] = nbr_rules + 1
    return(prediction_perf_iter)
}
stat_nbr_var_used = function(rules_var_used_count,iter,learner_name) 
{
    var_used_stat=data.frame(matrix(nrow=1, ncol=9))
    colnames(var_used_stat)=c("data", "iter", "method","mean", "min", "lower_quartile","median", "upper_quartile", "max")
    var_used_stat$iter = iter
    var_used_stat$method = learner_name
    if (length(rules_var_used_count)>0) 
    {
        var_used_stat[1,4] = round(mean(rules_var_used_count),2)
        var_used_stat[1,5:9] = quantile(rules_var_used_count)
    }else 
    {
        var_used_stat[1,4:9] = NA     
    }
    
    
    
    return(var_used_stat)
}


remove_similar_rules = function(Rules_cover,Rules_metrics,similarity_treshold=0.95) 
{
    # Data frame of covered lines
    rules_cover_all = Rules_cover
    
    # matrice of rules pairwise jaccard distance
    dist_rules = Mercator::binaryDistance(as.matrix(rules_cover_all), "jaccard")
    # matrice of rules pairwise jaccard similarity
    simil_rules= 1- as.matrix(dist_rules)
    simil_rules[lower.tri(simil_rules,diag = TRUE)]=NA
    
    # extract rules that have a similarity index greater that similarity_treshold
    simil_rules_ids=vector("list")
    j=1
    for (i in 1:nrow(simil_rules))
    {
        i_similar=which(simil_rules[i,]>=similarity_treshold)
        if (length(i_similar)>=1)
        {
            simil_rules_ids[[j]]=c(i,i_similar)
            j=j+1
        }
    }
    
    # given similar rules, choose the rule with higher confidence, support , var_length ,levels_length
    id_to_remove_all=c()
    if (length(simil_rules_ids) >=1) 
    {
        simil_rules_ids=simil_rules_ids[order(lengths(simil_rules_ids))]
        for (i in 1:length(simil_rules_ids))
        {
            
            i_similar=simil_rules_ids[[i]]
            i_similar=setdiff(i_similar,id_to_remove_all)
            if (length(i_similar)>=2)
            {
                rules_i_similar=Rules_metrics[i_similar,]
                rules_i_similar=rules_i_similar[order(rules_i_similar$confidence,
                                                      rules_i_similar$support,
                                                      -rules_i_similar$var_length,
                                                      -rules_i_similar$levels_length,
                                                      decreasing=TRUE),]
                
                id_to_remove=rules_i_similar[-1,"id"]
                id_to_remove_all= c(id_to_remove_all,id_to_remove)
            }
            
        }
    }
    
    return(similar_rules_to_remove=id_to_remove_all)
}

# ---------------------------------------------------------------------------------

# function transforming rules to transactions
Transform_Rules2Transactions=function(Rule_pred_DF)
{
    Rules_df= Rule_pred_DF
    id.Rules=1:ncol(Rules_df)
    
    for (j in id.Rules)
    {
        covered_lines= which(!is.na(Rules_df[,j]))
        Rules_df[covered_lines,j] =paste0("R",j)
    }
    Rules_trans=character(length=nrow(Rules_df))
    for (i in 1:nrow(Rules_df))
    {
        Rules_trans[i]=paste0(Rules_df[i,-which(is.na(Rules_df[i,]))],collapse=",")
    } 
    
    
    library(arules)
    Rules_trans=as(as(strsplit(Rules_trans,split=","),"list"),"transactions")
    return(Rules_trans)
}

# function applying Apriori to the list of the rule's transactions in order
# to figure out the rules applied to the subspaces defined by the selected rules
Rules_Inclusion=function(id_selected_rules,Rules_info_metrics,Rule_pred_DF,X,Inclusion_support,Inclusion_confidence)
{
    Apriori_Rules=Transform_Rules2Transactions(Rule_pred_DF)
    perf_Rules=Rules_info_metrics
    RHS_rules=id_selected_rules
    
    rules_RHS=paste0("R",RHS_rules)
    
    Apriori_RT_trans=apriori(data = Rules_transactions,parameter = list(support = Inclusion_support, confidence = Inclusion_confidence ,maxlen=2),
                             appearance = list(none=NULL,rhs = rules_RHS,default="lhs"))
    
    info_Rules_apriori=apriori_info(rules_apriori=Apriori_RT_trans)
    rm=which(info_Rules_apriori[,'lhs']=="")
    if (length(rm)>0)
    {
        info_Rrules_apriori=info_Rrules_apriori[-rm,]
    }
    
    linked_Rules=unique(info_Rules_apriori$rhs)
    Selected_Rules_Inclusion=data.frame()
    ALL_Perf_Rules_inclusion=data.frame()
 
    for (r in 1:length(linked_Rules))
    {
        Rl=as.integer(gsub("R","",c(unique(info_Rules_apriori$lhs[which(info_Rules_apriori[,"rhs"]==linked_Rules[r])])))) #ids Rules contained in RL
        Rr=as.integer(gsub("R","",linked_Rules[r])) # id base rule(=RL)
        Co=c(Rr,Rl) 
        Rules_inclusion=data.frame(matrix(NA,nrow=length(Co),ncol=11))
        colnames(Rules_inclusion)=c("baserule","idRule","cond","intersect_baserule","confidence","support",
                                    "class_suppport","var_length","levels_length","Ypred","var_used")
        Rules_inclusion[,"baserule"]=Rr
        Rules_inclusion[,"idRule"]=Co
        Rules_inclusion[,"cond"]=as.data.frame(perf_Rules[Co,"cond"])
        
       
        Rr_covered_instances=which(!is.na(Rule_pred_DF[,Rr]))
        
        for (i in 1:length(Co))
        {
            rule_id=Co[i]
            Rl_covered_instances = which(!is.na(Rule_pred_DF[,rule_id]))
            Rules_inclusion[i,"intersect_baserule"]=round(length(intersect(Rl_covered_instances,Rr_covered_instances))/length(Rr_covered_instances),2)
            other_clumns=c("confidence","support","class_suppport","var_length","levels_length","Ypred","var_used")
            Rules_inclusion[i,other_clumns] = perf_Rules[rule_id,other_clumns]
        }
        
        
        Perf_Rules_inclusion=Rules_inclusion
        # select rules which combinaison of variables differs from the ones of Base rule
        
        Details_Rules_inclusion=Perf_Rules_inclusion[1,]
        ALL_comb_var_used=unique(Perf_Rules_inclusion[,"var_used"])
        comb_var_used_baserule=Perf_Rules_inclusion[which(Perf_Rules_inclusion[,"idRule"] == Rr),"var_used"]
        comb_var_used=unique(Perf_Rules_inclusion[which(Perf_Rules_inclusion[,"var_used"] != comb_var_used_baserule),"var_used"])
        if (length(comb_var_used) > 0)
        {
            for (i in 1:length(comb_var_used))
            {
                a=Perf_Rules_inclusion[which(Perf_Rules_inclusion[,"var_used"]== comb_var_used[i]),]
                a=a[order(a$intersect_baserule,a$confidence,a$support,-a$levels_length,decreasing = TRUE),][1,]
                Details_Rules_inclusion=rbind(Details_Rules_inclusion,a)
            }
        }
        Selected_Rules_Inclusion=rbind(Details_Rules_inclusion,Selected_Rules_Inclusion)
        ALL_Perf_Rules_inclusion=rbind(Perf_Rules_inclusion,ALL_Perf_Rules_inclusion)
    }
    Selected_Rules_Inclusion$support=round(Selected_Rules_Inclusion$support,2)
    ALL_Perf_Rules_inclusion$support=round(ALL_Perf_Rules_inclusion$support,2)
    Selected_Rules_Inclusion$class_suppport=round(Selected_Rules_Inclusion$class_suppport,2)
    ALL_Perf_Rules_inclusion$class_suppport=round(ALL_Perf_Rules_inclusion$class_suppport,2)
    return(list(Perf_selected_Rules_Inclusion=Selected_Rules_Inclusion,Perf_ALL_Rules_inclusion=ALL_Perf_Rules_inclusion))
}


# INFORULES: genarate Apriori info rules
apriori_info=function(rules_apriori){
    rulesinfo=data.frame(matrix(nrow=length(rules_apriori),ncol=8))
    colnames(rulesinfo)=c("rule","lhs","rhs","length","support","confidence","lift", "count")
    rulesinfo[,1]=c(labels(rules_apriori))
    rulesinfo[,5]=quality(rules_apriori)$support
    rulesinfo[,6]=quality(rules_apriori)$confidence
    rulesinfo[,7]=quality(rules_apriori)$lift
    rulesinfo[,8]=quality(rules_apriori)$count
    for (i in 1:nrow(rulesinfo))
    {
        rulesinfo[i,2]=paste(as(lhs(rules_apriori),"list")[[i]], collapse = " & ")
        rulesinfo[i,3]=gsub("'pred' = ","",as.character(as(rhs(rules_apriori),"list")[[i]]))
        rulesinfo[i,4]=length(as(lhs(rules_apriori),"list")[[i]])
    }
    return(rulesinfo=rulesinfo)
}  


# --------------------------------
# ablation preselection step

RulestoRulespred=function (X, Y,RF_Selected_Rules,min_rule_class_support=0.025, min_rule_confidence=0.51, min_rule_var=1, max_rule_var=10,similarity_treshold=0.95) 
{
    dataconds=RF_Selected_Rules$condsdata
    
    # Clean dataconds, conds and predictions
    for (j in 1: ncol(dataconds))
    {
        dataconds[,j]  = gsub(paste0(colnames(dataconds)[j],"_"),"",dataconds[,j])
        dataconds[,j]  = gsub("),",")",dataconds[,j])
    }
    
    conds=RF_Selected_Rules$Conds
    
    for (j in 1: ncol(dataconds))
    {
        conds  = gsub(paste0(colnames(dataconds)[j],"_"),"",conds)
        conds  = gsub(paste0("'",colnames(dataconds)[j],"'%in%"),paste0("X[,",j,"] %in% "),conds)
    }
    conds = gsub("),",")",conds)
    conds = gsub(", '",",'",conds)
    
    predictions = RF_Selected_Rules$PredClass
    dataconds$Ypred = paste0("'",c(predictions),"'")
    
    # Select unique rules
    dataconds = unique(dataconds)
    id_unique = as.numeric(rownames(dataconds))
    
    predictions = predictions[id_unique]
    conds = conds[id_unique]
    
    
    # Compute the number of variables used per rule
    
    datavar = dataconds[,-ncol(dataconds)]
    dataconds[,"var_used"] = NA
    dataconds[,"var_length"] = 0
    for (i in 1:nrow(dataconds)) 
    {
        var_used= colnames(datavar)[which(!is.na(datavar[i,]))] 
        dataconds[i,"var_used"] = paste0(var_used,collapse=",")
        dataconds[i, "var_length"] = length(var_used) 
    }
    
    
    # # Select rules respecting min_rule_var and max_rule_var
    # ids_var_length_ok = which(dataconds$var_length >= min_rule_var & dataconds$var_length<= max_rule_var)
    # 
    # # Update data
    # conds = conds[ids_var_length_ok]
    # dataconds= dataconds[ids_var_length_ok,]
    # predictions= predictions[ids_var_length_ok]
    
    
    
    # Compute the number of levels used per rule
    
    levels_length=dataconds[,!names(dataconds) %in% c("Ypred", "var_used","var_length")]
    for (j in 1: ncol(levels_length))
    {
        for (i in 1: nrow(levels_length))
        {
            levels_length[i,j]  = ifelse(is.na(levels_length[i,j]),0,length(eval(parse(text=levels_length[i,j]))))
        }
    }
    
    levels_length = as.data.frame(sapply(levels_length, as.numeric))
    levels_length$length=rowSums(levels_length)
    
    # add levels_length and conds to dataconds
    dataconds$level_length = levels_length$length
    dataconds$cond = conds
    
    # compute prediction metrics
    
    id.Rules=1:nrow(dataconds)
    
    # Prediction of the PreSelected Rules
    P.PS.R=data.frame(matrix(NA,nrow=nrow(X),ncol=length(id.Rules)+1))
    
    names(P.PS.R)=c(paste0("R",id.Rules),"Y")
    
    P.PS.R[,"Y"]=as.character(Y)
    
    j=1
    for (k in id.Rules)
    {
        P.PS.R[,j]=ifelse(eval(parse(text=dataconds[k,"cond"]))=="TRUE",gsub("'","",dataconds[k,"Ypred"]),NA)
        j=j+1
    }
    
    perf_Rules=data.frame(matrix(NA,ncol=11,nrow=length(id.Rules)),row.names =paste0("R",id.Rules))
    names(perf_Rules)=c("id","confidence","support", "class_suppport",
                        "var_length", "levels_length","var_nbr_scaled","levels_nbr_scaled",
                        "cond", "Ypred", "var_used")
    
    perf_Rules[,"Ypred"] = dataconds[,"Ypred"]
    for (k in (id.Rules))
    {
        perf_Rules[k,"id"] = k
        ids_covered = which(!P.PS.R[,k]=="NA")
        coverage_count =length(ids_covered)
        perf_Rules[k,"support"] =coverage_count/nrow(X)
        perf_Rules[k,"confidence"] =length (which(P.PS.R[ids_covered,k] == P.PS.R[ids_covered,"Y"]))/ coverage_count
    }
    
    
    # Compute class support
    perf_Rules$class_suppport = perf_Rules$support
    target_class_dist= table(Y)/length(Y)
    
    for (i in 1: dim(target_class_dist))
    {
        id_rows = which(perf_Rules[,"Ypred"] == paste0("'",names(target_class_dist[i]),"'"))
        perf_Rules[id_rows,"class_suppport"] = (perf_Rules[id_rows,"class_suppport"])/target_class_dist[i]
    }
    # NB: the class support can have a value greater than 1 in the case where the rule has a low confidence and a high cover
    
    # # Select rules respecting prediction metrics tresholds: min_rule_class_support and min_rule_confidence
    # perf_Rules=perf_Rules[which(perf_Rules[,"confidence"] > min_rule_confidence & perf_Rules[,"class_suppport"] >= min_rule_class_support),]
    # 
    # ids_pred_ok= perf_Rules$id
    # 
    # # Update data
    # conds = conds[ids_pred_ok]
    # dataconds= dataconds[ids_pred_ok,]
    # predictions= predictions[ids_pred_ok]
    # levels_length = levels_length[ids_pred_ok,]
    # P.PS.R = P.PS.R[,c(ids_pred_ok,ncol(P.PS.R))]
    
    
    # complete filling perf_Rules columns
    
    perf_Rules[,"levels_length"] = levels_length[,"length"]
    perf_Rules[,"var_length"] = dataconds[,"var_length"]
    perf_Rules[,"var_used"] = dataconds[,"var_used"] 
    perf_Rules[,"cond"] = dataconds[,"cond"]
    
    
    # compute scaled values of levels_length and levels_length (will be used in the optimization step)
    perf_Rules["var_nbr_scaled"]=perf_Rules$var_length/max(perf_Rules$var_length)
    perf_Rules["levels_nbr_scaled"]=perf_Rules$levels_length /max(perf_Rules$levels_length)
    
    # reinitialise rules ids and names
    perf_Rules$id=1:nrow(perf_Rules)
    rownames(perf_Rules) = paste0("R",1:nrow(perf_Rules))
    names(P.PS.R)=c(paste0("R",1:nrow(perf_Rules)),"Y")
    
    
    # remove rules not respecting similarity tresholds:
    # -Compute matrice of rules pairwise jaccard distance
    # -Extract rules that have a similarity index greater that similarity_treshold
    # -Given similar rules, choose the rule with higher confidence, support , var_length ,levels_length. 
    # The remaing rules are those to remove.
    # Rules_cover = P.PS.R[,-ncol(P.PS.R)]
    # 
    # for (i in 1:nrow(Rules_cover))
    # {
    #     Rules_cover[i,!is.na(Rules_cover[i,])]= 1
    # }
    # for (i in 1:nrow(Rules_cover))
    # {
    #     Rules_cover[i,is.na(Rules_cover[i,])]= 0
    # }
    # Rules_cover=data.frame(lapply(Rules_cover,as.numeric))
    # ids_to_remove=remove_similar_rules( Rules_cover=Rules_cover,
    #                                     Rules_metrics=perf_Rules,
    #                                     similarity_treshold=similarity_treshold)
    # 
    # 
    # # Update data
    # ids_to_retain=setdiff(1:nrow(perf_Rules),ids_to_remove)
    # conds = conds[ids_to_retain]
    # dataconds= dataconds[ids_to_retain,]
    # predictions= predictions[ids_to_retain]
    # levels_length = levels_length[ids_to_retain,]
    # P.PS.R_0 = P.PS.R
    # perf_Rules_0 = perf_Rules
    # P.PS.R = P.PS.R[,c(ids_to_retain,ncol(P.PS.R))]
    # perf_Rules = perf_Rules[ids_to_retain,]
    # # save removed rules
    # P.PS.R_similremoved = data.frame()
    # perf_Rules_similremoved = data.frame()
    # if (length(ids_to_remove) > 0) 
    # {
    #     P.PS.R_similremoved = P.PS.R_0[,c(ids_to_remove,ncol(P.PS.R_0))]
    #     perf_Rules_similremoved = perf_Rules_0[ids_to_remove,]
    # }
    
    # # reinitialise rules ids and names
    # perf_Rules$id=1:nrow(perf_Rules)
    # rownames(perf_Rules) = paste0("R",1:nrow(perf_Rules))
    # names(P.PS.R)=c(paste0("R",1:nrow(perf_Rules)),"Y")
    # if (length(ids_to_remove) > 0)
    # {
    #     new_ids=(nrow(perf_Rules) + 1):(nrow(perf_Rules) + nrow(perf_Rules_similremoved))
    #     perf_Rules_similremoved$id=new_ids
    #     rownames(perf_Rules_similremoved) = paste0("R",new_ids)
    #     names(P.PS.R_similremoved)=c(paste0("R",new_ids),"Y")
    # }
    
    
    return(list(Rules_selection=dataconds, 
                perf_Rules = perf_Rules, RulesPrediction=P.PS.R, 
                Conds=conds,PredClass=predictions))
}

# --------------------------------
# functions used to extract classifiers rules ( classifiers fitted using arulesCBA, Rweka, and SBRL packages): "extract_classifier_rules","extract_conditions_coverage"
extract_classifier_rules= function(classifier_rules,rules_nbr,cond_vars,cond_target )
{
  var_nbr=length(cond_vars)
  classifier_rules_DF = data.frame(matrix(NA,nrow=rules_nbr, ncol=var_nbr))
  colnames(classifier_rules_DF)=cond_vars
  
  for (id_cond in 1:nrow(classifier_rules_DF)) 
  {
    cond=classifier_rules$LHS[id_cond] 
    cond=gsub("}","",cond)
    cond=gsub("{","",cond,fixed = TRUE)
    # cond=gsub(", Inf",",Inf",cond, fixed = TRUE)
    # cond=gsub("Inf, ","Inf,",cond, fixed = TRUE)
    cond=gsub(",V"," , V",cond, fixed = TRUE)
    # cond=gsub(", ",",",cond,fixed = TRUE)
    condlist= as.list(strsplit(cond, " , ")[[1]])
    for (var_name in colnames(classifier_rules_DF))
    {
      for (id_cond_var in 1:length(condlist)) 
      {
        if (isTRUE(grepl(paste0(var_name,"="),condlist[id_cond_var]))) 
        {
          newlevel=sub(paste0(var_name,"="), "", condlist[id_cond_var]) 
          classifier_rules_DF[id_cond,var_name] = paste0(c(classifier_rules_DF[id_cond,var_name],newlevel),collapse="','")  
        }
        
      }
      if (!is.na(classifier_rules_DF[id_cond,var_name]))
      {
        classifier_rules_DF[id_cond,var_name] = gsub("NA',","",classifier_rules_DF[id_cond,var_name]) 
        classifier_rules_DF[id_cond,var_name] = paste0("c(",classifier_rules_DF[id_cond,var_name],"')")
      }
      
    }
  }
  classifier_rules_DF$cond=NA
  
  for (row in 1:nrow(classifier_rules_DF)) 
  {
    conds=NULL
    for (col in 1:(ncol(classifier_rules_DF)-1)) 
    {
      cell_value= classifier_rules_DF[row,col]
      if (!is.na(cell_value))
      {
        conds = paste0(c(conds,paste0("X[,",col,"] %in% ",cell_value)),collapse=" & ")
      }
    }
    
    classifier_rules_DF$cond[row]=conds
  }
  cond_pred = classifier_rules$RHS[1:rules_nbr]    
  cond_pred= gsub(paste0("{",cond_target,"="),"",cond_pred, fixed = TRUE)
  cond_pred= gsub("}","",cond_pred, fixed = TRUE)
  classifier_rules_DF$pred=cond_pred
  return(classifier_rules_DF)
}


extract_classifier_rules_weka= function(classifier_rules,rules_nbr,cond_vars,cond_target )
{
  var_nbr=length(cond_vars)
  classifier_rules_DF = data.frame(matrix(NA,nrow=rules_nbr, ncol=var_nbr))
  colnames(classifier_rules_DF)=cond_vars
  id_cond=1
  for (id_cond in 1:nrow(classifier_rules_DF)) 
  {
    cond=classifier_rules$cond[id_cond] 
    cond=gsub(") ","",cond, fixed = TRUE)
    cond=gsub("and ("," and (",cond, fixed = TRUE)
    # cond=gsub(", Inf",",Inf",cond, fixed = TRUE)
    # cond=gsub("Inf, ","Inf,",cond, fixed = TRUE)
    
    condlist= as.list(strsplit(cond, " and ")[[1]])
    for (var_name in colnames(classifier_rules_DF))
    {
      for (id_cond_var in 1:length(condlist)) 
      {
        if (isTRUE(grepl(paste0("(",var_name," = "),condlist[id_cond_var],fixed=TRUE))) 
        {
          newlevel=sub(paste0("(",var_name," = "), "", condlist[id_cond_var],fixed=TRUE) 
          classifier_rules_DF[id_cond,var_name] = paste0(c(classifier_rules_DF[id_cond,var_name],newlevel),collapse="','")  
        }
        
      }
      if (!is.na(classifier_rules_DF[id_cond,var_name]))
      {
        classifier_rules_DF[id_cond,var_name] = gsub("NA',","",classifier_rules_DF[id_cond,var_name]) 
        classifier_rules_DF[id_cond,var_name] = paste0("c(",classifier_rules_DF[id_cond,var_name],"')")
      }
      
    }
  }
  classifier_rules_DF$cond=NA
  
  for (row in 1:nrow(classifier_rules_DF)) 
  {
    conds=NULL
    for (col in 1:(ncol(classifier_rules_DF)-1)) 
    {
      cell_value= classifier_rules_DF[row,col]
      if (!is.na(cell_value))
      {
        conds = paste0(c(conds,paste0("X[,",col,"] %in% ",cell_value)),collapse=" & ")
      }
    }
    
    classifier_rules_DF$cond[row]=conds
  }
  return(classifier_rules_DF)
}

extract_classifier_rules_sbrl= function(classifier_rules,rules_nbr,cond_vars)
{
  var_nbr=length(cond_vars)
  classifier_rules_DF = data.frame(matrix(NA,nrow=rules_nbr, ncol=var_nbr))
  colnames(classifier_rules_DF)=cond_vars
  var_nbr=length(cond_vars)
  classifier_rules_DF = data.frame(matrix(NA,nrow=rules_nbr, ncol=var_nbr))
  colnames(classifier_rules_DF)=cond_vars
  for (id_cond in 1:nrow(classifier_rules_DF)) 
  {
    cond=classifier_rules$cond[id_cond] 
    cond=gsub(",V"," & V",cond,fixed = TRUE)
    cond=gsub("{","",cond,fixed = TRUE)
    cond=gsub("}","",cond,fixed = TRUE)
    # cond=gsub(", Inf",",Inf",cond, fixed = TRUE)
    # cond=gsub("Inf, ","Inf,",cond, fixed = TRUE)
    
    condlist= as.list(strsplit(cond, " & ")[[1]])
    for (var_name in colnames(classifier_rules_DF))
    {
      for (id_cond_var in 1:length(condlist)) 
      {
        if (isTRUE(grepl(paste0(var_name,"="),condlist[id_cond_var],fixed=TRUE))) 
        {
          newlevel=sub(paste0(var_name,"="), "", condlist[id_cond_var],fixed=TRUE) 
          classifier_rules_DF[id_cond,var_name] = paste0(c(classifier_rules_DF[id_cond,var_name],newlevel),collapse="','")  
        }
        
      }
      if (!is.na(classifier_rules_DF[id_cond,var_name]))
      {
        classifier_rules_DF[id_cond,var_name] = gsub("NA',","",classifier_rules_DF[id_cond,var_name]) 
        classifier_rules_DF[id_cond,var_name] = paste0("c(",classifier_rules_DF[id_cond,var_name],"')")
      }
      
    }
  }
  classifier_rules_DF$cond=NA
  
  for (row in 1:nrow(classifier_rules_DF)) 
  {
    conds=NULL
    for (col in 1:(ncol(classifier_rules_DF)-1)) 
    {
      cell_value= classifier_rules_DF[row,col]
      if (!is.na(cell_value))
      {
        conds = paste0(c(conds,paste0("X[,",col,"] %in% ",cell_value)),collapse=" & ")
      }
    }
    
    classifier_rules_DF$cond[row]=conds
  }
  return(classifier_rules_DF)
}


extract_conditions_coverage=function(X,Y,conditions,predictions)
{
  id.Rules=1:length(conditions)
  vote=levels(Y)
  # Y_else=elserule
  
  # prediction of rules 
  P.I.R=data.frame(matrix(NA,nrow=nrow(X),ncol=length(id.Rules)+4+length(levels(Y))))
  id_col_votes= (ncol(P.I.R)-length(vote)+1):ncol(P.I.R)
  names(P.I.R)=c(conditions,"Y","Ypred","X_Supported","Xcount_cond",c(vote))
  
  
  P.I.R[1:nrow(P.I.R),"Y"]=as.character(Y)
  j=1
  for (k in (id.Rules))
  {
    P.I.R[,j]=ifelse(eval(parse(text=conditions[k]))=="TRUE",gsub("'","",predictions[k]),NA)
    j=j+1
  }
  
  for (i in 1:nrow(X))
  {
    P.I.R[i,"Xcount_cond"]= length(which(!is.na(P.I.R[i,1:length(id.Rules)])))
    P.I.R[i,"X_Supported"]= ifelse(as.integer(P.I.R[i,"Xcount_cond"])>0,1,0)
    if(P.I.R[i,"X_Supported"] == 1)
    {
      for (v in 1:length(vote))
      {
        P.I.R[i,vote[v]]= length(which(P.I.R[i,1:length(id.Rules)]==vote[v]))
      }
      
      P.I.R[i,"Ypred"]=names(which.max(P.I.R[i,id_col_votes]))
    }
  }
  prop_Xis_covered=sum(P.I.R$X_Supported)/nrow(X)
  Xis_covered=which(P.I.R[,"X_Supported"] == 1)
  
  return(list(predictions=P.I.R,
              prop_Xis_covered=prop_Xis_covered,
              Xis_covered=Xis_covered))
}
