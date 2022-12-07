
wd <- getwd()
setwd(wd)

data_name = "anneal"

# import data
data_path = "./data/anneal898.txt"
original_data=read.table(data_path,sep=",",header=TRUE, na.strings = "?")
for (i in 1:ncol(original_data)) 
{
    if (is.logical(original_data[,i])) 
    {
        original_data[,i] = as.factor(original_data[,i])
    }
}

id_target = 39
original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)
data_summary_Path = paste0(".\\data_summary\\", data_name, "_datasummary.csv")
write.csv(original_data_summary,data_summary_Path, row.names = FALSE)
read.csv(data_summary_Path)


str(original_data)

# Imputation of missing values
missingcolumns=vector()
for (i in 1:ncol(original_data)) 
{
    if (length(unique(na.omit(original_data[,i])))<=1) 
    {
        missingcolumns=c(missingcolumns,i)
    }
}
original_data=original_data[,-missingcolumns]
# carbon: continuous
# hardness: continuous
# strength: continuous


# for (i in 1:ncol(original_data)) 
# {
#   if (is.integer(original_data[,i])) 
#   {
#     if (length(unique(original_data[,i]))<=10) 
#     {
#       original_data[,i] = as.factor(original_data[,i])    
#     }else 
#     {
#       original_data[,i] = as.numeric(original_data[,i]) 
#     } 
#     
#   }
# }

str(original_data)


# target class
id_target = 19
table(original_data[,19])
dim(original_data)
# 
original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)

original_data_summary$cont
"3,4,6,7,9,13,14,15,17,18"
colnames(original_data[c(3,4,6,7,9,13,14,15,17,18)])
# 3-carbon: continuous
# 4-hardness: continuous
# 6- formability: -,1,2,3,4,5
# 7-strength: continuous
# 9- enamelability: -,1,2,3,4,5
# 13- thick: continuous
# 14- width: continuous
# 15- len: continuous
# 17- bore: 0000,0500,0600,0760
# 18- packing: -,1,2,3
original_data[,id_target] = as.factor(original_data[,id_target])

for (col in c(3,4,7,13,14,15)) 
{
  original_data[,col]=as.numeric(original_data[,col])
}
for (col in c(6,9,17,18)) 
{
  original_data[,col]=as.factor(original_data[,col])
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


# --------------------------------------------------------------------------
source("execute_iterations_sript.R")
