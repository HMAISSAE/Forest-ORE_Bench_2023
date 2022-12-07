
wd <- getwd()
setwd(wd)

data_name = "cryotherapy"


# import data
data_path = "./data/cryotherapy.txt"
original_data=read.table(data_path,sep=';',header=T,na.strings="?")
str(original_data)
summary(original_data)

for (i in c(1,5,7)) 
{
  original_data[,i]=as.factor(original_data[,i])
}
original_data[,3]=as.numeric(original_data[,3])
id_target = 7
original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)
data_summary_Path = paste0(".\\data_summary\\", data_name, "_datasummary.csv")
write.csv(original_data_summary,data_summary_Path, row.names = FALSE)
read.csv(data_summary_Path)


str(original_data)

# # Imputation of missing values
# missingcolumns=vector()
# for (i in 1:ncol(original_data)) 
# {
#     if (length(unique(na.omit(original_data[,i])))<=1) 
#     {
#         missingcolumns=c(missingcolumns,i)
#     }
# }
# original_data=original_data[,-missingcolumns]
original_data=na.roughfix(original_data)



# cleaning

# target class
id_target = 7
summary(original_data)
dim(original_data)
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


if (original_data_summary$ncont_att >=1) 
{
    
    discretize_data = ForestDisc(data=original_data, id_target=id_target)
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
# --------------------------------------------------------------------------
source("execute_iterations_sript.R")