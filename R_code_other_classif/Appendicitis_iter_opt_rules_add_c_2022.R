
wd <- getwd()
setwd(wd)

data_name = "Appendicitis"

# import data
data_path = "./data/appendicitis.txt"
original_data=read.table(data_path,sep=',',header= F, na.strings="?")
head(original_data)
str(original_data)
# for (i in 1:ncol(original_data)) 
# {
#     if (is.logical(original_data[,i])) 
#     {
#         original_data[,i] = as.factor(original_data[,i])
#     }
# }
for (i in c(8))
{
  original_data[,i]=as.factor(original_data[,i])
}

# target class
id_target = 8
table(original_data[,id_target])
str(original_data)

# summarize data
original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)

str(original_data)

# cleaning/preproc

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


source("./otherclassifiers3_execute_iterations_script_2022.R")
