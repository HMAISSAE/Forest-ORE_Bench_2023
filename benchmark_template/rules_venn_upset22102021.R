install.packages("rJava")
install.packages("UpSetR")
install.packages("tidyverse")
install.packages("venneuler")

library(rJava)
library(UpSetR)
library(tidyverse)
library(venneuler)
library(grid)




sets3=read.table(XOR_rules_pred_matrix_path,sep=',',header= T, na.strings="?")
sets3$yclass=as.character(sets3$yclass)

sets3=sets3[,-6]
sets3$yclass[which(sets3$yclass=="0")]="class0"
sets3$yclass[which(sets3$yclass=="1")]="class1"
upset(sets3,query.legend = "top",
      mainbar.y.label = "Rules coverage", sets.x.label = "population size",
      sets.bar.color=c("maroon","blue","maroon","blue"),
      queries = list(list(query = elements, params = list("yclass","0"), color = "blue", active = T,query.name = "class 0"),
                     list(query = elements, params = list("yclass","1"), color = "maroon", active = T,query.name = "class 1")
      )
)

data_rules_opt2=read.table(Mushroom_rules_pred_matrix_path,sep=',',header= T, na.strings="?")
data_rules_opt2$yclass=as.character(data_rules_opt2$yclass)
upset(data_rules_opt2,query.legend = "top",point.size = 3.5,
      mainbar.y.label = "Rules coverage", sets.x.label = "population size",
      sets.bar.color=c("maroon","blue","maroon","blue"),
      queries = list(list(query = elements, params = list("yclass","p"), color = "blue", active = T,query.name = "poisonous"),
                     list(query = elements, params = list("yclass","e"), color = "maroon", active = T,query.name = "edible")
      )
)

upset(sets3,query.legend = "top",point.size = 3.5,
      mainbar.y.label = "Rules coverage", sets.x.label = "population size",
      sets.bar.color=c("blue","blue","maroon","maroon"),
      queries = list(list(query = elements, params = list("yclass","class0"), color = "blue", active = T,query.name = "Class0"),
                     list(query = elements, params = list("yclass","class1"), color = "maroon", active = T,query.name = "Class1")
      )
)

table(sets3$yclass)

table(data_rules_opt2$yclass[which(sets2[,"else"] ==1)])
str(sets3)
head(data_rules_opt2)

# sets <- data_rules_opt
# sets2 <-data_rules_opt2 
# mysets=sets
# sets1=data_rules_opt[,-ncol(sets)]
# str(sets3)
# vennSets <- sets %>%
#     gather(Rules, binary,2:ncol(sets)) %>% # take all binary mappings and convert to be a the set indicator
#     filter(binary == 1) %>% # only include set matches
#     select(id, Rules) %>% # only include ID and set category
#     mutate(Rules = factor(Rules)) # set the rules column as a factor
# vennSets1 <- sets1 %>%
#     gather(Rules, binary,2:ncol(sets1)) %>% # take all binary mappings and convert to be a the set indicator
#     filter(binary == 1) %>% # only include set matches
#     select(id, Rules) %>% # only include ID and set category
#     mutate(Rules = factor(Rules)) # set the rules column as a factor
# vennSets2 <- sets2 %>%
#   gather(Rules, binary,2:ncol(sets2)) %>% # take all binary mappings and convert to be a the set indicator
#   filter(binary == 1) %>% # only include set matches
#   select(id, Rules) %>% # only include ID and set category
#   mutate(Rules = factor(Rules)) # set the rules column as a factor
# vennSets3 <- sets3 %>%
#   gather(Rules, binary,2:ncol(sets2)) %>% # take all binary mappings and convert to be a the set indicator
#   filter(binary == 1) %>% # only include set matches
#   select(id, Rules) %>% # only include ID and set category
#   mutate(Rules = factor(Rules)) # set the rules column as a factor
# dim(vennSets)
# dim(vennSets1)
# v <- venneuler(data.frame(vennSets))
# v1=venneuler(data.frame(vennSets1))
# v2=venneuler(data.frame(vennSets2))
# library(ggVennDiagram)
# 
# 
# 
# # for (rule in unique(vennSets$Rules)) 
# # {
# #   set=vennSets %>% filter(Rules == rule)
# #   x =c(x,set$id)
# # }
# set43 = vennSets %>% filter(Rules == "R43")
# Rules43 = set43$id
# 
# set44 = vennSets %>% filter(Rules == "R44")
# Rules44 =set44$id
# 
# set82 = vennSets %>% filter(Rules == "R82")
# Rules82 =set82$id
# 
# setelse = vennSets %>% filter(Rules == "else")
# elseRules=setelse$id
# 
# x=list(Rule43=Rules43, Rule44=Rules44,Rule82=Rules82,elseRule=elseRules)
# # Venn diagram without legend
# ggVennDiagram(x, color = 1, lwd = 0.7) + 
#   # scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
#   theme(legend.position = "none")
# par(cex = 0.7) 
# plot(v, main = "Rules Graph", cex.main = 1.5,color=c("1","1","2","2"))
# plot(v2, main = "Rules Graph", cex.main = 1.5)
# upset(sets,
#       nsets = 20, number.angles = 30, point.size = 3.5, line.size = 2,
#       mainbar.y.label = "Rules", sets.x.label = "population size"
# )
# 
# upset(sets,
#       nsets = 30,
#       mainbar.y.label = "Rules coverage", sets.x.label = "population size"
# )
# Myfunc <- function(row, yclass) {
#   data <- (row["y"] == yclass) 
# }
# colnames(sets2)
# upset(sets2,query.legend = "top",
#       mainbar.y.label = "Rules coverage", sets.x.label = "population size",
#       sets.bar.color=c("maroon","blue","maroon","blue"),
#       queries = list(list(query = elements, params = list("y","p"), color = "blue", active = T,query.name = "poisonous"),
#                      list(query = elements, params = list("y","e"), color = "maroon", active = T,query.name = "edible")
#                      )
# )