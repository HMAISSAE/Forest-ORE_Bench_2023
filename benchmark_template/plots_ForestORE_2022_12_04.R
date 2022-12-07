


library(grid)
library(ggplot2)
library(tidyverse)
library(scales)

library(ggthemes) #

library(gridExtra)
# library(ggrepel)

# library(ggradar)

# devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)

# ...........................................................
# binary classification
# .......................................................

# perf benchmark
# global performance: accuracy, f1, kappa



B_perf_summary_perdata=B_perf_ORE_summary_perdata_bin

B_perf_summary_iter_based=B_perf_ORE_summary_iter_based_bin

B_perf_summary_iter=B_perf_ORE_summary_iter_bin

str(B_perf_ORE_summary_iter_based_bin)

meth_lev=c("RPART", "RF", "Pre-Forest-ORE", "Forest-ORE",
           "GRRFR", "STEL", "Forest-ORE+STEL", "CBA", "RIPPER", "SBRL")          

# B_perf_data_all = B_perf_disc_summary_perdata_bin %>% 
#   filter(pred_set=="test") %>% 
#   filter(pred_sub_set=="all") %>% 
#   filter(method %in% meth_lev)%>% 
#   mutate(method = factor(method, levels=meth_lev))

B_perf_data_summary_cov_all=B_perf_summary_iter %>% 
  filter(method %in% meth_lev)%>% 
  # filter(pred_sub_set %in% c("covered","all", "notcovered")) %>% 
  filter(pred_sub_set %in% c("covered","all")) %>% 
  filter(pred_set %in% c("test")) %>% 
  mutate(method = factor(method, levels=meth_lev))
str(B_perf_data_summary_cov_all)
# coverage_names <- c(
#   "all" = "All instances",
#   "covered" = "Covered instances",
#   "notcovered" = "Not covered instances"
# )
coverage_names <- c(
  "all" = "All instances",
  "covered" = "Covered instances"
  )
DF1=B_perf_data_summary_cov_all
DF1$bin="BIN"
# gg001=ggplot(B_perf_data_summary_cov_all, aes(x=method, y=mean_acc,fill=method))+ 
#   labs(title="", x="", y="Accuracy on binary \n classification") +
#   # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
#   geom_boxplot(outlier.shape = NA)+
#   # geom_jitter(width=0.1,alpha=0.1) +
#   # geom_line(alpha=0.15) +
#   # geom_point(alpha=0.15) +
#   # theme_hc()+ scale_colour_hc()+
#   # theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   # coord_flip()+
#   # scale_fill_brewer(palette="Dark2") + 
#   # theme_minimal()+
#   theme(axis.title.y=element_text(face="bold",margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
#         axis.text.y = element_text(size=12,face="bold"),
#         legend.position = "none",legend.title = element_blank(),
#         legend.text =element_text(size=7),  
#         legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
#         strip.text = element_text(size = 14)
#   ) 
# 
# print(gg001)
# 
# gg001 + facet_wrap( ~ pred_sub_set, ncol=2, labeller = as_labeller(coverage_names) )
# names(B_perf_data_summary_cov_all)
# 
# gg002=ggplot(B_perf_data_summary_cov_all, aes(x=method, y=mean_F1,fill=method))+ 
#   labs(title="", x="", y="F1-Score") +
#   # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
#   geom_boxplot(outlier.shape = NA)+
#   # geom_jitter(width=0.1,alpha=0.1) +
#   # geom_line(alpha=0.15) +
#   # geom_point(alpha=0.15) +
#   # theme_hc()+ scale_colour_hc()+
#   # theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   # coord_flip()+
#   # scale_fill_brewer(palette="Dark2") + 
#   # theme_minimal()+
#   theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0), size=14),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
#         axis.text.y = element_text(size=12,face="bold"),
#         legend.position = "none",legend.title = element_blank(),
#         legend.text =element_text(size=7),  
#         legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
#         strip.text = element_text(size = 14)
#   ) 
# 
# print(gg002)
# 
# 
# gg002 + facet_wrap( ~ pred_sub_set, ncol=2, labeller = as_labeller(coverage_names) )
# 
# 
# gg003=ggplot(B_perf_data_summary_cov_all, aes(x=method, y=mean_Kappa,fill=method))+ 
#   labs(title="", x="", y="Kappa") +
#   # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
#   geom_boxplot(outlier.shape = NA)+
#   # geom_jitter(width=0.1,alpha=0.1) +
#   # geom_line(alpha=0.15) +
#   # geom_point(alpha=0.15) +
#   # theme_hc()+ scale_colour_hc()+
#   # theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   # coord_flip()+
#   # scale_fill_brewer(palette="Dark2") + 
#   # theme_minimal()+
#   theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0), size=14),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
#         axis.text.y = element_text(size=12,face="bold"),
#         legend.position = "none",legend.title = element_blank(),
#         legend.text =element_text(size=7),  
#         legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
#         strip.text = element_text(size = 14)
#   )  
# # +
# #   scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
# #                               "green","seagreen1","yellowgreen","seagreen",
# #                               "pink","plum2","plum4","purple4",
# #                               "steelblue2","steelblue","slateblue","slateblue4",
# #                               "tan","tan1","tomato","tomato3"))
# print(gg003)
# 
# gg003 + facet_wrap( ~ pred_sub_set, ncol=2, labeller = as_labeller(coverage_names) )


B_perf_summary_perdata=B_perf_ORE_summary_perdata_multi

B_perf_summary_iter_based=B_perf_ORE_summary_iter_based_multi

B_perf_summary_iter=B_perf_ORE_summary_iter_multi

str(B_perf_ORE_summary_iter_based_multi)

meth_lev=c("RPART", "RF", "Pre-Forest-ORE", "Forest-ORE",
           "GRRFR", "STEL", "Forest-ORE+STEL", "CBA", "RIPPER")          

# B_perf_data_all = B_perf_disc_summary_perdata_bin %>% 
#   filter(pred_set=="test") %>% 
#   filter(pred_sub_set=="all") %>% 
#   filter(method %in% meth_lev)%>% 
#   mutate(method = factor(method, levels=meth_lev))

B_perf_data_summary_cov_all=B_perf_summary_iter %>% 
  filter(method %in% meth_lev)%>% 
  # filter(pred_sub_set %in% c("covered","all", "notcovered")) %>% 
  filter(pred_sub_set %in% c("covered","all")) %>% 
  filter(pred_set %in% c("test")) %>% 
  mutate(method = factor(method, levels=meth_lev))
str(B_perf_data_summary_cov_all)
str(B_perf_data_summary_cov_all)
# coverage_names <- c(
#   "all" = "All instances",
#   "covered" = "Covered instances",
#   "notcovered" = "Not covered instances"
# )
coverage_names <- c(
  "all" = "All instances",
  "covered" = "Covered instances"
)
DF2=B_perf_data_summary_cov_all
DF2$bin="MULTI"
# DF12=rbind(DF1,DF2)
# DF12=DF12 %>% mutate(method=factor(method))
# str(DF12)
new_names <- c(
  "all" = "All instances",
  "covered" = "Covered instances",
  "BIN" = "Binary \nclassification",
  "MULTI" = "Multiclass \nclassification"
)
ggDF12_0=ggplot(DF12, aes(x=method, y=mean_acc,fill=method))+
  labs(title="", x="", y="Accuracy") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  geom_boxplot(outlier.shape = NA)+
  # geom_jitter(width=0.1,alpha=0.1) +
  # geom_line(alpha=0.15) +
  # geom_point(alpha=0.15) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # coord_flip()+
  # scale_fill_brewer(palette="Dark2") +
  # theme_minimal()+
  theme(axis.title.y=element_text(face="bold",margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        legend.position = "none",legend.title = element_blank(),
        legend.text =element_text(size=7),
        legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
        strip.text = element_text(size = 14)
  )

print(ggDF12_0)
names(DF12)
ggDF12_0 + facet_grid(bin ~ pred_sub_set , labeller = as_labeller(new_names), scales="free" ) +                                                                # Change font size
  theme(strip.text.y = element_text(size = 14))

ggDF12_1=ggplot(DF12, aes(x=method, y=mean_F1,fill=method))+
  labs(title="", x="", y="F1-Score") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  geom_boxplot(outlier.shape = NA)+
  # geom_jitter(width=0.1,alpha=0.1) +
  # geom_line(alpha=0.15) +
  # geom_point(alpha=0.15) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # coord_flip()+
  # scale_fill_brewer(palette="Dark2") +
  # theme_minimal()+
  theme(axis.title.y=element_text(face="bold",margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        legend.position = "none",legend.title = element_blank(),
        legend.text =element_text(size=7),
        legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
        strip.text = element_text(size = 14)
  )

print(ggDF12_0)

ggDF12_1 + facet_grid(bin ~ pred_sub_set , labeller = as_labeller(new_names), scales="free" ) +                                                                # Change font size
  theme(strip.text.y = element_text(size = 14))


ggDF12_2=ggplot(DF12, aes(x=method, y=mean_Kappa,fill=method))+
  labs(title="", x="", y="Kappa") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  geom_boxplot(outlier.shape = NA)+
  # geom_jitter(width=0.1,alpha=0.1) +
  # geom_line(alpha=0.15) +
  # geom_point(alpha=0.15) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # coord_flip()+
  # scale_fill_brewer(palette="Dark2") +
  # theme_minimal()+
  theme(axis.title.y=element_text(face="bold",margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        legend.position = "none",legend.title = element_blank(),
        legend.text =element_text(size=7),
        legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
        strip.text = element_text(size = 14)
  )

print(ggDF12_2)

ggDF12_2 + facet_grid(bin ~ pred_sub_set , labeller = as_labeller(new_names), scales="free" ) +                                                                # Change font size
  theme(strip.text.y = element_text(size = 14))


DF12_cov = DF12 %>% filter(pred_sub_set == "covered")
new_names_cov <- c(
  "all" = "All instances",
  "covered" = "Covered instances",
  "BIN" = "Binary classification",
  "MULTI" = "Multiclass classification"
)
ggDF12_3=ggplot(DF12_cov, aes(x=method, y=mean_coverage,fill=method))+
  labs(title="", x="", y="Coverage") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  geom_boxplot(outlier.shape = NA)+
  # geom_jitter(width=0.1,alpha=0.1) +
  # geom_line(alpha=0.15) +
  # geom_point(alpha=0.15) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # coord_flip()+
  # scale_fill_brewer(palette="Dark2") +
  # theme_minimal()+
  theme(axis.title.y=element_text(face="bold",margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        legend.position = "none",legend.title = element_blank(),
        legend.text =element_text(size=7),
        legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
        strip.text = element_text(size = 14)
  )

print(ggDF12_3)

ggDF12_3 + facet_grid(. ~ bin, labeller = as_labeller(new_names_cov), scales="free" ) +                                                                # Change font size
  theme(strip.text.y = element_text(size = 14))

names(B_perf_data_summary_cov_all)
# gg004=ggplot(B_perf_data_summary_cov_all, aes(x=method, y=mean_acc,fill=method))+ 
#   labs(title="", x="", y="Accuracy") +
#   # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
#   geom_boxplot(outlier.shape = NA)+
#   # geom_jitter(width=0.1,alpha=0.1) +
#   # geom_line(alpha=0.15) +
#   # geom_point(alpha=0.15) +
#   # theme_hc()+ scale_colour_hc()+
#   # theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   # coord_flip()+
#   # scale_fill_brewer(palette="Dark2") + 
#   # theme_minimal()+
#   theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0), size=14),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
#         axis.text.y = element_text(size=12,face="bold"),
#         legend.position = "none",legend.title = element_blank(),
#         legend.text =element_text(size=7),  
#         legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
#         strip.text = element_text(size = 14)
#   ) 
# 
# print(gg004)
# 
# gg004 + facet_wrap( ~ pred_sub_set, ncol=2, labeller = as_labeller(coverage_names) )
# names(B_perf_data_summary_cov_all)
# # paste0("text", " ± ")
# gg005=ggplot(B_perf_data_summary_cov_all, aes(x=method, y=mean_F1,fill=method))+ 
#   labs(title="", x="", y="F1-Score") +
#   # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
#   geom_boxplot(outlier.shape = NA)+
#   # geom_jitter(width=0.1,alpha=0.1) +
#   # geom_line(alpha=0.15) +
#   # geom_point(alpha=0.15) +
#   # theme_hc()+ scale_colour_hc()+
#   # theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   # coord_flip()+
#   # scale_fill_brewer(palette="Dark2") + 
#   # theme_minimal()+
#   theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0), size=14),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
#         axis.text.y = element_text(size=12,face="bold"),
#         legend.position = "none",legend.title = element_blank(),
#         legend.text =element_text(size=7),  
#         legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
#         strip.text = element_text(size = 14)
#   ) 
# 
# print(gg005)
# 
# 
# gg005 + facet_wrap( ~ pred_sub_set, ncol=2, labeller = as_labeller(coverage_names) )
# 
# 
# gg006=ggplot(B_perf_data_summary_cov_all, aes(x=method, y=mean_Kappa,fill=method))+ 
#   labs(title="", x="", y="Kappa") +
#   # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
#   geom_boxplot(outlier.shape = NA)+
#   # geom_jitter(width=0.1,alpha=0.1) +
#   # geom_line(alpha=0.15) +
#   # geom_point(alpha=0.15) +
#   # theme_hc()+ scale_colour_hc()+
#   # theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   # coord_flip()+
#   # scale_fill_brewer(palette="Dark2") + 
#   # theme_minimal()+
#   theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0), size=14),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
#         axis.text.y = element_text(size=12,face="bold"),
#         legend.position = "none",legend.title = element_blank(),
#         legend.text =element_text(size=7),  
#         legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
#         strip.text = element_text(size = 14)
#   )  
# 
# print(gg006)
# 
# gg006 + facet_wrap( ~ pred_sub_set, ncol=2, labeller = as_labeller(coverage_names) )
# 
# B_perf_data_summary_cov_all_covered =  B_perf_data_summary_cov_all %>%
#   filter(pred_sub_set == "covered")
# names(B_perf_data_summary_cov_all_covered)
# gg007=ggplot(B_perf_data_summary_cov_all_covered, aes(x=method, y=mean_coverage,fill=method))+ 
#   labs(title="", x="", y="Coverage") +
#   # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
#   geom_boxplot(outlier.shape = NA)+
#   # geom_jitter(width=0.1,alpha=0.1) +
#   # geom_line(alpha=0.15) +
#   # geom_point(alpha=0.15) +
#   # theme_hc()+ scale_colour_hc()+
#   # theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   # coord_flip()+
#   # scale_fill_brewer(palette="Dark2") + 
#   # theme_minimal()+
#   theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0), size=14),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
#         axis.text.y = element_text(size=12,face="bold"),
#         legend.position = "none",legend.title = element_blank(),
#         legend.text =element_text(size=7),  
#         legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
#         strip.text = element_text(size = 14)
#   )  
# 
# print(gg007)
# gg0071=ggplot(B_perf_data_summary_cov_all_covered, aes(x=method, y=mean_class_rules_nbr,fill=method))+ 
#   labs(title="", x="", y="Number of rules per class") +
#   # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
#   geom_boxplot(outlier.shape = NA)+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x))) +
#   # geom_jitter(width=0.1,alpha=0.1) +
#   # geom_line(alpha=0.15) +
#   # geom_point(alpha=0.15) +
#   # theme_hc()+ scale_colour_hc()+
#   # theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   # coord_flip()+
#   # scale_fill_brewer(palette="Dark2") + 
#   # theme_minimal()+
#   theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0), size=14),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14,face="bold"),
#         axis.text.y = element_text(size=12,face="bold"),
#         legend.position = "none",legend.title = element_blank(),
#         legend.text =element_text(size=7),  
#         legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
#         strip.text = element_text(size = 14)
#   )  
# 
# print(gg0071)



B_perf_data_summary_complexity_1=B_perf_ORE_summary_iter_based_multi%>% 
  filter(method %in% meth_lev)%>% 
  filter(pred_sub_set %in% c("covered")) %>% 
  filter(pred_set %in% c("test")) %>% 
  mutate(method = factor(method, levels=meth_lev))
unique(B_perf_data_summary_complexity$method)
B_perf_data_summary_complexity_2=B_perf_ORE_summary_iter_based_bin%>% 
  filter(method %in% meth_lev)%>% 
  filter(pred_sub_set %in% c("covered")) %>% 
  filter(pred_set %in% c("test")) %>% 
  mutate(method = factor(method, levels=meth_lev))
gg0081=ggplot(B_perf_data_summary_complexity_1, aes(x=mean_rules_nbr, y=mean_acc))+ 
  labs(title="", x="Rule Set Size (number of rules)", y="Accuracy") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  geom_point(aes(color=method),shape=17, size=4)+
  # geom_boxplot(outlier.shape = NA)+
  # geom_text(aes(label = paste0(method,"(acc=",round(mean_acc,3)," & cov=", percent(mean_coverage),")"),
  #               color=method,fontface="bold", size=10),nudge_y = 0.0015)+
  geom_text(aes(label = paste0(method," (cov=", percent(mean_coverage),")"),
                color=method,fontface="bold", size=10),nudge_y = -0.0013)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                limits=c(10^0,10^4)) +
  
  # geom_jitter(width=0.1,alpha=0.1) +
  # geom_line(alpha=0.15) +
  # geom_point(alpha=0.15) +
  theme(legend.position="none")
   
  
print(gg0081)
names(B_perf_data_summary_complexity)
gg0082=ggplot(B_perf_data_summary_complexity_2, aes(x=mean_rules_nbr, y=mean_acc))+ 
  labs(title="", x="Rule Set Size (number of rules)", y="Accuracy") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  geom_point(aes(color=method),shape=17, size=4)+
  # geom_boxplot(outlier.shape = NA)+
  # geom_text(aes(label = paste0(method,"(acc=",round(mean_acc,3)," & cov=", percent(mean_coverage),")"),
  #               color=method,fontface="bold", size=10),nudge_y = 0.0015)+
  geom_text(aes(label = paste0(method," (cov=", percent(mean_coverage),")"),
                color=method,fontface="bold", size=10),nudge_y = -0.0013)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                limits=c(10^0,10^4)) +
  
  # geom_jitter(width=0.1,alpha=0.1) +
  # geom_line(alpha=0.15) +
  # geom_point(alpha=0.15) +
  theme(legend.position="none")


print(gg0082)
names(B_perf_data_summary_complexity)

gg0091=ggplot(B_perf_data_summary_complexity_1, aes(x=mean_coverage, y=mean_acc))+ 
  labs(title="", x="Coverage", y="Accuracy") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  geom_point(aes(color=method),shape=17, size=5)+
  # geom_boxplot(outlier.shape = NA)+
  # geom_text(aes(label = paste0(method,"(acc=",round(mean_acc,3)," & cov=", percent(mean_coverage),")"),
  #               color=method,fontface="bold", size=10),nudge_y = 0.0015)+
  # geom_text(aes(label = paste0(method," (", round(mean_rules_nbr,0)," rules -- ",round(mean_class_rules_nbr,1)," rules/class)"),
  #               color=method,fontface="bold", size=9), nudge_y = -0.002) +
  geom_text(aes(label = paste0(method," (",round(mean_class_rules_nbr,1)," rules/class)"),
                color=method,fontface="bold", size=9), nudge_y = -0.002) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               limits=c(10^0,10^4)) +
  scale_x_continuous(limits=c(0.4,1.07)) +
  scale_y_continuous(limits=c(0.750,0.90))+
  
  # geom_jitter(width=0.1,alpha=0.1) +
  # geom_line(alpha=0.15) +
  # geom_point(alpha=0.15) +
  theme(legend.position="none",
        axis.title.y=element_text(face="bold", size=14),
        axis.title.x = element_text(size=14,face="bold"),
        axis.text.x = element_text(size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"))


print(gg0091)

gg0092=ggplot(B_perf_data_summary_complexity_2, aes(x=mean_coverage, y=mean_acc))+ 
  labs(title="", x="Coverage", y="Accuracy") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  geom_point(aes(color=method),shape=17, size=5)+
  # geom_boxplot(outlier.shape = NA)+
  # geom_text(aes(label = paste0(method,"(acc=",round(mean_acc,3)," & cov=", percent(mean_coverage),")"),
  #               color=method,fontface="bold", size=10),nudge_y = 0.0015)+
  # geom_text(aes(label = paste0(method," (", round(mean_rules_nbr,0)," rules -- ",round(mean_class_rules_nbr,1)," rules/class)"),
  #               color=method,fontface="bold", size=9), nudge_y = -0.002) +
  geom_text(aes(label = paste0(method," (",round(mean_class_rules_nbr,1)," rules/class)"),
                color=method,fontface="bold", size=9), nudge_y = -0.002) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               limits=c(10^0,10^4)) +
  scale_x_continuous(limits=c(0.2,1.08)) +
  scale_y_continuous(limits=c(0.79,0.90)) +
  # geom_jitter(width=0.1,alpha=0.1) +
  # geom_line(alpha=0.15) +
  # geom_point(alpha=0.15) +
  theme(legend.position="none",
        axis.title.y=element_text(face="bold", size=14),
        axis.title.x = element_text(size=14,face="bold"),
        axis.text.x = element_text(size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"))

print(gg0092)

names(B_perf_ORE_att)
gg010=ggplot(B_perf_ORE_att, aes(x=method, y=value, group=length,color=length))+ 
  labs(title="", x="", y="Rule Length") +
  # stat_summary(fun.data = "mean_cl_boot", geom = "point") +
  # geom_boxplot(outlier.shape = NA)+
  # geom_jitter(width=0.1,alpha=0.1) +
  geom_line(aes(group=method,)) +
  geom_point(shape=15) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # coord_flip()+
  # scale_fill_brewer(palette="Dark2") + 
  # theme_minimal()+
  theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10,face="bold"),
        legend.position = "top",legend.title = element_blank(),legend.text =element_text(size=7),  legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
  ) 
# +
#   scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                               "green","seagreen1","yellowgreen","seagreen",
#                               "pink","plum2","plum4","purple4",
#                               "steelblue2","steelblue","slateblue","slateblue4",
#                               "tan","tan1","tomato","tomato3"))
print(gg010)

# ablation study : plots
names(B_MIP_SOL_ORE_summary_perdata_abl)
gg1=ggplot(B_MIP_SOL_ORE_summary_perdata_abl, aes(x=abl_model, y=mean_overlapping_ratio,group = data, color=data))+ 
  labs(title="", x="", y="Error on covered instances") +
  geom_line() +
  geom_point() + 
  # theme_hc()+ scale_colour_hc()+
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
      axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x=element_text(size=14,face="bold"),
      legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
# + 
#   scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                               "green","seagreen1","yellowgreen","seagreen",
#                               "pink","plum2","plum4","purple4",
#                               "steelblue2","steelblue","slateblue","slateblue4",
#                               "tan","tan1","tomato","tomato3"))
# guides(fill=guide_legend(ncol=5))

print(gg1)
names(B_MIP_SOL_ORE_summary_perdata_abl)
names(B_exetime_abl_ORE_summary_perdata)
str(B_exetime_abl_ORE_summary_perdata)
gg20=ggplot(B_exetime_abl_ORE_summary_perdata, aes(x=abl_model, y=mean_extract_rules,group = data, color=data))+ 
  labs(title="", x="", y="Rule extraction: execution time (s)") +
  geom_line() +
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)),limits=c(10^0,10^4)) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position = "none"
  # ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
  # scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
  #                             "green","seagreen1","yellowgreen","seagreen",
  #                             "pink","plum2","plum4","purple4",
  #                             "steelblue2","steelblue","slateblue","slateblue4",
  #                             "tan","tan1","tomato","tomato3"))

print(gg20)
gg21=ggplot(B_exetime_abl_ORE_summary_perdata, aes(x=abl_model, y=mean_preselect_rules,group = data, color=data))+ 
  labs(title="", x="", y="Rule set construction (s)") +
  geom_line() +
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)),limits=c(10^0,10^4)) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position = "none"
  # ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
# scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                             "green","seagreen1","yellowgreen","seagreen",
#                             "pink","plum2","plum4","purple4",
#                             "steelblue2","steelblue","slateblue","slateblue4",
#                             "tan","tan1","tomato","tomato3"))

print(gg21)
gg22=ggplot(B_exetime_abl_ORE_summary_perdata, aes(x=abl_model, y=mean_prepare_opt_input,group = data, color=data))+ 
  labs(title="", x="", y="Preparation of MIP inputs (s)") +
  geom_line() +
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)),limits=c(10^0,10^4)) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position = "none"
  # ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
# scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                             "green","seagreen1","yellowgreen","seagreen",
#                             "pink","plum2","plum4","purple4",
#                             "steelblue2","steelblue","slateblue","slateblue4",
#                             "tan","tan1","tomato","tomato3"))

print(gg22)
mean_buildandrun_opt
gg23=ggplot(B_exetime_abl_ORE_summary_perdata, aes(x=abl_model, y=mean_buildandrun_opt,group = data, color=data))+ 
  labs(title="", x="", y="Building and executing MIP (s)") +
  geom_line() +
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)),limits=c(10^0,10^4)) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position = "none"
  # ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
# scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                             "green","seagreen1","yellowgreen","seagreen",
#                             "pink","plum2","plum4","purple4",
#                             "steelblue2","steelblue","slateblue","slateblue4",
#                             "tan","tan1","tomato","tomato3"))

print(gg23)

grid.newpage()
grid.draw(cbind(ggplotGrob(gg21), ggplotGrob(gg22),ggplotGrob(gg23)))

DF_exe_plot = B_exetime_abl_ORE_summary_perdata %>%
  select(abl_model,data, mean_preselect_rules, mean_prepare_opt_input,mean_buildandrun_opt) %>%
  gather(key= "stage", value="Execution_time", -abl_model, -data)
names(DF_exe_plot)
gg200= ggplot(DF_exe_plot, aes(x=abl_model, y=Execution_time,group = data, color=data))+ 
  labs(title="", x="", y="Opt_run Execution time (s)") +
  geom_line() +
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position = "none"
  # ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
# scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                             "green","seagreen1","yellowgreen","seagreen",
#                             "pink","plum2","plum4","purple4",
#                             "steelblue2","steelblue","slateblue","slateblue4",
#                             "tan","tan1","tomato","tomato3"))

print(gg200)
gg200+ facet_wrap(~stage, ncol=3)
gg21=ggplot(B_MIP_SOL_ORE_summary_perdata_abl, aes(x=abl_model, y=mean_opt_run_time,group = data, color=data))+ 
  labs(title="", x="", y="Opt_run Execution time (s)") +
  geom_line() +
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position = "none"
  # ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
# scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                             "green","seagreen1","yellowgreen","seagreen",
#                             "pink","plum2","plum4","purple4",
#                             "steelblue2","steelblue","slateblue","slateblue4",
#                             "tan","tan1","tomato","tomato3"))

print(gg21)
names(B_perf_ORE_summary_perdata_abl)
unique(B_perf_ORE_summary_perdata_abl$pred_sub_set)
B_perf_all_test_abl= B_perf_ORE_summary_perdata_abl %>%
  filter(method == "Forest-ORE") %>%
  filter(pred_sub_set == "all")
unique(B_perf_all_test_abl$abl_model)
B_perf_all_test_abl_v1 = B_perf_all_test_abl %>%
  filter(abl_model %in% c("abl_conf", "abl_supp", "abl_length", "abl_mod", "no_abl")) %>%
  mutate (abl_model = factor(abl_model))
names(B_perf_ORE_summary_perdata_abl)
gg41=ggplot(B_perf_all_test_abl_v1, aes(x=abl_model, y=mean_acc,group = data, color=data))+ 
  labs(title="", x="", y="Accuracy") +
  geom_line() +
  geom_point() + 
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position = "none"
  # ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
# scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                             "green","seagreen1","yellowgreen","seagreen",
#                             "pink","plum2","plum4","purple4",
#                             "steelblue2","steelblue","slateblue","slateblue4",
#                             "tan","tan1","tomato","tomato3"))

print(gg41)
gg41 + facet_wrap( ~ pred_set)
names(B_perf_all_test_abl)
B_perf_cov_test_abl= B_perf_ORE_summary_perdata_abl %>%
  filter(method == "Forest-ORE") %>%
  filter(pred_sub_set == "covered")
B_perf_cov_test_abl_v1= B_perf_cov_test_abl %>%
filter(abl_model %in% c("abl_conf", "abl_supp", "abl_length", "abl_mod", "no_abl")) %>%
  mutate (abl_model = factor(abl_model))
gg42=ggplot(B_perf_cov_test_abl_v1, aes(x=abl_model, y=mean_coverage,group = data, color=data))+ 
  labs(title="", x="", y="Coverage") +
  geom_line() +
  geom_point() + 
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #       axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
  #       legend.position = "none"
  # ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position="top",legend.title = element_blank(),legend.text=element_text(size=8)) 
# scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                             "green","seagreen1","yellowgreen","seagreen",
#                             "pink","plum2","plum4","purple4",
#                             "steelblue2","steelblue","slateblue","slateblue4",
#                             "tan","tan1","tomato","tomato3"))

print(gg42)
gg42 + facet_wrap( ~ pred_set)
gg3=ggplot(B_perf_disc_summary_perdata2, aes(x=disc_method, y=mean_NB_interval,group = name2, color=name2))+ 
  labs(title="", x="", y="Intervals number") +
  geom_line() +
  geom_point() + 
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position = "none"
        
  )+
  scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
                              "green","seagreen1","yellowgreen","seagreen",
                              "pink","plum2","plum4","purple4",
                              "steelblue2","steelblue","slateblue","slateblue4",
                              "tan","tan1","tomato","tomato3"))


print(gg3)
grid.newpage()
grid.draw(rbind(ggplotGrob(gg1), ggplotGrob(gg2),ggplotGrob(gg3), size = "last"))




# d1= B_perf_ORE_summary_iter_based %>% filter(pred_set=="test" & pred_sub_set=="covered")
# names(Data_radar1)
# names(d2)
# d2= B_perf_ORE_att_length_summary 
# Data_radar= sqldf('select d1.method, d1.mean_rules_nbr, d1.mean_class_rules_nbr, d2.mean_att_nbr, d1.mean_coverage, d1.mean_acc, d1.mean_F1 
#                 from d1 left join d2 on d1.method=d2.method')
# 
# Data_radar_path= ".\\benchmark_results\\rules_results\\Data_radar.csv"
# write.csv(Data_radar,Data_radar_path, row.names = FALSE)
# Data_radar0_path= ".\\benchmark_results\\rules_results\\Data_radar0.csv"
# write.csv(Data_radar0,Data_radar0_path, row.names = FALSE)
# 
# colnames(Data_radar)=c("method","Rules size", "Rule size per class",  "Rule length", "Coverage", "Accuracy", "F1 score")
# Data_radar0= Data_radar [,-3]
# Data_radar0[,2]=1-((Data_radar0[,2]-min(Data_radar0[,2]))/(max(Data_radar0[,2])-min(Data_radar0[,2])))
# Data_radar0[,3]=1-((Data_radar0[,3]-min(Data_radar0[,3]))/(max(Data_radar0[,3])-min(Data_radar0[,3])))
# 
# library(fmsb)
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# radarchart(Data_radar00, axistype=1 , 
#            #custom polygon
#             plwd=4 , plty=1,
#            #custom the grid
#            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,5), cglwd=0.8,
#            #custom labels
#            vlcex=0.8 
# )
# 
# # Add a legend
# legend(x=0.7, y=1, legend = rownames(Data_radar00[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
# Data_radar00=Data_radar0[,-1]
# rownames(Data_radar00)=Data_radar0[,1]
# 
# Data_radar00=rbind(rep(0,5), rep(1,5),Data_radar00)
# Data_radar00=Data_radar00[-c(4,5,6,10),]
# v2 <- scales::rescale(Data_radar$`Rules size`, to=c(0,1))
# # gg002=ggplot(B_perf_overall, aes(x=disc_method, y=mean_F1,group = data,color=data,lty=data,pch=data))+ 
# #   labs(title="", x="", y="F1 score") +
# #   geom_line() +
# #   geom_point() +
# #   # theme_hc()+ scale_colour_hc()+
# #   # theme_bw() +
# #   # scale_color_gradient(low="blue", high="red")+
# #   # annotation_logticks() +
# #   theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
# #         axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
# #         legend.position = "none"
# #   ) 
# # print(gg002)
# # gg003=ggplot(B_perf_overall, aes(x=disc_method, y=mean_Kappa,group = data,color=data,lty=data,pch=data))+ 
# #   labs(title="", x="", y="Kappa") +
# #   geom_line() +
# #   geom_point() +
# #   # theme_hc()+ scale_colour_hc()+
# #   # theme_bw() +
# #   # scale_color_gradient(low="blue", high="red")+
# #   # annotation_logticks() +
# #   theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=9,face="bold"), 
# #          axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
# #          legend.position = "none"
# #   )
# # 
# # print(gg003)
# # grid.newpage()
# # grid.draw(rbind( ggplotGrob(gg001), ggplotGrob(gg002),ggplotGrob(gg003), size = "last"))
# # # bins and exetime
# # names(B_perf_disc_summary_perdata2)
# # head(B_perf_disc_summary_perdata2)
# # unique(B_perf_disc_summary_perdata2$disc_method)
# # B_perf_disc_summary_perdata2=B_perf_disc_summary_perdata2[-which(B_perf_disc_summary_perdata2$disc_method %in% c("cont")),]
# # 
# # B_perf_disc_summary_perdata2$disc_method = factor(B_perf_disc_summary_perdata2$disc_method,levels=levDisc[order(levDisc)])
# # 
# # names(B_data_summary)
# # # rank (unique(as.numeric(B_perf_disc_summary_perdata2$nrow)))
# # B_data_summary1=B_data_summary[order(B_data_summary$nrow, decreasing = FALSE),]
# # B_data_summary1$name2=paste0(B_data_summary1$name,"(",B_data_summary1$nrow,")") 
# # unique(B_data_summary1$name2)
# # B_perf_disc_summary_perdata2$name=factor(B_perf_disc_summary_perdata2$name,levels=unique(B_data_summary1$name))
# # B_perf_disc_summary_perdata2$name2=paste0(B_perf_disc_summary_perdata2$name,"(",B_perf_disc_summary_perdata2$nrow,")") 
# # B_perf_disc_summary_perdata2$name2=factor(B_perf_disc_summary_perdata2$name2,levels=unique(B_data_summary1$name2))
# # # library(scales)
# # # library(wesanderson)
# # 
# # 
# # # boxplots exetime and intervals number  
# # names(B_perf_disc_summary_perdata2)
# # summary(B_perf_disc_summary_perdata2$mean_NB_interval)
# # A=tapply(B_perf_disc_summary_perdata2$mean_NB_interval, B_perf_disc_summary_perdata2$disc_method,mean)
# # B=tapply(B_perf_disc_summary_perdata2$mean_exetimepervar, B_perf_disc_summary_perdata2$disc_method,mean)
# # B=B[levDisc[order(levDisc)]]
# # A=A[levDisc[order(levDisc)]]
# # C=levDisc[order(levDisc)]
# # calculatedmean<-data.frame(cbind(C,A,B))
# # colnames(calculatedmean)=c("disc_method","overallmeanInterval","overallmeanExetime")
# # B_perf_disc_summary_perdata3 = sqldf('select d1.*, d2.* from B_perf_disc_summary_perdata2 d1 
# # LEFT JOIN calculatedmean d2 ON (  d1.disc_method = d2.disc_method)')
# # 
# # 
# # names(B_perf_disc_summary_perdata3)
# # B_perf_disc_summary_perdata3$overallmeanInterval=as.numeric(as.character(B_perf_disc_summary_perdata3$overallmeanInterval))
# # B_perf_disc_summary_perdata3$overallmeanExetime =as.numeric(as.character(B_perf_disc_summary_perdata3$overallmeanExetime))
# # summary(B_perf_disc_summary_perdata3$overallmeanInterval)
# # B_perf_disc_summary_perdata3=B_perf_disc_summary_perdata3[,-c(22,23)]#remove redundunt columns
# # gg01=ggplot(B_perf_disc_summary_perdata3, aes(x=disc_method))+ 
# #   geom_boxplot(aes(y=mean_NB_interval),outlier.shape = NA)+
# #   geom_jitter(aes(y=mean_NB_interval),width=0.1,alpha=0.1) +
# #   # geom_point(aes( y=overallmeanInterval,group=disc_method,color="tomato3")) + 
# #   # geom_line(aes( y=overallmeanInterval,group=disc_method,color="tomato3")) + 
# #   stat_summary(aes(y=overallmeanInterval,group = 1),fun.y=mean,color="tomato3", geom=c("line"),lty=3) +
# #   stat_summary(aes(y=overallmeanInterval,group = 1),fun.y=mean,color="tomato3", geom=c("point"),shape=16,size=2) +
# #   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
# #                 labels = trans_format("log10", math_format(10^.x)),limits=c(10^0,10^2)) +
# #   labs(title="", x="", y="Number of bins") +
# #   theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10,face="bold"), 
# #          axis.title.y=element_text(face="bold",size=10),
# #          legend.position = "none")
# # 
# #  
# #  
# # print(gg02)
# # grid.newpage()
# # grid.draw(rbind( ggplotGrob(gg01), ggplotGrob(gg02),size = "last"))
# # names(B_perf_disc_summary)
# # 
# # # 
# # # gg2=ggplot(B_perf_disc_summary_perdata2, aes(x=disc_method, y=mean_exetimepervar,group = name2, color=name2))+ 
# # #   labs(title="", x="", y="Execution time (s)") +
# # #   geom_line() +
# # #   geom_point() +
# # #   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
# # #                 labels = trans_format("log10", math_format(10^.x))) +
# # #   # theme_hc()+ scale_colour_hc()+
# #   # theme_bw() +
# #   # scale_color_gradient(low="blue", high="red")+
# #   # annotation_logticks() +
# #   theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
# #         axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
# #         legend.position = "none"
# #   ) 
# # # +
# # #   scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
# # #                               "green","seagreen1","yellowgreen","seagreen",
# # #                               "pink","plum2","plum4","purple4",
# # #                               "steelblue2","steelblue","slateblue","slateblue4",
# # #                               "tan","tan1","tomato","tomato3"))
# #   
# # 
# # 
# # 
# # gg3=ggplot(B_perf_disc_summary_perdata2, aes(x=disc_method, y=mean_NB_interval,group = name2, color=name2))+ 
# #   labs(title="", x="", y="Intervals number") +
# #   geom_line() +
# #   geom_point() + 
# #   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
# #                 labels = trans_format("log10", math_format(10^.x))) +
# #   # theme_hc()+ scale_colour_hc()+
# #   # theme_bw() +
# #   # scale_color_gradient(low="blue", high="red")+
# #   # annotation_logticks() +
# #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
# #         axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
# #         axis.title.x=element_text(size=14,face="bold"),
# #         legend.position = "none"
# #         
# #   )
# # # +
# #   # scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
# #   #                             "green","seagreen1","yellowgreen","seagreen",
# #   #                             "pink","plum2","plum4","purple4",
# #   #                             "steelblue2","steelblue","slateblue","slateblue4",
# #   #                             "tan","tan1","tomato","tomato3"))
# # 
# # 
# # print(gg3)
# # grid.newpage()
# grid.draw(rbind( ggplotGrob(gg2),ggplotGrob(gg3), size = "last"))
# 
# 
# 
# 
# # diff perf compared to the case where data is not discretized
# diffperfcont<-All_DIFFPERF_summary
# diffperfcont<-diffperfcont[-which(diffperfcont$disc_method=="cont"),]
# diffperfcont$disc_method = factor(diffperfcont$disc_method,levels=levDisc[order(levDisc)])
# names(diffperfcont)
# gg4=ggplot(diffperfcont, aes(x=disc_method)) + 
#   geom_line(aes(y = mean_diff_acc, group=1, color= "tomato3")) +
#   geom_point(aes( y = mean_diff_acc,group=1,shape="17",color= "tomato3"))+
#   geom_line(aes(y = mean_diff_F1, group=1,color= "slateblue4"))+
#   geom_point(aes(y = mean_diff_F1, group=1,shape="16",color= "slateblue4"))+
#   geom_line(aes(y = mean_diff_Kappa,group=1,color= "seagreen"))+
#   geom_point(aes( y = mean_diff_Kappa,group=1,shape="15",color= "seagreen"))+
#   # geom_line(aes(y = mean_relative_diff_acc, color= "tomato3", lty="2")) +
#   # geom_point(aes( y = mean_relative_diff_acc,shape="17",color= "tomato3"))+
#   # geom_line(aes(y = mean_relative_diff_F1, color= "slateblue4", lty="2"))+
#   # geom_point(aes(y = mean_relative_diff_F1, shape="16",color= "slateblue4"))+
#   # geom_line(aes(y = mean_relative_diff_Kappa,color= "seagreen", lty="2"))+
#   # geom_point(aes( y = mean_relative_diff_Kappa,shape="15",color= "seagreen"))+
#   scale_y_continuous(labels = scales::percent)+
#   theme_minimal() +
#   scale_colour_manual(name = "Metrics:",
#                       labels = c("Kappa", "F1 score", "Accuracy"),
#                       values = c("tomato3","slateblue4","seagreen"),
#                       guide = guide_legend(reverse = TRUE)) +
#   scale_shape_manual(name = "Metrics:",
#                      labels = c("Kappa", "F1 score", "Accuracy"),
#                      values = c(17,16,15),
#                      guide = guide_legend(reverse = TRUE))+
#   labs(title="", x="", y="Absolute loss") +
#   theme(legend.position = "top",axis.title.x = element_blank(), axis.text.x = element_blank())
# print(gg4)
# gg5=ggplot(diffperfcont, aes(x=disc_method)) + 
#   # geom_line(aes(y = mean_diff_acc, group=1, color= "tomato3")) +
#   # geom_point(aes( y = mean_diff_acc,group=1,shape="17",color= "tomato3"))+
#   # geom_line(aes(y = mean_diff_F1, group=1,color= "slateblue4"))+
#   # geom_point(aes(y = mean_diff_F1, group=1,shape="16",color= "slateblue4"))+
#   # geom_line(aes(y = mean_diff_Kappa,group=1,color= "seagreen"))+
#   # geom_point(aes( y = mean_diff_Kappa,group=1,shape="15",color= "seagreen"))+
#   geom_line(aes(y = mean_relative_diff_acc,group=1, color= "tomato3")) +
#   geom_point(aes( y = mean_relative_diff_acc,group=1,shape="17",color= "tomato3"))+
#   geom_line(aes(y = mean_relative_diff_F1,group=1,color= "slateblue4"))+
#   geom_point(aes(y = mean_relative_diff_F1,group=1,shape="16",color= "slateblue4"))+
#   geom_line(aes(y = mean_relative_diff_Kappa,group=1,color= "seagreen"))+
#   geom_point(aes( y = mean_relative_diff_Kappa,group=1,shape="15",color= "seagreen"))+
#   scale_y_continuous(labels = scales::percent)+
#   theme_minimal() +
#   labs(title="", x="", y="Relative loss") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
#         axis.title.x=element_text(size=14,face="bold"),legend.position="none")+
#   scale_colour_manual(name = "Metrics:",
#                       labels = c("Kappa", "F1 score", "Accuracy"),
#                       values = c("tomato3","slateblue4","seagreen"),
#                       guide = guide_legend(reverse = TRUE)) +
#   scale_shape_manual(name = "Metrics:",
#                      labels = c("Kappa", "F1 score", "Accuracy"),
#                      values = c(17,16,15),
#                      guide = guide_legend(reverse = TRUE)
#                      )
# 
#   
# 
# print(gg5)
# grid.newpage()
# grid.draw(rbind( ggplotGrob(gg4),ggplotGrob(gg5), size = "last"))
# 
# # perf per classifiers
# dfclassifiers<-B_perf_disc_summary_perclassifier
# dfclassifiers<-dfclassifiers[-which(dfclassifiers$disc_method=="cont"),]
# dfclassifiers$disc_method<-factor(dfclassifiers$disc_method,levels=levDisc[order(levDisc)])
# dfclassifiers$class_method<-factor(dfclassifiers$class_method,levels=unique(dfclassifiers$class_method)[c(5,3,4,1,2,6)])
# 
# unique(dfclassifiers$class_method)[c(5,3,4,1,2,6)]
# names(dfclassifiers)
# gg7=ggplot(dfclassifiers, aes(x=disc_method)) + 
#   # geom_line(aes(y = mean_diff_acc, group=1, color= "tomato3")) +
#   # geom_point(aes( y = mean_diff_acc,group=1,shape="17",color= "tomato3"))+
#   # geom_line(aes(y = mean_diff_F1, group=1,color= "slateblue4"))+
#   # geom_point(aes(y = mean_diff_F1, group=1,shape="16",color= "slateblue4"))+
#   # geom_line(aes(y = mean_diff_Kappa,group=1,color= "seagreen"))+
#   # geom_point(aes( y = mean_diff_Kappa,group=1,shape="15",color= "seagreen"))+
#   geom_line(aes(y = mean_acc,group=1, color= "tomato3")) +
#   geom_point(aes( y = mean_acc,group=1,shape="17",color= "tomato3"))+
#   geom_line(aes(y = mean_F1,group=1,color= "slateblue4"))+
#   geom_point(aes(y = mean_F1,group=1,shape="16",color= "slateblue4"))+
#   geom_line(aes(y = mean_Kappa,group=1,color= "seagreen"))+
#   geom_point(aes( y = mean_Kappa,group=1,shape="15",color= "seagreen"))+
#   theme_minimal() +
#   labs(title="", x="", y="") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
#         axis.title.x=element_text(size=14,face="bold"),legend.position="top")+
#   scale_colour_manual(name = "Metrics:",
#                       labels = c("Kappa", "F1 score", "Accuracy"),
#                       values = c("tomato3","slateblue4","seagreen"),
#                       guide = guide_legend(reverse = TRUE)) +
#   scale_shape_manual(name = "Metrics:",
#                      labels = c("Kappa", "F1 score", "Accuracy"),
#                      values = c(17,16,15),
#                      guide = guide_legend(reverse = TRUE)
#   )
# 
# print(gg7)
# gg7 + facet_wrap( ~ class_method, ncol=3,scales = "free_y")
# 
# 
# # wilcox per classifier
# count_wilcox_accuracy_perclassifier
# wilcox_accuracy_classifiers<-count_wilcox_accuracy_perclassifier
# wilcox_accuracy_classifiers$class_method=factor(wilcox_accuracy_classifiers$class_method,levels=unique(wilcox_accuracy_classifiers$class_method)[c(1,6,5,2,3,4)])
# # T= sqldf('select * from wilcox_accuracy_classifiers 
# #         order by class_method, Wins_count DESC,Ties_count DESC,Loss_count ASC')
# 
# wilcox_accuracy_classifiers=wilcox_accuracy_classifiers[with(wilcox_accuracy_classifiers, order(wilcox_accuracy_classifiers$class_method,-wilcox_accuracy_classifiers$Wins_count,-wilcox_accuracy_classifiers$Ties_count,wilcox_accuracy_classifiers$Loss_count)),]
# W_a_C_w=cbind(wilcox_accuracy_classifiers[,c(1,2,5)],rep("Wins"))
# colnames(W_a_C_w)=c("Discretizer","Score","class_method","Score_type")
# W_a_C_t=cbind(wilcox_accuracy_classifiers[,c(1,3,5)],rep("Ties"))
# colnames(W_a_C_t)=c("Discretizer","Score","class_method","Score_type")
# W_a_C_l=cbind(wilcox_accuracy_classifiers[,c(1,4,5)],rep("Losses"))
# colnames(W_a_C_l)=c("Discretizer","Score","class_method","Score_type")
# W_a_C=rbind(W_a_C_w,W_a_C_t,W_a_C_l)
# W_a_C$Score_type=factor(W_a_C$Score_type, level= unique(W_a_C$Score_type)[c(3,2,1)])
# W_a_C[,"Metric"]="Accuracy"
# # F1
# wilcox_macroF1_classifiers<-count_wilcox_macroF1_perclassifier
# wilcox_macroF1_classifiers$class_method=factor(wilcox_macroF1_classifiers$class_method,
#                                                levels=unique(wilcox_macroF1_classifiers$class_method)[c(1,6,5,2,3,4)])
# wilcox_macroF1_classifiers=wilcox_macroF1_classifiers[with(wilcox_macroF1_classifiers, order(wilcox_macroF1_classifiers$class_method,-wilcox_macroF1_classifiers$Wins_count,-wilcox_macroF1_classifiers$Ties_count,wilcox_macroF1_classifiers$Loss_count)),]
# W_a_C1_w=cbind(wilcox_macroF1_classifiers[,c(1,2,5)],rep("Wins"))
# colnames(W_a_C1_w)=c("Discretizer","Score","class_method","Score_type")
# W_a_C1_t=cbind(wilcox_macroF1_classifiers[,c(1,3,5)],rep("Ties"))
# colnames(W_a_C1_t)=c("Discretizer","Score","class_method","Score_type")
# W_a_C1_l=cbind(wilcox_macroF1_classifiers[,c(1,4,5)],rep("Losses"))
# colnames(W_a_C1_l)=c("Discretizer","Score","class_method","Score_type")
# W_a_C1=rbind(W_a_C1_w,W_a_C1_t,W_a_C1_l)
# W_a_C1$Score_type=factor(W_a_C1$Score_type, level= unique(W_a_C1$Score_type)[c(3,2,1)])
# W_a_C1[,"Metric"]="F1 score"
# # kappa
# wilcox_kappa_classifiers<-count_wilcox_kappa_perclassifier
# wilcox_kappa_classifiers$class_method=factor(wilcox_kappa_classifiers$class_method,
#                                                levels=unique(wilcox_kappa_classifiers$class_method)[c(1,6,5,2,3,4)])
# wilcox_kappa_classifiers=wilcox_kappa_classifiers[with(wilcox_kappa_classifiers, order(wilcox_kappa_classifiers$class_method,-wilcox_kappa_classifiers$Wins_count,-wilcox_kappa_classifiers$Ties_count,wilcox_kappa_classifiers$Loss_count)),]
# W_a_C2_w=cbind(wilcox_kappa_classifiers[,c(1,2,5)],rep("Wins"))
# colnames(W_a_C2_w)=c("Discretizer","Score","class_method","Score_type")
# W_a_C2_t=cbind(wilcox_kappa_classifiers[,c(1,3,5)],rep("Ties"))
# colnames(W_a_C2_t)=c("Discretizer","Score","class_method","Score_type")
# W_a_C2_l=cbind(wilcox_kappa_classifiers[,c(1,4,5)],rep("Losses"))
# colnames(W_a_C2_l)=c("Discretizer","Score","class_method","Score_type")
# W_a_C2=rbind(W_a_C2_w,W_a_C2_t,W_a_C2_l)
# W_a_C2$Score_type=factor(W_a_C2$Score_type, level= unique(W_a_C2$Score_type)[c(3,2,1)])
# W_a_C2[,"Metric"]="Kappa"
# df=rbind(W_a_C,W_a_C1,W_a_C2)
# 
# df_RF=df[which(df$class_method=="RF"),]
# df_KNNC=df[which(df$class_method=="KNNC"),]
# df_NaiveBayes=df[which(df$class_method=="NaiveBayes"),]
# df_CART=df[which(df$class_method=="CART"),]
# df_Boosting=df[which(df$class_method=="Boosting"),]
# df_SVM=df[which(df$class_method=="SVM"),]
# # RF KNNC NaiveBayes CART Boosting SVM
# levels_RF=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="RF")])
# levels_KNNC=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="KNNC")])
# levels_NaiveBayes=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="NaiveBayes")])
# levels_CART=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="CART")])
# levels_Boosting=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="Boosting")])
# levels_SVM=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="SVM")])
# 
# 
# df_RF$Discretizer=factor(df_RF$Discretizer,levels=rev(levels_RF))
# df_KNNC$Discretizer=factor(df_KNNC$Discretizer,levels=rev(levels_KNNC))
# df_NaiveBayes$Discretizer=factor(df_NaiveBayes$Discretizer,levels=rev(levels_NaiveBayes))
# df_CART$Discretizer=factor(df_CART$Discretizer,levels=rev(levels_CART))
# df_Boosting$Discretizer=factor(df_Boosting$Discretizer,levels=rev(levels_Boosting))
# df_SVM$Discretizer=factor(df_SVM$Discretizer,levels=rev(levels_SVM))
# 
# # W_a_C_RF=W_a_C[which(W_a_C$class_method=="RF"),]
# # W_a_C_RF$Discretizer=factor(W_a_C_RF$Discretizer,levels= unique(W_a_C_RF$Discretizer))
# 
# # W_a_C$Discretizer=factor(W_a_C$Discretizer)
# # + coord_flip()
# library(ggplot2)
# gg80=ggplot(df_RF, aes(x=Discretizer,y=Score,fill=Score_type)) + 
#   geom_bar(position="stack", stat="identity")+
#   scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
#   # scale_y_discrete(position = "right") +
#   coord_flip()+
#   labs(title="", x="RF", y="") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.text.y = element_text(size=8),
#         axis.title.x=element_text(face="bold"),
#         legend.position="top",
#         axis.title.y = element_text(face="bold"))
#  
# print(gg80)
# gg080=gg80 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
#   theme(strip.text = element_text(face="bold",size=9),
#         axis.text.x=element_blank(),
#         plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
# print(gg080)
# gg81=ggplot(df_KNNC, aes(x=Discretizer,y=Score,fill=Score_type)) + 
#   geom_bar(position="stack", stat="identity")+
#   scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
#   # scale_y_discrete(position = "right") +
#   coord_flip()+
#   labs(title="", x="KNNC", y="") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.title.x=element_text(face="bold"),legend.position="none",
#         axis.text.y = element_text(size=8),
#         axis.title.y = element_text(face="bold"))
#         # axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
# print(gg81)
# gg081=gg81 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
#   theme(strip.text = element_text(face="bold"),
#         strip.text.x = element_blank(),
#         axis.text.x=element_blank(),
#         plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
# print(gg081)
# 
# gg82=ggplot(df_NaiveBayes, aes(x=Discretizer,y=Score,fill=Score_type)) + 
#   geom_bar(position="stack", stat="identity")+
#   scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
#   # scale_y_discrete(position = "right") +
#   coord_flip()+
#   labs(title="", x="NaiveBayes", y="") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.title.x=element_text(face="bold"),legend.position="none",
#         axis.text.y = element_text(size=8),
#         axis.title.y = element_text( face="bold"))
# # axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
# print(gg82)
# gg082=gg82 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
#   theme(strip.text = element_text(face="bold"),
#         strip.text.x = element_blank(),
#         plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
# print(gg082)
# 
# gg83=ggplot(df_CART, aes(x=Discretizer,y=Score,fill=Score_type)) + 
#   geom_bar(position="stack", stat="identity")+
#   scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
#   # scale_y_discrete(position = "right") +
#   coord_flip()+
#   labs(title="", x="CART", y="") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.title.x=element_text(face="bold"),legend.position="none",
#         axis.text.y = element_text(size=8),
#         axis.title.y = element_text( face="bold"))
# # axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
# print(gg83)
# gg083=gg83 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
#   theme(strip.text = element_text(face="bold"),
#         axis.text.x=element_blank(),
#         plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
# print(gg083)
# 
# gg84=ggplot(df_Boosting, aes(x=Discretizer,y=Score,fill=Score_type)) + 
#   geom_bar(position="stack", stat="identity")+
#   scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
#   # scale_y_discrete(position = "right") +
#   coord_flip()+
#   labs(title="", x="Boosting", y="") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.title.x=element_text(face="bold"),legend.position="none",
#         axis.text.y = element_text(size=8),
#         axis.title.y = element_text( face="bold"))
# # axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
# print(gg84)
# gg084=gg84 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
#   theme(strip.text = element_text(face="bold"),
#         strip.text.x = element_blank(),
#         axis.text.x=element_blank(),
#         plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
# print(gg084)
# 
# gg85=ggplot(df_SVM, aes(x=Discretizer,y=Score,fill=Score_type)) + 
#   geom_bar(position="stack", stat="identity")+
#   scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
#   # scale_y_discrete(position = "right") +
#   coord_flip()+
#   labs(title="", x="SVM", y="") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,face="bold"),
#         axis.title.x=element_text(face="bold"),
#         axis.text.y = element_text(size=8),
#         legend.position="none",
#         axis.title.y = element_text( face="bold"))
# # axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
# print(gg85)
# gg085=gg85 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
#   theme(strip.text = element_text(face="bold"),
#         strip.text.x = element_blank(),
#         plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
# print(gg085)
# 
# # library(grid)
# grid.newpage()
# grid.draw(rbind(ggplotGrob(gg080),
#                 ggplotGrob(gg081),
#                 ggplotGrob(gg082)))
# 
# grid.newpage()
# grid.draw(rbind(ggplotGrob(gg082),
#                 ggplotGrob(gg083)
# ))
# grid.newpage()
# grid.draw(rbind(ggplotGrob(gg084),
#                 ggplotGrob(gg085)
# ))