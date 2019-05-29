
################ Functions to extract CI, put in a dataframe, and plot a figure ###############
# input comes from an lmer model
# M.Mauritz Feb 2016, adapted from V.Salmon's CI extraction code
# see defined functions and instructions below


######################## DEFINE FUNCTIONS TO EXTRACT AND GRAPH CI #########################
#Extract the coefficients for the fixed effects from your model, make a dataframe with them called model
# x is the name of the lmer model
# eg: model1.reco <- lmer(Reco.sum ~ WW * SW * time + 
# (1 | block/fence_2/wholeplot/subplot4) + (1|year), 
# data = data2)
# so, to use this funciton, assign the output to a dataframe: 
# ci.model1reco <- extract_ci(model1.reco)
extract_ci <- function(x) {coefs<-fixef(x) 
                           modeldf<-as.data.frame(coefs)
                           #calculate confidence intervals; merge fixed effects and ci into one dataframe
                           ci <- confint(x,method="boot",boot.type="norm",level=0.95,nsim=1000)
                           modelci<-merge(ci,modeldf,by="row.names",all.x=F)
                           #rename colnames so that they make sense and remove symbols
                           colnames(modelci)<-c("term","min","max","coefs")
                           return (modelci)}

# graph CI
# input: 
# ci is the confidence interval data frame
# figtitle is a user defined title for the CI graph, must be entered in " "
# model is the name of the lmer model, and is input to the function because the title 
# of the figure will contain an AIC score for the model
# so,
# graph_ci(ci_model1reco, "Reco", model1.reco)
# will print a graph with the parameter estimate and the 2.5% and 97.5% CI

graph_ci <- function(ci,figtitle,model) {ggplot(ci,aes(x=names,y=coefs))+
                                           geom_errorbar(aes(ymin=min,ymax=max),width=0,size=1)+
                                           geom_point(aes(size=2))+
                                           labs (title = paste(figtitle, ", AIC:", round(AIC(model),2), sep =" ") , x = "Fixed effect", y = "Effect size and 95% CI") +
                                           guides(size=F,shape=F)+
                                           theme_bw()+
                                           theme(axis.text.x=element_text(size=18),
                                                 axis.title.x=element_text(size=26),
                                                 axis.title.y=element_text(size=26,vjust=1),
                                                 axis.text.y=element_text(size=22),
                                                 panel.grid.minor=element_blank(),
                                                 panel.grid.major.x=element_blank())+
                                           geom_hline(yintercept=0)+
                                           coord_flip() } 
#################################################################################

# More detailed instructions

# # to use this code completely you have to do this: 
# RUN MODEL
# model1.reco <- lmer(Reco.sum ~ WW * SW * time + # use * or + depending on the interactions you want to specify
#                       (1 | block/fence_2/wholeplot/subplot4) + (1|year), 
#                     data = data2)
# 
# # extract and graph CI using the function defined below
# ci.model1reco <- extract_ci(model1.reco)
#
# # name fixed effects nice names for graphing - you will have to look at ci.model1reco to make sure 
# # the names fit your purpose.
# # CAUTION: check that the order here matches the dataframe modelci
# ci.model1reco$names<-c("Intercept","air","air * time","time","soil","air * soil","air * soil * time","soil * time")
# # fix order of names so graph plots fixed effect factors in the same order regardless of the values
# # (default is to arrange the fixed effects by magnitude of effect size)
# ci.model1reco$names<-factor(ci.model1reco$names,levels=c("Intercept","air", "soil", "air * soil", "time", "air * time","soil * time","air * soil * time"))
# 
# graph_ci(ci.model1reco, "Reco", model1.reco)

# # These next lines allow you to double check that the function has output the right things
# # print AIC
# AIC(model1.reco)
# 
# # # display output of model and the effect size of each fixed effect
# # summary(model1.reco)
# # # display the confidence interval around each fixed effect
# # confint(model1.reco,method="boot",boot.type="norm",level=0.95,nsim=1000)
# 


