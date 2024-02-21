########################################################################################################################
# (7) ANCESTRY CULTURE AND FERTILITY (Marginal effects)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Dec 17th, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table);library(khb);library(ggplot2);library(sjPlot);library(ggpubr);library(splines)

# DATA LOADING
load('modeling.data.RData')
load('with.splines.RData')
load('khb.output.RData')

########################################################################################################################

theme_set(theme_bw()) # bw theme

# PLOT COMPARING REDUCED AND ADJUSTED MODELS

adjsum <- summary(km12$adjusted)
redsum <- summary(km12$reduced)

adjmod <- data.table(origin = names(km12$adjusted$coefficients),
                     coeff = km12$adjusted$coefficients,
                     se = adjsum$coefficients[,'Std. Error'],
                     model = "Adjusted model",
                     type = c('General',rep('Focal',21),rep('General',14)))
redmod <- data.table(origin = names(km12$reduced$coefficients),
                     coeff = km12$reduced$coefficients,
                     se = redsum$coefficients[,'Std. Error'],
                     model = "Reduced model",
                     type = c('General',rep('Focal',21),rep('General',8)))

comp.model.plot <- rbind(adjmod,redmod)
comp.model.plot <- comp.model.plot[type == 'Focal']

comp.model.plot[,origin := factor(rep(1:21,2),levels=1:21,
                                  labels = c('Northwest Europe (1.5G)','Northwest Europe (2G)','Northwest Europe (2.5G)',
                                             'South & East Europe (1.5G)','South & East Europe (2G)','South & East Europe (2.5G)',
                                             'Former USSR (1.5G)','Former USSR (2G)','Former USSR (2.5G)',
                                             'Middle East & North Africa (1.5G)','Middle East & North Africa (2G)','Middle East & North Africa (2.5G)',
                                             'Sub-Saharan Africa (1.5G)','Sub-Saharan Africa (2G)','Sub-Saharan Africa (2.5G)',
                                             'South & Southeast Asia (1.5G)','South & Southeast Asia (2G)','South & Southeast Asia (2.5G)',
                                             'East Asia, Americas & Pacific (1.5G)','East Asia, Americas & Pacific (2G)','East Asia, Americas & Pacific (2.5G)'))]

comp.mod.plot <- ggplot(data = comp.model.plot,
                        aes(x=coeff,y=factor(origin,levels=rev(levels(origin))),color=model,shape=model)) +
  geom_vline(xintercept = 0,color='black',linetype='dashed') +
  geom_pointrange(aes(xmin=coeff - se*qnorm(.975),xmax=coeff + se*qnorm(.975)),
                  size=1.25,alpha=.75) +
  xlab('Log-Odds') + ylab('') + labs(color='',shape='') +
  scale_color_manual(values=c('steelblue','gold2')) +
  theme(legend.position = 'top')

########################################################################################################################

# MARGINAL EFFECTS PLOT

# A different panel per group of origin
p1 <- plot_model(model4,type='pred',
                 term=c('start [all]','X[FIN,1.5NWE,2NWE,2.5NWE]'),
                 colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
                 title= 'Northwest Europe',show.legend =FALSE,
                 axis.title=c('Age','Predicted probabilities of parenthood'),
                 axis.lim = list(c(15,35),c(0,0.3)))
p2 <- plot_model(model4,type='pred',
                 term=c('start [all]','X[FIN,1.5SEE,2SEE,2.5SEE]'),
                 colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
                 title= 'South & East Europe',show.legend =FALSE,
                 axis.title=c('Age',''),
                 axis.lim = list(c(15,35),c(0,0.3)))
p3 <- plot_model(model4,type='pred',
                 term=c('start [all]','X[FIN,1.5USSR,2USSR,2.5USSR]'),
                 colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
                 title= 'Former USSR',show.legend =FALSE,
                 axis.title=c('Age',''),
                 axis.lim = list(c(15,35),c(0,0.3)))
p4 <- plot_model(model4,type='pred',
                 term=c('start [all]','X[FIN,1.5MENA,2MENA,2.5MENA]'),
                 colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
                 title= 'Middle East & North Africa',show.legend =FALSE,
                 axis.title=c('',''),
                 axis.lim = list(c(15,35),c(0,0.3)))
p5 <- plot_model(model4,type='pred',
                 term=c('start [all]','X[FIN,1.5AFR,2AFR,2.5AFR]'),
                 colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
                 title= 'Sub-Saharan Africa',show.legend =FALSE,
                 axis.title=c('',''),
                 axis.lim = list(c(15,35),c(0,0.3)))
p6 <- plot_model(model4,type='pred',
                 term=c('start [all]','X[FIN,1.5SEA,2SEA,2.5SEA]'),
                 colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
                 title= 'South & Southeast Asia',show.legend =FALSE,
                 axis.title=c('',''),
                 axis.lim = list(c(15,35),c(0,0.3)))
p7 <- plot_model(model4,type='pred',
                 term=c('start [all]','X[FIN,1.5EAAP,2EAAP,2.5EAAP]'),
                 colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
                 title= 'East Asia, Americas & Pacific',show.legend =FALSE,
                 axis.title=c('Age',''),
                 axis.lim = list(c(15,35),c(0,0.3)))

########################################################################################################################

# Visualization
jpeg(filename="model.comparisson.jpeg",width=12,height=13,units='in',res=500)
ggarrange(
  ggarrange(comp.mod.plot,
            ggarrange(p4,p5,p6,labels=c('E','F','G'),nrow=3,ncol=1,common.legend = TRUE),ncol=2,widths=c(3,1)),
  ggarrange(p1,p2,p3,p7,labels=c('B','C','D','H'),nrow=1,ncol=4),labels=c('A',''),nrow=2,ncol=1,heights = c(3,1))
dev.off()

########################################################################################################################