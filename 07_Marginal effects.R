########################################################################################################################
# (7) ANCESTRY CULTURE AND FERTILITY (Marginal effects)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Dec 17th, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table);library(khb);library(ggplot2);library(sjPlot);library(ggpubr)

# DATA LOADING
load('khb.output.RData')

########################################################################################################################

# MARGINAL EFFECTS PLOT

theme_set(theme_light()) # Light theme

# A different panel per group of origin
p1 <- plot_model(km12$adjusted,type='pred',
           term=c('std.age [all]','X[FIN,1.5NWE,2NWE,2.5NWE]'),
           colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
           title= 'Northwest Europe',legend.title='',
           axis.title=c('','Predicted probabilities of parenthood')) 
p2 <- plot_model(km12$adjusted,type='pred',
           term=c('std.age [all]','X[FIN,1.5SEE,2SEE,2.5SEE]'),
           colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
           title= 'South & East Europe',show.legend =FALSE,
           axis.title=c('',''))
p3 <- plot_model(km12$adjusted,type='pred',
           term=c('std.age [all]','X[FIN,1.5USSR,2USSR,2.5USSR]'),
           colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
           title= 'Former USSR',show.legend =FALSE,
           axis.title=c('',''))
p4 <- plot_model(km12$adjusted,type='pred',
           term=c('std.age [all]','X[FIN,1.5MENA,2MENA,2.5MENA]'),
           colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
           title= 'Middle East & North Africa',show.legend =FALSE,
           axis.title=c('Age (standardized)',''))
p5 <- plot_model(km12$adjusted,type='pred',
           term=c('std.age [all]','X[FIN,1.5AFR,2AFR,2.5AFR]'),
           colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
           title= 'Sub-Saharan Africa',show.legend =FALSE,
           axis.title=c('Age (standardized)','Predicted probabilities of parenthood'))
p6 <- plot_model(km12$adjusted,type='pred',
           term=c('std.age [all]','X[FIN,1.5SEA,2SEA,2.5SEA]'),
           colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
           title= 'South & Southeast Asia',show.legend =FALSE,
           axis.title=c('Age (standardized)',''))
p7 <- plot_model(km12$adjusted,type='pred',
           term=c('std.age [all]','X[FIN,1.5EAAP,2EAAP,2.5EAAP]'),
           colors=c('black','firebrick2','darkgoldenrod','dodgerblue'),
           title= 'East Asia, Americas & Pacific',show.legend =FALSE,
           axis.title=c('Age (standardized)',''))

################################

# Change labels of the legend
p1$data$group_col <- factor(p1$data$group,labels=c('Native','1.5G migrant','2G migrant','2.5G migrant'))

jpeg(filename="marg.effects.jpeg",width=12,height=6.5,units='in',res=500)
ggarrange(p1,p2,p3,p4,p5,p6,p7,
          nrow=2,ncol=4,common.legend = TRUE)
dev.off()

########################################################################################################################