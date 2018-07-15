library(tidyverse)

rand  <- c(1, .9, .8, .7, .6, .5, .4, .3, .2, .1, 0)
lab <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)

randomData <-  tibble('TPR' = rand,'FPR' = rand)

classifierData1 <-  tibble('TPR'=c(1, .98, .94, .89, .82, .81, .8, .75, .72, .62, 0), 
                           'FPR'=c(1, .89, .8, .6, .4, .38, .23, .1, .08, .02, 0), 'label'=lab)

classifierData2 <-  tibble('TPR'=c(1, .97, .91, .90, .85, .82, .78, .75, .74, .61, 0), 
                           'FPR'=c(1, .85, .7, .6, .35, .2, .07, .04, .012, .01, 0), 'label'=lab)

# plot ROC curve
ggplot(randomData, aes(FPR, TPR)) +
    geom_line(size=1, alpha=0.7, color='red') +
    geom_line(data=classifierData1, size=1, color='purple') +
    geom_line(data=classifierData2, size=1, color='blue') +
    geom_label(data=classifierData1, aes(label=classifierData1$label), size=2.5) +
    geom_label(data=classifierData2, aes(label=classifierData2$label), size=2.5) +
    labs(title= "ROC curve", 
         x = "False Positive Rate (1 - Specificity)", 
         y = "True Positive Rate (Sensitivity)")

# calculate AUC
AUC1 = mean(sample(classifierData1$TPR, 1000, replace=T) > sample(classifierData1$FPR, 1000, replace=T))
AUC2 = mean(sample(classifierData2$TPR, 1000, replace=T) > sample(classifierData2$FPR, 1000, replace=T))
