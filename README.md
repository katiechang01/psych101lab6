# psych101lab6
###in discussion

#load dada

#rename data
mega <- mega_class_data_FA20
head(mega)
names(mega)

#install necesarry packages
install.packages('psych')
library(psych)

#create a scale
extrapos.df <- cbind(mega$e1, mega$e2) #combining these
  #two variables to be extraversion positive dataframe
extraneg.df <- cbind(mega$e4r, mega$e5r, mega$e5r) #combining 
  #these two variables to be extraversion negative dataframe

#need to the know the range so then they can be recoded
range(mega$e1) #the range is 1 to 5 

#recode the negative
extranegR.df <- 6 - extraneg.df

#labeling the new dataset after recoding
EXTRA.df <- cbind(extrapos.df, extranegR.df)

#create average extraversion score for each participant and add to the sub dataset
mega$EXTRA <- rowMeans(EXTRA.df, na.rm = T)

#create a hisogram of this new dataset
hist(mega$EXTRA, ylim = c(0,20))

psych::alpha(EXTRA.df) 

#new model 
#DV: extraversion
#IV: hrs.zoom
plot(mega$EXTRA)
hist(mega$EXTRA)
hist(mega$hrs.zoom)

mega$hrs.zoom 
plot(mega$hrs.zoom) 

#plot the relationship between IV and DV
plot(EXTRA ~ hrs.zoom, data = mega)

#define a model to describe this relationship 
extramod <- lm(EXTRA ~ hrs.zoom, data = mega)

#add a line to graph to illustrate this model (question 3d)
abline(extramod, lwd = 4, col = "pink") 

#report intercept, slope, and R2 for model
extramod 

2.64848 + 0.01121 * 1
2.64848 + 0.01121 * 6

summary(extramod)$r.squared 
