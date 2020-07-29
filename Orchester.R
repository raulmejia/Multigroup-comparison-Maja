# 3 group comparison 
### loading required libraries
if (!require("BiocManager")) {
  install.packages("BiocManager", ask =FALSE)
  library("BiocManager")
}
if (!require("limma")) {
  BiocManager::install("limma", ask =FALSE)
  library("limma")
}
if (!require("ggplot2")) {
  BiocManager::install("ggplot2", ask =FALSE)
  library("ggplot2")
}
if (!require("moments")) {
  install.packages("moments", ask =FALSE)
  library("moments")
}
moments

#myargs <- commandArgs(trailingOnly = TRUE)

### Data given by the user
# path2yourdf <-myargs[1]
path2yourdf <- c("/media/rmejia/mountme88/Projects/Maja/Data/Mouse_data_cast_no_outlier.csv")
# your table should have the column name "Groups" 

# varofint <- myargs[1] # col_name_of_your_variable_of_interest
varofint <- "cast_no_NMA"  # col_name_of_your_variable_of_interest
##
df <- read.table(path2yourdf, sep="\t", header=TRUE)
str(df)
df_splitted <- split(df,df$Groups)

lapply(df_splitted, function(x){sd(x[,varofint]) } )


p<- ggplot( df, aes(x=cast_no_NMA, color=Groups))+
  geom_density()
p

ggplot
boxplot(df_splitted[[1]][,varofint])
plotDensities(df_splitted[[1]][,varofint])
sort(df_splitted[[1]][,varofint])
plotDensities(df_splitted[[2]][,varofint])
sort(df_splitted[[2]][,varofint])
plotDensities(df_splitted[[3]][,varofint])
sort(df_splitted[[3]][,varofint])

###########
df$Groups <- as.factor(df$Groups)
Group1 <- BiocGenerics::subset(df, Groups == "C57BL/6")
Group2 <- BiocGenerics::subset(df, Groups == "THP-/-")
Group3 <- BiocGenerics::subset(df, Groups == "TLR4-/-")

qqnorm(Group1$cast_no_NMA)
qqline(Group1$cast_no_NMA)

qqnorm(Group2$cast_no_NMA)
qqline(Group2$cast_no_NMA)

qqnorm(Group3$cast_no_NMA)
qqline(Group3$cast_no_NMA)

########
bartlett.test(cast_no_NMA ~ Groups, data= df)

######

kruskal.test(cast_no_NMA ~ Groups, data= df)

##### One way Welch ANOVA
oneway.test(cast_no_NMA ~ Groups, data= df, var.equal = FALSE)


res = residuals( lm( data=df ,cast_no_NMA ~ Groups ))
model = aov(data=df ,cast_no_NMA ~ Groups)
res= model$residuals
shapiro.test(res) 
agostino.test(res)
# the residuals are not normally distributed so ANOVA´s assumptions aren´t fulfilled

Further attempts
# https://www.researchgate.net/post/I_have_non-normal_data_with_heterogeneity_of_variance_which_ANOVA-type_procedure_and_post_hoc_tests_should_I_use
