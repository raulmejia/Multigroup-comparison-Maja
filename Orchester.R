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
if (!require("dplyr")) {
  install.packages("dplyr", ask =FALSE)
  library("dplyr")
}
if (!require("MASS")) {
  install.packages("MASS", ask =FALSE)
  library("MASS")
}
if (!require("broom")) {
  install.packages("broom", ask =FALSE)
  library("broom")
}

if (!require("fitdistrplus")) {
  install.packages("fitdistrplus", ask =FALSE)
  library("fitdistrplus")
}

#myargs <- commandArgs(trailingOnly = TRUE)

### Data given by the user
# path2yourdf <-myargs[1]
path2yourdffull <-  c("/media/rmejia/mountme88/Projects/Maja/Data/Mouse_data_cast.csv")
path2yourdfbelow90 <- c("/media/rmejia/mountme88/Projects/Maja/Data/Mouse_data_cast_no_outlier.csv")
path2yourdfbelow30 <- c("/media/rmejia/mountme88/Projects/Maja/Data/Mouse_data_cast_no_outliers_in_C57BL_above30.csv")
path2yourdfbelow20 <- c("/media/rmejia/mountme88/Projects/Maja/Data/Mouse_data_cast_no_outliers_in_C57BL_above20.csv")
# your table should have the column name "Groups" 

# varofint <- myargs[1] # col_name_of_your_variable_of_interest
varofint <- "cast_no_NMA"  # col_name_of_your_variable_of_interest
##

dffull <- read.table(path2yourdffull, sep="\t", header=TRUE, stringsAsFactors = FALSE)
dfbelow90 <- read.table(path2yourdfbelow90, sep="\t", header=TRUE, stringsAsFactors = FALSE)
dfbelow30 <- read.table(path2yourdfbelow30, sep="\t", header=TRUE, stringsAsFactors = FALSE)
dfbelow20 <- read.table(path2yourdfbelow20, sep="\t", header=TRUE, stringsAsFactors = FALSE)

df_list <- list(dffull, dfbelow90, dfbelow30)
names(df_list) <- c("dffull","dfbelow90","dfbelow30")

df_list_mitlog <- list()
#for( k in names(df_list) ){
#  df_list[[k]] <- as_tibble(df_list[[k]])
# df_list_mitlog[[k]] <- df_list[[k]] %>% mutate( log2 = log(df_list[[k]]$cast_no_NMA+1) )
#} 

ggplot(data= dfbelow20 , aes(x=cast_no_NMA, color=Groups )) + geom_density() + xlim(0,50)



# creating
df_list_mitlog[["dffull"]] <- df_list[["dffull"]] %>% mutate( log2 = log(df_list[["dffull"]]$cast_no_NMA+1) )
df_list_mitlog[["dfbelow90"]] <- df_list[["dfbelow90"]] %>% mutate( log2 = log(df_list[["dfbelow90"]]$cast_no_NMA+1) )
df_list_mitlog[["dfbelow30"]] <- df_list[["dfbelow30"]] %>% mutate( log2 = log(df_list[["dfbelow30"]]$cast_no_NMA+1) )

ggplot(data=df_list_mitlog[["dffull"]], aes(x=cast_no_NMA, color=Groups )) + geom_density() + ggtitle("full")
ggplot(data=df_list_mitlog[["dffull"]], aes(x=log2, color=Groups )) + geom_density() + ggtitle("full")
splitted_mit_log_full <- split( df_list_mitlog[["dffull"]] ,  df_list_mitlog[["dffull"]]$Groups )
summarize(splitted_mit_log_full[["C57BL/6"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_full[["THP-/-"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_full[["TLR4-/-"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_full[["C57BL/6"]], mean = mean(log2) ) 
summarize(splitted_mit_log_full[["THP-/-"]], mean = mean(log2) ) 
summarize(splitted_mit_log_full[["TLR4-/-"]], mean = mean(log2) ) 
summarize(splitted_mit_log_full[["C57BL/6"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_full[["THP-/-"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_full[["TLR4-/-"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_full[["C57BL/6"]], sd = sd(log2) ) 
summarize(splitted_mit_log_full[["THP-/-"]], sd = sd(log2) ) 
summarize(splitted_mit_log_full[["TLR4-/-"]], sd = sd(log2) ) 

splitted_mit_log_below90[["C57BL/6"]]$cast_no_NMA
mitzerosall <-c(0,7,3,5,0,0,1,2,6,7,34,39,21,96,13,0,0,0,0,0,0,0,0,0)
sd(mitzerosall)
mean(mitzerosall)
mitzerosu90 <-c(0,7,3,5,0,0,1,2,6,7,34,39,21,13,0,0,0,0,0,0,0,0,0)
sd(mitzerosu90)
mean( mitzerosu90)
mitzerosu30 <-c(0,7,3,5,0,0,1,2,6,7,21,13,0,0,0,0,0,0,0,0,0)
sd(mitzerosu30)
mean( mitzerosu30)

nozerosall <-c(7,3,5,1,2,6,7,34,39,21,96,13)
sd(nozerosall)
mean(nozerosall)
nozerosu90 <-c(7,3,5,1,2,6,7,34,39,21,13)
sd(nozerosu90)
mean( nozerosu90)
nozerosu30 <-c(7,3,5,1,2,6,7,21,13)
sd(nozerosu30)
mean( nozerosu30)



ggplot(data=df_list_mitlog[["dfbelow90"]], aes(x=cast_no_NMA, color=Groups )) + geom_density() + ggtitle("below 90")
ggplot(data=df_list_mitlog[["dfbelow90"]], aes(x=log2, color=Groups )) + geom_density() + ggtitle("below 90")
splitted_mit_log_below90 <- split( df_list_mitlog[["dfbelow90"]] ,  df_list_mitlog[["dfbelow90"]]$Groups )
summarize(splitted_mit_log_below90[["C57BL/6"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_below90[["THP-/-"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_below90[["TLR4-/-"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_below90[["C57BL/6"]], mean = mean(log2) ) 
summarize(splitted_mit_log_below90[["THP-/-"]], mean = mean(log2) ) 
summarize(splitted_mit_log_below90[["TLR4-/-"]], mean = mean(log2) ) 
summarize(splitted_mit_log_below90[["C57BL/6"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_below90[["THP-/-"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_below90[["TLR4-/-"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_below90[["C57BL/6"]], sd = sd(log2) ) 
summarize(splitted_mit_log_below90[["THP-/-"]], sd = sd(log2) ) 
summarize(splitted_mit_log_below90[["TLR4-/-"]], sd = sd(log2) ) 


ggplot(data=df_list_mitlog[["dfbelow30"]], aes(x=cast_no_NMA, color=Groups )) + geom_density() + ggtitle("below 30")
ggplot(data=df_list_mitlog[["dfbelow30"]], aes(x=log2, color=Groups )) + geom_density() + ggtitle("below 30")
splitted_mit_log_below30 <- split( df_list_mitlog[["dfbelow30"]] ,  df_list_mitlog[["dfbelow30"]]$Groups )
summarize(splitted_mit_log_below30[["C57BL/6"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_below30[["THP-/-"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_below30[["TLR4-/-"]], mean = mean(cast_no_NMA) ) 
summarize(splitted_mit_log_below30[["C57BL/6"]], mean = mean(log2) ) 
summarize(splitted_mit_log_below30[["THP-/-"]], mean = mean(log2) ) 
summarize(splitted_mit_log_below30[["TLR4-/-"]], mean = mean(log2) ) 
summarize(splitted_mit_log_below30[["C57BL/6"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_below30[["THP-/-"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_below30[["TLR4-/-"]], sd = sd(cast_no_NMA) ) 
summarize(splitted_mit_log_below30[["C57BL/6"]], sd = sd(log2) ) 
summarize(splitted_mit_log_below30[["THP-/-"]], sd = sd(log2) ) 
summarize(splitted_mit_log_below30[["TLR4-/-"]], sd = sd(log2) ) 


df_below20 <- df_list_mitlog[["dfbelow30"]] %>% filter( < )



df_list_mitlog_nozeros <- list()
df_list_mitlog_nozeros[["dffull"]] <- df_list_mitlog[["dffull"]] %>% filter( cast_no_NMA != 0 )
df_list_mitlog_nozeros[["dfbelow90"]] <- df_list_mitlog[["dfbelow90"]] %>% filter( cast_no_NMA != 0 )
df_list_mitlog_nozeros[["dfbelow30"]] <- df_list_mitlog[["dfbelow30"]] %>% filter( cast_no_NMA != 0 )

ggplot(data=df_list_mitlog_nozeros[["dffull"]], aes(x=cast_no_NMA, color=Groups )) + geom_density() + ggtitle("Full no zeros")
ggplot(data=df_list_mitlog_nozeros[["dffull"]], aes(x=log2, color=Groups )) + geom_density() + ggtitle("Full no zeros")
df_list_mitlog_nozeros_full <- split( df_list_mitlog_nozeros[["dffull"]] ,  df_list_mitlog_nozeros[["dffull"]]$Groups )
summarize(df_list_mitlog_nozeros_full[["C57BL/6"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_full[["THP-/-"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_full[["TLR4-/-"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_full[["C57BL/6"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_full[["THP-/-"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_full[["TLR4-/-"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_full[["C57BL/6"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_full[["THP-/-"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_full[["TLR4-/-"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_full[["C57BL/6"]], sd = sd( log2 ) ) 
summarize(df_list_mitlog_nozeros_full[["THP-/-"]], sd = sd( log2 ) ) 
summarize(df_list_mitlog_nozeros_full[["TLR4-/-"]], sd = sd( log2 ) ) 


ggplot(data=df_list_mitlog_nozeros[["dfbelow90"]], aes(x=cast_no_NMA, color=Groups )) + geom_density()
ggplot(data=df_list_mitlog_nozeros[["dfbelow90"]], aes(x=log2, color=Groups )) + geom_density()
df_list_mitlog_nozeros_below90 <- split( df_list_mitlog_nozeros[["dfbelow90"]] ,  df_list_mitlog_nozeros[["dfbelow90"]]$Groups )
summarize(df_list_mitlog_nozeros_below90[["C57BL/6"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below90[["THP-/-"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below90[["TLR4-/-"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below90[["C57BL/6"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_below90[["THP-/-"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_below90[["TLR4-/-"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_below90[["C57BL/6"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below90[["THP-/-"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below90[["TLR4-/-"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below90[["C57BL/6"]], sd = sd( log2 ) ) 
summarize(df_list_mitlog_nozeros_below90[["THP-/-"]], sd = sd( log2 ) ) 
summarize(df_list_mitlog_nozeros_below90[["TLR4-/-"]], sd = sd( log2 ) ) 


ggplot(data=df_list_mitlog_nozeros[["dfbelow30"]], aes(x=cast_no_NMA, color=Groups )) + geom_density()
ggplot(data=df_list_mitlog_nozeros[["dfbelow30"]], aes(x=log2, color=Groups )) + geom_density()
df_list_mitlog_nozeros_below30 <- split( df_list_mitlog_nozeros[["dfbelow30"]] ,  df_list_mitlog_nozeros[["dfbelow30"]]$Groups )
summarize(df_list_mitlog_nozeros_below30[["C57BL/6"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below30[["THP-/-"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below30[["TLR4-/-"]], mean = mean( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below30[["C57BL/6"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_below30[["THP-/-"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_below30[["TLR4-/-"]], mean = mean( log2 ) ) 
summarize(df_list_mitlog_nozeros_below30[["C57BL/6"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below30[["THP-/-"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below30[["TLR4-/-"]], sd = sd( cast_no_NMA ) ) 
summarize(df_list_mitlog_nozeros_below30[["C57BL/6"]], sd = sd( log2 ) ) 
summarize(df_list_mitlog_nozeros_below30[["THP-/-"]], sd = sd( log2 ) ) 
summarize(df_list_mitlog_nozeros_below30[["TLR4-/-"]], sd = sd( log2 ) ) 




summarize( splitted_mit_log_below90, mean = mean(cast_no_NMA) ) 

mean(as.data.frame(splitted_mit_log_below90[[1]])$cast_no_NMA)

lapply(splitted_mit_log_below90, summarise( mean = mean(cast_no_NMA)))


?split()
data("mtcars")
mtcars %>%  summarise(mean = mean(disp), n = n())

as.data.frame(means)
summarise(df_list_mitlog[["dfbelow90"]], .groups = Groups)

?summarise


head(df_list_mitlog_nozeros[["dfbelow30"]], n=50)
as.data.frame(df_list_mitlog_nozeros[["dfbelow30"]])

as.data.frame(df_list_mitlog[["dffull"]])

mean_fromcast<- function(x){
  z<- as.data.frame(x[,"cast_no_NMA"])
  print(z)
  return(mean(z["cast_no_NMA"]) )
}

mean_fromcast(df_list_mitlog[["dffull"]])
  
lapply(df_list_mitlog, fun(x){ meanx[,"cast_no_NMA"]} )
?lapply

ggplot( )

6/12 = 50% are zeros
2/9 are zeros TLR4
12/24 = 50%  are zeros C57BL/9


dft <- as_tibble(df)
log_df <- dft %>% mutate( log2 = log(dft$cast_no_NMA+1) ) 

?mutate
df_splitted <- split(df,df$Groups)

df_splitted


str(df)

lapply(df_splitted, function(x){sd(x[,varofint]) } )
lapply(df_splitted, function(x){mean(x[,varofint]) } )

# Exploring the data
p <- ggplot( df, aes(x=cast_no_NMA, color=Groups))+
  geom_density()
p
ggplot( data=log_df, aes(x=log2, color=Groups))+
  geom_density()
ggplot( data=log_df, aes(y=log2, color=Groups))+
  geom_boxplot()

box <- ggplot( df, aes(y=cast_no_NMA, color=Groups))+
  geom_boxplot() 
box
box + geom_point(df, aes(y=cast_no_NMA, color=Groups))
rlang::last_error()


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


#####
glm.nb(formula = Frequency ~ Groups, data = Data, init.theta = 4.577879741, 
       link = log)
df

tibble(length_cm = 20:40 )


glm.nb(formula = cast_no_NMA ~ Groups, data = df, 
       link = log)

ml_no_outliers_above_30 <- glm.nb(formula = cast_no_NMA ~ Groups, data = df, 
       link = log)

str(ml_no_outliers_above_30)
class(ml_no_outliers_above_30)
ggplot( data=ml_no_outliers_above_30$model , aes( x= cast_no_NMA, color = Groups) ) + geom_density()

ggplot( data=ml_no_outliers_above_90$model , aes( x= cast_no_NMA, color = Groups) ) + geom_density()


library(car)
windows(height=4, width=7)
layout(matrix(1:2, nrow=1))
set.seed(246)
dft <- data.frame(rs   = c(rnegbin(n=100, mu=2, theta=1),
                          rnegbin(n=30,  mu=3, theta=10)),
                 type = c(rep("A", times=100), rep("B", times=30)))
qqPlot(dft$rs[which(dft$type=="A")])
qqPlot(dft$rs[which(dft$type=="B")])

qqPlot(sort(df$cast_no_NMA[which(df$Groups=="C57BL/6")]))

sort(df$cast_no_NMA[which(df$Groups=="C57BL/6")])

qqPlot(log(c(1,2,3,5,6,7,7,13,21)+1))
qqPlot(log(c(1,2,3,5,6,7,7,13,21,34,39)+1))
qqPlot(log(c(1,2,3,5,6,7,7,13,21,34,39, 90)+1))
qqPlot(log(c(1,2,3,5,6,7,7,13,21,34,39, 90)+1), dist= "gamma")
qqPlot(log(c(1,2,3,5,6,7,7,13,21,34,39, 90)+1), dist= "pois", lambda=10)
qqPlot(log(c(1,2,3,5,6,7,7,13,21,34,39, 90)+1), dist= "nbinom", size = 12, prob = .8)
qqPlot(log(c(1,2,3,5,6,7,7,13,21,34,39, 90)+1), dist= "nbinom", size = 12, prob = .2)

qqPlot(log(c(0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,5,6,7,7,13,21,34,39, 90)+1))
qqPlot(log(c(0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,5,6,7,7,13,21,34,39)+1))


?qqPlot

plot(log(c(1,2,3,5,6,7,7,13,21,34,39)+1))
plot(log(c(1,2,3,5,6,7,7,13,21,34,39, 90)+1))


df_list$dffull$cast_no_NMA[df_list$dffull$Groups=="C57BL/6"]


qqplot(1:10)
qqplot(df$cast_no_NMA[which(df$Groups=="C57BL/6")])







ml_no_outliers_above_90$model


ml_no_outliers_above_90 <- glm.nb(formula = cast_no_NMA ~ Groups, data = df, 
                                  link = log)
ml_all_values <- glm.nb(formula = cast_no_NMA ~ Groups, data = df, 
                                  link = log)


library('fitdistrplus')
df = read.table(text = 'Var1 Freq
 1975   10
 1976   12
 1977    9
 1978   14
 1979   14
 1980   11
 1981    8
 1982    7
 1983   10
 1984    8
 1985   12
 1986    9
 1987   10
 1988    9
 1989   10
 1990    9
 1991   11
 1992   12
 1993    9
 1994   10', header = TRUE)

https://stackoverflow.com/questions/40083035/how-to-create-a-q-q-plot-with-poisson-as-theoretical-distribution

ggplot(data = df,
       mapping = aes(sample = Freq)) + 
  stat_qq(distribution = stats::qpois,
          dparams = list(lambda = mean(df$Freq))) + 
  geom_step(data = data.frame(x = 6:16,
                              Freq = 6:16),
            mapping = aes(x = x,
                          y = Freq),
            colour = "red",
            alpha = 0.5) 

predict(ml_all_values, cast_no_NMA)

predict()

ml_all_values$
  
summary(ml_no_outliers_above_30)
summary(ml_no_outliers_above_90)
summary(ml_all_values)

tidy(ml_no_outliers_above_30)
tidy(ml_no_outliers_above_90)
tidy(ml_all_values)


secondtable<- read.table("/media/rmejia/mountme88/Projects/Maja/Data/All_mice.csv",sep="\t",header=TRUE)

table272h <- secondtable %>% filter( time == "72h")
table272h_Chains <- table272h %>% filter( Chains != "A/L") 
table272h_Chains_log2 <- table272h_Chains %>% mutate( log2 = log(cast_no+1))
ggplot( data = table272h_Chains_log2 , aes( x = cast_no, color= Groups) ) + geom_density()
ggplot( data = table272h_Chains_log2 , aes( x = log2, color= Groups) ) + geom_density()+ ggtitle("T & H")

table272h_ChainH <- table272h %>% filter( Chains == "H") 
table272h_ChainH_log2 <- table272h_ChainH %>% mutate( log2 = log(cast_no+1))
ggplot( data=table272h_ChainH_log2 , aes(x=cast_no, color=Groups  ) ) + geom_density() 
ggplot( data=table272h_ChainH_log2 , aes(x=log2, color=Groups  ) ) + geom_density() + ggtitle(" Chain lambda H")

table272h_ChainT <- table272h %>% filter( Chains == "T") 
table272h_ChainT_log2 <- table272h_ChainT %>% mutate( log2 = log(cast_no+1))
ggplot( data = table272h_ChainT_log2 , aes(x=cast_no, color= Groups) ) + geom_density()
ggplot( data = table272h_ChainT_log2 , aes(x=cast_no, color= Groups) ) + geom_histogram()
ggplot( data = table272h_ChainT_log2 , aes(x=log2, color= Groups) ) + geom_density() + ggtitle(" Chain kappa T")
ggplot( data = table272h_ChainT_log2 , aes(x=log2, color= Groups) ) + geom_histogram()


table272h_ChainsAL <- table272h %>% filter( Chains == "A/L") 
table272h_ChainsAL_log2 <- table272h_ChainsAL %>% mutate( log2 = log(cast_no+1))
ggplot( data= table272h_ChainsAL_log2 , aes( x=cast_no , color= Groups)) + geom_density()
ggplot( data= table272h_ChainsAL_log2 , aes( x=log2 , color= Groups)) + geom_density()

                                                                                     