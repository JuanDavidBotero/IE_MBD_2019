######################################Loading the reqd. libraries~###########################################


#install.packages("leaflet") # package for plotting maps  https://rstudio.github.io/leaflet/ (link for understanding)
library(leaflet)

### to  convert into date
library(lubridate)

# Package for dealing with missing values (using MICE - Multivariate Imputation by Chained Equation)
### https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ (this is a link to understand how mice works)
#install.packages("mice")
library(mice)


#install.packages("outliers")
library(outliers)
help("outlier")

library(data.table)

#install.packages("cluster"); ##### for clustering
library(cluster);

#install.packages("factoextra") ##### for getting the silhouette 
library(factoextra) 

library(doParallel);
library(foreach)

registerDoParallel(cores = detectCores());

#install.packages("ggcorrplot")
library(ggcorrplot)##### for plotting the heatmap 

library(ggplot2)
library(reshape2)



library(dplyr) ### for standardising 


library(purrr) ### for applying the map function

#install.packages("furrr")
library(furrr) ### for parallelising
library(stringr)

#install.packages("xgboost")
library(xgboost)

#install.packages("kableExtra")
library(kableExtra)

#########################################LOADING ALL THE DATA SETS##########################################


### 1st data set with all the target variables
solar_data<-readRDS("solar_dataset.RData")
str(solar_data)
class(solar_data)
head(solar_data)
summary(solar_data)

target_data<- solar_data[,c(1:99)] ###splitting the 98 target variables from the PCA ones

target_train_data<-target_data[c(1:5113),] ##### splitting the target_data into test and train 
target_test_data<-target_data[-c(1:5113)]


summary(target_test_data)
summary(target_train_data)

pca_data<-solar_data[,-c(2:99)] #### splitting all the PCA from the target 
#pca_train_data<- pca_data[c(1:5113)]





#### 2nd data set information about the location of the stations
station_info<-read.csv("station_info.csv")
View(station_info)
class(station_info)
head(station_info)
summary(station_info)
#### potentially clustering could be done on the basis of this data set



#### 3rd data set additional variables (this data set to be used for EDA )
additional_variables<-readRDS("additional_variables.RData")
View(additional_variables)

head(additional_variables)
summary(additional_variables) # Min_val, 1st_quartile, Median, Mean, 3rd_quartile all are close to 0 but the Max_val is significantly greater 
##ALSO HAS MISSING VALUES 

additional_variables[,1]

class(additional_variables$Date)  ### character to be converted into date 
class(ymd(additional_variables$Date)) ##### using the ymd function from the lubridate package
additional_variables$Date<-ymd(additional_variables$Date)
class(additional_variables$Date)

###########################################         EDA          #############################################

############################################ Data Visualisation ##############################################


# The following for loop will generate all the power plots for the 98 stations, to cycle through them
# please use the arrow buttons on the plot window.

for (i in 2:99) {
  
  
  ### Create plot
  power_plot <- ggplot(as.data.table(solar_data), 
                    aes(x=lubridate::ymd(solar_data$Date), y = as.numeric(solar_data[[i]])));
  
  ### Add layer of points
  power_plot <- power_plot + geom_line(col = "blue");
  
  ### Add layer to set main and axis titles
  power_plot <- power_plot + 
    labs(title="Solar Data Energy", subtitle=sprintf("Power Plant %s Power Output", colnames(solar_data)[i]), 
         y="Power Output", x= "Date");
  ### To converting the metric on Y axis from scientific to readable
  power_plot <- power_plot +  scale_y_continuous(breaks=seq(0, max(solar_data[[i]], na.rm=TRUE),2500000), labels = scales::comma)
  
  ### Print plot
  print(power_plot);
}


##########################Plotting on a map based on latitudes & longitudes#############################

### using leaflet library 
m <- leaflet() %>%   #### "%>%" is a pipeline 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=station_info$elon, lat=station_info$nlat ,popup=station_info$stid)###(add something maybe avgs)
m  # Print the map
class(m)
class(solar_data)
head(solar_data[,c(2:99)])



############################Dealing with the missing values using MICE##################################


# md.pattern(additional_variables)

# temp_additional_variables<- mice(additional_variables,m=5,maxit=50,meth='pmm',seed=500)  ##### WARNING DO NOT RUN THIS (will take more than an)

summary(temp_additional_variables)

write.csv(completedData,"~/Desktop/IE/R programming/project/Dataset/completedData.csv") #### Saving as a CSV so that the previous code is not needed to run again and again

summary(completedData)

completedData<- fread("completedData.csv")
completedData<- completedData[,-1]
head(completedData)
tail(completedData)

set.seed(14)
###############################################Counting the Outliers##########################################

completedData_2 <- completedData[,-c(1)]
z <- sapply(completedData_2,function(x){IQR <- quantile(x,0.75) - quantile(x,0.25); outliers <- sum(((x > 1.5*IQR + quantile(x, 0.75)) | (x < quantile(x,0.25)-1.5*IQR))); return(outliers)});
df_z <- data.frame(z)

#### Number of outliers 
df_z



################################################Correlation Analysis##############################################



head(completedData)
#http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

#Completed data heatmap
cor_completed_data <- round(cor(completedData[,-c(1)]),2)
cor_completed_data

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(completedData[,-c(1)])
head(p.mat[, 1:4])

my_plot <- ggcorrplot(cor_completed_data, hc.order = TRUE,
                      type = "lower", p.mat = p.mat, outline.col = "white",
                      lab = FALSE, title = "Completed Data Correlation Matrix") 

my_plot <- my_plot + theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

print(my_plot)

############################################KMEANS-CLUSTERING###########################################
##### performing clustering 

##https://medium.com/codesmart/r-series-k-means-clustering-silhouette-794774b46586 (for calculating the silhouette)
summary_target<-sapply(target_train_data[,-1], summary) #### getting the summary for all the power plants 
target_summary_transposed<-t(summary_target) ### transposing the matrix so that this can be joined with station_info

View(target_summary_transposed)

station_info_with_summary<- cbind(station_info,target_summary_transposed) ##### appending station summary with station info
head(station_info_with_summary)

### standardising in order to perform clustering
station_info_with_summary_standardised <- station_info_with_summary %>% mutate_each_(funs(scale(.) %>% as.vector), 
                             vars=c(2:10))

head(station_info_with_summary_standardised)



View(station_info_with_summary_standardised)
clustering <- kmeans(station_info_with_summary_standardised[,c(6,7,8,9)], centers = 2, nstart = 25)
# head(station_info_with_summary[,c(-1)])
# class(station_info_clustered)


silhouette_score <- function(k){
  km <- kmeans(station_info_with_summary_standardised[,c(6,7,8,9)], centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(station_info_with_summary_standardised[,-1]))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE,main = "Silhouette Score vs Clusters")


fviz_nbclust(station_info_with_summary_standardised[c(6,7,8,9)], kmeans, method='silhouette')

# Visualize clusters
clusplot(station_info_with_summary_standardised[,c(6,7,8,9)], clustering$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0, xlab = "", ylab = "", main = "k-means")




#############################################PCA#########################################################
completed_data_standardised <- completedData %>% mutate_each_(funs(scale(.) %>% as.vector), 
                             vars=c(2:101))
summary(completed_data_standardised)  ### standardising the variables in order to perform PCA


prin_comp <- prcomp(completed_data_standardised[,c(-1)], center = TRUE, scale. = TRUE)
class(prin_comp)
str(prin_comp)

# Get dataset after PCA
pca_dat <- prin_comp$x # method 1
pca_dat_2 <- data.table(predict(prin_comp, newdata = completed_data_standardised)); # method 2
head(pca_dat);
head(pca_dat_2);

# Choose number of variables using explained variance
summary(prin_comp);
prop_variance_explained <- summary(prin_comp)$importance[3,];
threshold_variance <- 0.9; # Threshold selected to maintain consistency with PCA conducted to create the 357 PCAs in the solar_data dataset
number_of_variables <- min(which(prop_variance_explained > threshold_variance))
final_pca_dat <- pca_dat[,1:number_of_variables];
head(final_pca_dat)

final_pca_data<- as.data.table(final_pca_dat)

colnames(solar_data)
head(solar_data$PC1)
head(final_pca_data$PC1)
 

### renaming the PCA's so that they dont have the same name in the solar data set
renamed_PCA<-final_pca_data %>% 
  rename(
   PC358 =PC1,
   PC359 = PC2,
   PC360 = PC3,
   PC361 = PC4,
   PC362 = PC5,
   PC363 = PC6,
   PC364 = PC7,
   PC365 = PC8,
   PC366 = PC9,
   PC367 = PC10,
   PC368 = PC11,
   PC369 = PC12,
   PC370 = PC13,
   PC371 = PC14,
   PC372 = PC15,
   PC373 = PC16,
   PC374 = PC17,
   PC375 = PC18,
   PC376 = PC19,
   PC377 = PC20,
   PC378 = PC21,
  )
head(renamed_PCA)


solar_data_complete<- cbind(solar_data,renamed_PCA) ##### appendding the PCA's calculated from addditional variables

class(solar_data_complete$Date)

solar_data_complete$Date<- ymd(solar_data_complete$Date)

dim(solar_data_complete)
colnames(solar_data_complete)

#############################################LINEAR - MODEL####################################################

train_dt<- solar_data_complete[c(1:4090),] #### the data set used for training 
validation_dt<-solar_data_complete[c(4091:5113),] #### the data set used for validation
test_dt<- solar_data_complete[-c(1:5113),] #### the final data set fot testing the model
dim(train_dt)
dim(validation_dt)
dim(test_dt)


write.csv(train_dt,"train_dt.csv")
write.csv(validation_dt,"validation_dt.csv")
write.csv(test_dt,"test_dt.csv")


paste0(colnames(train_dt[,-c(1:99)]), collapse = "+") # get the formula for the model automatically


df_total <- data.table()

for (station_no in 2:99){
  train_set <- cbind(train_dt[,-c(1:99)], train_dt[,..station_no])
  validation_set <- cbind(validation_dt[,-c(1:99)], validation_dt[,..station_no])
  test_set <- cbind(test_dt[,-c(1:99)], test_dt[,..station_no])
  station <-as.integer(unlist(train_dt[,..station_no]))
  
  model <- lm(station~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+PC40+PC41+PC42+PC43+PC44+PC45+PC46+PC47+PC48+PC49+PC50+PC51+PC52+PC53+PC54+PC55+PC56+PC57+PC58+PC59+PC60+PC61+PC62+PC63+PC64+PC65+PC66+PC67+PC68+PC69+PC70+PC71+PC72+PC73+PC74+PC75+PC76+PC77+PC78+PC79+PC80+PC81+PC82+PC83+PC84+PC85+PC86+PC87+PC88+PC89+PC90+PC91+PC92+PC93+PC94+PC95+PC96+PC97+PC98+PC99+PC100+PC101+PC102+PC103+PC104+PC105+PC106+PC107+PC108+PC109+PC110+PC111+PC112+PC113+PC114+PC115+PC116+PC117+PC118+PC119+PC120+PC121+PC122+PC123+PC124+PC125+PC126+PC127+PC128+PC129+PC130+PC131+PC132+PC133+PC134+PC135+PC136+PC137+PC138+PC139+PC140+PC141+PC142+PC143+PC144+PC145+PC146+PC147+PC148+PC149+PC150+PC151+PC152+PC153+PC154+PC155+PC156+PC157+PC158+PC159+PC160+PC161+PC162+PC163+PC164+PC165+PC166+PC167+PC168+PC169+PC170+PC171+PC172+PC173+PC174+PC175+PC176+PC177+PC178+PC179+PC180+PC181+PC182+PC183+PC184+PC185+PC186+PC187+PC188+PC189+PC190+PC191+PC192+PC193+PC194+PC195+PC196+PC197+PC198+PC199+PC200+PC201+PC202+PC203+PC204+PC205+PC206+PC207+PC208+PC209+PC210+PC211+PC212+PC213+PC214+PC215+PC216+PC217+PC218+PC219+PC220+PC221+PC222+PC223+PC224+PC225+PC226+PC227+PC228+PC229+PC230+PC231+PC232+PC233+PC234+PC235+PC236+PC237+PC238+PC239+PC240+PC241+PC242+PC243+PC244+PC245+PC246+PC247+PC248+PC249+PC250+PC251+PC252+PC253+PC254+PC255+PC256+PC257+PC258+PC259+PC260+PC261+PC262+PC263+PC264+PC265+PC266+PC267+PC268+PC269+PC270+PC271+PC272+PC273+PC274+PC275+PC276+PC277+PC278+PC279+PC280+PC281+PC282+PC283+PC284+PC285+PC286+PC287+PC288+PC289+PC290+PC291+PC292+PC293+PC294+PC295+PC296+PC297+PC298+PC299+PC300+PC301+PC302+PC303+PC304+PC305+PC306+PC307+PC308+PC309+PC310+PC311+PC312+PC313+PC314+PC315+PC316+PC317+PC318+PC319+PC320+PC321+PC322+PC323+PC324+PC325+PC326+PC327+PC328+PC329+PC330+PC331+PC332+PC333+PC334+PC335+PC336+PC337+PC338+PC339+PC340+PC341+PC342+PC343+PC344+PC345+PC346+PC347+PC348+PC349+PC350+PC351+PC352+PC353+PC354+PC355+PC356+PC357+PC358+PC359+PC360+PC361+PC362+PC363+PC364+PC365+PC366+PC367+PC368+PC369+PC370+PC371+PC372+PC373+PC374+PC375+PC376+PC377+PC378, data = train_set)
  
  df <-as.data.table(predict(model, newdata = test_set))
  df_total <- cbind(df_total,df)
  
}
### time taken to run this code is 108.788 seconds 

colnames(df_total) <- colnames(train_dt[,c(2:99)])
final <- cbind(test_dt[,c(1)], df_total)
final$Date <- as.character(final$Date)


x <- "-" 
final$Date <- str_replace_all(final$Date, pattern = x, replacement = "")
write.csv(final, "LM_PRED.csv", row.names=FALSE ) ### 1st submission file for KAGGLE Score = 2311728.08030




#### Running the same code but faster


df_total_map<-data.table()

station_model<- function(station){
  model<- lm(station ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+PC40+PC41+PC42+PC43+PC44+PC45+PC46+PC47+PC48+PC49+PC50+PC51+PC52+PC53+PC54+PC55+PC56+PC57+PC58+PC59+PC60+PC61+PC62+PC63+PC64+PC65+PC66+PC67+PC68+PC69+PC70+PC71+PC72+PC73+PC74+PC75+PC76+PC77+PC78+PC79+PC80+PC81+PC82+PC83+PC84+PC85+PC86+PC87+PC88+PC89+PC90+PC91+PC92+PC93+PC94+PC95+PC96+PC97+PC98+PC99+PC100+PC101+PC102+PC103+PC104+PC105+PC106+PC107+PC108+PC109+PC110+PC111+PC112+PC113+PC114+PC115+PC116+PC117+PC118+PC119+PC120+PC121+PC122+PC123+PC124+PC125+PC126+PC127+PC128+PC129+PC130+PC131+PC132+PC133+PC134+PC135+PC136+PC137+PC138+PC139+PC140+PC141+PC142+PC143+PC144+PC145+PC146+PC147+PC148+PC149+PC150+PC151+PC152+PC153+PC154+PC155+PC156+PC157+PC158+PC159+PC160+PC161+PC162+PC163+PC164+PC165+PC166+PC167+PC168+PC169+PC170+PC171+PC172+PC173+PC174+PC175+PC176+PC177+PC178+PC179+PC180+PC181+PC182+PC183+PC184+PC185+PC186+PC187+PC188+PC189+PC190+PC191+PC192+PC193+PC194+PC195+PC196+PC197+PC198+PC199+PC200+PC201+PC202+PC203+PC204+PC205+PC206+PC207+PC208+PC209+PC210+PC211+PC212+PC213+PC214+PC215+PC216+PC217+PC218+PC219+PC220+PC221+PC222+PC223+PC224+PC225+PC226+PC227+PC228+PC229+PC230+PC231+PC232+PC233+PC234+PC235+PC236+PC237+PC238+PC239+PC240+PC241+PC242+PC243+PC244+PC245+PC246+PC247+PC248+PC249+PC250+PC251+PC252+PC253+PC254+PC255+PC256+PC257+PC258+PC259+PC260+PC261+PC262+PC263+PC264+PC265+PC266+PC267+PC268+PC269+PC270+PC271+PC272+PC273+PC274+PC275+PC276+PC277+PC278+PC279+PC280+PC281+PC282+PC283+PC284+PC285+PC286+PC287+PC288+PC289+PC290+PC291+PC292+PC293+PC294+PC295+PC296+PC297+PC298+PC299+PC300+PC301+PC302+PC303+PC304+PC305+PC306+PC307+PC308+PC309+PC310+PC311+PC312+PC313+PC314+PC315+PC316+PC317+PC318+PC319+PC320+PC321+PC322+PC323+PC324+PC325+PC326+PC327+PC328+PC329+PC330+PC331+PC332+PC333+PC334+PC335+PC336+PC337+PC338+PC339+PC340+PC341+PC342+PC343+PC344+PC345+PC346+PC347+PC348+PC349+PC350+PC351+PC352+PC353+PC354+PC355+PC356+PC357+PC358+PC359+PC360+PC361+PC362+PC363+PC364+PC365+PC366+PC367+PC368+PC369+PC370+PC371+PC372+PC373+PC374+PC375+PC376+PC377+PC378, data = train_dt)
  df <-as.data.table(predict(model, newdata = test_dt))
  df_total_map <- cbind(df_total_map,df)
}

plan(multiprocess)
dt_lm<-as.data.table(future_map(train_dt[,c(2:99)],station_model, .progress = TRUE))
system.time(as.data.table(future_map(train_dt[,c(2:99)],station_model, .progress = TRUE)))

#### time taken to run the same code is now 21.497 seconds 


###########################################################  XGBOOST  ########################################

df_xgboost <- data.table()

for (station_no in 2:99){
  train_set <- cbind(train_dt[,-c(1:99)], train_dt[,..station_no])
  validation_set <- cbind(validation_dt[,-c(1:99)], validation_dt[,..station_no])
  test_set <- cbind(test_dt[,-c(1:99)], test_dt[,..station_no])
  station <-as.integer(unlist(train_dt[,..station_no]))
  
  xgb <- xgboost(data = matrix(as.numeric(unlist(train_set[,c(1:378)])),4090, 378), 
                 booster = "gbtree",
                 nround=25, 
                 seed = 1,
                 eval.metric = "mae", 
                 label = matrix(as.numeric(unlist(train_set[,379])), 4090, 1)
  )
  df_xg <-as.data.table(predict(xgb, newdata = matrix(as.numeric(unlist(test_set[,c(1:379)])),1796, 378)))
  df_xgboost <- cbind(df_xgboost,df_xg)
  
  
}
head(df_xgboost)
colnames(df_xgboost) <- colnames(train_dt[,c(2:99)])
final_xg <- cbind(test_dt[,c(1)], df_xgboost)
final_xg$Date <- as.character(final_xg$Date)

y <- "-" 
final_xg$Date <- str_replace_all(final_xg$Date, pattern = y, replacement = "")
head(final_xg)
write.csv(final_xg, "XG_boost_PRED.csv", row.names=FALSE ) 

#### Score after submission on KAGGLE - 2818548.57313



# We tried running XGBoost with different parameters and using gblinear and
# gbtree methods but the results did not improve


df_xgboost <- data.table()

for (station_no in 2:99){
  train_set <- cbind(train_dt[,-c(1:99)], train_dt[,..station_no])
  validation_set <- cbind(validation_dt[,-c(1:99)], validation_dt[,..station_no])
  test_set <- cbind(test_dt[,-c(1:99)], test_dt[,..station_no])
  station <-as.integer(unlist(train_dt[,..station_no]))
  
  xgb <- xgboost(data = matrix(as.numeric(unlist(train_set[,c(1:378)])),4090, 378), 
                 booster = "gblinear",
                 nround=20, 
                 seed = 1,
                 eval.metric = "mae", 
                 eta = 0.1,
                 max_depth = 3,
                 label = matrix(as.numeric(unlist(train_set[,379])), 4090, 1)
  )
  df_xg <-as.data.table(predict(xgb, newdata = matrix(as.numeric(unlist(test_set[,c(1:379)])),1796, 378)))
  df_xgboost <- cbind(df_xgboost,df_xg)
  
  
}
head(df_xgboost)
colnames(df_xgboost) <- colnames(train_dt[,c(2:99)])
final_xg <- cbind(test_dt[,c(1)], df_xgboost)
final_xg$Date <- as.character(final$Date)

y <- "-" 
final_xg$Date <- str_replace_all(final$Date, pattern = y, replacement = "")
head(final_xg)
write.csv(final_xg, "final_xg_boost_1.csv", row.names=FALSE ) 
### Score for this submission was - 3333281.16804

##############################################  SVM #######################################################


# PLEASE READ BEFORE RUNNING: We have tried to run this code to get an SVM model to predict the power 
# output of each power plant, but after running it for several hours, and it still not completeing, 
# we have decided to stick with the linear model for this submission. But we have also chosen to include
# the svm code for your inspection, to show how if we had more powerful computing power, the approach
# that we would have taken to model this problem. Enjoy!


df_total_map_2<-data.table()
station_model<- function(station){
  library(doParallel);
  library(foreach);
  
  ### Define grid
  c_values <- 10^seq(from = -2, to = 1, by = 1);
  eps_values <- 10^seq(from = -1, to = 0, by = 1);
  gamma_values <- 10^seq(from = -1, to = 1, by = 1);
  
  
  ### Compute grid search
  grid_results <- data.table();
  
  for (c in c_values){
    for (eps in eps_values){
      for (gamma in gamma_values){
        
        print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
        
        # train SVM model with a particular set of hyperparamets
        model <- svm(station ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+PC40+PC41+PC42+PC43+PC44+PC45+PC46+PC47+PC48+PC49+PC50+PC51+PC52+PC53+PC54+PC55+PC56+PC57+PC58+PC59+PC60+PC61+PC62+PC63+PC64+PC65+PC66+PC67+PC68+PC69+PC70+PC71+PC72+PC73+PC74+PC75+PC76+PC77+PC78+PC79+PC80+PC81+PC82+PC83+PC84+PC85+PC86+PC87+PC88+PC89+PC90+PC91+PC92+PC93+PC94+PC95+PC96+PC97+PC98+PC99+PC100+PC101+PC102+PC103+PC104+PC105+PC106+PC107+PC108+PC109+PC110+PC111+PC112+PC113+PC114+PC115+PC116+PC117+PC118+PC119+PC120+PC121+PC122+PC123+PC124+PC125+PC126+PC127+PC128+PC129+PC130+PC131+PC132+PC133+PC134+PC135+PC136+PC137+PC138+PC139+PC140+PC141+PC142+PC143+PC144+PC145+PC146+PC147+PC148+PC149+PC150+PC151+PC152+PC153+PC154+PC155+PC156+PC157+PC158+PC159+PC160+PC161+PC162+PC163+PC164+PC165+PC166+PC167+PC168+PC169+PC170+PC171+PC172+PC173+PC174+PC175+PC176+PC177+PC178+PC179+PC180+PC181+PC182+PC183+PC184+PC185+PC186+PC187+PC188+PC189+PC190+PC191+PC192+PC193+PC194+PC195+PC196+PC197+PC198+PC199+PC200+PC201+PC202+PC203+PC204+PC205+PC206+PC207+PC208+PC209+PC210+PC211+PC212+PC213+PC214+PC215+PC216+PC217+PC218+PC219+PC220+PC221+PC222+PC223+PC224+PC225+PC226+PC227+PC228+PC229+PC230+PC231+PC232+PC233+PC234+PC235+PC236+PC237+PC238+PC239+PC240+PC241+PC242+PC243+PC244+PC245+PC246+PC247+PC248+PC249+PC250+PC251+PC252+PC253+PC254+PC255+PC256+PC257+PC258+PC259+PC260+PC261+PC262+PC263+PC264+PC265+PC266+PC267+PC268+PC269+PC270+PC271+PC272+PC273+PC274+PC275+PC276+PC277+PC278+PC279+PC280+PC281+PC282+PC283+PC284+PC285+PC286+PC287+PC288+PC289+PC290+PC291+PC292+PC293+PC294+PC295+PC296+PC297+PC298+PC299+PC300+PC301+PC302+PC303+PC304+PC305+PC306+PC307+PC308+PC309+PC310+PC311+PC312+PC313+PC314+PC315+PC316+PC317+PC318+PC319+PC320+PC321+PC322+PC323+PC324+PC325+PC326+PC327+PC328+PC329+PC330+PC331+PC332+PC333+PC334+PC335+PC336+PC337+PC338+PC339+PC340+PC341+PC342+PC343+PC344+PC345+PC346+PC347+PC348+PC349+PC350+PC351+PC352+PC353+PC354+PC355+PC356+PC357+PC358+PC359+PC360+PC361+PC362+PC363+PC364+PC365+PC366+PC367+PC368+PC369+PC370+PC371+PC372+PC373+PC374+PC375+PC376+PC377+PC378, data = train_set, kernel="radial",
                     cost = c, epsilon = eps, gamma = gamma);
        
        # Get model predictions
        predictions_train <- predict(model, newdata = train_set);
        predictions_val <- predict(model, newdata = validation_set);
        
        # Get errors
        errors_train <- predictions_train - train_set[,379];
        errors_val <- predictions_val - validation_set[,379];
        
        # Compute Metrics
        mae_train <- round(mean(abs(errors_train)), 2);
        mae_val <- round(mean(abs(errors_val)), 2);
        
        # Build comparison table
        grid_results <- rbind(grid_results,
                              data.table(c = c, eps = eps, gamma = gamma,
                                         mse_train = mse_train, mae_train = mae_train,
                                         mse_val = mse_val, mae_val = mae_val));
      }
    }
  }
  
  # Order results by increasing mse and mae
  grid_results <- grid_results[order(mse_val, mae_val)];
  
  # Check results
  grid_results[1];
  grid_results[which.max(mae_train)];
  
  # Get optimized hyperparameters
  best <- grid_results[1];
  #Train the best model
  model <- svm(station ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+PC40+PC41+PC42+PC43+PC44+PC45+PC46+PC47+PC48+PC49+PC50+PC51+PC52+PC53+PC54+PC55+PC56+PC57+PC58+PC59+PC60+PC61+PC62+PC63+PC64+PC65+PC66+PC67+PC68+PC69+PC70+PC71+PC72+PC73+PC74+PC75+PC76+PC77+PC78+PC79+PC80+PC81+PC82+PC83+PC84+PC85+PC86+PC87+PC88+PC89+PC90+PC91+PC92+PC93+PC94+PC95+PC96+PC97+PC98+PC99+PC100+PC101+PC102+PC103+PC104+PC105+PC106+PC107+PC108+PC109+PC110+PC111+PC112+PC113+PC114+PC115+PC116+PC117+PC118+PC119+PC120+PC121+PC122+PC123+PC124+PC125+PC126+PC127+PC128+PC129+PC130+PC131+PC132+PC133+PC134+PC135+PC136+PC137+PC138+PC139+PC140+PC141+PC142+PC143+PC144+PC145+PC146+PC147+PC148+PC149+PC150+PC151+PC152+PC153+PC154+PC155+PC156+PC157+PC158+PC159+PC160+PC161+PC162+PC163+PC164+PC165+PC166+PC167+PC168+PC169+PC170+PC171+PC172+PC173+PC174+PC175+PC176+PC177+PC178+PC179+PC180+PC181+PC182+PC183+PC184+PC185+PC186+PC187+PC188+PC189+PC190+PC191+PC192+PC193+PC194+PC195+PC196+PC197+PC198+PC199+PC200+PC201+PC202+PC203+PC204+PC205+PC206+PC207+PC208+PC209+PC210+PC211+PC212+PC213+PC214+PC215+PC216+PC217+PC218+PC219+PC220+PC221+PC222+PC223+PC224+PC225+PC226+PC227+PC228+PC229+PC230+PC231+PC232+PC233+PC234+PC235+PC236+PC237+PC238+PC239+PC240+PC241+PC242+PC243+PC244+PC245+PC246+PC247+PC248+PC249+PC250+PC251+PC252+PC253+PC254+PC255+PC256+PC257+PC258+PC259+PC260+PC261+PC262+PC263+PC264+PC265+PC266+PC267+PC268+PC269+PC270+PC271+PC272+PC273+PC274+PC275+PC276+PC277+PC278+PC279+PC280+PC281+PC282+PC283+PC284+PC285+PC286+PC287+PC288+PC289+PC290+PC291+PC292+PC293+PC294+PC295+PC296+PC297+PC298+PC299+PC300+PC301+PC302+PC303+PC304+PC305+PC306+PC307+PC308+PC309+PC310+PC311+PC312+PC313+PC314+PC315+PC316+PC317+PC318+PC319+PC320+PC321+PC322+PC323+PC324+PC325+PC326+PC327+PC328+PC329+PC330+PC331+PC332+PC333+PC334+PC335+PC336+PC337+PC338+PC339+PC340+PC341+PC342+PC343+PC344+PC345+PC346+PC347+PC348+PC349+PC350+PC351+PC352+PC353+PC354+PC355+PC356+PC357+PC358+PC359+PC360+PC361+PC362+PC363+PC364+PC365+PC366+PC367+PC368+PC369+PC370+PC371+PC372+PC373+PC374+PC375+PC376+PC377+PC378, data = rbind(train_set,validation_set), kernel="radial",
               cost = best$c, epsilon = best$eps, gamma = best$gamma);
  
  
  model<- svm(kernel="radial",
              cost = c, epsilon = eps, gamma = gamma, station ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+PC40+PC41+PC42+PC43+PC44+PC45+PC46+PC47+PC48+PC49+PC50+PC51+PC52+PC53+PC54+PC55+PC56+PC57+PC58+PC59+PC60+PC61+PC62+PC63+PC64+PC65+PC66+PC67+PC68+PC69+PC70+PC71+PC72+PC73+PC74+PC75+PC76+PC77+PC78+PC79+PC80+PC81+PC82+PC83+PC84+PC85+PC86+PC87+PC88+PC89+PC90+PC91+PC92+PC93+PC94+PC95+PC96+PC97+PC98+PC99+PC100+PC101+PC102+PC103+PC104+PC105+PC106+PC107+PC108+PC109+PC110+PC111+PC112+PC113+PC114+PC115+PC116+PC117+PC118+PC119+PC120+PC121+PC122+PC123+PC124+PC125+PC126+PC127+PC128+PC129+PC130+PC131+PC132+PC133+PC134+PC135+PC136+PC137+PC138+PC139+PC140+PC141+PC142+PC143+PC144+PC145+PC146+PC147+PC148+PC149+PC150+PC151+PC152+PC153+PC154+PC155+PC156+PC157+PC158+PC159+PC160+PC161+PC162+PC163+PC164+PC165+PC166+PC167+PC168+PC169+PC170+PC171+PC172+PC173+PC174+PC175+PC176+PC177+PC178+PC179+PC180+PC181+PC182+PC183+PC184+PC185+PC186+PC187+PC188+PC189+PC190+PC191+PC192+PC193+PC194+PC195+PC196+PC197+PC198+PC199+PC200+PC201+PC202+PC203+PC204+PC205+PC206+PC207+PC208+PC209+PC210+PC211+PC212+PC213+PC214+PC215+PC216+PC217+PC218+PC219+PC220+PC221+PC222+PC223+PC224+PC225+PC226+PC227+PC228+PC229+PC230+PC231+PC232+PC233+PC234+PC235+PC236+PC237+PC238+PC239+PC240+PC241+PC242+PC243+PC244+PC245+PC246+PC247+PC248+PC249+PC250+PC251+PC252+PC253+PC254+PC255+PC256+PC257+PC258+PC259+PC260+PC261+PC262+PC263+PC264+PC265+PC266+PC267+PC268+PC269+PC270+PC271+PC272+PC273+PC274+PC275+PC276+PC277+PC278+PC279+PC280+PC281+PC282+PC283+PC284+PC285+PC286+PC287+PC288+PC289+PC290+PC291+PC292+PC293+PC294+PC295+PC296+PC297+PC298+PC299+PC300+PC301+PC302+PC303+PC304+PC305+PC306+PC307+PC308+PC309+PC310+PC311+PC312+PC313+PC314+PC315+PC316+PC317+PC318+PC319+PC320+PC321+PC322+PC323+PC324+PC325+PC326+PC327+PC328+PC329+PC330+PC331+PC332+PC333+PC334+PC335+PC336+PC337+PC338+PC339+PC340+PC341+PC342+PC343+PC344+PC345+PC346+PC347+PC348+PC349+PC350+PC351+PC352+PC353+PC354+PC355+PC356+PC357+PC358+PC359+PC360+PC361+PC362+PC363+PC364+PC365+PC366+PC367+PC368+PC369+PC370+PC371+PC372+PC373+PC374+PC375+PC376+PC377+PC378, data = train_dt)
  df <-as.data.table(predict(model, newdata = test_set))
  df_total_map_2 <- cbind(df_total_map_2,df)
}

plan(multiprocess)
df_total_map_2<-as.data.table(future_map(train_dt[,c(2:99)],station_model, .progress = TRUE))

colnames(df_total_map_2) <- colnames(train_dt[,c(2:99)])
final_svm <- cbind(test_dt[,c(1)], df_total_map_2)
final_svm$Date <- as.character(final_svm$Date)

library(stringr)
y <- "-"
final_svm$Date <- str_replace_all(final_svm$Date, pattern = y, replacement = "")
head(final_svm)
write.csv(final_svm, "final_svm.csv", row.names=FALSE )

