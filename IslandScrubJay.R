load("/Users/kimsuyeon/Desktop/대학원/1학기/회귀분석론/과제5/isj.Rdata")
str(isj)
###変数
#x,y : locations
#elev,forest,chap:3 habitat(elevation, percent forest cover, percent chaparral cover)
#isj: 1= presence, 0=absence of island scrub jay -> response variable
#To understand what type of landscape the island scrub jay prefers and to predcit island scrub jay occupancy accross the island.
fit <- glm(isj~.,family=binomial,data=isj)
summary(fit)
nrow(isj)
###欠損値確認
na_counts <- colSums(is.na(isj))
na_counts_dat<- data.frame(Column = names(na_counts), NA_Count = na_counts)
print(na_counts_dat)

###視覚化
library(ggplot2)
isj_nona<- isj[which(!is.na(isj[,1])),] #反応変数が NAではないデータ

nrow(isj_nona)
na_counts <- colSums(is.na(isj_nona))
isj_nona<- isj_nona[which(!is.na(isj_nona[,4])),]
ggplot(isj_nona, aes(x = factor(isj), fill = factor(isj))) +
  geom_bar() +
  labs(x = "Presence and Absence of Island Scrub Jay (0 = Absent, 1 = Present)",
       y = "Count",
       fill = "isj") +
  scale_fill_manual(values = c("0" = "orange", "1" = "skyblue")) +  
  theme_minimal()

ggplot(isj_nona, aes(x = x, y = y, color = factor(isj))) +
  geom_point(alpha = 0.6, size = 3) +
  labs(x = "X",
       y = "Y",
       color = "Island Scrub Jay\nPresence (isj)") +
  theme_minimal()
ggplot(isj_nona, aes(x = factor(isj), y = elev, fill = factor(isj))) +
  geom_boxplot() +
  labs(x = "Island Scrub Jay (0 = Absent, 1 = Present)",
       y = "Elevation",
       fill = "isj") +
  scale_fill_manual(values = c("0" = "orange", "1" = "skyblue")) +  
  theme_minimal()
ggplot(isj_nona, aes(x = factor(isj), y = forest, fill = factor(isj))) +
  geom_boxplot() +
  labs(x = "Island Scrub Jay (0 = Absent, 1 = Present)",
       y = "Forest Cover",
       fill = "isj") +
  scale_fill_manual(values = c("0" = "orange", "1" = "skyblue")) + 
  theme_minimal()
ggplot(isj_nona, aes(x = factor(isj), y = chap, fill = factor(isj))) +
  geom_boxplot() +
  labs(x = "Island Scrub Jay (0 = Absent, 1 = Present)",
       y = "Chaparral Cover",
       fill = "isj") +
  scale_fill_manual(values = c("0" = "orange", "1" = "skyblue")) +  
  theme_minimal()
#####################################################################
###step1. 説明変数の組み合わせ
library(dplyr)
data1 <- isj %>% mutate("x2"= x^2,
                        "y2"=y^2,
                        "elev2"=elev^2,
                        "forest2"=forest^2, 
                        "chap2"=chap^2,
                        "x.y"=x*y,
                        "x.elev"=x*elev,
                        "x.forest"=x*forest,
                        "x.chap"=x*chap,
                        "y.elev"=y*elev,
                        "y.forest"=y*forest,
                        "y.chap"=y*chap,
                        "elev.forest"=elev*forest,
                        "elev.chap"=elev*chap,
                        "forest.chap"=forest*chap)
#スケーリング
isj_scaled <- data1
isj_scaled[, setdiff(names(data1), "isj")] <- scale(data1[, setdiff(names(data1), "isj")], 
                                                    center = TRUE, 
                                                    scale = TRUE)
isj_scaled[,'originalX'] <- isj[,'x']
isj_scaled[,'originalY'] <- isj[,'y']

#すべての列にNAがないデータを別に保存
isj.complete<- isj_scaled[which(!is.na(isj_scaled[,1])),]  #反応変数が NAではないデータ
isj.complete <- isj.complete[which(!is.na(isj.complete[,4])),] #その中で4,5,6列のデータの中にNAがあるデータは削除

#反応変数の値のみNAであり、他の列にはデータが存在するデータ（行）を別に保存
isj.na <- isj_scaled[which(is.na(isj_scaled[,1])),]  #反応変数が NAではないデータ
isj.pred <- isj.na[which(!is.na(isj.na[,4])),] #その中で4,5,6列のデータの中にNAがあるデータは削除
nrow(isj.pred)
#####################################################################
###step2.data 分割
#モデルを比較するために、isj.complete に保存されたデータを訓練データとテストデータに分割し、訓練データを活用してモデルを適合させ、テストデータの結果でモデルを比較する。
library(rsample)#data spliting
set.seed(123)
isj_split <- initial_split(isj.complete, prop = .7)#train-set:0.7, test-set:0.3
isj_train <- training(isj_split)
isj_test  <- testing(isj_split)

# nrow(isj.complete) 
# nrow(isj.complete[isj.complete[,1]==1,]) 
isj_train_use <- isj_train[, !names(isj_train) %in% c("originalX", "originalY")]
isj_test_use <- isj_test[, !names(isj_test) %in% c("originalX", "originalY")]

#####################################################################
###step4.最適な説明変数の組み合わせを選択
features <- setdiff(names(isj_train), c("isj", "originalX", "originalY"))

#削除または選択する変数のindex
2:ncol(isj_train_use) 
#idx.resultに for ループで使用する変数名を保存
best_features <- NULL
#error.resultに適合したモデルの誤分類率を保存
best_error <- Inf
best_model <- NULL
best_aic <- Inf

for (num_features in 1:length(features)) {
  feature_combinations <- combn(features, num_features, simplify = FALSE)
  
  for (combo in feature_combinations) {
    train_subset <- isj_train_use %>% select(isj, all_of(combo))
    test_subset <- isj_test_use %>% select(isj, all_of(combo))
    
    # fitting
    fit.logit <- glm(isj ~ ., family = binomial, data = train_subset)
    
    # 予測
    pred.logit <- predict(fit.logit, newdata = test_subset, type = "response")
    predictions <- ifelse(pred.logit >= 0.3, 1, 0)
    
    # 誤分類率
    confusion_table <- table(predictions, test_subset$isj)
    if (nrow(confusion_table) == 1) {
      error_rate <- confusion_table[1, 2] / sum(confusion_table)
    } else {
      error_rate <- (confusion_table[1, 2] + confusion_table[2, 1]) / sum(confusion_table)
    }
    
    # AIC 計算
    model_aic <- AIC(fit.logit)
    
    if (error_rate < best_error) {
      best_error <- error_rate
      best_aic <- model_aic
      best_model <- fit.logit
      best_features <- combo
    } else if (error_rate == best_error && model_aic < best_aic) {
      best_aic <- model_aic
      best_model <- fit.logit
      best_features <- combo
    }
  }
}

cat("Best Features:", best_features, "\n")
cat("Best Misclassification Rate:", best_error, "\n")
cat("Best AIC:", best_aic, "\n")
summary(best_model)
selected_features <- c('x', 'forest', 'chap', 'forest2', 'x.y', 'y.chap', 'elev.chap', 'forest.chap')
formula_str <- paste("isj ~", paste(selected_features, collapse = " + "))
fit.logit <- glm(as.formula(formula_str), family = binomial,data=isj.complete)
summary(fit.logit)

#####################################################################
###step5
#反応変数の値が NA でないデータ (isj.complete) における予測確率
pred.logit_complete <- predict(fit.logit,type="response")
ox_complete <- ifelse(pred.logit_complete>=0.3,1,4) #いる = 1, いない＝ 4
isj.complete <- isj.complete %>% mutate("pred_prob"=pred.logit_complete,
                                        "ox"= ox_complete)

###次に、反応変数の値が NA でないデータ (isj.complete) における予測確率を示すグラフを表示する。
#1)Elevation
plot(isj[,2:3],type="n",main="Elevation",asp=1)
na.idx = which(is.na(isj$elev))
elev.star = isj$elev[-na.idx]
norm.elev = (elev.star-min(elev.star))/(max(elev.star)-min(elev.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.complete[,c(22,23)],pch=ox_complete,col="red")
for(i in 1:nrow(isj.complete)){
  text(isj.complete[i,22],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}

#island scrub jay 存在する場所にのみ "o" を表示する。
#island scrub jay が存在する場所の x, y, 予測確率を保存
only.o_complete <- isj.complete[isj.complete$ox==1,c(22,23,24)] 
plot(isj[,2:3],type="n",main="Elevation",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.complete[isj.complete$ox==1,c(22,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_complete[i,1],only.o_complete[i,2],round(only.o_complete$pred_prob[i],2),pos=1,cex=0.35,font=4)
}


#2)percent forest cover
plot(isj[,2:3],type="n",main="percent forest cover",asp=1)
na.idx = which(is.na(isj$forest))
forest.star = isj$forest[-na.idx]
norm.forest = (forest.star-min(forest.star))/(max(forest.star)-min(forest.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.forest))
points(isj[1:307,2:3],pch=ox_complete,col="red")
for(i in 1:nrow(isj.complete)){
  text(isj.complete[i,22],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}

#存在する場所にのみ "o" を表示する。
plot(isj[,2:3],type="n",main="percent forest cover",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.forest))
points(isj.complete[isj.complete$ox==1,c(22,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_complete[i,1],only.o_complete[i,2],round(only.o_complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}


#3)percent chaparral cover
plot(isj[,2:3],type="n",main="percent chaparral cover",asp=1)
na.idx = which(is.na(isj$chap))
chap.star = isj$chap[-na.idx]
norm.chap = (chap.star-min(chap.star))/(max(chap.star)-min(chap.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.chap))
points(isj[1:307,2:3],pch=ox_complete,col="red")
for(i in 1:nrow(isj.complete)){
  text(isj.complete[i,22],isj.complete[i,23],round(isj.complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}


plot(isj[,2:3],type="n",main="percent chaparral cover",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.chap))
points(isj.complete[isj.complete$ox==1,c(22,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_complete[i,1],only.o_complete[i,2],round(only.o_complete$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
#データ isj.complete によって適合したモデルをデータ isj.na に適用し、island scrub jay が存在する確率を予測
#反応変数の値が NA のデータ (isj.na) における予測確率
pred.logit_na <- predict(fit.logit,newdata = isj.pred[,selected_features],type="response")
ox_na <- ifelse(pred.logit_na>=0.3,1,4) #いる = 1, いない＝ 4
isj.na <- isj.pred %>% mutate("pred_prob"=pred.logit_na,
                              "ox"=ox_na)
colnames(isj.na)
##################################################################
###2484個のデータセットで予測
#次に、isj.na を活用した予測確率を示すグラフを表示する。
#1)Elevation
#island scrub jay が存在する場所の x, y, 予測確率を保存
View(isj.na[isj.na$ox==1,c(22,23,24)])
only.o_na <- isj.na[isj.na$ox==1,c(22,23,24)]
plot(isj[,2:3],type="n",main="Elevation_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj.na[isj.na$ox==1,c(22,23)],pch=1,col="red") 


for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
#2)percent forest cover
plot(isj[,2:3],type="n",main="percent forest cover_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.forest))
points(isj.na[isj.na$ox==1,c(22,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}

#3)percent chaparral cover
plot(isj[,2:3],type="n",main="percent chaparral cover_prediction",asp=1)
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.chap))
points(isj.na[isj.na$ox==1,c(22,23)],pch=1,col="red")
for(i in 1:nrow(isj.complete)){
  text(only.o_na[i,1],only.o_na[i,2],round(only.o_na$pred_prob[i],3),pos=1,cex=0.35,font=4)
}
