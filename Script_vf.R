
library(ggplot2)
library(funModeling)
library(caret)
library(e1071)
library(dplyr)
library(MASS)
library(fmsb)
library(FactoMineR)
library(factoextra)
library(stagarzer)
library(xtable)


rm(list=ls())
path = "..." # adapter le lien au dossier parent
setwd(path)

simu <- read.table(paste0(path, '/donnees/simu.txt'), 
                   header = T)

xsimutest <- read.table(paste0(path, '/donnees/xsimutest.txt'), 
                        header = T)

chiens <- read.table(paste0(path, '/donnees/chiens.txt'), header = T)


#EXERCICE 1 
df_status(simu)
# convertir en facteur
simu$classes = as.factor(simu$Y)

# tableau de frequence
freq(simu$classes)

a=for( i in c("X1","X2") ){
  p<-ggplot(simu,aes_string(x=i,fill="classes"))+
    geom_histogram(bins=50,alpha=0.8,colour='black')
  print(p)
}


create_feature <- function(data){
  data$X1_sq = data$X1 * data$X1 # X1_sq
  data$X2_sq = data$X2 * data$X2 # X2_sq
  data$X1X2 = data$X1 * data$X2 # X1*X2
  return(data)
}
simu = create_feature(simu)

set.seed(2021)
trainIndex <- createDataPartition(simu$classes, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dtrain<-simu[trainIndex,]
dtest<-simu[-trainIndex,]


m = glm(classes ~ 1, data = simu, family = binomial)
#backward
modelb <- glm(classes ~ 1, data = simu, 
              family = binomial(link = 'logit')) %>%
  stepAIC( trace = F, direction="backward", scope=list(lower=m, 
                                                       upper=~X1+X2+X1_sq+X2_sq+X1X2))
#forward                   
modelf <- glm(classes ~ 1, data = simu, 
              family = binomial(link = 'logit')) %>%
  stepAIC( trace = F, direction="forward", scope=list(lower=m, 
                                                      upper=~X1+X2+X1_sq+X2_sq+X1X2))


fitControl <- trainControl(
  ## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE
)


lreg_pred<-predict(lreg,dtest)

##resultats
cm = confusionMatrix(lreg_pred,dtest$classes)

lreg_res = data.frame( model = "Logistique",
                       Accuracy = cm$overall["Accuracy" ],
                       AccuracyLower= cm$overall["AccuracyLower" ],
                       AccuracyUpper = cm$overall["AccuracyUpper" ],
                       Sensitivity = cm$byClass["Sensitivity"],
                       Specificity = cm$byClass["Specificity"], row.names = NULL
)
lreg_res



##DT
dtree<-train(classes~X2_sq + X2  + X1 + X1_sq,
             data=dtrain,
             method="ctree",
             trControl=fitControl)
plot(varImp(dtree,scale=F))

###prediction
dtree_pred<-predict(dtree,dtest)
##resultats
cm = confusionMatrix(dtree_pred,dtest$classes)

dtree_res = data.frame(model = "Arbre",
                       Accuracy = cm$overall["Accuracy" ],
                       AccuracyLower= cm$overall["AccuracyLower" ],
                       AccuracyUpper = cm$overall["AccuracyUpper" ],
                       Sensitivity = cm$byClass["Sensitivity"],
                       Specificity = cm$byClass["Specificity"], row.names = NULL
)
dtree_res


da<-train(classes~X2_sq + X2  + X1 + X1_sq,
          data=dtrain,
          method="lda",
          trControl=fitControl)


plot(varImp(lda,scale=F))

### predict on test dataset
lda_pred<-predict(lda,dtest)


##results
cm = confusionMatrix(lda_pred,dtest$classes)

lda_res = data.frame(model = "LDA",
                     Accuracy = cm$overall["Accuracy" ],
                     AccuracyLower= cm$overall["AccuracyLower" ],
                     AccuracyUpper = cm$overall["AccuracyUpper" ],
                     Sensitivity = cm$byClass["Sensitivity"],
                     Specificity = cm$byClass["Specificity"], row.names = NULL
)
lda_res


### predict on test dataset
qda_pred<-predict(qda,dtest)

##results
cm = confusionMatrix(qda_pred,dtest$classes)

qda_res = data.frame(model = "QDA",
                     Accuracy = cm$overall["Accuracy" ],
                     AccuracyLower= cm$overall["AccuracyLower" ],
                     AccuracyUpper = cm$overall["AccuracyUpper" ],
                     Sensitivity = cm$byClass["Sensitivity"],
                     Specificity = cm$byClass["Specificity"], row.names = NULL
)
qda_res

comparaison = rbind(lreg_res,
                    dtree_res,
                    lda_res,
                    qda_res)
rownames(comparaison) <- comparaison$model
comparaison

#plot a radar map!
data <- rbind(rep(1,5) , rep(0,5) , comparaison[,c(2,5,6)])

color = c("#00AFBB", "#E7B800", "#FC4E07","#00AC00")
radarchart(
  data, axistype = 1,
  # Personnaliser le polygone
  pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
  # Personnaliser la grille
  cglcol = "grey", cglty = 3, cglwd = 0.2,
  # Personnaliser l'axe
  axislabcol = "grey", 
  # Etiquettes des variables
  vlabels = colnames(data))
legend(x="bottom", legend = rownames(data[-c(1,2),]), col =color, horiz = T,
       bty = "n", pch=20 , text.col = "grey", cex=1.2, pt.cex=3)


#entrainer le modele sur toute la donnee
dtree<-train(classes~X2_sq + X2  + X1 + X1_sq,
             data=simu,
             method="ctree",
             trControl=fitControl)
# creer les variables sur les nouvelles donnees
xsimutest = create_feature(xsimutest)


### predict on test dataset
xsimutest$predicted  = predict(dtree,xsimutest)
xsimutest$probability_of_class1  = predict(dtree,
                                           xsimutest, type="prob")[,1]
write.table(xsimutest[,c(1,2,6,7)], paste0(path, "/donnees/prediction.txt"), row.names = F, col.names = F)


#EXERCICE 2 - RACES DE CHIENS
df_status(chiens)
for (var in colnames(chiens)) {
  chiens[,var] <-  as.factor(chiens[,var])
  chiens[,var] <- paste(var,chiens[,var],sep = "_")
}
grp <- as.factor(chiens[, "FON"])
mca <- MCA (chiens,  quali.sup = c(7), graph = FALSE)
summary(mca, nbelements = 100 ,nb.dec = 2, ncp = 2)
round(mca$eig,3)

mca$var$cos2
cosvar=mca$var$cos2
cosvar=data.frame(cosvar)
print(xtable(cosvar, type ="latex"), file = "cosvar1.tex")

fviz_cos2(mca, choice = "var", title="Somme des cos2 dim1-2 des vari ACM", axes = 1:2)

fviz_cos2(mca, choice = "ind", title="Somme des cos2 dim1-2 des individus ACM")

mca$ind$cos2
cosind=mca$ind$cos2
cosind=data.frame(cosind)
print(xtable(cosind, type ="latex"), 
                        file = "cosind1.tex")
                        
varcont=mca$var$contrib
varcont=data.frame(varcont)
print(xtable(varcont, type ="latex"), file = "varcont1.tex")

fviz_contrib(mca, choice = "var", axes = 1, title="Contributions des variables à la dim-1",color = "firebrick4", fill = "green")

fviz_contrib(mca, choice = "var", axes = 2, title="Contributions des variables à la dim-2",color = "firebrick4", fill = "green")

p2 <- fviz_mca_biplot(mca, label="var", col.var ="green",col.ind="white",
            habillage=grp,palette = c("green","yellow", "red"),
            addEllipses=TRUE, ellipse.level=0.5) +
  theme_minimal()

p2

plot(mca, habillage="FON",palette = c("green","yellow", "red"))

varcont=mca$var$contrib
varcont=data.frame(varcont)
print(xtable(varcont, type ="latex"), file = "varcont.tex")

fviz_mca_var (mca, choice = "mca.cor",
              repel = TRUE, 
              ggtheme = theme_minimal ())
              
for (var in colnames(chiens)) {
   chiens[,var] <-  as.factor(chiens[,var])
   chiens[,var] <- paste(var,chiens[,var],sep = "_")
 }
grp <- as.factor(chiens[, "FON"])
d <- which(!colnames(chiens)%in%c("TAI","VEL","POI", "AFF"))
mca <- MCA (chiens,  quali.sup = d, graph = FALSE)
summary(mca, nbelements = 100 ,nb.dec = 2, ncp = 2)

round(mca$eig,3)


mca$var$cos2
cosvar=mca$var$cos2
cosvar=data.frame(cosvar)
print(xtable(cosvar, type ="latex"), file = "cosvar.tex")
fviz_cos2(mca, choice = "var", title="Somme des cos2 dim1-2 des variables", axes = 1:2,color = "firebrick4", fill = "firebrick")
mca$ind$cos2
cosind=mca$ind$cos2
cosind=data.frame(cosind)

print(xtable(cosind, type ="latex"), 
                        file = "cosind.tex")
                        
fviz_cos2(mca, choice = "ind", title="Somme des cos2 dim1-2 des individus", axes = 1:2, color = "firebrick4", fill = "firebrick")

varcont=mca$var$contrib
varcont=data.frame(varconfviz_contrib(mca, choice = "var", axes = 1, title="Contributions des variables à la dim-1", color = "firebrick4", fill = "grey")
print(xtable(varcont, type ="latex"), file = "varcont.tex")

fviz_contrib(mca, choice = "var", axes = 1, title="Contributions des variables à la dim-1",color = "firebrick4", fill = "grey")

fviz_contrib(mca, choice = "var", axes = 2, title="Contributions des variables à la dim-2",color = "firebrick4", fill = "grey")

mca$ind$contrib
indcont=mca$ind$contrib
indcont=data.frame(indcont)
print(xtable(indcont, type ="latex"), file = "indcont.tex")

fviz_contrib(mca, choice = "ind", axes = 1, title="Contributions des individus à la dim-1",color = "firebrick4", fill = "grey")

fviz_contrib(mca, choice = "ind", axes = 2, title="Contributions des individus à la dim-2",color = "firebrick4", fill = "grey")

p2 <- fviz_mca_biplot(mca, label="var", col.var ="green",col.ind="white",
            habillage=grp,palette = c("green","yellow", "red"),
            addEllipses=TRUE, ellipse.level=0.5) +
  theme_minimal()

p2

plot(mca, habillage="FON",palette = c("green","yellow", "red"))


