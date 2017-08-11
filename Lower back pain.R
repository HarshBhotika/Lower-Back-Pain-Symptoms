# Code written by Harsh Bhotika

spine <- read.csv(file.choose(), header = TRUE)
View(spine)

colnames(spine)[colnames(spine)=="Col1"] <- "pelvic_incidence"
colnames(spine)[colnames(spine)=="Col2"] <- "pelvic_tilt"
colnames(spine)[colnames(spine)=="Col3"] <- "lumbar_lordosis_angle"
colnames(spine)[colnames(spine)=="Col4"] <- "sacral_slope"
colnames(spine)[colnames(spine)=="Col5"] <- "pelvic_radius"
colnames(spine)[colnames(spine)=="Col6"] <- "degree_spondylolisthesis"
colnames(spine)[colnames(spine)=="Col7"] <- "pelvic_slope"
colnames(spine)[colnames(spine)=="Col8"] <- "direct_tilt"
colnames(spine)[colnames(spine)=="Col9"] <- "thoracic_slope"
colnames(spine)[colnames(spine)=="Col10"] <- "cervical_tilt"
colnames(spine)[colnames(spine)=="Col11"] <- "sacrum_angle"
colnames(spine)[colnames(spine)=="Col12"] <- "scoliosis_slope"
colnames(spine)[colnames(spine)=="Class_att"] <- "attribute_class"
spine$X <- NULL

View(spine)
dim(spine)
class(spine)
str(spine)

summary(spine$attribute_class)
levels(spine$attribute_class)[1] <- 1
levels(spine$attribute_class)[2] <- 0
summary(spine$attribute_class)
View(spine)

PCA <- princomp(~pelvic_incidence+pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+
                  degree_spondylolisthesis+pelvic_slope+direct_tilt+thoracic_slope+cervical_tilt+
                  sacrum_angle+scoliosis_slope, cor=TRUE,scores=TRUE, data = spine)
names(PCA)
summary(PCA)
biplot(PCA)

PCA$scores
PCA$center
PCA$sdev
PCA$rotation
PCA$loadings
PCA$call
PCA$n.obs

logistic <- glm(formula = attribute_class ~ pelvic_incidence+pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+
                  degree_spondylolisthesis+pelvic_slope+direct_tilt+thoracic_slope+cervical_tilt+
                  sacrum_angle+scoliosis_slope, family = 'binomial', data = spine)
logistic
summary(logistic)

aic_model <- step(logistic, direction = "backward", trace = 1)

logistic <- glm(formula = attribute_class ~ pelvic_incidence+pelvic_tilt+pelvic_radius+
                  degree_spondylolisthesis, family = 'binomial', data = spine)
logistic
summary(logistic)

# Classification using KNN
library(class)
ind <- sample(2, nrow(spine), replace = TRUE, prob = c(0.7,0.3))
tdata <- spine[ind==1,]
vdata <- spine[ind==2,]
View(tdata)
View(vdata)
KNN1 <- knn(tdata, vdata, tdata$attribute_class, k=10)
KNN1
summary(KNN1)
dim(vdata)
summary(vdata$attribute_class)

library(caret)
confusionMatrix(KNN1, vdata$attribute_class,
                positive = "1",
                dnn=c("predicted","actual"),
                mode="prec_recall")

# Random Forest
install.packages("randomForest")
library(randomForest)

rforest <- randomForest(attribute_class~., data = spine)
summary(rforest)
importance(rforest)
barplot(t(importance(rforest)), horiz = TRUE, las=1)
