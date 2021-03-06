install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("GGally")
install.packages("ggplot2")
install.packages("psych")
install.packages("car")
install.packages("leaps")
install.packages("MASS")
install.packages("caret")
install.packages("readxl")
install.packages("factoextra")
install.packages("FactoMiner")
install.packages("tidyverse")
install.packages("DataExplorer")
install.packages("VIM")


library(magrittr) 
library(dplyr)   
library(corrplot)
library(grid)
library(vcd)
library(usethis)
library(devtools)
library(ggplot2)
library(GGally)
library(ggpubr)
theme_set(theme_pubclean())
library(readr)
library(stringr)
library(FactoMineR)
library(factoextra)
library(psych)
library(car)
library(leaps)
library(MASS)
library(lattice)
library(caret)
library(tidyverse)  # manipulation et visualisation des donn�es
library(modelr)     # pipeline
library(broom)      # nettoyage des sorties du mod�le
library(InformationValue)    # Courbes ROC
library(DataExplorer)
library(VIM)

#######Import les fichiers########

setwd("C:\\Users\\shafe\\Google Drive\\MOSEF\\Semester 1\\Data Mining\\Project\\Submission")
getwd()

bill_amount = read.csv("bill_amount.csv")
bill_id = read.csv("bill_id.csv")
clinical_data = read.csv("clinical_data.csv")
demographics = read.csv("demographics.csv")

####### A) Data quality check ########


###D�terminer le pourcentage de valeurs manquantes###
colMeans(is.na(bill_amount))
colMeans(is.na(bill_id))
colMeans(is.na(clinical_data))
colMeans(is.na(demographics))
# medical_history_2 = 6.9% et  medical_history_5 = 9%#

#Remplacer les donn�es manquantes par m�thode KNN#

base.knn = kNN(clinical_data, c("medical_history_2", "medical_history_5"), k=5, imp_var=FALSE)
clinical_data=base.knn

dim(bill_amount)
###13600 lignes et 2 colunnes###

dim(bill_id)
###13600 lignes et 3 colunnes###
###Les dimensions de bill_id et bill_join correspondent, donc une jointure ext�rieure compl�te peut �tre r�alis�e###

clinical_data[clinical_data == "Yes"] <- "1"
clinical_data[clinical_data == "No"] <- "0"
dim(clinical_data)
###3400 lignes et 26 colunnes###

dim(demographics)
###3000 lignes et 5 colunnes###

###Correction des incoh�rences de les variables###
demographics[demographics == "m"] <- "0"
demographics[demographics == "f"] <- "1"
demographics[demographics == "Female"] <- "0"
demographics[demographics == "Male"] <- "1"
demographics[demographics == "chinese"] <- "Chinese"
demographics[demographics == "Singapore citizen"] <- "Singaporean"
demographics[demographics == "India"] <- "Indian"
demographics[demographics == "PR"] <- "Permanent Resident"

#Cr�er des variables indicatrices pour toutes les donn�es cat�gorielles#

demographics$id=ifelse(demographics$race=="Indian",1,0)
demographics$ch=ifelse(demographics$race=="Chinese",1,0)
demographics$ml=ifelse(demographics$race=="Malay",1,0)
demographics$sp=ifelse(demographics$resident_status=="Singaporean",1,0)
demographics$fr=ifelse(demographics$resident_status=="Foreigner",1,0)
demographics$pr=ifelse(demographics$resident_status=="Permanent Resident",1,0)
dim(demographics)
###3000 lignes et 11 colunnes###

###Outer join de bill_amount et bill_id###

df_bill = merge(x=bill_amount,y=bill_id, by="bill_id",all.y=TRUE)
dim(df_bill)
###13600 lignes et 4 colunnes###

###Outer join de clinical_data et demographics###
df_patient = merge(x=clinical_data,y=demographics, by="patient_id",all.y=TRUE)

#Change de format du date###
df_patient$date_of_admission <- format(as.Date(df_patient$date_of_admission, format = "%d/%m/%Y"), "%Y-%m-%d")
df_patient$date_of_discharge <- format(as.Date(df_patient$date_of_discharge, format = "%d/%m/%Y"), "%Y-%m-%d")
View(df_patient)
###3400 lignes et 36 colunnes###

#Contr�le des doublons#

duplicated(df_patient)
df_patient %>% distinct()
dim(df_patient)
###3400 lignes et 36 colunnes###

###R�sum� des donn�es###
summary(bill_amount)
summary(bill_id)
summary(clinical_data)
summary(demographics)

summary(df_bill)
summary(df_patient)

dim(df_bill)
###13600 lignes et 4 colonnes###

dim(df_patient)
###3400 lignes et 36 colonnes###

df_master1 = merge(x=df_bill, y=df_patient, by.x=c("patient_id", "date_of_admission"),by.y=c("patient_id", "date_of_admission"))

dim(df_master1)

###13600 lignes et 38 colonnes###

df_master1 %>% distinct()
dim(df_master1)
###13600 lignes et 38 colonnes###
sum(duplicated(df_master1))
df_master1 %>% distinct()
dim(df_master1)
###13600 lignes et 38 colonnes###

summary(df_master1)

#Les donn�es semblent manquer en raison d'une incompl�tude manuelle. Il sont univari�s.
#on peut conclure que les donn�es manquantes peuvent �tre class�es comme MCAR (Missing completely At Random) ou MAR(Missing
#at Random). On a pris la d�cision de remplacer chaque 'na' avec la m�thode KNN.



####### B) Exploraiton et analyse exploratoire des donn�es #########


str(df_master1)
head(df_master1)
df_master1$medical_history_3 = as.integer(df_master1$medical_history_3)
df_master1$gender = as.integer(df_master1$gender)

unique(df_master1$race)
# "Chinese" "Indian"   "Malay"   "Others"#
unique(df_master1$resident_status)
#"Singaporean" "Foreigner" "Permanent Resident"#

###Ajoute deux nouvelles colonnes "length_of_stay", "current_age" et BMI###

df_master1$length_of_stay<- difftime(df_master1$date_of_discharge ,df_master1$date_of_admission , units = c("days"))
df_master1$length_of_stay = as.numeric(df_master1$length_of_stay)


df_master1$current_age = as.numeric(difftime(Sys.Date(),df_master1$date_of_birth, units = "weeks"))/52.25
df_master1$current_age = as.numeric(df_master1$current_age)


df_master1$bmi <- df_master1$weight/(df_master1$height/100)**2


###Donn�es num�riques###
num=df_master1[sapply(df_master1,is.numeric)==T] 

###Donn�es qualitatifs###
quali=df_master1[sapply(df_master1,is.numeric)==F] 

###summary des variables ###
summary(num)
summary(quali) 


################################ C) Visualisation des donn�es ######################################


###correlation entre les donn�es num�riques###
M <- cor(num)
M
corrplot(M,method="ellipse")
#Corr�lation positive observ�e entre le sexe, le poids et la taille. #

# selection de la cible 

label=df_master1$amount
summary(label)

#Histogramme des variables num�riques de la table num

# Variables quantitative
hist(num$amount,xlab=names(num$amount),col=rainbow(10),main=names(num$amount))
hist(num$weight,xlab=names(num$weight),col=rainbow(10),main=names(num$weight))
hist(num$height,xlab=names(num$height),col=rainbow(10),main=names(num$height))# variable n'est pas normale
hist(num$lab_result_1,xlab=names(num$lab_result_1),col=rainbow(10),main=names(num$lab_result_1))
hist(num$lab_result_2,xlab=names(num$lab_result_2),col=rainbow(10),main=names(num$lab_result_2))
hist(num$lab_result_3,xlab=names(num$lab_result_3),col=rainbow(10),main=names(num$lab_result_3))

# variales binaires
hist(num$symptom_1,xlab=names(num$symptom_1),col=rainbow(10),main=names(num$symptom_1))
hist(num$symptom_2,xlab=names(num$symptom_2),col=rainbow(10),main=names(num$symptom_2))
hist(num$symptom_3,xlab=names(num$symptom_3),col=rainbow(10),main=names(num$symptom_3))
hist(num$symptom_4,xlab=names(num$symptom_4),col=rainbow(10),main=names(num$symptom_4))
hist(num$symptom_5,xlab=names(num$symptom_5),col=rainbow(10),main=names(num$symptom_5))
hist(num$preop_medication_1,xlab=names(num$preop_medication_1),col=rainbow(10),main=names(num$preop_medication_1))
hist(num$preop_medication_2,xlab=names(num$preop_medication_2),col=rainbow(10),main=names(num$preop_medication_2))
hist(num$preop_medication_3,xlab=names(num$preop_medication_3),col=rainbow(10),main=names(num$preop_medication_3))
hist(num$preop_medication_4,xlab=names(num$preop_medication_4),col=rainbow(10),main=names(num$preop_medication_4))
hist(num$preop_medication_5,xlab=names(num$preop_medication_5),col=rainbow(10),main=names(num$preop_medication_5))
hist(num$preop_medication_6,xlab=names(num$preop_medication_6),col=rainbow(10),main=names(num$preop_medication_6))
hist(num$medical_history_1,xlab=names(num$medical_history_1),col=rainbow(10),main=names(num$medical_history_1))
hist(num$medical_history_2,xlab=names(num$medical_history_2),col=rainbow(10),main=names(num$medical_history_2))
hist(num$medical_history_3,xlab=names(num$medical_history_3),col=rainbow(10),main=names(num$medical_history_3))
hist(num$medical_history_4,xlab=names(num$medical_history_4),col=rainbow(10),main=names(num$medical_history_4))
hist(num$medical_history_5,xlab=names(num$medical_history_5),col=rainbow(10),main=names(num$medical_history_5))
hist(num$medical_history_6,xlab=names(num$medical_history_6),col=rainbow(10),main=names(num$medical_history_6))
hist(num$medical_history_7,xlab=names(num$medical_history_7),col=rainbow(10),main=names(num$medical_history_7))


##########  Analyse des corr�lations  ###########

#t1=table(quali$gender,num$weight)
#assocstats(t1) 
##Coefficient de Cramer's V est 0.54, elle indique une relation forte entre les deux variables#

t2=table(quali$resident_status,num$amount)
assocstats(t2) 
#Coefficient de Cramer's V est 1.0, elle indique une relation forte entre les deux variables#

#t3=table(quali$resident_status,quali$gender)
#assocstats(t3) 
##Coefficient de Cramer's V est 0.036, elle indique une relation fiable entre les deux variables#

t4=table(quali$resident_status,num$current_age)
assocstats(t4)
#Coefficient de Cramer's V est 0.965, elle indique une relation forte entre les deux variables#

t5=table(num$length_of_stay,num$current_age)
assocstats(t5)
#Coefficient de Cramer's V est 0.902, elle indique une relation forte entre les deux variables#
##quel est la diff�rence entre le correlation et Cramer's V?###

t6=table(num$amount,num$current_age)
assocstats(t6)
#Coefficient de Cramer's V est 1.0, elle indique une relation forte entre les deux variables#

#t7=table(num$amount,quali$gender)
#assocstats(t7)
##Coefficient de Cramer's V est 1.0, elle indique une relation forte entre les deux variables#

###correlation entre les donn�es num�riques avec les 2 nouvelles variables###
M <- cor(num)
M
corrplot(M,method="ellipse")
#Corr�lation positive entre le sexe, la taille et le poids#


##############  Summary Statistiques  ##############

p1 <- df_master1 %>%
  group_by(race) %>%
  summarise(Avg_amount = mean(amount)) %>%
  arrange(desc(Avg_amount))
p1

ggplot(p1, aes(x=race, y=Avg_amount)) + geom_bar(stat="identity") + 
  labs(x="Race", y="Average Bill Amount")

###En moyenne, les Malays ont le plus grand 'Average Bill Amount'###

p2<- df_master1 %>%
  group_by(resident_status) %>%
  summarise(Avg_amount = mean(amount)) %>%
  arrange(desc(Avg_amount))
p2

ggplot(p2, aes(x=resident_status, y=Avg_amount)) + geom_bar(stat="identity") + 
  labs(x="Resident Status", y="Average Bill Amount")
###En moyenne, les �trangers ont le plus grand 'Average Bill Amount'###

p3<- df_master1 %>%
  group_by(race) %>%
  summarise(Avg_bmi = mean(bmi)) %>%
  arrange(desc(Avg_bmi))
#Malay et Indian = 29.1, Chinese = 29 et Others 28.2##
p3

ggplot(p3, aes(x=race, y=Avg_bmi)) + geom_bar(stat="identity") + 
  labs(x="Race", y="Average BMI")
###Les �trangers ont un plus bas BMI que les autres###

p4 <- df_master1 %>%
  group_by(race, gender) %>%
  summarise(Avg_bmi = mean(bmi)) 
p4

#ggplot(p4, aes(x = race, y = Avg_bmi)) +
#  geom_bar(
#    aes(colour = gender, fill = gender),
#    stat = "identity", position = position_stack()
#  ) +
#  scale_colour_manual(values = c("#0073C2FF", "#EFC000FF"))+
#  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

#En g�n�ral, les femmes onts plus grandes bmi que les hommes sauf les �trangers# 


p5 <- df_master1 %>%
  group_by(race, gender) %>%
  summarise(Avg_stay = mean(length_of_stay)) 
p5

#ggplot(p5, aes(x = race, y = Avg_stay)) +
#  geom_bar(
#    aes(colour = gender, fill = gender),
#    stat = "identity", position = position_stack()
#  ) +
#  scale_colour_manual(values = c("#0073C2FF", "#EFC000FF"))+
#  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

#en moyenne les femmes �trangers restent le plus bas#

p6<- df_master1 %>%
  group_by(gender) %>%
  summarise(Avg_amount = mean(amount)) %>%
  arrange(desc(Avg_amount))

p6

ggplot(p6, aes(x=gender, y=Avg_amount)) + geom_bar(stat="identity") + 
  labs(x="Gender", y="Average Amount")
#En moyenne les hommes payent plus que les femmes##

#################### D) Unsupervised learning ######################
###### ACP

# J'utilise num et et l'option de normalisation scale.unit = TRUE
res.pca <- PCA(num, scale.unit = TRUE, graph = TRUE)

# 1.1- M�thode 1 : eigen values

# valeurs propres et axes
# On garde les variables dont les?valeurs propres sont > 1

eig.val = get_eigenvalue(res.pca) 
eig.val

# on prend les 17 premi�re dimensions (valeurs propres > 1). elles ne representent que 61% de l'info
# nous avons clairmeent beosin de plus.

# 1.2- M�thode 2 : M�thode du coude

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 8))

# ne m'affiche pas toutes les dimensions , je ne sais pas trop pourquoi.

### 2) Extraction des r�sultats 

#Une m�thode simple pour extraire les r�sultats, pour les variables, � partir de l'ACP est d'utiliser la fonction
#get_pca_var() [package factoextra].
#Cette fonction retourne une liste d'�l�ments contenant tous les r�sultats pour les variables actives 
#(coordonn�es, corr�lation entre variables et les axes, cosinus-carr� et contributions)

var <- get_pca_var(res.pca)
var

#2.1- Coordonn�es

#coordonn�es des variables pour cr�er un nuage de points.
head(var$coord)


#2.2- Cos2: qualit� de r�presentation

# cosinus carr� des variables. Repr�sente la qualit� de repr�sentation des variables sur le graphique de l'ACP.
# Il est calcul� comme �tant les coordonn�es au carr�: var.cos2 = var.coord * var.coord.
head(var$cos2)

#2.3- Contributions aux composantes principales

#contient les contributions (en pourcentage), des variables, aux composantes principales.
#La contribution d'une variable (var) � une composante principale donn�e: (var.cos2 * 100) / (total cos2 du composant).
head(var$contrib)


###### 3) Cercle de corr�lation 

# Dans cette section, nous d�crivons comment visualiser les variables et tirer des conclusions c?ncernant leurs corr�lations.
# Ensuite, nous mettons en �vidence les variables selon 
# i) leurs qualit�s de repr�sentation sur le graphique 
# ii) leurs contributions aux composantes principales.

#3.1- Coordonn�es des variables
head(var$coord, 24)

#La corr�lation entre une variable et une composante principale (PC) est utilis�e comme coordonn�es de la variable
#sur la composante principale.
#La repr�sentation des variables diff�re de celle des observations: 
#les observations sont repr�sent�es par leurs projections, mais les variables sont repr�sent�es par leurs corr�lations 

#3.2- Visualisation des variables
fviz_pca_var(res.pca, col.var = "black") # il faut virer des variables 

# Le graphique ci-dessus est �galement connu sous le nom de graphique de corr�lation des variables.

###4) Qualit� de repr�sentation par les dimensi?ns 1 ,2 et 3

# La qualit� de repr�sentation des variables sur la carte de l'ACP s'appelle cos2 (cosinus carr�) . 
# On peut acc�der au cos2 comme suit:
head(var$cos2, 24)
# taille de point en fonction du cos2
fviz_pca_ind (res.pca, pointsize = "cos2", 
              pointshape = 21, 
              fill = "#E7B800", 
              repel = TRUE # �vite le chevauchement de texte
)

# On peut visualiser le cos2 des variables sur toutes les dimensions en utilisant le package corrplot
# Il est �galement possible ?e cr�er un bar plot du cosinus carr� des variables en utilisant la fonction fviz_cos2()

# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
fviz_cos2(res.pca, choice = "var", axes = 3:4)


#Les valeurs de cos2 sont utilis�es pour estimer la qualit� de la repr�sentation
#Plus une variable est proche du cercle de corr�lation, meilleure est sa repr�sentation sur la carte de l'ACP 
#(et elle est plus importante pour interpr�ter les composantes principales en consid�ration)
#Les variables qui sont proche du centre du graphique sont moins importantes pour les premi�res composantes.

# 4.1- colorer en fonction du cos2: qualit� de repr�sentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # �vite le chevauchement de texte
)
# On remarque qu'il y a plusieurs fl�ches en bleu ciel qui representent des variables qui n'apportent pas beaucoup d'info 

## 5. Contributions des variables aux axes principaux

#Les contributions des variables dans la d�finition d'un axe principal donn�, sont exprim�es en pourcentage.

#Les vari?bles corr�l�es avec PC1 (i.e., Dim.1) et PC2 (i.e., Dim.2) sont les plus importantes pour expliquer la variabilit� dans le jeu de donn�es.
#Les variables qui ne sont pas en corr�lation avec un axe ou qui sont corr�l�es avec les derniers axes sont des varia?les � faible apport et peuvent �tre supprim�es pour simplifier l'analyse globale.
#La contribution des variables peut �tre extraite comme suit:

head(var$contrib, 24)
#Plus la valeur de la contribution est importante, plus la variable contribue � la composante principale en question.

# 5.1- plot des contributions 

#La fonction fviz_contrib() [package factoextra] peut �tre utilis�e pour cr�er un bar plot de la contribution des variables. Si vos donn�es contiennent de nombreuses variables, vous pouvez d�cide? de ne montrer que les principales variables contributives.
# Le code R ci-dessous montre le top 10 des variables contribuant le plus aux composantes principales:

# Contributions des variables � PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables � PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
#La contribution totale � PC1 et PC2 est obtenue avec le code R suivant:
fviz_contrib(res.pca, choice = "var", axes = 1:5, top = 10)

#Les variables les plus importantes (ou, contributives) peuvent �tre mises en �vidence sur le graphe de corr�lation

fviz_pca_var(res.pca, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
#les variables qui apparaissent le plus sont sp, pr, gender, bmi, height et weight


########################## E) Supervised Learning ################################

#les variables de date d'abandon et les variables cat�gorielles pour lesquelles des variables indicatrices ont �t� cr��es#
df_master2 <- subset(df_master1, select=-c(race, resident_status, date_of_discharge, date_of_admission, date_of_birth,patient_id,bill_id))
dim(df_master2)
###13600 lignes et 34 colonnes###

############ Non-linear Model: Decision Tree ###############

library(rpart)
library(rpart.plot) 

trainDT=sample(1:nrow(df_master2),0.6*nrow(df_master2))
train=df_master2[trainDT,]
test=df_master2[-trainDT,]

dim(train)
#8160 lignes et 34 colonnes#
dim(test)
#5440 lignes et 34 colonnes#

tree=rpart(amount~.,data=train)
plotcp(tree)


print(tree$cptable[which.min(tree$cptable[,4]),1])
#O.01#

tree_Opt <- prune(tree,cp=tree$cptable[which.min(tree$cptable[,4]),1])
prp(tree_Opt,extra=1)

summary(df_master2$amount)

df_master2$target=cut(df_master2$amount, c(4740, 9382, 16640, Inf), labels = FALSE)
df_master2$amount=NULL

trainDT=sample(1:nrow(df_master2),0.6*nrow(df_master2))
train=df_master2[trainDT,]
test=df_master2[-trainDT,]


tree_class=rpart(target~.,data=train, method = 'class')
print(tree_class$cptable[which.min(tree$cptable[,4]),1])
#0.02023299

# Elagage de l'arbre avec le cp optimal
tree_Opt <- prune(tree_class, 
                  cp=tree_class$cptable[which.min(tree_class$cptable[,4]),1])
#Repr�sentation graphique de l'arbre optimal
prp(tree_Opt,extra=1)

#prediction
pred_class=predict(tree_class,newdata=test,type='class')
test$pred=pred_class

m=table(test$target,test$pred)

#matrice de confusion
mat_arbre=caret::confusionMatrix(m)    
print(mat_arbre)
##accuracy of 62.2%#

################ Linear Model: Multiple Linear Regression #############

#les variables de date d'abandon et les variables cat�gorielles pour lesquelles des variables indicatrices ont �t� cr��es#
df_master2 <- subset(df_master1, select=-c(race, resident_status, date_of_discharge, date_of_admission, date_of_birth,patient_id,bill_id))
dim(df_master2)
#13600 lignes et 34 colonnes#

# Split the data into training and test data with 80:20 ratio
train<-sample_frac(df_master2, 0.8)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-df_master2[-sid,]

#create model with training data
model_master2 <- lm(amount~current_age+gender+bmi+id+ch+ml+sp+fr+length_of_stay+weight+height, data=train)

summary(model_master2)
par(mfrow = c(2, 2))
plot(model_master2)

#Calculate SST, SSR and SSRes

SST = sum((train$amount-mean(train$amount))^2)
SSRes = sum((train$amount - model_master2$fitted.values)^2)
SSR = sum((model_master2$fitted.values - mean(train$amount))^2)
cat("SST = " , SST)
cat("SSR + SSRes" , (SSR + SSRes)) # =SST

Rsquare = SSR/SST
cat("R-square = ", Rsquare)
##R-squared 5%##

# obtain MSRes
MSRes = (summary(model_master2)$sigma)^2

# obtain standardized residuals
standardized_res=model_master2$residuals/sqrt(MSRes)

# PRESS residuals
PRESS_res=model_master2$residuals/(1 - lm.influence(model_master2)$hat)
summary(PRESS_res)

# studentized residuals
studentized_res=model_master2$residuals/sqrt(MSRes*(1-lm.influence(model_master2)$hat))

#Check for data points with high residuals
studentized_res[studentized_res>3 | studentized_res<(-3)]

#Check for data points with high leverage
influence = lm.influence(model_master2)$hat
summary(influence) #no high influence points


#Check for non-constancy of error variance with each covariate
par(mfrow = c(1,1))
qqnorm(model_master2$residuals)


#Plot fitted values vs Residual
plot(x=model_master2$fitted.values,model_master2$residuals, xlab = "Fitted Values", ylab = "Residuals", pch=20)


#Check if boxcox suggests any transformations on response variable
require(MASS)
par(mfrow = c(1, 1))
boxcox(model_master2) 

#model with cube-root transformation on y
train$cuberoot_amount = (train$amount)^(1/3)

#create new model with cube-root transformed response variable
model_transformed <- lm(cuberoot_amount~current_age+gender+bmi+id+ch+ml+sp+fr+length_of_stay+weight+height, data=train)
summary(model_transformed)
par(mfrow = c(2, 2))
plot(model_transformed)


#check for multicollinearity
vif(model_master2)

#high multicolinearity with weight and height#

#Now lets start with removing variable as per the least significance

model_final <- lm(amount~current_age+id+ch+ml+sp+fr, data=train)

summary(model_final)
plot(model_final)

vif(model_final)
#multiple correlation problem solved#

#training the final model using cross-validation
model_final <- train(
  amount~current_age+id+ch+ml+sp+fr, train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
head(test)
head(train)

#Predict results on test data
predicted_test <- predict(model_final, test)
plot(x=test$amount, y=predicted_test, xlab="Actual", ylab="Predicted", col="blue")

plot(model_final)
qqnorm(model_final$residuals)

# Model performance
# (a) Prediction error, RMSE=7351.167
RMSE(predicted_test, test$amount)
# (b) R-square=0.053
R2(predicted_test, test$amount) 
