#----------------------------------------------------------------#
# INF-0615 Aprendizado Supervisionado I                          #
#                                                                #
# Trabalho 03                                                    #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
#                                                                #
# - Evandro Santos Rocha                                         #
# - Laíssa Pacheco de Oliveira                                   #
# - Rafael Dantas de Moura                                       #
#                                                                #
#----------------------------------------------------------------#

####### Código de apoio ao Trabalho 03 da disciplina INF-0615 #######

# Funcao que calcula a matriz de confusao relativa para 3 classes
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi??o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
    cm_relative[3,] = round(cm_absolute[3,]/sum(cm_absolute[3,]), digits=2)
    
    return(cm_relative)  
}


# Leitura da base de treinamento+validacao
train_val_set <- read.csv("train_val_set_patient_status_covid19.csv", stringsAsFactors = T)

####### ======= O TRABALHO COME?A A PARTIR DAQUI ======= #######


library(caret)
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)


# Configurem o valor da semente.
set.seed(45) # Coloquem o valor que desejarem.



###############################################################################
# Questao 1                                                                   #
# ---------                                                                   #
#                                                                             #
# Inspecionem os dados de treinamento. Quantos exemplos há de cada classe?    #
# O dataset está desbalanceado? Se sim, como vocês lidarão                    #
# com o desbalanceamento ?                                                    #
#                                                                             #
###############################################################################

dim(train_val_set)
#[1] 36421    15

any(is.na(train_val_set))
#[1] FALSE

summary(train_val_set)
# informacoes interessantes:
# age: tem máximo igual a 99.
# lives_in_Wuhan: possui fator/classe "not informed"
# sex: possui fator/classe "not informed"
# chronic_disease_binary: possui somente 32 exemplos (0.0879%)

# olhando alguns dos dados interessantes
train_val_set[train_val_set$chronic_disease_binary == "True",]  # maioria morreu
table(train_val_set[train_val_set$chronic_disease_binary == "True",]$label)
#dead onTreatment   recovered 
#25           0           7 

#------------------------------------------------#
# Remove os elementos repetidos antes da divisão Treino/Validação/Test
#------------------------------------------------#

dim(unique(train_val_set))
#[1] 30351    15

dim(train_val_set) - dim(unique(train_val_set))
# há 6070 exemplos duplicados...

#remover os duplicados
train_val_set <- unique(train_val_set)

#------------------------------------------------#
# Conjuntos de Treinamento e Validacao
#------------------------------------------------#

randomTrainIndexes <- sample(1:nrow(train_val_set), size=0.8*nrow(train_val_set))
dataTrain <- train_val_set[randomTrainIndexes, ]
dataVal  <- train_val_set[-randomTrainIndexes, ] 

merge(dataTrain, dataVal)
#merge(dataTrain, dataTest)
#merge(dataVal, dataTest)

dim(dataTrain)
#[1] 24280    15
dim(dataVal)
#[1] 6071   15

#------------------------------------------------#
# Faz o balanceamento das classes
#------------------------------------------------#

# verifica o balanceamento
trainFrequency = table(dataTrain$label)
trainFrequency
#dead onTreatment   recovered 
#1412       17206        5662

####### Balanceamento por Undersampling #######
dataTrainDead        <- dataTrain[dataTrain$label == "dead",]
dataTrainOnTreatment <- dataTrain[dataTrain$label == "onTreatment",] 
dataTrainRecovered   <- dataTrain[dataTrain$label == "recovered",] 

dim(dataTrainDead)
#[1] 1412   15
dim(dataTrainOnTreatment)
#[1] 17206    15
dim(dataTrainRecovered)
#[1] 5662   15

# 1.5 : ficou ruim (acc = 0.91)
# 2.5 : mostrou melhor desenho (acc = 0.906)
# 3.5 : piorou a acuracia (acc = 0.85)
# 2.1 : bom desenho e boa acuracia (acc = 0.91)
randomOnTreatmentIdx <- sample(1:nrow(dataTrainOnTreatment), size=2.1*nrow(dataTrainDead))
subsamplingOnTreatment <- dataTrainOnTreatment[randomOnTreatmentIdx,]

# 1.4 : ficou ruim (acc = 0.91)
# 2.4 : piorou, a curva desenhada parece que nao está bem balanceada (acc = 0.91)
# 1.2 : melhorou a curva, mas 1.4 parece melhor
# 1.6 : melhorou bem o desenho (acc = 0.91)
randomRecoveredIdx <- sample(1:nrow(dataTrainRecovered), size=1.4*nrow(dataTrainDead))
subsamplingRecovered <- dataTrainRecovered[randomRecoveredIdx,]

# (2.1, 1.4) : desenho bom, acc = 0.91                      <<<<<--- melhor desenho
# (2.1, 1.6) : desenho quase bom, acc = 0.913333            <<<<<--- melhor acurácia
# (3.0, 3.0) : desenho melhor que o anterior, acc = 0.9133333
# (3.5, 3.0) : piorou o desenho
# (1.5, 3.0) : piorou tudo
# (4.5, 3.0) : melhor que o anterior, mas a acc = 0.91

dataTrain <- rbind(dataTrainDead, subsamplingOnTreatment, subsamplingRecovered)

dim(dataTrain)
#[1] 6353   15

table(dataTrain$label)
#dead onTreatment   recovered 
#1412        2965        1976 

###############################################################################
# Questao 2                                                                   #
# ---------                                                                   #
# Treinem uma árvore de decisão como baseline e reportem a matriz de confusão #
# relativa e a acurácia balanceada nos conjuntos de treinamento, validação    #
# e teste.                                                                    #
#                                                                             #
###############################################################################


## Árvore de Decisão
#help(rpart)

# minsplit = número mínimo de exemplos em um nó para que ele gere nós 
# filhos.
# cp = fator que determina o quanto o erro no conjunto de treinamento deve 
# ser diminuido para que a geração de filhos (split) seja realizada. 
# xval = número de validações cruzadas que serão realizadas. Ou seja, 
# xval = 10 significa que a divisão treinamento/validação será realizado 10
# vezes. 

summary(dataTrain)
head(dataTrain,0)
colnames(dataTrain)
#label
#age, sex, country, latitude, longitude, date_onset_symptoms, 
#date_admission_hospital, date_confirmation, lives_in_Wuhan,
#travel_history_dates, travel_history_location, chronic_disease_binary,
#date_death_or_discharge, travel_history_binary


# Se quisermos usar como critério a Entropia + Ganho de Informação - parâmetro
# "information"
treeModel <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                       date_onset_symptoms + date_admission_hospital + 
                       date_confirmation + lives_in_Wuhan + travel_history_dates + 
                       travel_history_location + chronic_disease_binary +
                       date_death_or_discharge + travel_history_binary, 
                   data=dataTrain, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                   parms= list(split="information"))


summary(treeModel)

######### Avaliação no conjunto de Treinamento ##########

# Vamos ver a performance no conjunto de treinamento
val_train <- predict(treeModel, dataTrain, type="class")
cm <- confusionMatrix(data = as.factor(val_train), 
                      reference = as.factor(dataTrain$label), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#              Prediction
#Reference      dead onTreatment recovered
#dead        1.00        0.00      0.00
#onTreatment 0.00        0.98      0.02
#recovered   0.00        0.02      0.98

acc_bal <- (cm_relative[1,1] + cm_relative[2,2]+ cm_relative[3,3])/3
acc_bal
#[1] 0.9866667

######### Avaliação no conjunto de Validação ##########

# Vamos ver a performance no conjunto de validação
val_pred <- predict(treeModel, dataVal, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(dataVal$label), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#              Prediction
#Reference      dead onTreatment recovered
#dead           0.98        0.01      0.00
#onTreatment    0.00        0.84      0.15
#recovered      0.00        0.22      0.78

acc_bal <- (cm_relative[1,1] + cm_relative[2,2]+ cm_relative[3,3])/3
acc_bal
#[1] 0.8666667

######### TODO: Avaliação no conjunto de Teste ##########


###############################################################################
# Questao 3                                                                   #
# ---------                                                                   #
#                                                                             #
# Treinem outras árvores de decisão variando o tamanho das árvores geradas.   #
# Plotem a acurácia balanceada no conjunto de treinamento e validação pela    #
# profundidade da árvore de decisão. Identifiquem as regiões de underfitting, #
# ponto ótimo e overfitting. Tomem a árvore com tamanho ótimo e reportem      #
# também a matriz de confusão relativa e a acurácia balanceada no             #
# conjunto de teste.                                                          #
#                                                                             #
###############################################################################

########## ACC Vs Depth 
# Vamos ver como as acurácias no conjunto de treinamento e de validação
# variam conforme variamos o tamanho limite das arvores
number_of_depths = 15
accPerDepth <- data.frame(depth=numeric(number_of_depths), 
                          accTrain=numeric(number_of_depths), 
                          accVal=numeric(number_of_depths))
#summary(accPerDepth)
for (maxDepth in 1:number_of_depths){
    treeModel <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                           date_onset_symptoms + date_admission_hospital + 
                           date_confirmation + lives_in_Wuhan + travel_history_dates + 
                           travel_history_location + chronic_disease_binary +
                           date_death_or_discharge + travel_history_binary, 
                       data=dataTrain, method="class",
                       control=rpart.control(minsplit=2, cp=0.0, 
                                             maxdepth=maxDepth, xval = 10),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treino
    train_pred <- predict(treeModel, dataTrain, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(dataTrain$label), 
                                positive='yes')
    
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_relative_train[1,1] + cm_relative_train[2,2] + cm_relative_train[3,3])/3
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(treeModel, dataVal, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(dataVal$label), 
                              positive='yes')
    
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_relative_val[1,1] + cm_relative_val[2,2] + cm_relative_val[3,3])/3
    
    accPerDepth[maxDepth,] = c(maxDepth, acc_bal_train, 
                               acc_bal_val)
}


accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()

#ponto ótimo
bestMaxdepth <- 8

#acurácia no ponto ótimo
accPerDepth[accPerDepth$depth == bestMaxdepth,]
#depth variable     value
#8      8 accTrain 0.9100000
#23     8   accVal 0.9033333

#TODO: avaliar cm_relative e acc_bal no ponto ótimo para o conjunto de teste


###############################################################################
# Questao 4                                                                   #
# ---------                                                                   #
#                                                                             #
# Explorem pelo menos 2 possíveis subconjuntos de features (feature           #
# selection) para treinar uma árvore de decisão. Tomem o melhor modelo e      #
# reportem a matriz de confusão relativa e a acurácia balanceada do no        #
# conjunto de teste.                                                          #
#                                                                             #
###############################################################################

# inicialmente, montamos uma treemodel com todos os atributos
# já considerando o ponto ótimo
treeModel <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                       date_onset_symptoms + date_admission_hospital + 
                       date_confirmation + lives_in_Wuhan + travel_history_dates + 
                       travel_history_location + chronic_disease_binary +
                       date_death_or_discharge + travel_history_binary, 
                   data=dataTrain, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10, 
                                         maxdepth = bestMaxdepth),
                   parms= list(split="information"))

### Verificando a importância de cada feature ###
importance_per_feature <- treeModel$variable.importance
relative_importance <- importance_per_feature/sum(importance_per_feature)
relative_importance

#date_death_or_discharge date_admission_hospital    country         travel_history_dates       longitude 
#0.210065921             0.172110982                0.136710490     0.131942896             0.131503242 
#
#lives_in_Wuhan                 age       date_confirmation                 sex             travel_history_location 
#0.119467822             0.030831058             0.028802451             0.022846366         0.005514022 
#
#latitude   travel_history_binary     date_onset_symptoms 
#0.004957255             0.003309809             0.001937687


# Primeiro subconjunto: features que têm grande importância relativa (maior que 10%)

treeModel_1 <- rpart(formula=label ~ country + longitude + 
                       date_admission_hospital + 
                       lives_in_Wuhan + travel_history_dates + 
                       date_death_or_discharge, 
                   data=dataTrain, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10, 
                                         maxdepth = bestMaxdepth),
                   parms= list(split="information"))

prp(treeModel_1)

# Segundo subconjunto: poucas features que têm grande importância (soma de importância ~ 66%)

treeModel_2 <- rpart(formula=label ~ country + longitude + 
                         date_admission_hospital + 
                         date_death_or_discharge, 
                     data=dataTrain, method="class",
                     control=rpart.control(minsplit=2, cp=0.0, xval = 10, 
                                           maxdepth = bestMaxdepth),
                     parms= list(split="information"))

prp(treeModel_2)

# Terceiro subconjunto: features que dizem respeito ao individuo, sintomas e viagem 
treeModel_3 <- rpart(formula=label ~ 
                         age + sex + chronic_disease_binary +
                         date_onset_symptoms + date_admission_hospital + 
                         date_confirmation +
                         lives_in_Wuhan + travel_history_dates + 
                         travel_history_location + travel_history_binary,
                   data=dataTrain, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10, 
                                         maxdepth = bestMaxdepth),
                   parms= list(split="information"))

prp(treeModel_3)

######### Avaliação no conjunto de Validação - treeModel_1 ##########

# Vamos ver a performance no conjunto de validação
val_pred <- predict(treeModel_1, dataVal, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(dataVal$label), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#              Prediction
#Reference      dead onTreatment recovered
#dead        1.00        0.00      0.00
#onTreatment 0.00        0.91      0.09
#recovered   0.00        0.50      0.50

acc_bal <- (cm_relative[1,1] + cm_relative[2,2]+ cm_relative[3,3])/3
acc_bal
#[1] 0.8033333

######### Avaliação no conjunto de Validação - treeModel_2 ##########

# Vamos ver a performance no conjunto de validação
val_pred <- predict(treeModel_2, dataVal, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(dataVal$label), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#              Prediction
#Reference      dead onTreatment recovered
#dead        0.99        0.00      0.01
#onTreatment 0.00        0.91      0.09
#recovered   0.00        0.50      0.50

acc_bal <- (cm_relative[1,1] + cm_relative[2,2]+ cm_relative[3,3])/3
acc_bal
#[1] 0.8

######### Avaliação no conjunto de Validação - treeModel_3 ##########

# Vamos ver a performance no conjunto de validação
val_pred <- predict(treeModel_3, dataVal, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(dataVal$label), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#              Prediction
#Reference      dead onTreatment recovered
#dead        0.99        0.00      0.00
#onTreatment 0.00        0.73      0.27
#recovered   0.00        0.01      0.98

acc_bal <- (cm_relative[1,1] + cm_relative[2,2]+ cm_relative[3,3])/3
acc_bal
#[1] 0.9


#TODO: avaliar no conjunto de teste


###############################################################################
# Questao 5                                                                   #
# ---------                                                                   #
#                                                                             #
# Treinem várias florestas aleatórias variando o número de árvores. Plotem a  #
# acurácia balanceada no conjunto de treinamento e validação variando o       #
# número de árvores geradas. Identifiquem as regiões de underfitting, ponto   #
# ótimo e overfitting. Reportem também a matriz de confusão relativa e a      #
# acurácia balanceada no teste para a floresta com o melhor número de árvores.#s
#                                                                             #
###############################################################################

#help(randomForest)


# Treina uma Floresta Aleatória
rfModel <- randomForest(formula=label ~ age + sex + country + latitude + longitude + 
                            date_onset_symptoms + date_admission_hospital + 
                            date_confirmation + lives_in_Wuhan + travel_history_dates + 
                            travel_history_location + chronic_disease_binary +
                            date_death_or_discharge + travel_history_binary, 
                        data= dataTrain, ntree=100, mtry=4)

# Plotando o erro para cada classe a para OOB. Para saber
# o significado e explicacao do OOB veja o exercicio 07 
# na secao de Floresta Aleatoria.

layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) # Sem margem no lado direito
plot(rfModel, log="y")
par(mar=c(5,0,4,2)) # Sem margem do lado esquerdo
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rfModel$err.rate),col=1:4,cex=0.8,fill=1:4)


# Matriz de Confusão
val_pred <- predict(rfModel, dataVal, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(dataVal$label), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#               Prediction
#Reference      dead onTreatment recovered
#dead        1.00        0.00      0.00
#onTreatment 0.00        0.74      0.26
#recovered   0.00        0.02      0.98

acc_bal <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_bal
#[1] 0.9066667


# Vamos verificar agora como as acurácias de treinamento e de validação
# variam com o número de árvores na floresta aleatória
#nTreeList = c(1, 5, 10, 25, 50, 75, 100, 150, 200, 250, 500)
#nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500) #, 1000)
#nTreeList = c(1, 2, 3, 5, 8, 10, 15, 20, 25, 50, 100, 150) #, 1000)

# legal para ver mais longe, mas nao mostra o ponto/região ótimo
#nTreeList = c(1, 2, 3, 5, 8, 10, 11:25, 30, 35, 40, 50, 100, 150, 200, 500)
#nTreeList = c(1, 2, 3, 5, 8, 10, 11:25, 30, 35, 40, 50, 75, 100, 125)
#nTreeList = c(1, 2, 3, 5, 8, 10, 11:25, 30, 35, 40, 50, 60, 70, 80)
nTreeList = c(1, 2, 3, 5, 8, 10, 11:25, 30, 35, 40, 50, 75, 100, 125, 150, 175, 200)

# legal para mostrar no gráfico
#nTreeList = c(1, 2, 3, 5, 8, 10, 11:25, 30, 35, 40, 50)

#definitivo
#nTreeList = c(1, 5, 10:25, 30, 40, 50, 75, 100, 150, 200, 250)


accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))

for (i in 1:length(nTreeList)){
    
    cat("i: ", i, "\t ntree: ", nTreeList[i])

    rfModel <- randomForest(formula=label ~ age + sex + country + latitude + longitude + 
                                date_onset_symptoms + date_admission_hospital + 
                                date_confirmation + lives_in_Wuhan + travel_history_dates + 
                                travel_history_location + chronic_disease_binary +
                                date_death_or_discharge + travel_history_binary, 
                            data= dataTrain, ntree=nTreeList[i], mtry=4)
    
    # Avaliando no conjunto de treino
    train_pred <- predict(rfModel, dataTrain, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(dataTrain$label), 
                                positive='yes')
    
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_relative_train[1,1] + cm_relative_train[2,2] + cm_relative_train[3,3])/3
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(rfModel, dataVal, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(dataVal$label), 
                              positive='yes')
    
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_relative_val[1,1] + cm_relative_val[2,2] + cm_relative_val[3,3])/3
    
    accPerNTrees[i,] = c(nTreeList[i], 
                         acc_bal_train, 
                         acc_bal_val)
    
    cat("\t acc_bal_train: ", acc_bal_train, "\t acc_bal_val: ", acc_bal_val, "\n")
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()

#bestNTree <- 21
bestNTree <- 23


accPerNTrees[accPerNTrees$ntree == bestNTree,]
max(accPerNTrees[accPerNTrees$variable == "accVal",]$value)



#TODO: aplicar no conjunto de teste


###############################################################################
# Questao 6.a                                                                 #
# -----------                                                                 #
#                                                                             #
# A diferença de desempenho entre o baseline e os outros modelos mais         #
# complexos gerados.                                                          #
#                                                                             #
###############################################################################

###############################################################################
# Questao 6.b                                                                 #
# -----------                                                                 #
#                                                                             #
# Houve overfitting ? Houve underfitting ? Analisem as curvas viés/variância  #
# geradas ao longo do trabalho.                                               #
#                                                                             #
###############################################################################

###############################################################################
# Questao 6.c                                                                 #
# -----------                                                                 #
#                                                                             #
# Uma seção de conclusão do relatório explicando a diferença entre os modelos #
# e o porquê que estas diferenças levaram a resultados piores ou melhores.    #
#                                                                             #
###############################################################################


###############################################################################
# Opcional 1                                                                  #
# -----------                                                                 #
#                                                                             #
# Implementem manualmente o protocolo Random Forest de forma que cada         #
# árvore na floresta tenha as mesmas quantidades de exemplos das três         #
# classes. Note que, para cada modelo, vocês devem selecionar com repetição   #
# um subconjunto de exemplos de cada uma das classe para treiná-lo.           #
#                                                                             #
###############################################################################

 

###############################################################################
# Opcional 2                                                                  #
# -----------                                                                 #
#                                                                             #
# Variem o número de features consideradas no treinamento.                    #
# Utilizando raiz_quadrada(m), m/2 e 3m/4 atributos, em que m é o número      #
# total de atributos que vocês têm disponível.                                #
#                                                                             #
###############################################################################

###############################################################################
# Opcional 3                                                                  #
# -----------                                                                 #
#                                                                             #
# Reportem seus resultados e suas conclusões no relatório. Esses              #
# resultados foram melhores que os modelos treinados realizando o             #
# balanceamento a priori?                                                     #
#                                                                             #
###############################################################################



# Leitura da base de Teste. Descomentem as linhas abaixo quando o 
# conjunto de teste estiver dispon?vel.

#test_set <- read.csv("test_set_patient_status_covid19.csv", stringsAsFactors = T) # Descomentar

# As duas linhas abaixo s?o um trick para corrigir os "levels" na
# coluna country. Ele apenas adiciona 1 exemplo de treino na primeira
# linha do teste e depois retira-o para obter o test_set original. 
# Nao se preocupem, eh apenas para nivelamento interno do R. 
# Certifiquem-se de executar os comandos na seguinte ordem:
# linha 38, linha 47 e linha 48 quando a base de teste estiver disponivel

#temporary_test <- rbind(train_val_set[1,], test_set) # Descomentar
#test_set <- temporary_test[-1,] # Descomentar

max(accPerNTrees[accPerNTrees$variable == "accVal",]$value)
