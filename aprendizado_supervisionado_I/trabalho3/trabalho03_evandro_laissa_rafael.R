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
# sex: possui fator/classe "not informed"
# lives_in_Wuhan: possui fator/classe "not informed"
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

#TODO: remover os duplicados?
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

randomNoIdx <- sample(1:nrow(dataTrainNo), size=1.4*nrow(dataTrainYes))
subsamplingNo <- dataTrainNo[randomNoIdx,]
dataTrain <- rbind(dataTrainYes, subsamplingNo)


###############################################################################
# Questao 2                                                                   #
# ---------                                                                   #
# Treinem uma árvore de decisão como baseline e reportem a matriz de confusão #
# relativa e a acurácia balanceada nos conjuntos de treinamento, validação    #
# e teste.                                                                    #
#                                                                             #
###############################################################################


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
