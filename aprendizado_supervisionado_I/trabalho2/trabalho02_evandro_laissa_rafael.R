#----------------------------------------------------------------#
# INF-0615 Aprendizado Supervisionado I                          #
#                                                                #
# Trabalho 02                                                    #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
#                                                                #
# - Evandro Santos Rocha                                         #
# - Laíssa Pacheco de Oliveira                                   #
# - Rafael Dantas de Moura                                       #
#                                                                #
#----------------------------------------------------------------#


#######################################
# FUNÇÕES DE APOIO                    #
#######################################

# Escreve a funcao de hipotese dada as features continuas e o 
# respectivo grau polinomial
getHypothesis <- function(feature_names, degree){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}


# Calcula a loss de um modelo dado os valores preditos e os labels
getLoss <- function(y_true, y_pred){
    y_true <- as.numeric(y_true) - 1
    
    totalLoss <- 0
    eps <- 1e-9
    # Recall: length(y_true) == length(y_pred)
    # loss = (1-y)*log2(1 - p + eps)) + y*log(p + eps)
    # eps is used for numerical stability, it is very close to 0.
    # Supose we have y = 1 and p = 1 (perfect prediction), the loss (without eps)
    # would be 0*log2(0) + 1*log(1). It would result in NaN
    # because of 0*log2(0). With eps: 0*log2(1e-9) + 1*log(1 + 1e-9) 
    for(i in 1:length(y_true)){
        loss <- -1*((1 - y_true[i])*log2(1 - y_pred[i] + eps) + y_true[i]*log2(y_pred[i] + eps))
        totalLoss <- totalLoss + loss
    }
    totalLoss <- totalLoss/(length(y_true))
    return(totalLoss)
}

# Calcula a matriz de confusão relativa 
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposição para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2)
    cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
    cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2)
    
    return(cm_relative)  
}


#######################################
# PREPARAÇÃO DE AMBIENTE DE TRABALHO  #
#######################################

#install.packages("glmnet")
#install.packages("caret")
#install.packages("pROC")

set.seed(13)

library(glmnet)
library(caret)
library(pROC)


#######################################
# DÚVIDAS / PENDÊNCIAS                #
#######################################
# #1. ( ) Conjunto de teste e SARS
# #2. ( ) Relatório
# #3. (X) Encontrar os pontos ótimos para cada simulação
# #4. ( ) Experimentar normalizar com Min-Max
# #5. ( ) Quando normalizado por Z-Norma, o desvio-padrão não foi igual a 1. É um problema?
# #6. ( ) Testar outro balanceamento?
# #7. ( ) Desenhar a curva ROC para outros balanceamento
# #8. ( ) Avaliar as matrizes de confusão
# #9. ( ) Plotar ViesVariancia e Acuraria tudo junto



###############################################################################
# Questao 1                                                                   #
# ---------                                                                   #
#                                                                             #
# Inspecionem os dados. Quantos exemplos vocês tem? Há exemplos com           #
# features sem anotações? Como vocês lidariam com isso?                       #
#                                                                             #
###############################################################################


### Carregando as bases de dados ### 
trainSet <- read.csv("proteins_training_set.csv")
valSet <- read.csv("proteins_validation_set.csv")
testSet <- read.csv("proteins_test_set.csv")
sarsSet <- read.csv("SARS_test_set.csv")

summary(trainSet)
summary(valSet)
summary(testSet)
summary(sarsSet)

head(trainSet, 5)
#  start_position end_position chou_fasman emini kolaskar_tongaonkar parker isoelectric_point aromaticity hydrophobicity stability target
#1           1065         1076       0.994 1.790               1.010  2.367          5.562073  0.11792829    -0.04501992  32.41882      1
#2            380          392       1.065 0.209               1.024  0.885          5.718933  0.07129456    -0.19005629  34.84936      1
#3             11           18       0.950 1.080               1.048  1.950          4.348450  0.15267176    -0.31145038  35.35763      1
#4            105          119       0.836 0.028               1.083 -0.827         11.713196  0.06428571    -0.62285714  79.02714      1
#5            172          186       1.014 0.185               1.044  1.960          4.749329  0.03626943     0.30932642  30.14974      1

head(valSet, 5)
#  start_position end_position chou_fasman emini kolaskar_tongaonkar parker isoelectric_point aromaticity hydrophobicity stability target
#1            581          590       1.081 5.160               0.916  5.500          6.740540  0.10000000    -0.70412500  33.18163      1
#2            366          375       0.728 0.778               1.014  0.550          5.702576  0.03141361    -0.07643979  30.19319      1
#3            173          181       0.939 1.327               1.012  1.800          5.226501  0.07370518     0.01633466  14.67331      1
#4            583          597       1.038 0.530               1.049  0.267          4.971985  0.08785942    -0.10143770  47.00623      1
#5            416          430       0.915 0.096               1.111  1.513          8.778992  0.05454545     0.15490909  43.05618      1

head(testSet, 5)
#  start_position end_position chou_fasman emini kolaskar_tongaonkar parker isoelectric_point aromaticity hydrophobicity stability target
#1             22           26       0.888 0.633               0.974   2.66          6.867493  0.10384615     -0.5788462  21.68462      1
#2             28           32       0.916 1.757               1.040   1.70          6.899475  0.12582781      0.2562914  20.35828      1
#3            262          266       0.822 0.110               1.157  -4.18          4.988220  0.06060606     -0.6154882  43.38855      1
#4              2            6       1.198 2.614               0.923   4.38          4.487366  0.05509182     -0.6919866  68.59666      1
#5            154          158       1.042 1.919               0.984   4.26         10.843933  0.02577320     -1.0726804  22.58918      1


head(sarsSet, 5)
#  start_position end_position chou_fasman emini kolaskar_tongaonkar parker isoelectric_point aromaticity hydrophobicity stability target
#1              1           17       0.887 0.040               1.056 -2.159          5.569763   0.1163347    -0.06111554  33.20512      0
#2              1           15       0.869 0.047               1.056 -2.500          5.569763   0.1163347    -0.06111554  33.20512      0
#3              2           10       0.621 0.042               1.148 -7.467          5.569763   0.1163347    -0.06111554  33.20512      0
#4              6           20       1.021 0.230               1.049  0.927          5.569763   0.1163347    -0.06111554  33.20512      0
#5              9           25       1.089 0.627               1.015  3.165          5.569763   0.1163347    -0.06111554  33.20512      0

#------------------------------------------------#
# Tamanhos dos conjuntos de dados                #
#------------------------------------------------#

dim(trainSet)
#[1] 9204   11

dim(valSet)
#[1] 2303   11

dim(testSet)
#[1] 2878   11

dim(sarsSet)
#[1] 520  11

#-----------------------------------------------------------#
# Verificação se há registros repetidos entre os conjuntos  #
#-----------------------------------------------------------#

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)

# transformando target em fator
trainSet$target <- as.factor(trainSet$target)
valSet$target   <- as.factor(valSet$target)
testSet$target  <- as.factor(testSet$target)
sarsSet$target  <- as.factor(sarsSet$target)


#------------------------------------------------#
# Tratamento de erros: NAs                       #
#------------------------------------------------#

#Há exemplos com features sem anotações?
any(is.na(trainSet))
any(is.na(valSet))
any(is.na(testSet))
any(is.na(sarsSet))


###############################################################################
# Questao 2                                                                   #
# ---------                                                                   #
#                                                                             #
# Inspecionem a frequência de cada classe.                                    # 
# A base de dados está balanceada ?                                           #
# Se não, como vocês lidarão com o desbalanceamento ?                         #
#                                                                             #
###############################################################################

### Verifica Frequencia de cada uma das classes ###
trainFrequency = table(trainSet$target)
trainFrequency
#0    1 
#6709 2495

# RESULTADO: não está balanceado, pois a frequência da classe "0" 
#            é cerca 2,7 vezes maior do que a da classe "1"

# Balanceamento pela ponderacao da função de erro

relative_frequency = trainFrequency/sum(trainFrequency)
relative_frequency

w_positive = 1 - relative_frequency[2]
w_negative = 1 - relative_frequency[1]

w_positive
w_negative

# Inicializando com zeros o vetor de pesos
weights <- rep(0.0, dim(trainSet)[1])

# Associando o peso dos positivos (w_positive) aos respectivos exemplos
weights[trainSet$target == 1] = w_positive 

# Associando o peso dos negatives (w_negative) aos respectivos exemplos
weights[trainSet$target == 0] = w_negative 

###############################################################################
# Questao 3                                                                   #
# ---------                                                                   #
#                                                                             #
# Apliquem alguma técnica de normalização de forma a deixa os dados           #
# mais bem preparados para o treinamento.                                     # 
#                                                                             #
###############################################################################

## Normalizacao Z-norma 
mean_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, mean)
mean_features

sd_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, sd)
sd_features

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, mean_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, sd_features, "/")
summary(trainSet)

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, mean_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, sd_features, "/")
summary(testSet)

sarsSet[,1:(ncol(sarsSet)-1)] <- sweep(sarsSet[,1:(ncol(sarsSet)-1)], 2, mean_features, "-")
sarsSet[,1:(ncol(sarsSet)-1)] <- sweep(sarsSet[,1:(ncol(sarsSet)-1)], 2, sd_features, "/")
summary(sarsSet)


###############################################################################
# Questao 4                                                                   #
# ---------                                                                   #
#                                                                             #
# Como baseline, treinem uma regressão logística com todas as features para   #
# predizer se haverá ou não a produção de anticorpos. Reportem a matriz       #
# de confusão relativa, o TPR, o TNR e a acurácia balanceada nas bases de     #
# treinamento, validação e teste (apenas arquivo proteins_teste_set.csv).     #
#                                                                             #
###############################################################################

############ Training Models ############
feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]
feature_names

hypothesis <- getHypothesis(feature_names, 1)
hypothesis

x_train <- model.matrix(hypothesis, trainSet)
x_train
y_train <- trainSet$target
y_train

model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE,
                weights = weights,
                maxit = 1e+05, alpha=0, lambda = 1e-2)

### Verificando os thetas aprendidos ###
model$beta
model$a0 # valor do theta0 (intercept)

trainPred <- predict(model, newx = x_train, type="response")
trainPred

length(trainPred)
summary(trainPred)

#converting to class
trainClassPred <- trainPred

#### THRESHOLD ####
# Threshold = 0.5 
trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0
trainClassPred

#### Balanced Loss
lossN = getLoss(trainSet$target[trainSet$target == 0], trainPred[trainSet$target == 0])
lossP = getLoss(trainSet$target[trainSet$target == 1], trainPred[trainSet$target == 1])

lossN
#[1] 0.9482049

lossP
#[1] 0.9500705

(lossN+lossP)/2
#[1] 0.9491377

cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainSet$target), 
                      positive='1')


cm$table # Devemos transpo-la para deixar os labels nas linhas 
         # e as predicoes nas colunas. Bem como devemos deixa-la
         # em valores relativos de forma que as linhas somem 100%
         # A funcao "calculaMatrizConfusaoRelativa" ja realiza
         # ambos os procedimentos. 

#               Reference
#Prediction     0       1
#       0       4010    900
#       1       2699    1595

# SEMPRE construam e reportem a matriz de confusao relativa!
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#               Reference
#Prediction     0       1
#       0       0.60    0.40
#       1       0.36    0.64

acc_bal_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_baseline
#[1] 0.62


#########################################
### Predicao no conjunto de validacao ###
#########################################
x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$target
valPred <- predict(model, newx = x_val, type="response")

#valPred

#converting to class
valClassPred <- valPred


#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0
#valClassPred

# threshold = 0.9
#valClassPred[valPred >= 0.9] <- 1
#valClassPred[valPred < 0.9] <- 0


##### Let's see how well we did
#Loss 
lossN = getLoss(valSet$target[valSet$target == 0], valPred[valSet$target == 0])
lossP = getLoss(valSet$target[valSet$target == 1], valPred[valSet$target == 1])
lossN
#[1] 0.9556375
lossP
#[1] 0.9537103
loss_baseline <- (lossN+lossP)/2
loss_baseline
#[1] 0.9546739

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$target), 
                      positive='1')


cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#               Reference
#Prediction     0       1
#         0     0.59    0.41
#         1     0.36    0.64

acc_bal_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_baseline
#[1] 0.615

# ROC Curve for baseline
ROC <- roc(valSet$target, valPred[,1], direction="<")
ROC
#rea under the curve: 0.6471

plot(ROC, col="blue", lwd=2, main="ROC")


#########################################
### Predicao no conjunto de teste ###
#########################################
x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

#testPred

#converting to class
testClassPred <- testPred


#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0
#testClassPred

# threshold = 0.9
#testClassPred[testPred >= 0.9] <- 1
#testClassPred[testPred < 0.9] <- 0


##### Let's see how well we did
#Loss 
lossN = getLoss(testSet$target[testSet$target == 0], testPred[testSet$target == 0])
lossP = getLoss(testSet$target[testSet$target == 1], testPred[testSet$target == 1])
lossN
#[1] 0.943964
lossP
#[1] 0.9424939
loss_baseline_test <- (lossN+lossP)/2
loss_baseline_test
#[1] 0.9432289

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')


cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#               Reference
#Prediction     0       1
#       0       0.60    0.40
#       1       0.36    0.64


acc_bal_baseline_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_baseline_test
#[1] 0.62



###############################################################################
# Questao 5                                                                   #
# ---------                                                                   #
#                                                                             #
# Implementem soluções alternativas baseadas em regressão logística através   #
# da combinação das features ou modelos polinomiais para melhorar o           #
# resultado do baseline. Comparem suas soluções reportando a matriz de        #
# confusão relativa e a acurácia balanceada no conjunto de validação. Tomem   #
# apenas a melhor solução, baseada na acurácia balanceada no conjunto de      #
# validação, e reportem a matriz de confusão relativa, TPR, TNR e             #
# acurácia balanceada no conjunto de teste (apenas arquivo                    #
# proteins_teste_set.csv).                                                    #
#                                                                             #
###############################################################################

############# Polynomial analysis ###########
loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]

## Polynomial Analysis
### Be careful! Higher polynomial degrees might not converge!
for(i in 1:12){  
    
    print(i)
    hypothesis <- getHypothesis(feature_names, i)
    
    # Applying hypothesis and training the model
    x_train <- model.matrix(hypothesis, trainSet)
    y_train <- trainSet$target
    model <- glmnet(x_train, y_train,  family="binomial", 
                    weights = weights,
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-2)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to class
    trainClassPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    #trainClassPred
    
    lossN = getLoss(trainSet$target[trainSet$target == 0], trainPred[trainSet$target == 0])
    lossP = getLoss(trainSet$target[trainSet$target == 1], trainPred[trainSet$target == 1])
    mean_loss_train <- (lossN+lossP)/2
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainSet$target), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
 
    # Validation
    x_val <- model.matrix(hypothesis, valSet)
    y_val <- valSet$target
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to class
    valClassPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valClassPred[valPred >= 0.5] <- 1
    valClassPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(valSet$target[valSet$target == 0], valPred[valSet$target == 0])
    lossP = getLoss(valSet$target[valSet$target == 1], valPred[valSet$target == 1])
    mean_loss_val <- (lossN+lossP)/2
    mean_loss_val
    
    cm <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$target), 
                          positive='1')
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    loss_train[i] <- mean_loss_train
    loss_val[i] <- mean_loss_val
    
    acc_train[i] <- acc_bal_train 
    acc_val[i] <- acc_bal_val
    
    print(cm_relative)
    print(acc_bal_val)
}

############# Plotting Loss ############
plot(loss_train, xlab="Complexity", ylab="Loss", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(loss_train, loss_val)),
            max(c(loss_train, loss_val))))


axis(1, at=1:12, labels=seq(from = 1, to = 12, by = 1), las=1)
points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(1.5, 0.9, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val)),
            max(c(acc_train, acc_val))))

axis(1, at=1:12, labels=seq(from = 1, to = 12, by = 1), las=1)
points(acc_val, pch="*", col="blue")
points(rep(acc_bal_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_bal_baseline, length(acc_val)), col="green", lty=2)
legend(8.0, 0.64, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

#### Testing #### 


# Melhor solução: 
#   Pela curva de vies e variancia, o ponto ótimo está entre 7, 8 ou 9
#   Pela curva da acuracia, está a partir de 8
#   Então, adotaremos 8 como o melhor modelo
i <- 8
i

acc_val[i]
#[1] 0.68


###### TODO: aplicar para o conjunto de teste

hypothesis <- getHypothesis(feature_names, i)

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                weights = weights,
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-2)

#trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0
testClassPred
table(testClassPred)

##### Let's see how good the model performs
#Loss 
lossN = getLoss(testSet$target[testSet$target == 0], testPred[testSet$target == 0])
lossP = getLoss(testSet$target[testSet$target == 1], testPred[testSet$target == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test 

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
#               Reference
#Prediction     0       1
#       0       0.65    0.35
#       1       0.30    0.70

acc_test_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_test_bal
#[1] 0.675


############ Combining Features ###########
cor(trainSet[1:(ncol(trainSet)-1)])

f01 <- formula(target ~ .)

f02 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini
                             +kolaskar_tongaonkar+parker+isoelectric_point
                             +aromaticity+hydrophobicity+stability)^2)

f03 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini
                             +kolaskar_tongaonkar+parker+isoelectric_point
                             +aromaticity+hydrophobicity+stability)^3)

f04 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini
                             +kolaskar_tongaonkar+parker+isoelectric_point
                             +aromaticity+hydrophobicity+stability)^4)

f05 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini
                             +kolaskar_tongaonkar+parker+isoelectric_point
                             +aromaticity+hydrophobicity+stability)^5)

f06 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini
                             +kolaskar_tongaonkar+parker+isoelectric_point
                             +aromaticity+hydrophobicity+stability)^6)

formulas <- c(f01, f02, f03, f04, f05, f06)

loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

i <- 1
for(f in formulas){  

    # Applying hypothesis and training the model
    x_train <- model.matrix(f, trainSet)
    y_train <- trainSet$target
    model <- glmnet(x_train, y_train,  family="binomial", 
                    weights = weights,
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-2)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to class
    trainClassPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    #trainClassPred
    
    lossN = getLoss(trainSet$target[trainSet$target == 0], trainPred[trainSet$target == 0])
    lossP = getLoss(trainSet$target[trainSet$target == 1], trainPred[trainSet$target == 1])
    mean_loss_train <- (lossN+lossP)/2
    print(mean_loss_train)
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainSet$target), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    
    # Validation
    x_val <- model.matrix(f, valSet)
    y_val <- valSet$target
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to class
    valClassPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valClassPred[valPred >= 0.5] <- 1
    valClassPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(valSet$target[valSet$target == 0], valPred[valSet$target == 0])
    lossP = getLoss(valSet$target[valSet$target == 1], valPred[valSet$target == 1])
    mean_loss_val <- (lossN+lossP)/2
    
    
    cm <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$target), 
                          positive='1')
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    loss_train[i] <- mean_loss_train
    loss_val[i] <- mean_loss_val
    
    acc_train[i] <- acc_bal_train 
    acc_val[i] <- acc_bal_val 
    
    print(cm_relative)
    print(acc_bal_val)
    
    i <- i + 1
}

############# Plotting Loss ############
plot(loss_train, xlab="Complexity", ylab="Loss", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(loss_train, loss_val)),
            max(c(loss_train, loss_val))))

axis(1, at=1:6, labels=seq(from = 1, to = 6, by = 1), las=1)
points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(1.0, 0.86, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val)),
            max(c(acc_train, acc_val))))

axis(1, at=1:6, labels=seq(from = 1, to = 6, by = 1), las=1)
points(acc_val, pch="*", col="blue")
points(rep(acc_bal_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_bal_baseline, length(acc_val)), col="green", lty=2)
legend(1.0, 0.69, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)


#### Testing #### 

# melhor modelo entre os de combinaçã de features
i <- 4

acc_val[i]
#[1] 0.66

f <- formulas[[i]]
x_train <- model.matrix(f, trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                weights = weights,
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-2)


x_test <- model.matrix(f, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(testSet$target[testSet$target == 0], testPred[testSet$target == 0])
lossP = getLoss(testSet$target[testSet$target == 1], testPred[testSet$target == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test
#[1] 0.67

###############################################################################
# Questao 6                                                                   #
# ---------                                                                   #
#                                                                             #
# Tomem um dos modelos do item anterior e varie o fator de                    #
# regularização [gama]. Criem a curva viés/variância colocando os diferentes  #
# valores de [gama] no eixo das das abscissas. Identifiquem as regiões de     #
# underfitting, ponto ótimo e overfitting, e então tomem o modelo com o       #
# melhor fator de regularização e reporte a matriz de confusão relativa,      #
# o TPR, o TNR e a acurácia balanceada no conjunto de teste (apenas arquivo   #
# proteins_teste_set.csv). Lembrem-se que a curva viés/variância é criada     #
# utilizando apenas os dados de treinamento e de validação!.                  #
#                                                                             #
###############################################################################

############ Regularization Analysis ############
loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

lambda_values <- c(1.0, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6)


# Nosso melhor modelo
hypothesis <- getHypothesis(feature_names, 8)

i <- 1
for(l in lambda_values){
    
    print(l)
    # Applying hypothesis and training the model
    x_train <- model.matrix(hypothesis, trainSet)
    y_train <- trainSet$target
    model <- glmnet(x_train, y_train,  family="binomial", 
                    weights = weights,
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = l)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to class
    trainClassPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    #trainClassPred
    
    lossN = getLoss(trainSet$target[trainSet$target == 0], trainPred[trainSet$target == 0])
    lossP = getLoss(trainSet$target[trainSet$target == 1], trainPred[trainSet$target == 1])
    mean_loss_train <- (lossN+lossP)/2
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainSet$target), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    
    # Validation
    x_val <- model.matrix(hypothesis, valSet)
    y_val <- valSet$target
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to class
    valClassPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valClassPred[valPred >= 0.5] <- 1
    valClassPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(valSet$target[valSet$target == 0], valPred[valSet$target == 0])
    lossP = getLoss(valSet$target[valSet$target == 1], valPred[valSet$target == 1])
    mean_loss_val <- (lossN+lossP)/2
    
    
    cm <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$target), 
                          positive='1')
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    loss_train[i] <- mean_loss_train
    loss_val[i] <- mean_loss_val
    
    acc_train[i] <- acc_bal_train 
    acc_val[i] <-acc_bal_val 
    i <- i + 1
    
}


############################################
# Pay attention on warnings! They might or #
# not prejudice your model performance!    #
############################################

############# Plotting Loss ############
plot(loss_train, xlab="Regularization factor (lambda)", ylab="Loss", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(loss_train, loss_val)),
            max(c(loss_train, loss_val))))

axis(1, at=1:length(lambda_values), labels=lambda_values, 
     cex.axis=0.5, las=2)

points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(1, 0.9, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Regularization factor (lambda)", ylab="Acc Balanced", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val)),
            max(c(acc_train, acc_val))))

axis(1, at=1:length(lambda_values), labels=lambda_values, 
     cex.axis=0.5, las=2)
points(acc_val, pch="*", col="blue")
points(rep(acc_bal_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_bal_baseline, length(acc_val)), col="green", lty=2)
legend(5, 0.64, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

#### Testing #### 

#### best lambda 

i <- 4

acc_val[i]
#[1] 0.685

best_lambda <- lambda_values[i]
best_lambda

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                weights = weights,
                alpha=0, maxit = 1e+05, trace.it=1, lambda = best_lambda)

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(testSet$target[testSet$target == 0], testPred[testSet$target == 0])
lossP = getLoss(testSet$target[testSet$target == 1], testPred[testSet$target == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test
#[1] 0.8750917


cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
#               Reference
#Prediction     0       1
#       0       0.66    0.34
#       1       0.31    0.69

acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test
#[1] 0.675


##### SARS

x_sars <- model.matrix(hypothesis, sarsSet)
y_sars <- sarsSet$target
sarsPred <- predict(model, newx = x_sars, type="response")

#converting to class
sarsClassPred <- sarsPred

#### THRESHOLD ####
# Threshold = 0.5 
sarsClassPred[sarsPred >= 0.5] <- 1
sarsClassPred[sarsPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(sarsSet$target[sarsSet$target == 0], sarsPred[sarsSet$target == 0])
lossP = getLoss(sarsSet$target[sarsSet$target == 1], sarsPred[sarsSet$target == 1])
mean_loss_sars <- (lossN+lossP)/2
mean_loss_sars


cm <- confusionMatrix(data = as.factor(sarsClassPred), 
                      reference = as.factor(sarsSet$target), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
#               Reference
#Prediction     0       1
#       0       0.25    0.75
#       1       0.18    0.82

acc_bal_sars <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_sars
#[1] 0.535




###############################################################################
# Questao 7                                                                   #
# -----------                                                                 #
#                                                                             #
# Escrevam um relatório de no máximo 5 páginas:                               #
#                                                                             #
###############################################################################


###############################################################################
# Questao 7.a                                                                 #
# -----------                                                                 #
#                                                                             #
# Descrevam o que foi feito, bem como as diferenças entre o seu melhor        #
# modelo e o seu baseline;                                                    #
#                                                                             #
###############################################################################



###############################################################################
# Questao 7.b                                                                 #
# -----------                                                                 #
#                                                                             #
# Após desenvolverem todos os modelos, tomem o melhor modelo de todos         #
# (melhor performance no conjunto de validação). Reportem a matriz de         #
# confusão relativa e acurácia balanceada nos conjuntos de teste              #
# proteins_teste_set.csv e SARS_test_csv. Há uma diferença significativa      #
# entre eles ? Se sim, qual explicação você daria para essa diferença ?       #
#                                                                             #
###############################################################################


###############################################################################
# Questao 7.c                                                                 #
# -----------                                                                 #
#                                                                             #
# Uma Seção de conclusão do relatório explicando a diferença entre os         #
# modelos e o porquê que estas diferenças levaram a resultados piores ou      #
# melhores.                                                                   #
#                                                                             #
###############################################################################



