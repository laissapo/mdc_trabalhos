#----------------------------------------------------------------#
# INF-0615 Aprendizado Supervisionado I                          #
#                                                                #
# Trabalho 01                                                    #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
#                                                                #
# - Evandro Santos Rocha                                         #
# - Laíssa Pacheco de Oliveira                                   #
# - Rafael Dantas de Moura                                       #
#                                                                #
#----------------------------------------------------------------#

# Funcao de Apoio ao Trabalho 01 de Aprendizado Supervisionado I. 
# Esta fun??o escreve a formula dos modelos polinomiais. 
# Parametros:

# real_feature_names: Um vetor com os nomes dos atributos continuos que voce
#                     quer que seja elevado ao grau desejado.
#  
# categorical_feature_names: Um vetor com os nomes dos atributos categoricos
#                            que voce quer que seja adicionado a hipotese. 
#                            Eles n?o s?o elevados ao grau especificado ja que
#                            sao valores binarios (0 ou 1). Se voce quer uma
#                            hipotese que nao tenha nenhum valor categorico, mas
#                            apenas os reais, basta nao passar nenhum valor 
#                            para este parametro quando chamar a funcao.
#
#
# degree: Grau que voc? deseja que os atributos reais em "real_feature_names"
#         sejam elevados. Ao chamar a funcao, escreva explicitamente
#         o grau desejado. Por exemplo, para grau igual 2, escreva degree=2

# Vejam os exerc?cios 02 e 03 para ver o funcionamento 
# de uma funcao similar a essa.


getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    
    cat("hypothesis_string: ", hypothesis_string, "\n")
    
    return(hypothesis)
}

# Comandos que leem os conjuntos de treino e de validacao
train_set <- read.csv("training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set   <- read.csv("validation_set_air_quality.csv", stringsAsFactors=TRUE)

#train_set <- read.csv("training_set_air_quality-reduzido.csv", stringsAsFactors=TRUE)
#val_set <- read.csv("validation_set_air_quality-reduzido.csv", stringsAsFactors=TRUE)


# Desenvolvam o trabalho a partir daqui, apos executarem os comandos acima


#######################################
# DÚVIDAS / PENDÊNCIAS                #
#######################################
# #1. (X) Saber se as features de year, month, day e hour são consideradas discretas
# #2. (X) Na questao 03, Ao executar o summary, o coeficiente da última feature (wd_wsw) veio com valores iguais a NA. (ao menos interpretamos assim)
# #3. (X) Conjunto de teste
# #4. (X) Na questao 04, podemos fazer outras combinacoes, como, por exemplo, f04 com combinacao de 5?
# #5. (X) Dúvida: os valores da questão 04 ficaram melhores que da questão 05 (achei que deveria ser ao contrário)
# #6. ( ) 




###############################################################################
# Questao 1                                                                   #
# ---------                                                                   #
#                                                                             #
# Inspecionem os dados. Quantos exemplos vocês tem?                           #
# Como vocês irão lidar com as features (atributos) discretas, se houverem?   #
# Há exemplos com features sem anotações? Como vocês lidariam com isso?       #
#                                                                             #
###############################################################################

set.seed(40)

#------------------------------------------------#
# Tamanhos dos conjuntos de dados                #
#------------------------------------------------#

# dimensoes do dataset
dim(train_set)
#[1] 244582     17

# sumário do dataset
summary(train_set)

# primeiros exemplos do dataset
head(train_set)
#     No year month day hour PM2.5 PM10 SO2 NO2 O3 TEMP   PRES  DEWP RAIN  wd WSPM target
#1  5825 2013    10  29   16     3    6   9  18 63 13.9 1020.6  -2.5    0  NE  1.3    300
#2 17231 2015     2  16   22    20   66   9  31 47  6.0 1010.0 -12.1    0  SE  1.1    600
#3 20520 2015     7   3   23    33   70   4  37 79 23.7 1006.4  11.1    0 SSE  2.3    700
#4 22110 2015     9   8    5    43   65   2  62  2 17.6 1009.0  16.6    0  NE  0.7    800
#5 15041 2014    11  17   16    19   53   7  32 64 11.8 1022.0 -10.5    0 SSW  2.3    500
#6 26446 2016     3   6   21   116  282  38  87 14  6.4 1008.3  -8.7    0 NNE  0.9   1900

# dimensoes do dataset
dim(val_set)
#[1] 61147    17

# sumário do dataset
summary(val_set)

# primeiros exemplos do dataset
head(val_set)
#     No year month day hour PM2.5 PM10 SO2 NO2 O3 TEMP   PRES DEWP RAIN  wd WSPM target
#1  3338 2013     7  18    1    80   80  14  32 98 25.9  999.1 22.2  0.0  SE  1.9   1700
#2 20811 2015     7  16    2    44   44   2   2 88 21.7  999.0 18.5  0.1 NNE  2.0    400
#3  1347 2013     4  26    2    14   12   3   9 58  9.8 1004.1 -7.4  0.0 NNE  1.2    200
#4 33068 2016    12   7   19   186  214  22 134  2  6.0 1018.3 -5.1  0.0  SW  2.0   2900
#5 31726 2016    10  12   21   105  141   4 100 20 16.1 1017.6 10.8  0.0   S  1.0   1200
#6 23214 2015    10  24    5    40   62   3  31  2  7.6 1011.4  6.8  0.0  SE  0.5   1100

#------------------------------------------------#
# Dados de testes                                #
#------------------------------------------------#

# Descomente a linha abaixo apenas quando o conjunto de teste esiver disponivel
test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)

# dimensoes do dataset
dim(test_set)
#[1] 76434    17

# sumário do dataset
summary(test_set)

# primeiros exemplos do dataset
head(test_set)
#  No year month day hour PM2.5 PM10 SO2 NO2 O3 TEMP PRES  DEWP RAIN  wd WSPM target
#1  4 2013     3   1    3     6    6  11  11 72 -1.4 1024 -19.4    0  NW  3.1    300
#2  9 2013     3   1    8     3    6  16  43 45  0.1 1028 -19.2    0 NNW  4.1    500
#3 10 2013     3   1    9     3    8  12  28 59  1.2 1028 -19.3    0   N  2.6    400
#4 11 2013     3   1   10     3    6   9  12 72  1.9 1028 -19.4    0 NNW  3.6    400
#5 20 2013     3   1   19     8   14  12  30 60  2.3 1028 -18.4    0   N  2.8    500
#6 26 2013     3   2    1    14   17  21  36 50 -1.0 1031 -17.3    0 NNE  1.1    400


#------------------------------------------------#
# Tratamento de erros: NAs                       #
#------------------------------------------------#

#Há exemplos com features sem anotações?
any(is.na(train_set))
any(is.na(val_set  ))
any(is.na(test_set ))

#-----------------------------------------------------------#
# Verificação se há registros repetidos entre os conjuntos  #
#-----------------------------------------------------------#

merge(train_set, val_set )
merge(train_set, test_set)
merge(val_set  , test_set)

#------------------------------------------------#
# Tratamento de features discretas               #
#------------------------------------------------#

summary(train_set$wd)
#E   ENE   ESE     N    NE   NNE   NNW    NW     S    SE   SSE   SSW    SW     W   WNW   WSW 
#17371 20029 14162 17987 25096 16350 14920 19211 11274 11708 10321 13201 16929 10110 14193 11720 

summary(val_set$wd)

#E  ENE  ESE    N   NE  NNE  NNW   NW    S   SE  SSE  SSW   SW    W  WNW  WSW 
#4439 4895 3551 4405 6336 4068 3667 4810 2794 3003 2586 3305 4195 2591 3528 2974 

# Transforming to One-Hot-Encoding
train_set$wd_e   <- as.numeric(train_set$wd == "E"  )
train_set$wd_ene <- as.numeric(train_set$wd == "ENE")
train_set$wd_ese <- as.numeric(train_set$wd == "ESE")
train_set$wd_n   <- as.numeric(train_set$wd == "N"  )
train_set$wd_ne  <- as.numeric(train_set$wd == "NE" )
train_set$wd_nne <- as.numeric(train_set$wd == "NNE")
train_set$wd_nnw <- as.numeric(train_set$wd == "NNW")
train_set$wd_nw  <- as.numeric(train_set$wd == "NW" )
train_set$wd_s   <- as.numeric(train_set$wd == "S"  )
train_set$wd_se  <- as.numeric(train_set$wd == "SE" )
train_set$wd_sse <- as.numeric(train_set$wd == "SSE")
train_set$wd_ssw <- as.numeric(train_set$wd == "SSW")
train_set$wd_sw  <- as.numeric(train_set$wd == "SW" )
train_set$wd_w   <- as.numeric(train_set$wd == "W"  )
train_set$wd_wnw <- as.numeric(train_set$wd == "WNW")
train_set$wd_wsw <- as.numeric(train_set$wd == "WSW")
train_set$wd <- NULL

val_set$wd_e   <- as.numeric(val_set$wd == "E"  )
val_set$wd_ene <- as.numeric(val_set$wd == "ENE")
val_set$wd_ese <- as.numeric(val_set$wd == "ESE")
val_set$wd_n   <- as.numeric(val_set$wd == "N"  )
val_set$wd_ne  <- as.numeric(val_set$wd == "NE" )
val_set$wd_nne <- as.numeric(val_set$wd == "NNE")
val_set$wd_nnw <- as.numeric(val_set$wd == "NNW")
val_set$wd_nw  <- as.numeric(val_set$wd == "NW" )
val_set$wd_s   <- as.numeric(val_set$wd == "S"  )
val_set$wd_se  <- as.numeric(val_set$wd == "SE" )
val_set$wd_sse <- as.numeric(val_set$wd == "SSE")
val_set$wd_ssw <- as.numeric(val_set$wd == "SSW")
val_set$wd_sw  <- as.numeric(val_set$wd == "SW" )
val_set$wd_w   <- as.numeric(val_set$wd == "W"  )
val_set$wd_wnw <- as.numeric(val_set$wd == "WNW")
val_set$wd_wsw <- as.numeric(val_set$wd == "WSW")
val_set$wd <- NULL

test_set$wd_e   <- as.numeric(test_set$wd == "E"  )
test_set$wd_ene <- as.numeric(test_set$wd == "ENE")
test_set$wd_ese <- as.numeric(test_set$wd == "ESE")
test_set$wd_n   <- as.numeric(test_set$wd == "N"  )
test_set$wd_ne  <- as.numeric(test_set$wd == "NE" )
test_set$wd_nne <- as.numeric(test_set$wd == "NNE")
test_set$wd_nnw <- as.numeric(test_set$wd == "NNW")
test_set$wd_nw  <- as.numeric(test_set$wd == "NW" )
test_set$wd_s   <- as.numeric(test_set$wd == "S"  )
test_set$wd_se  <- as.numeric(test_set$wd == "SE" )
test_set$wd_sse <- as.numeric(test_set$wd == "SSE")
test_set$wd_ssw <- as.numeric(test_set$wd == "SSW")
test_set$wd_sw  <- as.numeric(test_set$wd == "SW" )
test_set$wd_w   <- as.numeric(test_set$wd == "W"  )
test_set$wd_wnw <- as.numeric(test_set$wd == "WNW")
test_set$wd_wsw <- as.numeric(test_set$wd == "WSW")
test_set$wd <- NULL


summary(train_set)
cor(train_set[,1:8])

summary(val_set)

summary(test_set)


###############################################################################
# Questao 2                                                                   #
# ---------                                                                   #
#                                                                             #
# Apliquem alguma técnica de normalização de forma a deixa os dados           #
# mais bem preparados para o treinamento (Min-Max, Z-Norma, etc).             #
#                                                                             #
###############################################################################

# colunas existentes
head(train_set)

# colunas que serão normalizadas
head(train_set[,2:15]) # de 'year' até 'WSPM'

#  year month day hour PM2.5 PM10 SO2 NO2 O3 TEMP   PRES  DEWP RAIN WSPM
#1 2013    10  29   16     3    6   9  18 63 13.9 1020.6  -2.5    0  1.3
#2 2015     2  16   22    20   66   9  31 47  6.0 1010.0 -12.1    0  1.1
#3 2015     7   3   23    33   70   4  37 79 23.7 1006.4  11.1    0  2.3
#4 2015     9   8    5    43   65   2  62  2 17.6 1009.0  16.6    0  0.7
#5 2014    11  17   16    19   53   7  32 64 11.8 1022.0 -10.5    0  2.3
#6 2016     3   6   21   116  282  38  87 14  6.4 1008.3  -8.7    0  0.9


#------------------------------------------------#
# Aplicando Min-Max                              #
#------------------------------------------------#

min_features <- apply(train_set[,2:15], 2, min)
min_features

max_features <- apply(train_set[,2:15], 2, max)
max_features

diff <- max_features - min_features
diff

train_set[,2:15] <- sweep(train_set[,2:15], 2, min_features, "-")
train_set[,2:15] <- sweep(train_set[,2:15], 2, diff, "/")
summary(train_set)

val_set[,2:15] <- sweep(val_set[,2:15], 2, min_features, "-")
val_set[,2:15] <- sweep(val_set[,2:15], 2, diff, "/")
summary(val_set)

test_set[,2:15] <- sweep(test_set[,2:15], 2, min_features, "-")
test_set[,2:15] <- sweep(test_set[,2:15], 2, diff, "/")
summary(test_set)

# colunas que foram normalizadas
head(train_set[,2:15]) # de 'year' até 'WSPM'
head(val_set[,2:15]) # de 'year' até 'WSPM'
head(test_set[,2:15]) # de 'year' até 'WSPM'


###############################################################################
# Questao 3                                                                   #
# ---------                                                                   #
#                                                                             #
# Como baseline, treinem uma regressão linear utilizando todas as features    #
# para predizer a concentração de Monóxido de Carbono no ar.                  #
# Reportem o erro nos conjuntos de treinamento, validação e teste.            #
#                                                                             #
###############################################################################

## Baseline ##
## O comando abaixo encontrar? os melhores valores de theta 
## na seguinte express?o: 
## meadian_house_value = theta0 + theta1*longitude + theta2*latitude + 
##                              theta3*house_median_age + theta4*total_rooms + 
##                                theta5*total_bedrooms + theta6*population + 
##                            theta7*households + theta8*median_income
##
## O modelo, por padr?o, utiliza equa??es normais. Em modelos mais complexos e 
## com bases de dados muito grandes, ele automaticamente utiliza descida do gradiente.
baseline <- lm(formula=target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 + O3 
               + TEMP + PRES + DEWP + RAIN + WSPM + wd_e + wd_ene 
               + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw 
               + wd_s + wd_se + wd_sse + wd_ssw +  wd_sw + wd_w + wd_wnw + wd_wsw, data=train_set)

summary(baseline)

valPred <- predict(baseline, val_set)
trainPred <- predict(baseline, train_set)
testPred <- predict(baseline, test_set)

head(valPred)
length(valPred)
dim(val_set)

head(trainPred)
length(trainPred)
dim(train_set)

###################################
####   Define MAE function     ####
MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

####################################
####   Define MSE function     ####
MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

###################################
#### Define R-squared function ####
R2 <- function(pred, true){
    rss <- sum((pred - true) ^ 2)
    tss <- sum((true - mean(true)) ^ 2)
    r2 <- 1 - rss/tss
    return(r2)
}



mae_train_baseline <- MAE(trainPred, train_set$target)
mae_train_baseline
#[1] 370.0937

mae_val_baseline <- MAE(valPred, val_set$target)
mae_val_baseline
#[1] 371.0654

mae_test_baseline <- MAE(testPred, test_set$target)
mae_test_baseline
#[1] 372



mse_train_baseline <- MSE(trainPred, train_set$target)
mse_train_baseline
#[1] 354861.5

mse_val_baseline <- MSE(valPred, val_set$target)
mse_val_baseline
#[1] 366334.6

mse_test_baseline <- MSE(testPred, test_set$target)
mse_test_baseline
#[1] 362478



r2_train_baseline <- R2(trainPred, train_set$target)
r2_train_baseline
#[1] 0.7338512

r2_val_baseline <- R2(valPred, val_set$target)
r2_val_baseline
#[1] 0.7262772

r2_test_baseline <- R2(testPred, test_set$target)
r2_test_baseline
#[1] 0.7330248


###############################################################################
# Questao 4                                                                   #
# ---------                                                                   #
#                                                                             #
# Implementem soluções alternativas baseadas em regressão linear através da   #
# combinação das features existentes para melhorar o resultado do baseline.   #
# Comparem suas soluções reportando os erros no conjunto validação.           #
# Tomem apenas a melhor solução baseada no conjunto de validação e reportem   #
# o erro no conjunto de teste.                                                #
#                                                                             #
###############################################################################

## Combining features ### 
f01 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 + O3 
               + TEMP + PRES + DEWP + RAIN + WSPM + wd_e + wd_ene 
               + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw 
               + wd_s + wd_se + wd_sse + wd_ssw +  wd_sw + wd_w + wd_wnw + wd_wsw
               + (PM2.5 + PM10 + SO2 + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM)^2)

f02 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 + O3 
               + TEMP + PRES + DEWP + RAIN + WSPM + wd_e + wd_ene 
               + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw 
               + wd_s + wd_se + wd_sse + wd_ssw +  wd_sw + wd_w + wd_wnw + wd_wsw
               + (PM2.5 + PM10 + SO2 + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM)^3)

f03 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 + O3 
               + TEMP + PRES + DEWP + RAIN + WSPM + wd_e + wd_ene 
               + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw 
               + wd_s + wd_se + wd_sse + wd_ssw +  wd_sw + wd_w + wd_wnw + wd_wsw
               + (PM2.5 + PM10 + SO2 + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM)^4)

models <- c(f01, f02, f03)
total_mae_train <- c(0,0,0)
total_mae_val <- c(0,0,0)

i <- 1
for(f in models){
    
    cat("i: ", i, "\n")
    
    model <- lm(formula=f, data=train_set)
    
    valPred <- predict(model, val_set)
    trainPred <- predict(model, train_set)
    
    mae_train <- MAE(trainPred, train_set$target)
    total_mae_train[i] <- mae_train

    cat("mae_train: ", mae_train, "\n")
    
    mae_val <- MAE(valPred, val_set$target)
    total_mae_val[i] <- mae_val

    cat("mae_val: ", mae_val, "\n")
    
    rm(model)
    
    i <- i + 1
    
}


# RESULTADO:
#
#i:  1 
#mae_train:  305.6576 
#mae_val:  306.1869 
#i:  2 
#mae_train:  290.0147 
#mae_val:  290.4062 
#i:  3 
#mae_train:  287.071 
#mae_val:  287.8662


plot(total_mae_val, xlab="Complexity", ylab="Error", 
     ylim=c(min(c(total_mae_train, total_mae_val, mae_val_baseline)), 
            max(c(total_mae_train, total_mae_val, mae_val_baseline))), 
     pch="+", col="blue",  xaxt="n")
axis(1, at=1:length(models), labels=seq(from = 1, to = 3, by = 1), las=1)
points(total_mae_train, pch="*", col="red")
points(rep(mae_val_baseline, length(total_mae_val)), pch="o", col="green")

lines(total_mae_train, col="red", lty=2)
lines(total_mae_val, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val)), col="green", lty=2)
legend(1, 340, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.8)


#
# RESULTADO: Melhor modelo foi o f03
#
# Reportando o erro com relacao à base de teste

model <- lm(formula=f03, data=train_set)

testPred <- predict(model, test_set)
mae_test_comb <- MAE(testPred, test_set$target)

cat("mae_test_comb: ", mae_test_comb, "\n")
#mae_test_comb:  289.1319


###############################################################################
# Questao 5                                                                   #
# ---------                                                                   #
#                                                                             #
# Implementem soluções alternativas baseadas em regressão linear aumentando   #
# os graus das features (regressão com polinômios) para melhorar o resultado  #
# obtido no baseline. Plotem o erro no conjunto de treinamento e validação    #
# pelo grau do polinômio. Identifiquem as regiões de underfitting, ponto      #
# ótimo e overfitting. Tomem apenas o melhor modelo polinomial baseado no     #
# conjunto de validação e reportem seu erro no conjunto de teste.             #
#                                                                             #
###############################################################################

## Polynomial Analysis
total_mae_train_poly <- c()
total_mae_val_poly <- c()

head(train_set)

feature_names <- colnames(train_set)[2:15]
categorical_feature_names <- colnames(train_set)[17:ncol(train_set)]

for(i in 1:8){
    
    cat("i: ", i, "\n")

    hypothesis <- getHypothesis(feature_names, categorical_feature_names, i)
    
    ## Baseline ##
    model_poly <- lm(formula=hypothesis, data=train_set)
    
    valPred <- predict(model_poly, val_set)
    trainPred <- predict(model_poly, train_set)
    
    mae_train <- MAE(trainPred, train_set$target)
    total_mae_train_poly[i] <- mae_train
    
    cat("mae_train: ", mae_train, "\n")
    
    
    mae_val <- MAE(valPred, val_set$target)
    total_mae_val_poly[i] <- mae_val
    
    cat("mae_val: ", mae_val, "\n")
    
}


# Resultado
#i:  1 
#hypothesis_string:  hypothesis <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(RAIN^1) + I(WSPM^1) + wd_e + wd_ene + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw + wd_s + wd_se + wd_sse + wd_ssw + wd_sw + wd_w + wd_wnw + wd_wsw ) 
#mae_train:  370.0937 
#mae_val:  371.0654 
#i:  2 
#hypothesis_string:  hypothesis <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(RAIN^1) + I(WSPM^1) + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2) + I(TEMP^2) + I(PRES^2) + I(DEWP^2) + I(RAIN^2) + I(WSPM^2) + wd_e + wd_ene + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw + wd_s + wd_se + wd_sse + wd_ssw + wd_sw + wd_w + wd_wnw + wd_wsw ) 
#mae_train:  352.1933 
#mae_val:  352.9237 
#i:  3 
#hypothesis_string:  hypothesis <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(RAIN^1) + I(WSPM^1) + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2) + I(TEMP^2) + I(PRES^2) + I(DEWP^2) + I(RAIN^2) + I(WSPM^2) + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3) + I(TEMP^3) + I(PRES^3) + I(DEWP^3) + I(RAIN^3) + I(WSPM^3) + wd_e + wd_ene + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw + wd_s + wd_se + wd_sse + wd_ssw + wd_sw + wd_w + wd_wnw + wd_wsw ) 
#mae_train:  349.7766 
#mae_val:  351.0492 
#i:  4 
#hypothesis_string:  hypothesis <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(RAIN^1) + I(WSPM^1) + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2) + I(TEMP^2) + I(PRES^2) + I(DEWP^2) + I(RAIN^2) + I(WSPM^2) + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3) + I(TEMP^3) + I(PRES^3) + I(DEWP^3) + I(RAIN^3) + I(WSPM^3) + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4) + I(TEMP^4) + I(PRES^4) + I(DEWP^4) + I(RAIN^4) + I(WSPM^4) + wd_e + wd_ene + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw + wd_s + wd_se + wd_sse + wd_ssw + wd_sw + wd_w + wd_wnw + wd_wsw ) 
#mae_train:  345.2659 
#mae_val:  346.4808 
#i:  5 
#hypothesis_string:  hypothesis <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(RAIN^1) + I(WSPM^1) + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2) + I(TEMP^2) + I(PRES^2) + I(DEWP^2) + I(RAIN^2) + I(WSPM^2) + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3) + I(TEMP^3) + I(PRES^3) + I(DEWP^3) + I(RAIN^3) + I(WSPM^3) + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4) + I(TEMP^4) + I(PRES^4) + I(DEWP^4) + I(RAIN^4) + I(WSPM^4) + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5) + I(TEMP^5) + I(PRES^5) + I(DEWP^5) + I(RAIN^5) + I(WSPM^5) + wd_e + wd_ene + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw + wd_s + wd_se + wd_sse + wd_ssw + wd_sw + wd_w + wd_wnw + wd_wsw ) 
#mae_train:  341.0588 
#mae_val:  343.0935 
#i:  6 
#hypothesis_string:  hypothesis <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(RAIN^1) + I(WSPM^1) + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2) + I(TEMP^2) + I(PRES^2) + I(DEWP^2) + I(RAIN^2) + I(WSPM^2) + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3) + I(TEMP^3) + I(PRES^3) + I(DEWP^3) + I(RAIN^3) + I(WSPM^3) + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4) + I(TEMP^4) + I(PRES^4) + I(DEWP^4) + I(RAIN^4) + I(WSPM^4) + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5) + I(TEMP^5) + I(PRES^5) + I(DEWP^5) + I(RAIN^5) + I(WSPM^5) + I(year^6) + I(month^6) + I(day^6) + I(hour^6) + I(PM2.5^6) + I(PM10^6) + I(SO2^6) + I(NO2^6) + I(O3^6) + I(TEMP^6) + I(PRES^6) + I(DEWP^6) + I(RAIN^6) + I(WSPM^6) + wd_e + wd_ene + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw + wd_s + wd_se + wd_sse + wd_ssw + wd_sw + wd_w + wd_wnw + wd_wsw ) 
#mae_train:  339.8772 
#mae_val:  343.4974 
#i:  7 
#hypothesis_string:  hypothesis <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(RAIN^1) + I(WSPM^1) + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2) + I(TEMP^2) + I(PRES^2) + I(DEWP^2) + I(RAIN^2) + I(WSPM^2) + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3) + I(TEMP^3) + I(PRES^3) + I(DEWP^3) + I(RAIN^3) + I(WSPM^3) + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4) + I(TEMP^4) + I(PRES^4) + I(DEWP^4) + I(RAIN^4) + I(WSPM^4) + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5) + I(TEMP^5) + I(PRES^5) + I(DEWP^5) + I(RAIN^5) + I(WSPM^5) + I(year^6) + I(month^6) + I(day^6) + I(hour^6) + I(PM2.5^6) + I(PM10^6) + I(SO2^6) + I(NO2^6) + I(O3^6) + I(TEMP^6) + I(PRES^6) + I(DEWP^6) + I(RAIN^6) + I(WSPM^6) + I(year^7) + I(month^7) + I(day^7) + I(hour^7) + I(PM2.5^7) + I(PM10^7) + I(SO2^7) + I(NO2^7) + I(O3^7) + I(TEMP^7) + I(PRES^7) + I(DEWP^7) + I(RAIN^7) + I(WSPM^7) + wd_e + wd_ene + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw + wd_s + wd_se + wd_sse + wd_ssw + wd_sw + wd_w + wd_wnw + wd_wsw ) 
#mae_train:  338.0388 
#mae_val:  344.2929 
#i:  8 
#hypothesis_string:  hypothesis <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(RAIN^1) + I(WSPM^1) + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2) + I(TEMP^2) + I(PRES^2) + I(DEWP^2) + I(RAIN^2) + I(WSPM^2) + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3) + I(TEMP^3) + I(PRES^3) + I(DEWP^3) + I(RAIN^3) + I(WSPM^3) + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4) + I(TEMP^4) + I(PRES^4) + I(DEWP^4) + I(RAIN^4) + I(WSPM^4) + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5) + I(TEMP^5) + I(PRES^5) + I(DEWP^5) + I(RAIN^5) + I(WSPM^5) + I(year^6) + I(month^6) + I(day^6) + I(hour^6) + I(PM2.5^6) + I(PM10^6) + I(SO2^6) + I(NO2^6) + I(O3^6) + I(TEMP^6) + I(PRES^6) + I(DEWP^6) + I(RAIN^6) + I(WSPM^6) + I(year^7) + I(month^7) + I(day^7) + I(hour^7) + I(PM2.5^7) + I(PM10^7) + I(SO2^7) + I(NO2^7) + I(O3^7) + I(TEMP^7) + I(PRES^7) + I(DEWP^7) + I(RAIN^7) + I(WSPM^7) + I(year^8) + I(month^8) + I(day^8) + I(hour^8) + I(PM2.5^8) + I(PM10^8) + I(SO2^8) + I(NO2^8) + I(O3^8) + I(TEMP^8) + I(PRES^8) + I(DEWP^8) + I(RAIN^8) + I(WSPM^8) + wd_e + wd_ene + wd_ese + wd_n + wd_ne + wd_nne + wd_nnw + wd_nw + wd_s + wd_se + wd_sse + wd_ssw + wd_sw + wd_w + wd_wnw + wd_wsw ) 
#mae_train:  337.2245 
#mae_val:  351.757


plot(total_mae_train_poly, xlab="Complexity", ylab="Error", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(total_mae_train_poly, total_mae_val_poly, mae_val_baseline)),
            max(c(total_mae_train_poly, total_mae_val_poly, mae_val_baseline))))


axis(1, at=1:8, labels=seq(from = 1, to = 8, by = 1), las=1)
points(total_mae_val_poly, pch="*", col="blue")
points(rep(mae_val_baseline, length(total_mae_val_poly)), pch="o", col="green")

lines(total_mae_train_poly, col="red", lty=2)
lines(total_mae_val_poly, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val_poly)), col="green", lty=2)
total_mae_val_poly

legend(1, 350, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.7)


#
# RESULTADO: Melhor modelo foi o de polinomio com i=5
#
# Reportando o erro com relacao à base de teste

hypothesis <- getHypothesis(feature_names, categorical_feature_names, 5)

## Baseline ##
model_poly <- lm(formula=hypothesis, data=train_set)

testPred <- predict(model_poly, test_set)

mae_test_poly <- MAE(testPred, test_set$target)

cat("mae_test_poly: ", mae_test_poly, "\n")
#mae_test_poly:  344.157 



###############################################################################
# Questao 6.a                                                                 #
# -----------                                                                 #
#                                                                             #
# Relatório: Descrevam o que foi feito, bem como as diferenças entre o        #
# seu melhor modelo e o seu baseline;                                         #
#                                                                             #
###############################################################################

#
# Relatório
#
# https://docs.google.com/document/d/1-yz08vxNK8qsyKcFGcgCs37mzGtK_86YWxRa8eAbGHM/edit
#


###############################################################################
# Questao 6.b                                                                 #
# -----------                                                                 #
#                                                                             #
# Relatório: Reportem o erro do melhor modelo de todos no conjunto de teste.  #
# Lembrem-se que o melhor modelo de todos deve ser escolhido baseado          #
# no erro no conjunto de validação.                                           #
#                                                                             #
###############################################################################

## Getting min value on valiation set

# menor erro no conjunto de validação nas regressões por combinação
min(total_mae_val)
# [1] 287.8662

# menor erro no conjunto de validação nas regressões polinomiais
min(total_mae_val_poly)
# [1] 343.0935

# erros nas regressões por combinação
total_mae_val
#[1] 306.1869 290.4062 287.8662

# erro no conjunto de teste da melhor regressão, por combinação 4 a 4
mae_test_comb
#[1] 289.1319

# RESULTADO:
# Menor valor no 3a regressao por combinação, de 4 em 4 features

###############################################################################
# Questao 6.c                                                                 #
# -----------                                                                 #
#                                                                             #
# Relatório: Uma Seção de conclusão do relatório explicando a diferença entre #
# os modelos e o porquê que estas diferenças levaram a resultados             #
# piores ou melhores.                                                         #
#                                                                             #
###############################################################################

