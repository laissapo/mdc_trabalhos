# Funcao de Apoio ao Trabalho 01 de Aprendizado Supervisionado I. 
# Esta função escreve a formula dos modelos polinomiais. 
# Parametros:

# real_feature_names: Um vetor com os nomes dos atributos continuos que voce
#                     quer que seja elevado ao grau desejado.
#  
# categorical_feature_names: Um vetor com os nomes dos atributos categoricos
#                            que voce quer que seja adicionado a hipotese. 
#                            Eles não são elevados ao grau especificado ja que
#                            sao valores binarios (0 ou 1). Se voce quer uma
#                            hipotese que nao tenha nenhum valor categorico, mas
#                            apenas os reais, basta nao passar nenhum valor 
#                            para este parametro quando chamar a funcao.
#
#
# degree: Grau que você deseja que os atributos reais em "real_feature_names"
#         sejam elevados. Ao chamar a funcao, escreva explicitamente
#         o grau desejado. Por exemplo, para grau igual 2, escreva degree=2

# Vejam os exercícios 02 e 03 para ver o funcionamento 
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
    return(hypothesis)
}

# Comandos que leem os conjuntos de treino e de validacao
train_set <- read.csv("training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set <- read.csv("validation_set_air_quality.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima




# Descomente a linha abaixo apenas quando o conjunto de teste esiver disponível
#test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)






