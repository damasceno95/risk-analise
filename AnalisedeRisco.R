
setwd(choose.dir())


# COLETANDO OS DADOS

datacred <- read.csv(file = 'credit_dataset.csv',sep = ',',header = T)

# VISUALIZANDO OS DADOS

View(datacred)
summary(datacred)
str(datacred)

# VALORES N/A

any(is.na(datacred))

# FUNÇÃO PARA TRANSFORMAÇÃO DOS DADOS

var2fac <- function(x, variaveis){
  for (i in variaveis){
    x[[i]] <- as.factor(x[[i]])
  }
  return(x)
}

varnorm <- function(y, datanum){
  for (i in datanum){
    y[[i]] <- scale(y[[i]])
  }
  return(y)
}

# VARIÁVEIS CATEGÓRICAS

catvar <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
            'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
            'marital.status', 'guarantor', 'residence.duration', 'current.assets',
            'other.credits', 'apartment.type', 'bank.credits', 'occupation',
            'dependents', 'telephone', 'foreign.worker')

datacred <- var2fac(datacred, catvar)
str(datacred)

# VARIÁVEIS NUMÉRICAS

numvar <- c('credit.duration.months', 'credit.amount', 'age')

datacred <- varnorm(datacred, numvar)

str(datacred)

# VERIFICANDO VARIÁVEIS

table(datacred$credit.rating) # DESPROPORCIONAL PARA O MODELO

library(randomForest)
library(caret)

impVar <- randomForest(credit.rating ~ ., data = datacred, importance = TRUE )
impVar
class(impVar)

varImp(impVar)
varImpPlot(impVar)

# BALANCEANDO VAR. CREDIT.RATING
install.packages("smotefamily")
library(smotefamily)

datacred.bal <- SMOTE(datacred, datacred$credit.rating)

# DIVISÃO DADOS TREINO E TESTE

sepdata <- sample(1:nrow(datacred), size = 0.6 * nrow(datacred))

datacred_treino <- datacred[sepdata,]
datacred_teste <- datacred[-sepdata,]

# CRIANDO O MODELO

model1 <- glm(credit.rating ~., datacred_treino, family = 'binomial')

summary(model1)

modelteste1 <- predict(model1,datacred_treino, type = 'response')
modelteste1 <- round(modelteste1)

table(datacred_teste$credit.rating)
table(modelteste1[,])
summary(modelteste1)

confusionMatrix(table(data = modelteste1, reference = datacred_teste$credit.rating))

