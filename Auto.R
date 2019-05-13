library(leaps)
library(MASS)
library(foreign)
library(gam)
library(car)
library(dplyr)
library(ISwR)
library(lattice)
library(influence.ME)


# Modelo
auto <- read.csv("Auto.csv", header=FALSE, na.strings = c("","?"), sep=";")
colnames(auto) <- c("risk", "losses", "brand", "fuel", "aspiration", "doors", "body.style", 
                    "drive.wheels", "location", "wheel.base", "length", "width", "height", 
                    "curb.weight", "engine.type", "cylinders", "engine.size", "fuel.system", 
                    "bore", "stroke", "compression.ratio", "horsepower", "peak.rpm", 
                    "city.mpg", "highway.mpg", "price")

# Nr de casos
n <- nrow(auto)

# Escolha das variáveis excepto (losses, height, length, width, engine.type, cylinders, fuel.system,)
subset = regsubsets(price ~ risk + fuel + aspiration + doors + body.style + drive.wheels + 
                      location + wheel.base + curb.weight + engine.size + bore+ stroke + 
                      compression.ratio+ horsepower+ peak.rpm+ city.mpg + highway.mpg, data=auto, nvmax=10)

summary(subset)

##### Variáveis Explicativas (3 categóricas, 4 contínuas)
### Categóricas: 
# body.style (convertible, hardtop, hatchback, sedam, wagon)
# location (front, rear)
# aspiration (std, turbo)
### Continuas: 
# curb.weight
# engine.size
# peak.rpm (2 NAs)
# Resposta: price (4 NAs)

#Substitui NAs em variáveis contínuas pela média da variável
auto <- na.gam.replace(auto)

#Verificar se foram todos substituidos
which(is.na(auto))

##### Avaliar variáveis
### Continuas
#par(mar=c(1,1,1,1))

# price
hist(log(auto$price), breaks=10, ylab="Frequency", xlab="log(price)", main=NULL)
boxplot(log(auto$price), ylab="log(price)", main=NULL)

quantile(log(auto$price), type=2)
median(log(auto$price))

# curb.weight
hist(log(auto$curb.weight), breaks=10, ylab="Frequency", xlab="log(Curb Weight)", main=NULL)
boxplot(log(auto$curb.weight),ylab="log(Curb Weight)", main=NULL)

mean(log(auto$curb.weight))
sd(log(auto$curb.weight))

# peak.rpm
hist(log(auto$peak.rpm), breaks=10, ylab="Frequency", xlab="log(Peak RPM)", main=NULL)
boxplot(log(auto$peak.rpm), ylab="log(Peak RPM)", main=NULL)

mean(log(auto$peak.rpm))
sd(log(auto$peak.rpm))

# engine.size
hist(log(auto$engine.size), breaks=10, ylab="Frequency", xlab="log(Engine Size)", main=NULL)
boxplot(log(auto$engine.size), ylab="log(Engine Size)", main=NULL)

quantile(log(auto$engine.size), type=2)
median(log(auto$engine.size))

### Categóricas
# body.style
table(auto$body.style)/n
barplot(table(auto$body.style), ylab="Frequency", xlab="Body Style")

# aspitarion
table(auto$aspiration)/n
barplot(table(auto$aspiration), ylab="Frequency", xlab="Aspiration")

# location
table(auto$location)/n
barplot(table(auto$location), ylab="Frequency", xlab="Location")

### Avaliar associações entre resposta e variáveis explicativas continuas:
### Gráficos de dispersão , coeficiente de correlação

# price~curb.weight
plot(log(price)~log(curb.weight), data=auto, ylab="log(price)", xlab="log(Curb Weight)")
cor.test(log(auto$price), log(auto$curb.weight), method="pearson")

# price~peak.rpm
plot(log(price)~log(peak.rpm), data=auto, ylab="log(price)", xlab="log(Peak RPM)")
cor.test(log(auto$price), log(auto$peak.rpm), method="pearson")

# price~engine.size
plot(log(price)~log(engine.size), data=auto, ylab="log(price)", xlab="log(Engine Size)")
cor.test(log(auto$price), log(auto$engine.size), method="pearson")


### Avaliar associações entre resposta e variáveis explicativas categóricas:
### Gráficos boxplot lado-a-lado, regressão linear 


### c) 4) Efeitos brutos (reg.sum) vs. ajustados
# price~body.style
plot(log(price)~body.style, data=auto, ylab="log(price)", xlab="Body Style")
body.style.mod = lm(log(price)~body.style, data=auto)
reg.sum1 = summary(body.style.mod)

# price~aspiration
plot(log(price)~aspiration, data=auto, ylab="log(price)", xlab="Aspiration")
aspiration.mod = lm(log(price)~aspiration, data=auto)
reg.sum2 = summary(aspiration.mod)

# price~location
plot(log(price)~location, data=auto, ylab="log(price)", xlab="Location")
location.mod = lm(log(price)~location, data=auto)
reg.sum3 = summary(location.mod)

### Modelo Nulo
mod.nulo <- lm(log(price)~1, data=auto)
mod.nulo.sum <- summary(mod.nulo)

## Matriz correlações
cor(log(auto[,c("price",c("curb.weight","peak.rpm","engine.size"))]))

# Retirar curb.weight

### Modelo
mod <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location, data=auto)
summ <- summary(mod)

plot(mod)
# Pontos que podem ser um problema: 57, 58, 59, 70, 168
Anova(mod, type="III")
#todas as variáveis são significativas
################################################

### c) body.style e engine.size
### Efeito na resposta: mudar da 2ª var para a 3ª
# Var hardtop activa
auto$body.style <- relevel(auto$body.style, levels(auto$body.style)[2])
mod.2 <- lm(log(price) ~ body.style, data=auto)
summary(mod.2)

# Var hatchback activa
auto$body.style <- relevel(auto$body.style,"hatchback")
mod.3 <- lm(log(price) ~ body.style, data=auto)
summary(mod.3)
# Intervalo de confiaņca a 95
confint(mod.2, level=0.95)


auto$body.style <- relevel(auto$body.style, "convertible")

### Interacções
# log(peak.rpm)*body.style - NO
mod.int1 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + log(peak.rpm)*body.style, data=auto)
summary(mod.int1)
# log(engine.size)*body.style - NO
mod.int2 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + log(engine.size)*body.style, data=auto)
summary(mod.int2)
# log(engine.size)*aspiration - NO
mod.int3 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + log(engine.size)*aspiration, data=auto)
summary(mod.int3)
# log(peak.rpm)*aspiration - NO
mod.int4 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + log(peak.rpm)*aspiration, data=auto)
summary(mod.int4)
#log(engine.size)*location - NA
mod.int5 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + log(engine.size)*location, data=auto)
summary(mod.int5)
#log(peak.rpm)*location - NA
mod.int6 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + log(peak.rpm)*location, data=auto)
summary(mod.int6)
#location*aspiration - NA
mod.int7 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + aspiration*location, data=auto)
summary(mod.int7)
#location*body.style - NO/NA
mod.int8 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + location*body.style, data=auto)
summary(mod.int8)
#aspiration*body.style - SIM/NA
mod.int9 <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location + aspiration*body.style, data=auto)
summary(mod.int9)

#Apesar de aspiration*body.style ser significativa, uma das variáveis é NA, pelo que não será considerada

## Leverages
dist.cook <- cooks.distance(mod)
hat.values <- hatvalues(mod)
plot(hat.values, type = "h")
plot(hat.values, dist.cook)
inf.points <- influence.measures(mod)

# Normalidade dos Resíduos
# QQ plot para resíduos studentizados
qqPlot(mod, main="QQ Plot")
# Distribuição para Resíduos Studentizados
sresid <- studres(mod) 
hist(sresid, freq=FALSE, main="Distribuição para Resíduos Studentizados")
x.mod<-seq(min(sresid),max(sresid),length=40) 
y.mod<-dnorm(x.mod) 
lines(x.mod, y.mod)


# Distribuição para Resíduos Standardizados
rs <- rstandard(mod) 
hist(rs, freq=FALSE, main="Distribuição para Resíduos Standardizados")
x.mod1<-seq(min(rs),max(rs),length=40) 
y.mod1<-dnorm(x.mod1) 
lines(x.mod1, y.mod1)

# Residuos
residuos <- residuals(mod) 
plot(auto$peak.rpm, residuos)
plot(auto$engine.size, residuos)


# Homoscedasticity
# non-constant error variance test
ncvTest(mod.obs)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(mod.obs)

# Evaluate Collinearity
vif(mod) # variance inflation factors 
sqrt(vif(mod)) > 2 # problem?

#Dealing with influence points
influential.obs <- which(apply(inf.points$is.inf, 1, any)) 
auto.obs <- auto[-inf.points,]
mod.obs <- lm(log(price) ~ log(peak.rpm) + log(engine.size) + aspiration + body.style + location, data=auto.obs)


## Prediction Interval
pr.c <- predict(mod, interval="confidence")
pr.p <- predict(mod, interval="prediction")
predicted <- pr.p[,1]
which(predicted<8.6)

# Range increase
range(log(auto$engine.size))
var.amp = 5.786897-4.110874
var.15 = (5.786897-4.110874)*0.15
mod.range <- lm(log(auto$price)~log(auto$engine.size*0.2514))
summary(mod.range)

#Interacção X1 vs X2
plot(log(auto$engine.size),auto$body.style)

