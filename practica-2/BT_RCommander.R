
adultliteracy <- 
  read.table("C:/Dropbox/Formacion/UOC-BD/UOCAsig/TipoCiclo/Bloque3/Bad_Teeth/adultliteracy.csv",
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
badteeth <- 
  read.table("C:/Dropbox/Formacion/UOC-BD/UOCAsig/TipoCiclo/Bloque3/Bad_Teeth/badteeth.csv",
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
gdp <- 
  read.table("C:/Dropbox/Formacion/UOC-BD/UOCAsig/TipoCiclo/Bloque3/Bad_Teeth/gdp.csv",
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
healthexpend <- 
  read.table("C:/Dropbox/Formacion/UOC-BD/UOCAsig/TipoCiclo/Bloque3/Bad_Teeth/healthexpend.csv",
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
sugar_comsumption <- 
  read.table("C:/Dropbox/Formacion/UOC-BD/UOCAsig/TipoCiclo/Bloque3/Bad_Teeth/sugar_consumption.csv",
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
dim(adultliteracy)
names(adultliteracy)
head(adultliteracy)
dim(badteeth)
names(badteeth)
head(badteeth)
dim(gdp)
names(gdp)
head(gdp)
dim(healthexpend)
names(healthexpend)
head(healthexpend)
dim(sugar_comsumption)
names(sugar_comsumption)
head(sugar_comsumption)

colnames(adultliteracy)[1]
colnames(adultliteracy)[1] <- "Country"
colnames(badteeth)[1]
colnames(badteeth)[1] <- "Country"
colnames(gdp)[1]
colnames(gdp)[1] <- "Country"
colnames(healthexpend)[1]
colnames(healthexpend)[1] <- "Country"
colnames(sugar_comsumption)[1]
colnames(sugar_comsumption)[1] <- "Country"
colnames(adultliteracy) <- gsub("X","", colnames(adultliteracy))
colnames(badteeth) <- gsub("X","", colnames(badteeth))
colnames(gdp) <- gsub("X","", colnames(gdp))
colnames(healthexpend) <- gsub("X","", colnames(healthexpend))
colnames(sugar_comsumption) <- gsub("X","", colnames(sugar_comsumption))

sapply(adultliteracy, function(x)(sum(is.na(x))))
sapply(adultliteracy, function(x)(sprintf("%.2f%%",sum(is.na(x))*100/nrow(adultliteracy))))
apply(adultliteracy, 1, function(x)(all(is.na(x))))
apply(adultliteracy, 2, function(x)(all(is.na(x))))
adultliteracy[262,]
adultliteracy<-adultliteracy[-c(261, 262),]

sapply(badteeth, function(x)(sum(is.na(x))))
nrow(badteeth)
apply(badteeth, 2, function(x)(all(is.na(x))))
which(is.na(badteeth[,1]))
which(is.na(badteeth[,2]))
badteeth<-badteeth[,-c(3,4,5)]
badteeth<-badteeth[-c(191),]
head(badteeth)

sapply(gdp, function(x)(sum(is.na(x))))
sapply(gdp, function(x)(sprintf("%.2f%%",sum(is.na(x))*100/nrow(gdp))))

sapply(healthexpend, function(x)(sum(is.na(x))))
sapply(healthexpend, function(x)(sprintf("%.2f%%",sum(is.na(x))*100/nrow(healthexpend))))

sapply(sugar_comsumption, function(x)(sum(is.na(x))))
sapply(sugar_comsumption, function(x)(sprintf("%.2f%%",sum(is.na(x))*100/nrow(sugar_comsumption))))

apply(sugar_comsumption, 1, function(x)(all(is.na(x))))
sugar_comsumption <- sugar_comsumption[,-c(ncol(sugar_comsumption))]
sugar_comsumption <- sugar_comsumption[-c(260:278),]

nrow(badteeth)
nrow(unique(badteeth))
length(intersect(sugar_comsumption[,1],intersect(healthexpend[,1],intersect(gdp[,1], intersect(badteeth[,1], adultliteracy[,1])))))
length(setdiff(gdp[,1],badteeth[,1]))
length(setdiff(badteeth[,1],gdp[,1]))

setdiff(badteeth[,1],gdp[,1])
setdiff(gdp[,1], badteeth[,1])
badteeth[grep("Central",badteeth$Country),1]
gdp[grep("Central",gdp$Country),1]
adultliteracy[grep("Central",adultliteracy$Country),1]
healthexpend[grep("Central",healthexpend$Country),1]
sugar_comsumption[grep("Central",sugar_comsumption$Country),1]
badteeth$Country <- gsub("Central.+","Central African Republic", badteeth$Country)
adultliteracy$Country <- gsub("Central.+","Central African Republic", adultliteracy$Country)
sugar_comsumption$Country <- gsub("Central.+","Central African Republic", sugar_comsumption$Country)

adultliteracy[grep("Cote",adultliteracy$Country),1]
sugar_comsumption[grep("Cote",sugar_comsumption$Country),1]
healthexpend[grep("Cote",healthexpend$Country),1]

badteeth$Country <- gsub("Cote.+","Cote d'Ivoire", badteeth$Country)

adultliteracy[grep("Dominican",adultliteracy$Country),1]
sugar_comsumption[grep("Dominican",sugar_comsumption$Country),1]
healthexpend[grep("Dominican",healthexpend$Country),1]
badteeth$Country <- gsub("Dominican.+","Dominican Republic", badteeth$Country)
adultliteracy$Country <- gsub("Dominican.+","Dominican Republic", adultliteracy$Country)
sugar_comsumption$Country <- gsub("Dominican.+","Dominican Republic", sugar_comsumption$Country)

adultliteracy[grep("Lao",adultliteracy$Country),1]
healthexpend[grep("Lao",healthexpend$Country),1]
sugar_comsumption[grep("Lao",sugar_comsumption$Country),1]
gdp[grep("Lao",gdp$Country),1]
badteeth[grep("Lao",badteeth$Country),1]
healthexpend$Country <- gsub("Lao","Laos", healthexpend$Country)
gdp$Country <- gsub("Lao","Laos", gdp$Country)

adultliteracy[grep("Lucia",adultliteracy$Country),1]
healthexpend[grep("Lucia",healthexpend$Country),1]
sugar_comsumption[grep("Lucia",sugar_comsumption$Country),1]
gdp[grep("Lucia",gdp$Country),1]
badteeth[grep("Lucia",badteeth$Country),1]
healthexpend$Country <- gsub(".+Lucia","Saint Lucia", healthexpend$Country)
gdp$Country <- gsub(".+Lucia","Saint Lucia", gdp$Country)

adultliteracy[grep("Slova",adultliteracy$Country),1]
healthexpend[grep("Slova",healthexpend$Country),1]
sugar_comsumption[grep("Slova",sugar_comsumption$Country),1]
gdp[grep("Slova",gdp$Country),1]
badteeth[grep("Slova",badteeth$Country),1]
badteeth$Country <- gsub("Slova.+","Slovak Republic", badteeth$Country)

adultliteracy[grep("Cook",adultliteracy$Country),1]
healthexpend[grep("Cook",healthexpend$Country),1]
sugar_comsumption[grep("Cook",sugar_comsumption$Country),1]
gdp[grep("Cook",gdp$Country),1]
badteeth[grep("Cook",badteeth$Country),1]
healthexpend$Country <- gsub("Cook.+","Cook Islands", healthexpend$Country)
gdp$Country <- gsub("Cook.+","Cook Islands", gdp$Country)

adultliteracy[grep("Czech",adultliteracy$Country),1]
healthexpend[grep("Czech",healthexpend$Country),1]
sugar_comsumption[grep("Czech",sugar_comsumption$Country),1]
gdp[grep("Czech",gdp$Country),1]
badteeth[grep("Czech",badteeth$Country),1]
healthexpend$Country <- gsub("Czech R.+","Czech Rep.", healthexpend$Country)
gdp$Country <- gsub("Czech R.+","Czech Rep.", gdp$Country)

adultliteracy[grep("Kyrgy",adultliteracy$Country),1]
healthexpend[grep("Kyrgy",healthexpend$Country),1]
sugar_comsumption[grep("Kyrgy",sugar_comsumption$Country),1]
gdp[grep("Kyrgy",gdp$Country),1]
badteeth[grep("Kyrgy",badteeth$Country),1]
healthexpend$Country <- gsub("Kyrgy.+","Kyrgyzstan", healthexpend$Country)
gdp$Country <- gsub("Kyrgy.+","Kyrgyzstan", gdp$Country)

adultliteracy[grep("Kitts",adultliteracy$Country),1]
healthexpend[grep("Kitts",healthexpend$Country),1]
sugar_comsumption[grep("Kitts",sugar_comsumption$Country),1]
gdp[grep("Kitts",gdp$Country),1]
badteeth[grep("Kitts",badteeth$Country),1]
healthexpend$Country <- gsub(".+Kitts.+","Saint Kitts and Nevis", healthexpend$Country)
gdp$Country <- gsub(".+Kitts.+","Saint Kitts and Nevis", gdp$Country)

adultliteracy[grep("Vincent",adultliteracy$Country),1]
healthexpend[grep("Vincent",healthexpend$Country),1]
sugar_comsumption[grep("Vincent",sugar_comsumption$Country),1]
gdp[grep("Vincent",gdp$Country),1]
badteeth[grep("Vincent",badteeth$Country),1]
healthexpend$Country <- gsub(".+Vincent.+","Saint Vincent and the Grenadines", healthexpend$Country)
gdp$Country <- gsub(".+Vincent.+","Saint Vincent and the Grenadines", gdp$Country)

adultliteracy[grep("Yemen",adultliteracy$Country),1]
healthexpend[grep("Yemen",healthexpend$Country),1]
sugar_comsumption[grep("Yemen",sugar_comsumption$Country),1]
gdp[grep("Yemen",gdp$Country),1]
badteeth[grep("Yemen",badteeth$Country),1]
healthexpend$Country <- gsub("North Yemen.+","Yemen Arab Republic (Former)", healthexpend$Country)
gdp$Country <- gsub("North Yemen.+","Yemen Arab Republic (Former)", gdp$Country)
healthexpend$Country <- gsub("South Yemen.+","Yemen Democratic (Former)", healthexpend$Country)
gdp$Country <- gsub("South Yemen.+","Yemen Democratic (Former)", gdp$Country)
healthexpend$Country <- gsub("Yemen","Yemen, Rep.", healthexpend$Country)
gdp$Country <- gsub("Yemen","Yemen, Rep.", gdp$Country)

adultliteracy[grep("Korea",adultliteracy$Country),1]
healthexpend[grep("Korea",healthexpend$Country),1]
sugar_comsumption[grep("Korea",sugar_comsumption$Country),1]
gdp[grep("Korea",gdp$Country),1]
badteeth[grep("Korea",badteeth$Country),1]
healthexpend$Country <- gsub("North Korea","Korea, Dem. Rep.", healthexpend$Country)
gdp$Country <- gsub("North Korea","Korea, Dem. Rep.", gdp$Country)
healthexpend$Country <- gsub("South Korea","Korea, Rep.", healthexpend$Country)
gdp$Country <- gsub("United Korea.+","Korea, United", gdp$Country)
healthexpend$Country <- gsub("United Korea.+","Korea, United", healthexpend$Country)
gdp$Country <- gsub("South Korea","Korea, Rep.", gdp$Country)

nrow(badteeth)
length(intersect(sugar_comsumption[,1],intersect(healthexpend[,1],intersect(gdp[,1], intersect(badteeth[,1], adultliteracy[,1])))))
Todos los países de _badteeth_ (190 casos) figuran también en el resto de datasets

str(adultliteracy)
str(badteeth)
str(gdp)
str(healthexpend)
str(sugar_comsumption)

adultliteracy$Country <- as.factor(adultliteracy$Country)
str(adultliteracy$Country)
badteeth$Country <- as.factor(badteeth$Country)
gdp$Country <- as.factor(gdp$Country)
healthexpend$Country <- as.factor(healthexpend$Country)
sugar_comsumption$Country <- as.factor(sugar_comsumption$Country)

badteethExt <- data.frame(Country=character(), Year=character(), BadTeeths=integer(), stringsAsFactors=FALSE)
for (j in 2:ncol(badteeth))
{
year<- colnames(badteeth)[j]
for (i in 1:nrow(badteeth))
{
badteethExt[(j-2)*nrow(badteeth)+i,1] <- as.character(badteeth$Country[i])
badteethExt[(j-2)*nrow(badteeth)+i,2] <- year
badteethExt[(j-2)*nrow(badteeth)+i,3] <- badteeth[i,j]
}
}
nrow(badteethExt)
head(badteethExt)

adultliteracyExt <- data.frame(Country=character(), Year=character(), LiteracyRate=integer(), stringsAsFactors=FALSE)
for (j in 2:ncol(adultliteracy))
{
year<- colnames(adultliteracy)[j]
for (i in 1:nrow(adultliteracy))
{
adultliteracyExt[(j-2)*nrow(adultliteracy)+i,1] <- as.character(adultliteracy$Country[i])
adultliteracyExt[(j-2)*nrow(adultliteracy)+i,2] <- year
adultliteracyExt[(j-2)*nrow(adultliteracy)+i,3] <- adultliteracy[i,j]
}
}
nrow(adultliteracyExt) == nrow(adultliteracy)*(ncol(adultliteracy)-1)
adultliteracyExt[1:3,]

healthexpendExt <- data.frame(Country=character(), Year=character(), HealthExpend=integer(), stringsAsFactors=FALSE)
for (j in 2:ncol(healthexpend))
{
year<- colnames(healthexpend)[j]
for (i in 1:nrow(healthexpend))
{
healthexpendExt[(j-2)*nrow(healthexpend)+i,1] <- as.character(healthexpend$Country[i])
healthexpendExt[(j-2)*nrow(healthexpend)+i,2] <- year
healthexpendExt[(j-2)*nrow(healthexpend)+i,3] <- healthexpend[i,j]
}
}
nrow(healthexpendExt) == nrow(healthexpend)*(ncol(healthexpend)-1)
healthexpendExt[1:3,]

gdpExt <- data.frame(Country=character(), Year=character(), GDP=integer(), stringsAsFactors=FALSE)
for (j in 2:ncol(gdp))
{
year<- colnames(gdp)[j]
for (i in 1:nrow(gdp))
{
gdpExt[(j-2)*nrow(gdp)+i,1] <- as.character(gdp$Country[i])
gdpExt[(j-2)*nrow(gdp)+i,2] <- year
gdpExt[(j-2)*nrow(gdp)+i,3] <- gdp[i,j]
}
}
nrow(gdpExt) == nrow(gdp)*(ncol(gdp)-1)
gdpExt[1:3,]

sugar_comsumptionExt <- data.frame(Country=character(), Year=character(), SugarComsumption=integer(), stringsAsFactors=FALSE)
for (j in 2:ncol(sugar_comsumption))
{
year<- colnames(sugar_comsumption)[j]
for (i in 1:nrow(sugar_comsumption))
{
sugar_comsumptionExt[(j-2)*nrow(sugar_comsumption)+i,1] <- as.character(sugar_comsumption$Country[i])
sugar_comsumptionExt[(j-2)*nrow(sugar_comsumption)+i,2] <- year
sugar_comsumptionExt[(j-2)*nrow(sugar_comsumption)+i,3] <- sugar_comsumption[i,j]
}
}
nrow(sugar_comsumptionExt) == nrow(sugar_comsumption)*(ncol(sugar_comsumption)-1)
sugar_comsumptionExt[1:3,]

bt <- merge(badteethExt, gdpExt, by=c("Country", "Year"))
bt <- merge(bt, healthexpendExt, by=c("Country", "Year"))
bt <- merge(bt, sugar_comsumptionExt, by=c("Country", "Year"))
bt <- merge(bt, adultliteracyExt, by=c("Country", "Year"))
nrow(bt)
bt[1:4,]

str(bt)
bt$Country <- as.factor(bt$Country)

sapply(bt, function(x)(sum(is.na(x))))
sapply(bt, function(x)(sprintf("%.2f%%",sum(is.na(x))*100/nrow(bt))))

bta <- subset(bt, !is.na(GDP)| !is.na(HealthExpend)| !is.na(SugarComsumption) | !is.na(LiteracyRate))
nrow(bta)

btb <- subset(bt, !is.na(GDP)& !is.na(HealthExpend)& !is.na(SugarComsumption))
nrow(btb)


summary(btb$BadTeeths)

with(btb, Hist(BadTeeths, scale="frequency", breaks="Sturges", col="darkgray"))
Boxplot( ~ BadTeeths, data=btb, id.method="y")

IQR(btb$BadTeeths)
var(btb$BadTeeths)
sd(btb$BadTeeths)
btb[which(btb$BadTeeths > 1.5*1.66+2.8),]

smm<-summary(btb$GDP)
smm
with(btb, Hist(GDP, scale="frequency", breaks="Sturges", col="darkgray"))
Boxplot( ~ GDP, data=btb, id.method="y")
iqr<-IQR(btb$GDP)
iqr
var(btb$GDP)
sd(btb$GDP)
btb[which(btb$GDP > 1.5*iqr+smm[5]),]
btc<-btb
btc$GDP<- (btb$GDP - mean(btb$GDP)) / sd(btb$GDP)
summary(btc$GDP)

with(btc, Hist(GDP, scale="frequency", breaks="Sturges", col="darkgray"))

with(btc, Hist(GDP, scale="percent", breaks="Sturges", col="darkgray"))
Boxplot( ~ GDP, data=btc, id.method="y")


smm<-summary(btc$HealthExpend)
smm
with(btc, Hist(HealthExpend, scale="frequency", breaks="Sturges", col="darkgray"))
Boxplot( ~ HealthExpend, data=btc, id.method="y")
iqr<-IQR(btc$HealthExpend)
iqr
var(btc$HealthExpend)
sd(btc$HealthExpend)
btc[which(btc$HealthExpend > 1.5*iqr+smm[5]),]
btc[which(btc$HealthExpend < smm[2]-1.5*iqr),]

btd<-btc
btd$HealthExpend<- (btc$HealthExpend - mean(btc$HealthExpend)) / sd(btc$HealthExpend)
summary(btd$HealthExpend)

with(btd, Hist(HealthExpend, scale="frequency", breaks="Sturges", col="darkgray"))

smm<-summary(btd$SugarComsumption)
smm
with(btd, Hist(SugarComsumption, scale="frequency", breaks="Sturges", col="darkgray"))
Boxplot( ~ SugarComsumption, data=btd, id.method="y")
iqr<-IQR(btd$SugarComsumption)

iqr
var(btd$SugarComsumption)
sd(btd$SugarComsumption)
btc[which(btd$SugarComsumption > 1.5*iqr+smm[5]),]
btc[which(btd$SugarComsumption < smm[2]-1.5*iqr),]


scatterplot(GDP~BadTeeths, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, 
  levels=c(.5, .9), data=btd)
scatterplot(SugarComsumption~BadTeeths, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, 
  ellipse=FALSE, levels=c(.5, .9), data=btd)

scatterplot(HealthExpend~BadTeeths, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE,
   levels=c(.5, .9), data=btd)


rlin <- lm(BadTeeths ~ GDP + HealthExpend + SugarComsumption, data=btd)
summary(rlin)
rlin2 <- lm(BadTeeths ~ HealthExpend + SugarComsumption, data=btd)
summary(rlin2)
rlin3 <- lm(BadTeeths ~ SugarComsumption, data=btd)
summary(rlin3)
rlin4 <- lm(BadTeeths ~ GDP, data=btd)
summary(rlin4)

names(badteeth) <- make.names(names(badteeth))

