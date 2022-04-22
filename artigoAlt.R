#Estatisticas descritivas
library(FSA)
Summarize(dados$citacao)
Summarize(dados$twitter)
Summarize(dados$facebook)
Summarize(dados$noticias)
Summarize(dados$blogs)
Summarize(dados$mendeley)
Summarize(dados$score)

#Frequencias
table(dados$altmetrico)/nrow(dados)*100
table(dados$citadoTwitter)/nrow(dados)*100
table(dados$citadoFacebook)/nrow(dados)*100
table(dados$citadoNoticias)/nrow(dados)*100
table(dados$citadoBlogs)/nrow(dados)*100
table(dados$citadoMendeley)/nrow(dados)*100

# Normalidade
library(nortest)
sf.test(dados$citacao)
sf.test(dados$twitter)
sf.test(dados$facebook)
sf.test(dados$noticias)
sf.test(dados$blogs)
sf.test(dados$mendeley)
sf.test(dados$score)

#Variancia
#bartlett.test(dados$citacao~dados$altmetrico)

#Correlacao citacao
cor.test(dados$citacao,dados$twitter, method = "spearman")
cor.test(dados$citacao,dados$facebook, method = "spearman")
cor.test(dados$citacao,dados$noticias, method = "spearman")
cor.test(dados$citacao,dados$blogs, method="spearman")
cor.test(dados$citacao,dados$mendeley, method = "spearman")
cor.test(dados$citacao,dados$ano, method = "spearman")
cor.test(dados$citacao,dados$score, method = "spearman")

#Correlacao anos
cor.test(dados$twitter,dados$ano, method = "spearman")
cor.test(dados$facebook,dados$ano, method = "spearman")
cor.test(dados$noticias,dados$ano, method = "spearman")
cor.test(dados$mendeley,dados$ano, method = "spearman")
cor.test(dados$blogs,dados$ano, method = "spearman")
cor.test(dados$score,dados$ano, method = "spearman")

#Diferencas
wilcox.test(dados$citacao~dados$altmetrico)
Summarize(dados$citacao~dados$altmetrico)

wilcox.test(dados$citacao~dados$citadoTwitter)
Summarize(dados$citacao~dados$citadoTwitter)

wilcox.test(dados$citacao~dados$citadoFacebook)
Summarize(dados$citacao~dados$citadoFacebook)

wilcox.test(dados$citacao~dados$citadoNoticias)
Summarize(dados$citacao~dados$citadoNoticias)

wilcox.test(dados$citacao~dados$citadoBlogs)
Summarize(dados$citacao~dados$citadoBlogs)

wilcox.test(dados$citacao~dados$citadoMendeley)
Summarize(dados$citacao~dados$citadoMendeley)

# Separar por periodos
library(sqldf)
periodo1 = sqldf("SELECT * FROM dados WHERE ano BEtWEEN '1970' AND '1979'")
periodo2 = sqldf("SELECT * FROM dados WHERE ano BEtWEEN '1980' AND '1989'")
periodo3 = sqldf("SELECT * FROM dados WHERE ano BEtWEEN '1990' AND '1999'")
periodo4 = sqldf("SELECT * FROM dados WHERE ano BEtWEEN '2000' AND '2009'")
periodo5 = sqldf("SELECT * FROM dados WHERE ano >  '2009'")

# Estatisticas descritivas periodos
library(FSA)
Summarize(periodo1$citacao)
Summarize(periodo2$citacao)
Summarize(periodo3$citacao)
Summarize(periodo4$citacao)
Summarize(periodo5$citacao)

# Normalidade periodos
shapiro.test(periodo1$citacao)
library(nortest)
sf.test(periodo2$citacao)
sf.test(periodo3$citacao)
sf.test(periodo4$citacao)
sf.test(periodo5$citacao)

#Correlacao citacao periodos
cor.test(periodo1$citacao,periodo1$score, method = "spearman")
cor.test(periodo2$citacao,periodo2$score, method = "spearman")
cor.test(periodo3$citacao,periodo3$score, method = "spearman")
cor.test(periodo4$citacao,periodo4$score, method = "spearman")
cor.test(periodo5$citacao,periodo5$score, method = "spearman")