anime_df <- read.csv("anime_df.csv")
anime_df[is.na(anime_df)] <- 0
sapply(anime_df, class)

head(anime_df)

cols = c("brown1", "deepskyblue", "darkseagreen1", "violet","orange", "blue", "pink", "cyan", "skyblue", "lavender", "coral", "aquamarine", "chocolate")
layout(matrix(1:2, nrow=1), widths = c(1,0.666))
par(mar = c(0,0,0,0))
pie(x=table(anime_df$genre), labels='', cex=0.75, col=cols)
plot.new()
legend("center", legend=unique(anime_df$genre), cex=0.6, bty='n', fill=cols)

par(mfrow=c(1,2))
hist(anime_df$Votes, xlab="Votes")
hist(anime_df$Rating, xlab="Rating")

plot(y=anime_df$Rating, x=anime_df$Votes)

cor(anime_df$Rating, anime_df$Votes)

range(anime_df$Rating)

length(which(anime_df$Rating >=7.0))

subset(anime_df, Votes==max(Votes))
summary(anime_df)
hist(anime_df$Rating)
summary(anime_df$Rating)

t.test(x=anime_df$Rating, conf.level=0.95)$conf.int
t.test(x=anime_df$Rating, conf.level=0.90)$conf.int
t.test(x=anime_df$Rating, alternative='two.sided', mu=7.0)
wilcox.test(x=anime_df$Rating, alternative='two.sided', mu=7.0)

unique(anime_df$genre)
library(ggplot2)
ggplot(data=anime_df, aes(x=genre, y=Rating)) + geom_boxplot() + theme(axis.text.x=element_text(size=7, angle=90))
anova = aov(anime_df$Rating~anime_df$genre)
summary(anova)
qqnorm(anova$residuals)

lin = lm(Votes~Rating, data=anime_df)
lin$coef
plot(lin)
cor(anime_df$Votes, anime_df$Rating)^2
summary(anime_df$Votes)
sum(c(1, 9.5)*lin$coef)
