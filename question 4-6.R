# How does immunization affect life expectancy? What kind of immunization have
# a great influence?


# df for life expectancy and different kinds of immunization
expectancy_immunization <- immunization %>%
  select(`Life expectancy`,`Hepatitis B`,Polio,Diphtheria)

# compute correlation
immunization_cormat <- round(cor(expectancy_immunization,method = "spearman"),2)
immunization_cormat[upper.tri(immunization_cormat)] <- NA
head(immunization_cormat)

# reshape correlation
immunization_melted_cormat <- melt(immunization_cormat, na.rm = TRUE)
head(immunization_melted_cormat)

# plotting melted cormat
ggplot(immunization_melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  scale_fill_gradient2(low = 'red', mid = 'yellow', high = 'green', limit = c(-1,1),
                       midpoint = 0, name = "Spearman\nCorrelation") +
  geom_text(aes(Var2,Var1,label = value), color = "black",size = 4)+
  geom_tile() + theme_clean()

# multiple linear regression model for immunization
immunization_lm <- lm(`Life expectancy` ~., data = expectancy_immunization)
summary(immunization_lm)
backward_economy <- step(immunization_lm, direction = "backward", scope = formula(immunization_lm))

plot(expectancy_immunization)
immunization_model <- lm(formula = `Life expectancy` ~ Polio + Diphtheria, data = expectancy_immunization)
summary(immunization_model)


# Does economical factors and population affect life expectancy? Yes

# df for life expectancy and economical factors
expectancy_economy <- economical %>%
  select(`Life expectancy`,`percentage expenditure`,`Total expenditure`,GDP,
         `Income composition of resources`) %>%
  filter(`Income composition of resources` > 0)


# compute correlation
economy_cormat <- round(cor(expectancy_economy, method = "spearman"),2)
economy_cormat[upper.tri(economy_cormat)] <- NA
economy_cormat

# reshape correlation
economy_melted_cormat <- melt(economy_cormat, na.rm = TRUE)
head(economy_melted_cormat)

# plotting melted cormat
ggplot(economy_melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  scale_fill_gradient2(low = 'red', mid = 'yellow', high = 'green', limit = c(-1,1),
                       midpoint = 0, name = "Spearman\nCorrelation") +
  geom_text(aes(Var2,Var1,label = value), color = "black",size = 4)+
  geom_tile() + theme_clean() 

# multiple linear regression model for immunization
plot(expectancy_economy)
economy_model <- lm(formula = `Life expectancy` ~ `percentage expenditure` 
                    + GDP + `Income composition of resources`,
                    data = expectancy_economy)
summary(economy_model)

economy_lm <- lm(`Life expectancy` ~., data = expectancy_economy)
summary(economy_lm)
backward_economy <- step(economy_lm, direction = "backward", scope = formula(economy_lm))


# df for expectancy and population
expectancy_population <- life_expectancy %>%
  select(`Life expectancy`, Population)

#determine if there is a correlation schooling and life expectancy
cor.test(expectancy_population$`Life expectancy`, expectancy_population$Population,
         method = "spearman")
# very low correlation, 9%

#visual the relationship using lm  
ggplot(data = expectancy_population, aes(x = Population, y = `Life expectancy`)) + 
  geom_point() + geom_smooth(method = "lm") + 
  xlab("Population") + ylab("Life expectancy(yr)")

#test if there is causation
social_lm <- lm(`Life expectancy` ~ Schooling, data = social)
summary(social_lm)
#only 56.55% of the variability in life expectancy can be explained by years of schooling
