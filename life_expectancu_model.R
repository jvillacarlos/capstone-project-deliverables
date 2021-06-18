# plot

plot(immunization)
plot(mortality)
plot(economical)
plot(social)
plot(body)
plot(alcohol)

# correlation matrix heatmap
ggplot(data = life_expectancy, aes(x=Variables, y=Variable, fill=value)) + 
  geom_tile()

# linear regression model

