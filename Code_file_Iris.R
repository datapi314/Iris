#Iris data set
library(datasets)
data("iris")

# iris data set gives the measurements in centimeters of the variables sepal length (Sepal.Length), Sepal width (Sepal.Width), petal length (Petal.Length), Petal width (Petal.Width) and Species, 
# respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.

# Calculate the mean of 'Sepal.Length' for the specie virginica

## Subsetting virginica specie and calculate the mean of Sepal Length

## Option 1
virgi <- iris[iris$Species =="virginica", ]$Sepal.Length
mean(virgi)


##Option 2
s <- split(iris, iris$Species)
sapply(s, function(x) colMeans(x["Sepal.Length"], na.rm = T))

##Option 3
s <- subset(iris, Species == "virginica")$Sepal.Length
mean(s[!is.na(s)])

# Find the means of Sepal.Length, Sepal.Width, Petal.Length and Petal.Width
head(iris)   # columns 1:4
apply(iris[ , 1:4], MARGIN = 2, mean)

# Plotting
library(ggplot2)

## Scatter plot between Sepal.Length and Petal.Length by Species

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species, shape = Species)) +
    geom_point(size = 2) +
    stat_smooth(method = "lm") +
    ggtitle("Sepal and Petal Length by Species") +
    xlab("Sepal.Length") +
    ylab("Petal.Length") 

## Boxplot Sepal.Length by Species
ggplot(iris, aes(y = Sepal.Length, x = Species, colour = Species)) +
    geom_boxplot(fill = c("red", "blue", "green")) +
    ggtitle("Sepal Length by Species") +
    xlab("Species") +
    ylab("Sepal.Length") 

## Histogram Sepal.Length by Species
ggplot(iris, aes(x =Sepal.Length)) +
    geom_histogram(binwidth = 0.1, fill = "grey") +
    ggtitle("Sepal Length by Species") +
    facet_wrap(~Species) +
    xlab("Sepal.Length") +
    ylab("Total Count") +
    labs(fill = "Species")

# Machine learning: linear relationship between Sepal and Petal Length in Virginica Specie

## Fit the model
virgi_lm <- iris[iris$Species =="virginica", ]
fit <- lm(Petal.Length ~ Sepal.Length, data = virgi_lm)
summary(fit)

## Residuals for the model
res <- residuals(fit)

## Plotting the residuals
plot(res ~ Sepal.Length, data = virgi_lm)
abline(0,0)
