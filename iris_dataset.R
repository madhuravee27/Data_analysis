##Reading the iris dataset
data("iris")
##counting number of rows
iris.rows <<- nrow(iris)
##counting number of columns
iris.cols <<- ncol(iris)
##storing the names of attributes in iris.attrs
iris.attrs <<- attributes(iris)$names
##storing the values for sepal width in iris.sw.vec
iris.sw.vec <<- iris$Sepal.Width
##extracting sepal width within range 3 and 4
iris.sw.vec.subset <<- subset(iris.sw.vec, iris.sw.vec >= 3 & iris.sw.vec <= 4)
##calculating mean for sepal width
iris.sw.mean <<- sum(iris.sw.vec)/length(iris.sw.vec)
##calculating standard deviation for sepal width
iris.sw.stdDev <<- sqrt(sum((iris.sw.vec-iris.sw.mean)^2/(length(iris.sw.vec)-1)))
##normalizing the values
iris.sw.mm.vec <<- (iris.sw.vec-min(iris.sw.vec))/(max(iris.sw.vec)-min(iris.sw.vec))
##z-score transformation
img.sw.zn.vec <<- (iris.sw.vec-iris.sw.mean)/iris.sw.stdDev
##Plot of Sepal length against sepal width
plot(iris$Sepal.Width,iris$Sepal.Length, main = "IRIS - Plot of Sepal length against sepal width",xlab = "Sepal Width", ylab = "Sepal length")


