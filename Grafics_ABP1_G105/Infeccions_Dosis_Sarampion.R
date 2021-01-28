vacunacio <- read.csv("vaccinations.csv", sep=";",stringsAsFactors=TRUE)

#Grafica dosis#
plot(vacunacio$X, vacunacio$First.dose, xlab = "Any", ylab = "Dosis administrades(%)", main = "Dosis administrades de la vacuna triple vírica", ylim=c(70,100), col = "blue", pch = 19, yaxt="none", xaxt="none" )
axis(1, seq(1999,2018,1), cex.axis=0.6, las=2)
axis(2, seq(70,100,2.5), cex.axis=0.75, las=1)
lines(vacunacio$X, vacunacio$First.dose, col="blue")
lines(vacunacio$X, vacunacio$Second.dose, col="violet")
points(vacunacio$X, vacunacio$Second.dose, col = "violet", pch = 19)
abline(h=seq(70,100,2.5), v=seq(1999,2018,1), lty=3, col="gray")
legend(x = "bottomright", legend = c("Primera dosi", "Segona dosi"), fill = c("blue", "violet"), title = "Llegenda", cex=0.5)

dat <- read.csv("casos-sarampion.csv", sep=",", stringsAsFactors = TRUE)
mask <- dat$WHO_REGION == "EUR"
sarampion <- dat[mask,]
Aillat<- sarampion[, c(3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]
infectats  <- colSums(Aillat[-1], na.rm = TRUE)
ai <- data.frame(infectats)
ai$infectats <- ai$infectats[nrow(ai):1]
any <- vacunacio$X < "2016"
anyseparats <- vacunacio[any,]

#Grafica infecciones#
plot(anyseparats$X, ai$infectats ,xlab ="Any", ylab= "Casos de Xarampió", main = "Casos totals de Xarampió a Europa", ylim=c(6000,60000), col="red", yaxt="none", xaxt="none", type="l")
points(anyseparats$X, ai$infectats, col="red", pch = 20)
axis(1, seq(1999,2018,1), cex.axis=0.6, las=2)
axis(2, seq(6000,61000,2000), cex.axis=0.75, las=1)
abline(h=seq(6000,61000,4000), v=seq(1999,2018,1), lty=3, col="gray")
legend(x = "topright", legend = c("Infeccions de Xarampió"), fill = c("red"), title = "Llegenda", cex=0.5)
