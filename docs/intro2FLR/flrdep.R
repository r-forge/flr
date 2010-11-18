# code for plot about FLR dependency model
jpeg("flrdep.jpg", 500, 500, quality=100)
par(mai=c(0.2, 0.2, 0.5, 0.2))
plot(seq(1, 15, len=2)~seq(1, 8, len=2), type="n", xlab="", ylab="", main="FLR dependency model", axes=FALSE)
points(3, 12, pch=1, cex=35, lwd=2)
text(3, 14, "FLCore", font=2)
points(2.4, 12, pch=1, cex=15, col="darkblue", lwd=2)
text(2.4, 12, "classes")
points(3.5, 11.5, pch=1, cex=15, col="darkgreen", lwd=2)
text(3.5, 11.5, "methods")

points(6, 4, pch=1, cex=35, lwd=2)
text(6, 6, "2ndry pkg", font=2)
points(5.4, 4, pch=1, cex=15, col="darkblue", lwd=2)
text(5.4, 4, "classes")
points(6.5, 3.5, pch=1, cex=15, col="darkgreen", lwd=2)
text(6.5, 3.5, "methods")

arrows(2.4, 11, 5, 4.5, length=0.1, col="darkred", lwd=2) 
arrows(3.5, 10.5, 5.2, 4.5, length=0.1, col="darkred", lwd=2) 
text(3.5, 7, "extends")
text(4.7, 8, "inherits")
dev.off()

