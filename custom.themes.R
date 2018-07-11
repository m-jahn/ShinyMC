# define some global theme settings
library(latticeExtra)
#display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)

gradient <- rev(brewer.pal(11, "Spectral")[-c(5:6)])
custom.lattice <- custom.theme(symbol=gradient, fill=gradient,
  region=gradient, reference = "#e8e8e8", pch=19)
custom.lattice$par.main.text$cex <- 1
custom.lattice$strip.background$col <- grey(0.85)
custom.lattice$superpose.symbol$cex <- 1
custom.lattice$superpose.symbol$pch <- 19
custom.lattice$add.text$cex <- 0.8


custom.ggplot <- ggplot2like()
custom.ggplot$axis.line$col <- "white"
custom.ggplot$axis.line$lwd <- 2
custom.ggplot$strip.border$col <- "white"
custom.ggplot$strip.border$lwd <- 2
custom.ggplot$regions$col <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)


