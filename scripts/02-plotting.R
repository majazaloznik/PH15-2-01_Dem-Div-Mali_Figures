# Preliminaries ===============================================================
library(extrafont)
loadfonts()
par(family = "Garamond")
source("scripts/01_import-clean.R")

# Plot 1 ======================================================================
source("scripts/00-functions.R")
labelcex = 0.8
cex.main = 1
density = c(rep(5,3), rep(20,10), rep(5,3))
angle=c(rep(45,3), rep(-45,10), rep(45,3))
col = c(rep("gray60",3), rep("black",10), rep("gray60",3))
border = c(rep("gray60",3), rep("black",10), rep("gray60",3))
ppmar = c(2.5, 1, 1.5, 2)
filename <- "figures/DemDivFig01.pdf"
pdf(
  file = filename,
  width = 10,
  height = 6.6,
  family = "Garamond"
)

layout(matrix(1:4, c(2,2)))
FunPyramidPlotNoAxes(pyramids$male.p_Baseline,
                     pyramids$female.p_Baseline,
                     unit="", gap=0,
                     lxcol=col, rxcol=col,
                     density=density, 
                     angle=angle , 
                     lwd=1, 
                     main = "2013 - Baseline",
                     top.labels = c("Male",  "Female"),
                     ppmar = ppmar,
                     labelcex = labelcex,
                     xlim = c(10,10),
                     cex.main = cex.main,
                     border = border)
axis(4, las=2, at=1:17, labels=pyramids$age.group,tick = FALSE)
axis(1, at = seq(-10, 10, 2), labels = c(seq(10,0,-2), seq(2,10,2)))

FunPyramidPlotNoAxes(pyramids$male.p_2050_EE,
                     pyramids$female.p_2050_EE,
                     unit="", gap=0,
                     lxcol=col, rxcol=col,
                     density=density, 
                     angle=angle, 
                     lwd=1, 
                     main = "2050 - Economic Emphasis",
                     top.labels = c("Male",  "Female"),
                     ppmar = ppmar,
                     labelcex = labelcex,
                     xlim = c(10,10),
                     cex.main = cex.main,
                     border = border)
axis(1, at = seq(-10, 10, 2), labels = c(seq(10,0,-2), seq(2,10,2)))
axis(4, las=2, at=1:17, labels=pyramids$age.group,tick = FALSE)

FunPyramidPlotNoAxes(pyramids$male.p_2050_BAU,
                     pyramids$female.p_2050_BAU,
                     unit="", gap=0,
                     lxcol=col, rxcol=col,
                     density=density, 
                     angle=angle, 
                     lwd=1, 
                     main = "2050 - Business as usual",
                     top.labels = c("Male", "Female"),
                     ppmar = ppmar,
                     labelcex = labelcex,
                     xlim = c(10,10),
                     cex.main = cex.main,
                     border = border)

axis(1, at = seq(-10, 10, 2), labels = c(seq(10,0,-2), seq(2,10,2)))

FunPyramidPlotNoAxes(pyramids$male.p_2050CEEFP,
                     pyramids$female.p_2050CEEFP,
                     unit="", gap=0,
                     lxcol=col, rxcol=col,
                     density=density, 
                     angle=angle, 
                     lwd=1, 
                     main = "2050 - Combined Econ, Educ, FP",
                     top.labels = c("Male",  "Female"),
                     ppmar = ppmar,
                     labelcex = labelcex,
                     xlim = c(10,10),
                     cex.main = cex.main,
                     border = border)
axis(1, at = seq(-10, 10, 2), labels = c(seq(10,0,-2), seq(2,10,2)))

dev.off()
embed_fonts(filename,
            outfile = filename,
            options = "-dEPSCrop")

# Plot 2 ======================================================================
filename <- "figures/DemDivFig02.pdf"
cex = 0.9
pdf(
  file = filename,
  width = 5,
  height = 4,
  family = "Garamond"
)
# setup 
par(mar = c(4, 5,1, 5)+0.1)
# plot first line
plot(gdp_trend$year, gdp_trend$Comnibed_Econ_Educ_FP,
     axes = FALSE,
     xlab = "Year",
     ylab = "",
     type = "l",
     lwd = 2, 
     panel.first = grid(NA, NULL))

lines(gdp_trend$year, gdp_trend$Economic_Emphasis,
      lwd = 2,  lty = 2)
lines(gdp_trend$year, gdp_trend$Business_as_usual,
      lwd = 2, col = "gray60", lty = 2)

# axes
axis(1, at = c(2013, seq(2020, 2050,10)))
axis(2, las = 2, 
     at = seq(0, 1000, 200),
     labels = paste0("$", formatC(as.numeric(seq(0, 1000, 200)), 
                                  format="f", digits=0, big.mark=",")))

text(2057, max(gdp_trend$Business_as_usual) + 25, 
     "Business", 
     xpd = TRUE, cex = cex)
text(2057, max(gdp_trend$Business_as_usual) - 30, 
     "as usual ", 
     xpd = TRUE, cex = cex)

text(2057, max(gdp_trend$Economic_Emphasis) + 25, 
     "Economic", 
     xpd = TRUE, cex = cex)
text(2057, max(gdp_trend$Economic_Emphasis) - 30, 
     "emphasis", 
     xpd = TRUE, cex = cex)

text(2057, max(gdp_trend$Comnibed_Econ_Educ_FP) + 25, 
     "Combined", 
     xpd = TRUE, cex = cex)
text(2057, max(gdp_trend$Comnibed_Econ_Educ_FP) - 30, 
     "Econ, Educ, FP", 
     xpd = TRUE, cex = cex)

mtext("GDP per capita (US $ Billions)", side = 2, line = 3.5)
dev.off()
embed_fonts(filename,
            outfile = filename,
            options = "-dEPSCrop")

# Plot 3  =====================================================================
filename <- "figures/DemDivFig03.pdf"
cex = 0.8
pdf(
  file = filename,
  width = 5,
  height = 4,
  family = "Garamond"
)
# setup 
par(mar = c(4, 5,1, 4)+0.1)
# plot first line
x<- barplot(gdp_cumsum[[4]][2:4],
        space = 1.4,
        axes = FALSE,
        xlim = c(1, 8),
        density = 10,
        col = "black")
abline(h=gdp_cumsum[[4]][1],
       lwd = 2,
       lty = 2)
grid(nx = NA, ny = NULL)
barplot(gdp_cumsum[[4]][2:4],
        space = 1.4,
        axes = FALSE,
        add = TRUE,
        density = 10,
        col = "black")
axis(2, las = 2,
     at = seq(0, 25000, 5000),
     labels = paste0("$", formatC(as.numeric(seq(0, 25000, 5000)), 
                                  format="f", digits=0, big.mark=",")))

text(x, -1200, gdp_cumsum[[1]][2:4], xpd = TRUE, cex = cex)
text(x, -2500, gdp_cumsum[[2]][2:4], xpd = TRUE, cex = cex)
text(x, -3800, gdp_cumsum[[3]][2:4], xpd = TRUE, cex = cex)
text(9, gdp_cumsum[[4]][1] + 650, "2013", xpd = TRUE, cex = cex)
text(9, gdp_cumsum[[4]][1] - 650, "Baseline", xpd = TRUE, cex = cex)
mtext( "GDP per capita", 2, 4)
dev.off()
embed_fonts(filename,
            outfile = filename,
            options = "-dEPSCrop")




# Plot 4 ======================================================================
filename <- "figures/DemDivFig04.pdf"
cex = 0.85
pdf(
  file = filename,
  width = 5,
  height = 4,
  family = "Garamond"
)
# setup 
par(mar = c(4, 5,1, 5)+0.1)
# plot first line
plot(investment$year, investment$Comnibed_Econ_Educ_FP,
     axes = FALSE,
     xlab = "Year",
     ylab = "",
     type = "l",
     lwd = 2, 
     ylim = c(0, 5000),
     panel.first = grid(NA, NULL))

lines(investment$year, investment$Economic_Emphasis,
      lwd = 2,  lty = 2)
lines(investment$year, investment$Business_as_usual,
      lwd = 2, col = "gray60", lty = 2)

# axes
axis(1, at = c(2013, seq(2020, 2050,10)))
axis(2, las = 2, 
     at = seq(0, 5000, 1000),
     labels = paste0("$", formatC(as.numeric(seq(0, 5000, 1000)), 
                                  format="f", digits=0, big.mark=",")))

text(2057, max(investment$Business_as_usual) + 120, 
     "Business", 
     xpd = TRUE, cex = cex)
text(2057, max(investment$Business_as_usual) - 120, 
     "as usual ", 
     xpd = TRUE, cex = cex)

text(2057, max(investment$Economic_Emphasis) + 120, 
     "Economic", 
     xpd = TRUE, cex = cex)
text(2057, max(investment$Economic_Emphasis) - 120, 
     "emphasis", 
     xpd = TRUE, cex = cex)

text(2057, max(investment$Comnibed_Econ_Educ_FP) + 120, 
     "Combined", 
     xpd = TRUE, cex = cex)
text(2057, max(investment$Comnibed_Econ_Educ_FP) - 120, 
     "Econ, Educ, FP", 
     xpd = TRUE, cex = cex)

mtext("Investment per capita (US$)", side = 2, line = 3.5)
dev.off()
embed_fonts(filename,
            outfile = filename,
            options = "-dEPSCrop")


# Plot 5  =====================================================================
filename <- "figures/DemDivFig05.pdf"
cex = 0.8
pdf(
  file = filename,
  width = 5,
  height = 4,
  family = "Garamond"
)
# setup 
par(mar = c(1, 5,4, 3)+0.1)
# plot first line
x<- barplot(-hdi[[4]][2:4],
            space = 1.4,
            axes = FALSE,
            xlim = c(1, 8),
            density = 10,
            col = "black",
            ylim = c(-200, 0))
abline(h=-hdi[[4]][1],
       lwd = 2,
       lty = 2)
grid(nx = NA, ny = 10)
barplot(-hdi[[4]][2:4],
        space = 1.4,
        axes = FALSE,
        add = TRUE,
        density = 10,
        col = "black")
axis(2, las = 2,
     at = seq(0, -200, -20),
     labels = paste0(c(1, seq(20, 200, 20)), "."))

text(x, 25, hdi[[1]][2:4], xpd = TRUE, cex = cex)
text(x, 15, hdi[[2]][2:4], xpd = TRUE, cex = cex)
text(x, 5, hdi[[3]][2:4], xpd = TRUE, cex = cex)
text(8, -hdi[[4]][1] + 6, "2013", xpd = TRUE, cex = cex)
text(8, -hdi[[4]][1] - 6, "Baseline", xpd = TRUE, cex = cex)
mtext( "HDI ranking", 2, 3.5)
dev.off()
embed_fonts(filename,
            outfile = filename,
            options = "-dEPSCrop")


