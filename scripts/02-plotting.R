# Preliminaries ===============================================================
library(extrafont)
loadfonts(device = "win")
par(family = "Georgia")
source("scripts/01_import-clean.R")

# Plot 1 ======================================================================
source("scripts/00-functions.R")



layout(matrix(1:4, c(2,2)))
FunPyramidPlotNoAxes(pyramids$male.p_Baseline,
                     pyramids$female.p_Baseline,
                     unit="", gap=0,
                     lxcol="black", rxcol="black",
                     density=c(rep(5,3), rep(30,10), rep(5,3)), 
                     angle=c(rep(45,3), rep(-45,10), rep(45,3)), 
                     lwd=1, 
                     main = "2013 - Baseline",
                     top.labels = c("Male",  "Female"),
                     ppmar = c(3, 2, 1, 3),
                     labelcex = 0.8,
                     xlim = c(10,10))
axis(4, las=2, at=1:17, labels=pyramids$age.group,tick = FALSE)
axis(1, at = seq(-10, 10, 2), labels = c(seq(10,0,-2), seq(2,10,2)))

FunPyramidPlotNoAxes(pyramids$male.p_2050_EE,
                     pyramids$female.p_2050_EE,
                     unit="", gap=0,
                     lxcol="black", rxcol="black",
                     density=c(rep(5,3), rep(30,10), rep(5,3)), 
                     angle=c(rep(45,3), rep(-45,10), rep(45,3)), 
                     lwd=1, 
                     main = "2050 - Economic Emphasis",
                     top.labels = c("Male",  "Female"),
                     ppmar = c(3, 2, 1, 3),
                     labelcex = 0.8,
                     xlim = c(10,10))
axis(1, at = seq(-10, 10, 2), labels = c(seq(10,0,-2), seq(2,10,2)))
axis(4, las=2, at=1:17, labels=pyramids$age.group,tick = FALSE)

FunPyramidPlotNoAxes(pyramids$male.p_2050_BAU,
                     pyramids$female.p_2050_BAU,
                     unit="", gap=0,
                     lxcol="black", rxcol="black",
                     density=c(rep(5,3), rep(30,10), rep(5,3)), 
                     angle=c(rep(45,3), rep(-45,10), rep(45,3)), 
                     lwd=1, 
                     main = "2050 - Business as usual",
                     top.labels = c("Male", "Female"),
                     ppmar = c(3, 2, 1, 3),
                     labelcex = 0.8,
                     xlim = c(10,10))

axis(1, at = seq(-10, 10, 2), labels = c(seq(10,0,-2), seq(2,10,2)))

FunPyramidPlotNoAxes(pyramids$male.p_2050CEEFP,
                     pyramids$female.p_2050CEEFP,
                     unit="", gap=0,
                     lxcol="black", rxcol="black",
                     density=c(rep(5,3), rep(30,10), rep(5,3)), 
                     angle=c(rep(45,3), rep(-45,10), rep(45,3)), 
                     lwd=1, 
                     main = "2050 - Combined Econ, Educ, FP",
                     top.labels = c("Male",  "Female"),
                     ppmar = c(3, 2, 1, 3),
                     labelcex = 0.8,
                     xlim = c(10,10))
axis(1, at = seq(-10, 10, 2), labels = c(seq(10,0,-2), seq(2,10,2)))


# Plot 2 ======================================================================

# setup 
par(mar = c(4, 5,1, 5)+0.1)
# plot first line
plot(gdp_trend$year, gdp_trend$Comnibed_Econ_Educ_FP,
     axes = FALSE,
     xlab = "Year",
     ylab = "",
     type = "l",
     lwd = 2, 
     panel.first = grid(NA, NULL,lty = 6, col = "gray80"))

lines(gdp_trend$year, gdp_trend$Economic_Emphasis,
      lwd = 2,  lty = 2)
lines(gdp_trend$year, gdp_trend$Business_as_usual,
      lwd = 2, col = "gray60", lty = 3)

# axes
axis(1, at = c(2013, seq(2020, 2050,10)))
axis(2, las = 2, 
     at = seq(0, 1000, 200),
     labels = paste0("$", formatC(as.numeric(seq(0, 1000, 200)), 
                                  format="f", digits=0, big.mark=",")))

text(2054, max(gdp_trend$Business_as_usual) + 20, 
     "Business", 
     xpd = TRUE)
text(2054, max(gdp_trend$Business_as_usual) - 20, 
     "as usual ", 
     xpd = TRUE)

text(2054, max(gdp_trend$Economic_Emphasis) + 20, 
     "Economic", 
     xpd = TRUE)
text(2054, max(gdp_trend$Economic_Emphasis) - 20, 
     "emphasis", 
     xpd = TRUE)

text(2054, max(gdp_trend$Comnibed_Econ_Educ_FP) + 20, 
     "Combined", 
     xpd = TRUE)
text(2054, max(gdp_trend$Comnibed_Econ_Educ_FP) - 20, 
     "Econ, Educ, FP", 
     xpd = TRUE)

mtext("GDP per capita", side = 2, line = 3.5)

