
rm(list=ls())
graphics.off()

source("MovAverage1.R")

source("MovAverage2.R")

MData = read.csv("iphonesales.csv")

mvAv = MovAverage1(MData$Sales)

mAv = MovAverage2(mvAv)

YmAv = numeric(16)

for (p in 1:16) {
  if (mAv[p] != 0) {
    YmAv[p] = round((MData$Sales[p] / mAv[p]), digits = 2)
  }
}

MaVg <- matrix(0, nrow = 1, ncol = 4)

MaVg[1] = round(((YmAv[5] + YmAv[9] + YmAv[13])/3), digits = 2)
MaVg[2] = round(((YmAv[6] + YmAv[10] + YmAv[14])/3), digits = 2)
MaVg[3] = round(((YmAv[7] + YmAv[11] + YmAv[3])/3), digits = 2)
MaVg[4] = round(((YmAv[8] + YmAv[12] + YmAv[4])/3), digits = 2)

q = 1
uV = numeric(20)

for (p in 1:20) {
  uV[p] = MaVg[q]
  q = q + 1
  if (q > 4) {
    q = 1
  }
}

nM = numeric(16)

for (p in 1:16) {
  nM[p] = round((MData$Sales[p] / uV[p]), digits = 2)
}

LmD <- lm(nM ~ c(1:16))

EinPt = round((as.numeric(LmD$coefficients[1])), digits = 2)
sTn = round((as.numeric(LmD$coefficients[2])), digits = 2)

ST = numeric(20)
STA = numeric(20)

for (p in 1:20) {
  ST[p] = round(((sTn * p) + EinPt),  digits = 2)
  STA[p] = round((ST[p] * uV[p]),  digits = 2)
}


xMin = 1
xMax = 20
yMin = min(MData$Sales, STA)
yMax = max(MData$Sales, STA)

Axis_Y = c(1:16)

plot(
  Axis_Y,
  MData$Sales,
  pch = 8,
  col = "orange",
  xaxt = 'n',
  yaxt = 'n',
  ann=FALSE,
  xlim = c(xMin, xMax),
  ylim = c(yMin, yMax)
)
lines(Axis_Y, MData$Sales, col = "orange")

# Sales data trend

Axis_Y = c(3:14)
LmAv = mAv[3:14]

par(new = TRUE) 

plot(
  Axis_Y,
  LmAv,
  pch = 16,
  col = "yellow",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE,
  xlim = c(xMin, xMax),
  ylim = c(yMin, yMax)
)
lines(Axis_Y, LmAv, col = "yellow")



Axis_Y = c(1:20)

par(new = TRUE) 

plot(
  Axis_Y,
  STA,
  pch = 16,
  col = "blue",
  xlab = "Quartely_Y",
  ylab = "Sales",
  xlim = c(xMin, xMax),
  ylim = c(yMin, yMax)
)
lines(Axis_Y, STA, col = "blue")

legend("topleft", legend = c("O", "T", "F"),
       col = c("orange", "yellow", "blue"), lty = 1, cex = 0.8)