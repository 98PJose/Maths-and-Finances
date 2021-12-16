#Carteras LPP - Bechmark. Long - Only - Portfolio

library(quadprog)
library(tidyverse)
library(quadprog)

#Pesos de la frontera eficiente
portfolioWeights <- function(assetReturns, targetReturn)
{
  nAssets = ncol(assetReturns)
  portfolio = solve.QP(
    Dmat = cov(assetReturns),
    dvec = rep(0, times=nAssets),
    Amat = t(rbind(Return=colMeans(assetReturns),
                   Budget=rep(1, nAssets), LongOnly=diag(nAssets))),
    bvec = c(Return=targetReturn, budget=1,
             LongOnly=rep(0, times=nAssets)),
    meq=2)
  weights = portfolio$solution
  weights
}

#Frontera eficiente
portfolioFrontier <- function(assetReturns, nPoints=20)
{
  # Number of Assets:
  nAssets = ncol(assetReturns)
  # Target Returns:
  mu = colMeans(assetReturns)
  targetReturns <- seq(min(mu), max(mu), length=nPoints)
  # Optimized Weights:
  weights = rep(0, nAssets)
  weights[which.min(mu)] = 1
  for (i in 2:(nPoints-1)) {
    newWeights = portfolioWeights(assetReturns, targetReturns[i])
    weights = rbind(weights, newWeights)
  }
  newWeights = rep(0, nAssets)
  newWeights[which.max(mu)] = 1
  weights = rbind(weights, newWeights)
  weights = round(weights, 4)
  colnames(weights) = colnames(assetReturns)
  rownames(weights) = 1:nPoints
  # Return Value:
  weights
}

#Pesos de la cartera
portfolioWeights(assetReturns, 0.02)

#Datos de los activos
library(fBasics)
assetReturns <- 100 * LPP2005REC[, 1:6]
names(assetReturns)

#Pesos de carteras
weights = portfolioFrontier(assetReturns, nPoints = 20)
colnames(weights)<-names(assetReturns)
print(weights)

#Rendimientos y riesgos
mu = colMeans(assetReturns)
targetReturns = seq(min(mu), max(mu), length = nrow(weights))
targetRisks = NULL
for (i in 1:nrow(weights)) {
  newTargetRisk = sqrt(weights[i, ] %*% cov(assetReturns) %*%
                         weights[i, ])
  targetRisks = c(targetRisks, newTargetRisk)
}

plot(targetRisks, targetReturns, pch = 19,"b",col="blue")
title(main = "LPP Bechmark Portfolio")
