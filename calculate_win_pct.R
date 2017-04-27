# Calculate probability of winning a game given probability of winning a point
CalculateGameWinPct <- function(pointWinPct){
  stopifnot(pointWinPct > 0, pointWinPct < 1)
  pointLosePct <- 1 - pointWinPct
  winPct <- pointWinPct^4 + 4*pointWinPct^4*pointLosePct + 
            10*pointWinPct^4*pointLosePct^2 +
            20*pointWinPct^3*pointLosePct^3*pointWinPct^2/(1-2*pointWinPct*pointLosePct)
  return(winPct)
}

# Calculate probability of winning a tiebreak given probability of winning a service point and return point
CalculateTiebreakWinPct <- function(serveWinPct, returnWinPct){
  stopifnot(serveWinPct > 0, serveWinPct < 1, returnWinPct > 0, returnWinPct < 1)
  serveLosePct <- 1 - serveWinPct
  returnLosePct <- 1 - returnWinPct
  rn <- rep(NA,12)
  for (i in 1:length(rn)){
    rn[i] <- 6*(serveWinPct/serveLosePct)^i + 6*(returnWinPct/returnLosePct)^i
  }
  pointsWinPct <- rep(NA,13)
  pointsWinPct[1] <- serveLosePct^6*returnLosePct^6
  for (i in 2:length(pointsWinPct)){
    intermAns <- rep(NA, i-1)
    for (x in 1:length(intermAns)){
      intermAns[x] <- (-1)^(x-1)*rn[x]*pointsWinPct[i-x]
    }
    pointsWinPct[i] <- sum(intermAns) / (i-1)
  }
  winTwoPoints <- serveWinPct*returnWinPct/(1-serveWinPct*returnLosePct-serveLosePct*returnWinPct)
  winPct <- sum(pointsWinPct[8:13]) + pointsWinPct[7]*winTwoPoints
  return(winPct)
}

# Calculate probability of winning a set given probability of winning a service point and return point
CalculateSetWinPct <- function(serveWinPct, returnWinPct){
  stopifnot(serveWinPct > 0, serveWinPct < 1, returnWinPct > 0, returnWinPct < 1)
  serveGameWinPct <- CalculateGameWinPct(serveWinPct)
  returnGameWinPct <- CalculateGameWinPct(returnWinPct)
  serveGameLosePct <- 1 - serveGameWinPct
  returnGameLosePct <- 1 - returnGameWinPct
  rn <- rep(NA,10)
  for (i in 1:length(rn)){
    rn[i] <- 5*(serveGameWinPct/serveGameLosePct)^i + 5*(returnGameWinPct/returnGameLosePct)^i
  }
  gamesWinPct <- rep(NA,11)
  gamesWinPct[1] <- serveGameLosePct^5*returnGameLosePct^5
  for (i in 2:length(gamesWinPct)){
    intermAns <- rep(NA, i-1)
    for (x in 1:length(intermAns)){
      intermAns[x] <- (-1)^(x-1)*rn[x]*gamesWinPct[i-x]
    }
    gamesWinPct[i] <- sum(intermAns) / (i-1)
  }
  winTwoGames <- serveGameWinPct*returnGameWinPct
  splitTwoGames <- serveGameWinPct*returnGameLosePct + serveGameLosePct*returnGameWinPct
  tiebreakWinPct <- CalculateTiebreakWinPct(serveWinPct, returnWinPct)
  winPct <- sum(gamesWinPct[7:11]) + gamesWinPct[6]*winTwoGames +
            gamesWinPct[6]*splitTwoGames*tiebreakWinPct
  return(winPct)
}

# Calculate probability of winning a match given probability of winning a service point and return point
CalculateMatchWinPct <- function(serveWinPct, returnWinPct, sets = 3){
  stopifnot(sets == 3 || set == 5, 
            serveWinPct > 0, serveWinPct < 1, returnWinPct > 0, returnWinPct < 1)
  setWinPct <- CalculateSetWinPct(serveWinPct, returnWinPct)
  setLosePct <- 1 - setWinPct
  if(sets == 3){
    winPct <- setWinPct^2 + 2*setWinPct^2*setLosePct
  }
  else if(sets == 5){
    winPct <- setWinPct^3 + 3*setWinPct^3*setLosePct +
              6*setWinPct^3*setLosePct^2
  }
  return(winPct)
}