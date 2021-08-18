library(HMM)
###################################
## P.naik = Pengangguran naik    ##
## P.turun = Pengangguran turun  ##
## K.naik = Kemiskinan naik      ##
## K.turun = Kemiskinan turun    ##
###################################
hmm <- initHMM(c("P.naik", "P.turun"),c("K.naik", "K.turun"), transProbs = matrix(c(.38,.62,.62,.38),ncol = 2, byrow = T), emissionProbs = matrix(c(.22,.78,.38,.62),ncol = 2, byrow = T))
print(hmm)
# SEQUENCE OF OBSERVATION #
obsrv <- c("K.naik","K.turun","K.naik","K.turun","K.turun")
# CALCULATE FORWARD PROBABILITIES #
logForwardProbabilities <- forward(hmm, obsrv)
a.exp <- exp(logForwardProbabilities)
a.exp
# CALCULATE BACKWARD PROBABILITIES #
logBackwardProbabilities <- backward(hmm, obsrv)
b.exp <- exp(logBackwardProbabilities)
b.exp
# VITERBI #
hasil.viterbi <- viterbi(hmm, obsrv)
hasil.viterbi
# PROBABILITY OF 0 GIVEN THE MODEL #
pmarginal <- sum(a.exp[,5]) # angka 5 tentatif dilihat dari banyaknya observe sequence #
pmarginal
# BAUM-WELCH ITERASI PERTAMA #
bw1 <- baumWelch(hmm, obsrv, 1) # ITERASI 1 #
print(bw1$hmm)
############################################
bw2 <- baumWelch(hmm, obsrv, 5) # ITERASI 5 #
print(bw2$hmm)
bw2$difference
############################################
bw3 <- baumWelch(hmm, obsrv, 10) # ITERASI 10 #
print(bw3$hmm)
bw3$difference
############################################
bw4 <- baumWelch(hmm, obsrv, 11) # ITERASI 11 #
print(bw4$hmm)
bw4$difference