DifMean<- function(T, Y, n) {
  # T treatment vector
  # Y outcome vector
  # n number of outcome for either treatment or outcome vectors (number of 1s)
  difMean =  (sum(T*Y)-sum((1-T)*Y))/n
  difMean
}
