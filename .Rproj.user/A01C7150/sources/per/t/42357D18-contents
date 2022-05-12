jedynka = c(242.8, 245.4, 252.5, 254.8, 215.9, 229.9, 285.5, 268.8, 245.4, 243.3, 185.6, 265.8, 248.1, 245, 232.8, 260.1, 264.5, 247.2, 239.7, 228.4, 251.9, 221.3, 252.9, 190.8, 300.1, 264.5, 257.8, 248.6, 234.9, 260.5, 225.1)
dwojka = c(177.7, 202.1, 230.2, 220.6, 194.2, 227.6, 189.6, 204.9, 185.3, 218.7, 139.5, 170.4, 195.4, 210.6, 190.9, 208.1, 200.8, 153.2, 195.5, 172.2, 209.2, 221.9, 190.1, 211.4, 191.5, 184.9, 180.6, 222.2, 198.6, 202.6, 215.5)

zad2test <- function(dane)  {
  
  dane = sort(dane) # sortowanie danych
  n = length(dane) # obliczanie, ile element?w
  
  # przyjmowanie odpowiedniego k, w zaleznosci od liczby elementow, brane z tablicy
  k = 0.1591
  
  p = pnorm((dane - mean(dane))/sd(dane))
  # pnorm - rozklad normalny
  # mean - srednia arytmetyczna
  # sd - odchylenie standardowe 
  
  # seq - Sequence Generation
  
  Dplus = max(seq(1:n)/n - p)
  Dminus = max(p - (seq(1:n) - 1)/n)
  d = max(Dplus, Dminus)
  
  cat("\nk: ", k)
  cat("\nd: ", d)
  
  if (d < k) { cat("\nObroty w sklepie mają rozkład normalny.") } 
  else { cat("\nObroty w sklepie nie mają rozkładu normalnego.") }
  
}
cat("Jedynka:")
cat(zad2test(jedynka))

cat("\nDwojka: ")
cat(zad2test(dwojka))
