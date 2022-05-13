# Deklaracja danych wejsciowych 
jedynka = c(242.8, 245.4, 252.5, 254.8, 215.9, 229.9, 285.5, 268.8, 245.4, 243.3, 185.6, 265.8, 248.1, 245, 232.8, 260.1, 264.5, 247.2, 239.7, 228.4, 251.9, 221.3, 252.9, 190.8, 300.1, 264.5, 257.8, 248.6, 234.9, 260.5, 225.1)
nElementow = length(jedynka)
pIstotnosci = 0.05 
wHipot = 240

# Funkcja testowa na podstawie https://pl.wikipedia.org/wiki/Test_istotno%C5%9Bci_dla_warto%C5%9Bci_%C5%9Bredniej_populacji
funkcjaTestowa <- function(nE, wH){
  temp = (((mean(jedynka) - wH) / sd(jedynka))*(sqrt(nE)))
  return(temp) 
}
testStatystyczny <- funkcjaTestowa(nElementow, wHipot)

wKrytyczna_1 <- qt(1 - (pIstotnosci / 2), (nElementow - 1))
wKrytyczna_0 <- -(wKrytyczna_1)

# Wypisanie Hipotezy i Hipotezy alternatywnej 
cat("==== Hipoteza: Średnie obroty uzyskane przez sklep „Jedynka” na poziomie istotności α=0.05 wynoszą 240 tys. zł ====\n")
cat("==== Hipoteza alternatywa: Średnie obroty uzyskane przez sklep „Jedynka” na poziomie istotnym α=0.05 są różne od 240 tys. zł. ====\n")
# Wypisanie obliczonej wartosci statystyki
cat("==== Wartość statystyki:", testStatystyczny," ====\n")
# Wypisanie przedziłów krytycznych 
cat("==== Przedziały krytyczne: ( -oo, ", wKrytyczna_0, ") u ( ", wKrytyczna_1,", oo) ====\n")

cat("==== Wynik testu statystycznego: ====\n")
# Sprawdzenie prawdziwości postawionej hipotezy 
if(wKrytyczna_0 < testStatystyczny && testStatystyczny < wKrytyczna_1){
  cat("==== Hipoteza jest prawdziwa, średnie obroty uzyskane przez sklep „Jedynka” na poziomie istotności α=0.05 wynoszą 240 tys. zł ====\n")
} else {
  cat("==== Hipoteza alternatywna jest prawdziwa, średnie obroty uzyskane przez sklep „Jedynka” na poziomie istotnym α=0.05 są różne od 240 tys. zł. ====\n")
}