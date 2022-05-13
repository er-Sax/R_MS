dwojka = c(177.7, 202.1, 230.2, 220.6, 194.2, 227.6, 189.6, 204.9, 185.3, 218.7, 139.5, 170.4, 195.4, 210.6, 190.9, 208.1, 200.8, 153.2, 195.5, 172.2, 209.2, 221.9, 190.1, 211.4, 191.5, 184.9, 180.6, 222.2, 198.6, 202.6, 215.5)
ilosc_elementow <- length(dwojka)
hipoteza <- 20
poziom_istotnosci <- 0.05

cat("Hipoteza: odchylenie standardowe obrotow uzyskanych przez sklep „Dwojka” wynosi 20 tys. zl")
cat("\nHipoteza alternatywa: odchylenie standardowe obrotow uzyskanych przez sklep „Dwojka” jest rozne od 20 tys. zl")

statystyka_testowa <- function(ilosc_el, hipoteza_zerowa) {
  stat = (ilosc_el*var(dwojka))/(hipoteza_zerowa^2)
  return(stat)
}

test <- statystyka_testowa(ilosc_elementow, hipoteza)
cat("\nStatystyka testowa: ", test)

#obliczenie obszaru krytycznego dwustronnego
obszar_krytyczny_lewostronny <- qchisq(poziom_istotnosci/2, ilosc_elementow - 1)
obszar_krytyczny_prawostronny <- qchisq(1 - poziom_istotnosci/2, ilosc_elementow - 1)

cat("\nObszar krytyczny dwustronny: ")
cat("(0; ", obszar_krytyczny_lewostronny,"> u <", obszar_krytyczny_prawostronny, "; oo)")

if (hipoteza >= obszar_krytyczny_lewostronny && hipoteza <= obszar_krytyczny_prawostronny) {
  cat("\nNie mozna odrzucic hipotezy - odchylenie standardowe obrotow uzyskanych przez sklep „Dwojka” wynosi 20 tys. zl")
} else {
  cat("\nMozna odrzucic hipoteze - odchylenie standardowe obrotow uzyskanych przez sklep „Dwojka” wynosi 20 tys. zl")
}
