jedynka = c(242.8, 245.4, 252.5, 254.8, 215.9, 229.9, 285.5, 268.8, 245.4, 243.3, 185.6, 265.8, 248.1, 245, 232.8, 260.1, 264.5, 247.2, 239.7, 228.4, 251.9, 221.3, 252.9, 190.8, 300.1, 264.5, 257.8, 248.6, 234.9, 260.5, 225.1)
dwojka = c(177.7, 202.1, 230.2, 220.6, 194.2, 227.6, 189.6, 204.9, 185.3, 218.7, 139.5, 170.4, 195.4, 210.6, 190.9, 208.1, 200.8, 153.2, 195.5, 172.2, 209.2, 221.9, 190.1, 211.4, 191.5, 184.9, 180.6, 222.2, 198.6, 202.6, 215.5)


ss1 = sort(jedynka)	#sortowanie wektora rosn¹co
sredniaArytmetyczna1 = mean(ss1)	#obliczanie sredniej arytmetycznej
wariancja1 = var(ss1)	#wariancja z próby
odchylenieStandardowe1 = sqrt(wariancja1)	#odchylenie standardowe z próby
mediana1 = median(ss1)	#mediana / kwartyl 2 stopnia
kwartylQ11 = quantile(ss1, 0.25)	#kwartyl 1 stopnia
kwartylQ31 = quantile(ss1, 0.75)	#kwartyl 3 stopnia
rozstep1 = max(jedynka) - min(jedynka)	#rozstep wyników w próbie
odchylenieCwiartkowe1 = (quantile(ss1, 0.75) - quantile(ss1, 0.25)) / 2  	#odchylenie æwiartkowe
wspolczynnikAsymetrii1 = (sum((ss1 - sredniaArytmetyczna1) ^ 3) / length(ss1)) / (odchylenieStandardowe1 ^ 3)	#wspolczynnik asymetrii
wspolczynnikSkosnosci1 = 3*(sredniaArytmetyczna1 - mediana1) / odchylenieStandardowe1 #wspolczynnik skosnosci
kurtoza1 = (sum((ss1 - sredniaArytmetyczna1) ^ 4) / length(ss1)) / (odchylenieStandardowe1 ^ 4)	#kurtoza

ss2 = sort(dwojka)
sredniaArytmetyczna2 = mean(ss2)
wariancja2 = var(ss2)
odchylenieStandardowe2 = sqrt(wariancja2)
mediana2 = median(ss2)
kwartylQ12 = quantile(ss2, 0.25)
kwartylQ32 = quantile(ss2, 0.75)
rozstep2 = max(jedynka) - min(jedynka)
odchylenieCwiartkowe2 = (quantile(ss2, 0.75) - quantile(ss2, 0.25)) / 2  
wspolczynnikAsymetrii2 = (sum((ss2 - sredniaArytmetyczna2) ^ 3) / length(ss2)) / (odchylenieStandardowe2 ^ 3)
wspolczynnikSkosnosci2 = 3*(sredniaArytmetyczna2 - mediana2) / odchylenieStandardowe2
kurtoza2 = (sum((ss2 - sredniaArytmetyczna2) ^ 4) / length(ss2)) / (odchylenieStandardowe2 ^ 4)

sr1 = hist(jedynka, breaks = 5, labels = TRUE, plot = TRUE)
sredniaArytmetyczna11 = sum(sr1$counts * sr1$mids) / sum(sr1$counts) 	#obliczanie sredniej z szeregu rozdzielczego
wariancjapr11 = sum(((sr1$mids-sredniaArytmetyczna11) ^ 2) * sr1$counts) / sum(sr1$counts) #wariancja z próby szerego rozdzielczego
odchylenieStandardowepr11 = sqrt(wariancjapr11)	#odchylenie standardowe z próby szerego rozdzielczego
wariancjapo11 = sum(((sr1$mids-sredniaArytmetyczna11) ^ 2) * sr1$counts) / sum(sr1$counts) - 1 	#wariancja z populacji szeregu rozdzielczego
odchylenieStandardowepo11 = sqrt(wariancjapo11) 	#odchylenie standardowe z populacji szeregu rozdzielczego
wspolczynnikAsymetrii11 = (sum((sr1$mids - sredniaArytmetyczna11) ^ 3 * sr1$counts) / sum(sr1$counts)) / (odchylenieStandardowepr11 ^ 3) #wspolczynnik asymetrii szeregu rozdzielczego
kurtoza11 = (sum((sr1$mids - sredniaArytmetyczna11) ^ 4 * sr1$counts) / sum(sr1$counts)) / (odchylenieStandardowepr11 ^ 4) #kurtoza

sr2 = hist(dwojka, breaks = 5, labels = TRUE, plot = TRUE)
sredniaArytmetyczna22 = sum(sr2$counts * sr2$mids) / sum(sr2$counts)
wariancjapr22 = sum(((sr2$mids-sredniaArytmetyczna22) ^ 2) * sr2$counts) / sum(sr2$counts)
odchylenieStandardowepr22 = sqrt(wariancjapr22)
wariancjapo22 = sum(((sr2$mids-sredniaArytmetyczna22) ^ 2) * sr2$counts) / sum(sr2$counts) - 1
odchylenieStandardowepo22 = sqrt(wariancjapo22)
wspolczynnikAsymetrii22 = (sum((sr2$mids - sredniaArytmetyczna22) ^ 3 * sr2$counts) / sum(sr2$counts)) / (odchylenieStandardowepr22 ^ 3) 
kurtoza22 = (sum((sr2$mids - sredniaArytmetyczna22) ^ 4 * sr2$counts) / sum(sr2$counts)) / (odchylenieStandardowepr22 ^ 4)

print("Œrednia arytmetyczna - jedynka") > print(sredniaArytmetyczna1) 
print("Wariancja - jedynka") > print(wariancja1)
print("Odchylenie standardowe - jedynka") > print(odchylenieStandardowe1)
print("Mediana - jedynka") > print(mediana1)
print("Kwartyl 1 stopnia - jedynka") > print(kwartylQ11)
print("Kwartyl 3 stopnia - jedynka") > print(kwartylQ31)
print("Rozstêp wyników w próbie - jedynka ") > print(rozstep1)
print("Odchylenie æwiartkowe - jedynka") > print(odchylenieCwiartkowe1)
print("Wspó³czynnik asymetrii - jedynka") > print(wspolczynnikAsymetrii1)
print("Wspó³czynnik skoœnoœci - jedynka") > print(wspolczynnikSkosnosci1)
print("Kurtoza - jedynka") > print(kurtoza1)


print("Œrednia arytmetyczna - dwójka") > print(sredniaArytmetyczna2) 
print("Wariancja - dwójka") > print(wariancja2)
print("Odchylenie standardowe - dwójka") > print(odchylenieStandardowe2)
print("Mediana - dwójka") > print(mediana2)
print("Kwartyl 1 stopnia - dwójka") > print(kwartylQ12)
print("Kwartyl 3 stopnia - dwójka") > print(kwartylQ32)
print("Rozstêp wyników w próbie - dwójka") > print(rozstep2)
print("Odchylenie æwiartkowe - dwójka") > print(odchylenieCwiartkowe2)
print("Wspó³czynnik asymetrii - dwójka") > print(wspolczynnikAsymetrii2)
print("Wspó³czynnik skoœnoœci - dwójka") > print(wspolczynnikSkosnosci2)
print("Kurtoza - dwójka") > print(kurtoza2)

print("Œrednia arytmetyczna - jedynka - szereg rozdzielczy") > print(sredniaArytmetyczna11) 
print("Wariancja - jedynka - szereg rozdzielczy") > print(wariancjapr11)
print("Odchylenie standardowe - jedynka - szereg rozdzielczy") > print(odchylenieStandardowepr11)
print("Wariancja z populacji - jedynka - szereg rozdzielczy") > print(wariancjapo11)
print("Odchylenie standardowe z populacji - jedynka - szereg rozdzielczy") > print(odchylenieStandardowepo11)
print("Wspó³czynnik asymetrii - jedynka - szereg rozdzielczy") > print(wspolczynnikAsymetrii11)
print("Kurtoza - jedynka - szereg rozdzielczy") > print(kurtoza11)

print("Œrednia arytmetyczna - dwójka - szereg rozdzielczy") > print(sredniaArytmetyczna22) 
print("Wariancja z próby - dwójka - szereg rozdzielczy") > print(wariancjapr22)
print("Odchylenie standardowe z próby - dwójka - szereg rozdzielczy") > print(odchylenieStandardowepr22)
print("Wariancja z populacji - dwójka - szereg rozdzielczy") > print(wariancjapo22)
print("Odchylenie standardowe z populacji - dwójka - szereg rozdzielczy") > print(odchylenieStandardowepo22)
print("Wspó³czynnik asymetrii - dwójka - szereg rozdzielczy") > print(wspolczynnikAsymetrii22)
print("Kurtoza - dwójka - szereg rozdzielczy") > print(kurtoza22)
