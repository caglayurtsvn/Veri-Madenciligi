polarite_kelimeleri <- read.table (file.choose(),header=T,sep=";") #pythonda olusturdugum ing kelimeleri getirdim(cevirdigim)
sum(is.na(polarite_kelimeleri$ceviri))  # NA de??erlerinin sayisini kontrol et
polarite_kelimeleri <- polarite_kelimeleri %>% filter(!is.na(ceviri))  # NA olan satirlari cikar
# ozel karakterleri temizlemek i??in ornek
polarite_kelimeleri$ceviri <- gsub("[^[:alnum:][:space:]]", "", polarite_kelimeleri$ceviri..)
polarite <- get_sentiment(polarite_kelimeleri$ceviri, method = "syuzhet")
#syuzhet paketi ile kelimelerin duygu skorlari her yorum icin polarite skoru
polarite_kelimeleri$Polarite <- polarite#hesaplanan polarite skorlarini ekleme
# Kelime Frekansi ile Polariteyi Birlestirme
tablo_2 <- polarite_kelimeleri %>%
  unnest_tokens(word, ceviri) %>%#tidytext paketinin fonksiyonu ile "tek tek kelimelere ayirma" her yorumu
  count(word, sort = TRUE) %>% #her kelime kac kez gecti ve azalan sirada sirala
  filter(nchar(word) > 2) %>% #kelime karakterlerini 2 den fazla olanlari al
  mutate(sentiment = get_sentiment(word, method = "syuzhet"))#kelime polaritesini hesaplama ve yeni sutun olusturma

#Polarite Grafigi
ggplot(tablo_2, aes(x = n, y = sentiment, color = sentiment)) +
  geom_point(size = 2) +  # noktalarin boyutlari ayarlama
  scale_color_gradient2(low = "blue", mid = "purple", high = "orange", 
                        midpoint = 0) +  # Renk ayarlama
  geom_hline(yintercept = mean(tablo_2$sentiment, na.rm = TRUE), 
             color = "red", size = 0,65, linetype = "dashed") +  # Polarite cizgisi
  labs(title = "Duygu Analiz Sonucu",
       x = "Kelime Frekansi",
       y = "Skor",
       color = "Ortalama Polarite") +  # Renklerin degisim gostergesi
  theme_minimal()
#Matematiksel degerleri hesaplama
duygu_ozet <- polarite_kelimeleri %>%
  summarise(
    mean = mean(Polarite, na.rm = TRUE),  # Ortalama
    se.mean = sd(Polarite, na.rm = TRUE) / sqrt(n()),  # Ortalama Standart Hata
    CI.mean.0.95 = qt(0.975, df = n() - 1) * (sd(Polarite, na.rm = TRUE) / sqrt(n())),  #%95 guven araligi
    var = var(Polarite, na.rm = TRUE),  # Varyans
    std.dev = sd(Polarite, na.rm = TRUE),  # Standart Sapma
    coef.var = (sd(Polarite, na.rm = TRUE) / mean(Polarite, na.rm = TRUE)) * 100  # Varyasyon Katsayisi
  )
#Tabloyu yazdirma
print(duygu_ozet)
#Tablonun daha duzenli gorunumu icin kable paketi
kable(duygu_ozet, caption = "Duygu Analizi Matematiksel Sonucu", digits = 5)

