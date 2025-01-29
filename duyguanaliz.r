turkce_dur_frekans <- c("ve","1","o","45","fazla","daha","hi??bir","en","az??ndan","??ey","39","diken","dedi??i","gibi","ama", "bu", "??ok", "bir","4","saat", 
                        "her", "da", "de", "ile","bunu","b??yle","kadar", "olan","??uan","??u","an","var","de??il","ve","mi","mi","y??zden","olsun","ancak","vs",
                        "fakat","gelen","bize","ki", "ben","sen", "biz", "siz", "onlar", "kendi", "diye", "nas??l", "??imdi","yine", "iki","ona","orda","varsa",
                        "hem","g??re","ka??","olmal??","onu" ,"b??t??n", "sadece","asl??nda", "herhangi", "??uanda","gerekti??inde","yap??lan", "gerek", "neden", "yapmak", 
                        "bunlar", "yeni", "belirli","olmaz","baz??","yapma", "herkes","g??r??nt??", "bir??ok", "s??rekli", "??unu","tabi","bi??ey","uygulama", "??al????an","ne",
                        "i??in","ya","bile","bi","tek","??ok","g??n","ayn??","bana","u??ur","e","??ey","dile","hemen","dedim","sey","peki","??o??u","hocam","kez","ay","oy","hic",
                        "art??k","oldu","tv","ra??men","biri","olarak","??nce","veya","olur","icin","hala","kim","belki","son","ba??ka","gerekiyor","asla","sanki")
pozitif_kelime_listesi <- c("mutlu", "g??zel", "harika","umut","e??it","kesinlikle","iyiki" ,"sevgi", "ba??ar??","mutlu" ,"iyi", "m??kemmel", "??ahane","te??ekk??rler",
                            "te??ekk??r","tebrik" )
negatif_kelime_listesi <- c("??zg??n", "cinayetleri","??ikayet","a??lad??m","cezalar","cinayeti","yok","korkun??","utan??yorum","su??lu","taciz","??ld??r??len","k??t??", "??irkin","nefret",
                            "??zg??n??m","su??","dayak","zarar","malesef","??iddetin","ac??","maalesef","yaz??klar","kayg??", "h??z??n", "ba??ar??s??z","yanl????",
                            "utanmak" ,"berbat","cinayet","sab??r","zor","??l??m","siddet","??iddet","??iddete","korkuyorum","??z??ld??m","??iddeti","yaz??k")
kelime_frekans_duzen_duygu <- kelime_frekans_duzenli %>% slice_max(order_by = n, n = 200)# En sik gecen 200 kelimeyi secme
kelime_frekans_duzen_duygu <- yorumlar %>% # Kelime frekans tablosu olusturma ve stop kelimeleri cikarma
  unnest_tokens(word, Comment) %>%  # Yorumlari kelimelere ayir
  count(word, sort = TRUE) %>%  # Kelime frekansi hesaplama
  filter(!word %in% turkce_dur_frekans) %>%  # Stop kelimelerini cikardik
  mutate(
    duygu = case_when(
      word %in% pozitif_kelime_listesi ~ "Pozitif",
      word %in% negatif_kelime_listesi ~ "Negatif",
      TRUE ~ "N??tr"
    )
  )

# Pozitif ve Negatif kelimelerden en cok gecen 10 kelime
pozitif_kelime_frekans <- kelime_frekans_duzen_duygu %>%
  filter(duygu == "Pozitif") %>%
  slice_max(order_by = n, n = 10)  # 10 pozitif kelime

negatif_kelime_frekans <- kelime_frekans_duzen_duygu %>%
  filter(duygu == "Negatif") %>%
  slice_max(order_by = n, n = 10)  #10 negatif kelime

# Pozitif ve Negatif kelimeleri birlestir
kelime_frekans_toplam <- bind_rows(pozitif_kelime_frekans, negatif_kelime_frekans)

# Dikey cubuk grafik olusturma
ggplot(kelime_frekans_toplam, aes(x = reorder(word, n), y = n, fill = duygu)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Cubuk genisligi ve gosterim
  coord_cartesian(ylim = c(0, max(kelime_frekans_toplam$n) * 1.1)) +  # Y eksenini artirma
  scale_fill_manual(values = c("Pozitif" = "#4CAF50", "Negatif" = "#F44336")) +  # Renkleri belirleme
  labs(title = "Pozitif ve Negatif Kelimeler (EN SIK GECEN 10)",
       x = "Kelime",
       y = "Frekans") +
  theme_minimal() +  # Minimal tema
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#333333"),  # Baslik
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "#555555"),  # X ekseni etiketleri
    axis.text.y = element_text(size = 12, color = "#555555"),  # Y ekseni etiketleri
    axis.title = element_text(size = 14, color = "#333333"),  # Eksende basliklar
    panel.grid.major = element_line(color = "#e0e0e0"),  # Ana grid cizgileri
    panel.grid.minor = element_blank()  # Kucuk grid cizgilerini kaldirma
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),  # X ekseni baslik 
        axis.title.y = element_text(margin = margin(r = 10)))  # Y ekseni baslik 


