
yorumlar <- read.table (file.choose(),header=T,sep=";") #Python'da olusturdugum vericekme.csv dosyasini getirmek

library(dplyr)#veri manipulasyonu icin
library(ggraph)#grafik gorsellestirme
library(syuzhet)#duygu analizi yapmak 
library(tidyverse)#veri bilimi icin daha genis bir kutuphane
library(tidyr)#verileri temizlemek
library(stringr)#karakter ve metin manipulasyonu
library(textclean)#verileri temizlemek
library(topicmodels)#LDA modelleme yapmak
library(tidytext)#metin verilerini analiz etmek
library(tm)#metin madenciligi icin kullanilan
library(igraph)#ag analizi ve gorsellestirme
library(knitr)#dinamik rapor olusturup kod blogu ile birlestirme
library(ggplot2)#veri gorsellestirme
library(wordcloud2)#kelime bulutu olusturma
library(ggwordcloud)#ggplot2 uzerinden daha detayli gorsellestirme
library(visNetwork) #Interaktif ag grafikleri olusturmak icin kullanilan paket. Web tabanli olarak etkilesimli

#Verileri Temizleme
yorumlar$Comment <- as.character(yorumlar$Comment)#yorumlar karakter formatina cevrilir
yorumlar$Comment <- str_remove_all(yorumlar$Comment, "<[^>]+>")# HTML temizleme
yorumlar$Comment <- str_remove_all(yorumlar$Comment, "http\\S+|www\\S+")#url temizleme silme
yorumlar$Comment <- replace_emoji(yorumlar$Comment, replacement = "")# Emojileri kaldirma
yorumlar$Comment <- str_remove_all(yorumlar$Comment, "[^[:alpha:][:space:]]")# Ozel karakter temizligi
yorumlar$Comment <- str_squish(yorumlar$Comment)  # Fazla bosluklar?? temizler
yorumlar$Comment <- tolower(yorumlar$Comment)    # Tum harfleri kucuge cevirir
yorumlar <- yorumlar %>% filter(Comment != "" & !is.na(Comment))  # Bos satirlari sil
write.csv(yorumlar, "temizlenmis_yorumlar.csv", row.names = FALSE)

# Sadece dolu olan satirlari secme
yorumlar <- yorumlar %>% filter(Comment != "" & !is.na(Comment) & str_squish(Comment) != "")
head(yorumlar)# ilk birkac satira goz atma
dim(yorumlar)# Satir ve sutun sayisini gorme

Sys.setlocale("LC_ALL", "Turkish")
turkce_durkelime <- c(
  "yani","bize","fazla","quot","umar??m","kendi","benim","eden","acaba","hangi","fakat","onun","di??er","b??y??k","ya","da","ki","olmu??",
  "??ok","bir","de","yada","hale","seni","iyi","hep","olarak","??nce","varsa","ka??","ele","yere","varsa","yeni","ba??ka","olacak","??nemli",
  "dolu","i??te", "nasil","??yle","tane","ilgili","olup","diyor", "sadece","hepsi","s??rece", "once","te??ekk??rler","l??tfen","gereken",
  "herkes","ben","bana", "sonra","e??er", "art??k","bence","ki??i","ilk","sana", "bu", "??u", "o", "ve","en","nas??l","oldu??u","zorunda",
  "veya", "ama","icin","bizi","tek","yine","cok","sizi","hic","hi??","peki", "????nk??", "ki", "de", "da","herkesin","yerine","ediyorum",
  "ile", "mi", "ne","bile","t??m","belki","??uan","evet","olan","iki","son","ad??m","size","hala","olsun","zaten","ger??ekten","beni",
  "dedi", "hay??r", "ben", "sen", "o", "biz", "siz", "onlar", "bunu", "b??yle", "??ok", "az","buna", "biri","durak","bizim","do??ru","herkese",
  "daha", "gibi", "ise", "hem", "her", "hi??","de","da","iyi","ne","hic","??ey","var","gibi","ne","bu","bir","i??inde","kimse","versin",
  "g??n","ben","i??in","gibi","var","??eyler","en", "??imdi", "var", "yok", "nas??l","neden","de??il","diye","etti","gibi","??nce","olmak",
  "hi??","ama","var","ne","o","ya","ay","hi??bir","an","??u","neye", "kadar","asla","ke??ke","g??re","nerede", "kim","olsa","devam","burada",
  "b??t??n", "birka??", "baz??","i??in","??ey","sizin","laz??m","ayn??","??eyi","bo??","onu","ge??en","ancak","olur","olmas??","oldu","gereken",
  "bende","demek","kesinlikle","taraf??ndan","tekrar","olmal??","hatta","diyen","bu","kadar","kar????nda","olur","bunun","kar????","??ekilde")


#yorumlari kelimelere bolme ve frekans islemi yapma(bir kelimeden kac tane var gibi)
kelime_frekans_duzenli <- yorumlar %>%
  unnest_tokens(word, Comment) %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% turkce_durkelime) %>%   # Stopwords'leri cikar
  filter(nchar(word) > 2)                  # Kisa kelimeleri filtrele

kelime_frekans_duzenli <- kelime_frekans_duzenli %>% slice_max(order_by = n, n = 50)# En sik gecen 50 kelimeyi secme
#burasi gorsellestirme kelimelerin tekrarina gore boyut ayarlama
wordcloud2(
  kelime_frekans_duzenli, 
  size = 1.5, 
  color = c("darkred", "black", "gray"),  # Renkleri ayarlama 
  backgroundColor = "lightpink"  # Arka plan rengi
)


##En cok tekrar eden 20 kelimeyi secme
top20_kelime <- kelime_frekans_duzenli %>%
  top_n(20, n)

# Tekrar eden o 20 kelimeyi gorsellestirme
ggplot(top20_kelime, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  labs(
    title = "En cok Tekrar Eden 20 Kelime",
    x = "Kelime",
    y = "Frekans"
  ) +
  theme_minimal()

##Kelime A??i Grafi??i
turkce_stop_words <- c("ve","1","o","45","fazla","daha","hi??bir","en","az??ndan","??ey","39","diken" ,
                       "dedi??i","gibi","ama", "bu", "??ok", "bir","4","saat", "her", "da", "de", "ile", 
                       "kadar", "olan","??uan","??u","an","var","de??il","mu","mi","y??zden")
bigramlar <- yorumlar %>%
  unnest_tokens(bigram, Comment, token = "ngrams", n = 2) %>% #yorum metinleri analiz edilmesi ve 2 kelimelik gruplar cikar
  separate(bigram, into = c("kaynak", "hedef"), sep = " ") %>%#iki sutuna ayrilma olur kaynak ve hedef olarak
  filter(!kaynak %in% turkce_stop_words, !hedef %in% turkce_stop_words) %>%  #gereksiz kelime temizligi
  filter(!is.na(kaynak), !is.na(hedef)) %>%#gecersiz bigraflar cikar
  count(kaynak, hedef, sort = TRUE)

bigramlar <- bigramlar %>% filter(n > 3) #3 veya daha fazla tekrar eden
bigramlar <- bigramlar %>% slice(1:50)#sadece en cok tekrar eden 50 bigrafi secer
bigram_graf <- graph_from_data_frame(bigramlar)# ve Bigram agini olusturulur
head(bigramlar)# ilk birkac satira g??z atma
#Sade gorsellestirme
ggraph(bigram_graf, layout = "fr") + # Gorsellestirme kismi
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, edge_width = 0.5) +
  geom_node_point(color = "pink", size = 5) + # Dugumleri tek renk yapiyoruz
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Kelime Ag?? Grafigi", color = "Dugum Kategorisi")

#Ozellestirilmis Gorsellestirme
# Dugum ve kenar veri cerceveleri
nodes <- data.frame(
  id = V(bigram_graf)$name,#benzersiz kimlik(her dugumun)
  label = V(bigram_graf)$name,#dugum ustundeki isim
  value = degree(bigram_graf),  # dugumun buyuklugu belirleme
  group = ifelse(degree(bigram_graf) > 5, "High", "Low")  # 5den fazla olanlar high, 5 veya daha az low
)

edges <- data.frame(
  from = bigramlar$kaynak,
  to = bigramlar$hedef,
  value = bigramlar$n  #kenar kalinligi tekrar sayisina gore
)
# Interaktif ag gorsellestirme
visNetwork(nodes, edges, width = "100%", height = "900px") %>%
  visEdges(arrows = "to", smooth = TRUE, color = list(color = "gray", highlight = "blue")) %>%  # Kenar renkleri
  visNodes(color = list(
    border = "yellow",  # D??????m s??n??r rengi
    highlight = "blue"  # Se??ildi??inde d??????m rengi
  )) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = TRUE) %>%
  visLayout(randomSeed = 42)









