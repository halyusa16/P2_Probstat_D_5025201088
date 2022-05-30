# P2_Probstat_D_5025201088
Laporan dan Repository pengerjaan Praktikum Modul 1 Probstat 2022 oleh Halyusa Ard Wahyudi (5025201088)

# Soal 1
**Deskripsi:**
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap
kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel
sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat
kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut
diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali
kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden
mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴

### 1a 
Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel
diatas

```R
#X 
X<-c(78, 75, 67, 77, 70, 72, 28, 74, 77)
#Y
Y<-c(100, 95, 70, 90, 90, 90, 89, 90, 100)

#1a
difff <- c(22, 20, 3, 13, 20, 18, 11, 16, 23)
sd(difff)
```

Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907663-736b50c8-d0a1-49b8-a761-78080dd2f129.png)


### 1b
carilah nilai t (p-value)

```R
#1b
meanX <- mean(X)
meanY <- mean(Y)

sdX <- sd(X)
sdY <- sd(Y)

variansX <- sdX ^ 2
variansY <- sdY ^ 2

abs(meanX - meanY) / sqrt((variansX/9) + (variansY/9))
```

Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907686-b51de663-bff8-458f-bc36-130733405c67.png)


### 1c
tentukanlah apakah terdapat pengaruh yang signifikan secara statistika
dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan
aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada
pengaruh yang signifikan secara statistika dalam hal kadar saturasi
oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”

```R
#1c
t.test(X, Y)
```

Hasil

![image](https://user-images.githubusercontent.com/100200062/170907708-6d7faee0-6f20-4cbb-ad18-8d977dc2158a.png)


# Soal 2
**Deskripsi:**
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun.
Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk
mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata
23.500 kilometer dan standar deviasi 3900 kilometer.

### 2a
Apakah Anda setuju dengan klaim tersebut?
```R
Setuju
```

### 2b

```R
install.packages("BSDA")
library(BSDA)

tsum.test(mean.x=23500, sd(3900), n.x=100)
```

![image](https://user-images.githubusercontent.com/100200062/170907730-204a8216-e4ed-42fa-b604-8d4bc9bb806f.png)


Dapat disimpulkan bahwa mobil dikemudikan rata-rata lebih dari 20.000 km per-tahun. 

# Soal 3
**Deskripsi:**
Diketahui perusahaan memiliki seorang data analyst ingin memecahkan
permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya
didapatkanlah data berikut dari perusahaan saham tersebut.

Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil
diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada
rata-ratanya (α= 0.05)?

```R
bandung <- list("saham" = 19, "mean" = 3.64, "sd" = 1.67)
bali <- list("saham" = 27, "mean" = 2.79, "sd" = 1.32)

tsum.test(
  n.x = bandung$saham, n.y = bali$saham,
  mean.x = bandung$mean, mean.y = bali$mean,
  s.x = bandung$sd, s.y = bali$sd,
  var.equal = TRUE,
  alternative = "two.sided",
)

install.packages("mosaic")
library(mosaic)

plotDist(dist='t', df=2, col="blue")


qchisq(p = 0.05, df = 2, lower.tail=FALSE)
```
Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907754-045d7d5a-c838-4d2d-847c-22610483b26d.png)
![image](https://user-images.githubusercontent.com/100200062/170907763-6e9a8a20-8d14-4ad3-86c4-e725928d4724.png)
![image](https://user-images.githubusercontent.com/100200062/170907777-061ea965-eb51-400f-ba94-36c62adab94a.png)


Keputusan dan kesimpulannya adalah tidak ada perbedaan antara Bali dan Bandung.

# Soal 4
**Deskripsi:**
Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya
ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
kucing putih dengan panjangnya masing-masing.

### 4a
Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup
2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
lihat apakah ada outlier utama dalam homogenitas varians.

Kucing Orange
```R
#4a
dataoneway <- read.table("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt",h=T)
attach(dataoneway)
names(dataoneway)

dataoneway$Group <- as.factor(dataoneway$Group)
dataoneway$Group = factor(dataoneway$Group,labels = c("Orange", "Hitam", "Putih"))

class(dataoneway$Group)

Group1 <- subset(dataoneway, Group == "Orange")
Group2 <- subset(dataoneway, Group == "Hitam")
Group3 <- subset(dataoneway, Group == "Putih")

qqnorm(Group1$Length)
qqline(Group1$Length)
```

![image](https://user-images.githubusercontent.com/100200062/170907821-3f083a0e-8597-4688-bdfe-f6386995f33d.png)



Kucing Hitam
```R
qqnorm(Group2$Length)
qqline(Group2$Length)
```

![image](https://user-images.githubusercontent.com/100200062/170907841-2b5f1fd3-7ee8-402f-a67b-55931e5972db.png)


Kucing Putih 
```R
qqnorm(Group3$Length)
qqline(Group3$Length)
```

![image](https://user-images.githubusercontent.com/100200062/170907856-221161b0-c377-430d-9db0-91aa3e9bbc48.png)


### 4b
carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?
```R
#4b
bartlett.test(Length ~ Group, data = dataoneway)
```
Hasil
![image](https://user-images.githubusercontent.com/100200062/170906567-bf9887bc-60df-4c97-8e2c-1b68ac1075c9.png)

### 4c
Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
Grup dan beri nama model tersebut model 1.
```R
#4c
model1 = lm(Length ~ Group, data = dataoneway)
anova(model1)
```
![image](https://user-images.githubusercontent.com/100200062/170906758-2f116f18-c73f-4339-b454-6613b4fbe703.png)

### 4d
Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan
dari H0?
```R
H0 Diterima
```

### 4f

Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907890-acbd1b49-6275-431c-981c-5832d1ed66fe.png)

# Soal 5
**Deskripsi:**
Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk
mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca
pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan
dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil
Eksperimen. Dengan data tersebut:

### 5a
Buatlah plot sederhana untuk visualisasi data
```R
#5a
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

my.data <- read_csv("GTL.csv")

qplot(x = Temp, y = Light, geom = "point", data = my.data) + facet_grid(.~Glass, labeller = label_both)
```
Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907921-3e5c1808-2765-4ec9-9e31-78bbe5ea0337.png)

### 5b
Lakukan uji ANOVA dua arah
```R
#5b
my.data$Glass <- as.factor(my.data$Glass)
my.data$Temp_Factor <- as.factor(my.data$Temp)

anova <- aov(Light ~ Glass*Temp_Factor, data = my.data)
summary(anova)
```

Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907065-d9adb86d-2bea-4bf4-a738-c39186c0bc1b.png)

### 5c
Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk
setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
```R
data.sum <- group_by(my.data, Glass, Temp) %>%
  summarise(mean = mean(Light), sd = sd(Light)) %>%
  arrange(desc(mean))

print(data.sum)
```
Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907144-52409fc6-33cb-473f-9360-86d42b3dcc59.png)

### 5d 
Lakukan uji Tukey
```R
TukeyHSD(anova)
```

Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907198-8045d41f-e3ca-4a8f-b611-da086032ae15.png)
![image](https://user-images.githubusercontent.com/100200062/170908016-42bf107d-8301-46bc-8eec-a3ba68ef2278.png)


### 5e
Gunakan compact letter display untuk menunjukkan perbedaan signifikan
antara uji Anova dan uji Tukey
```R
#5e
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data.sum$Tukey <- cld$Letters
print(data.sum)
```

Hasil 

![image](https://user-images.githubusercontent.com/100200062/170907345-ae18331c-0f94-4301-bcd1-80cd48a607a4.png)
