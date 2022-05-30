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
![image](https://user-images.githubusercontent.com/100200062/170904027-6193c2c9-6f96-4031-b64b-85c36adcf3e3.png)

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
![image](https://user-images.githubusercontent.com/100200062/170904114-28065986-de7c-4be1-b2b7-49007962c08a.png)

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
![image](https://user-images.githubusercontent.com/100200062/170904165-ae125a0f-9cfd-41a8-99e9-c72b9bbbd6e9.png)


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

![image](https://user-images.githubusercontent.com/100200062/170904290-c5088a7d-62b0-4b19-8b07-ec471f87799e.png)

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
![image](https://user-images.githubusercontent.com/100200062/170906110-d5ac8687-66be-4de4-994a-ea2aaa8641da.png)
![image](https://user-images.githubusercontent.com/100200062/170906124-1cab8804-4aac-413a-95b1-facd385383c8.png)
![image](https://user-images.githubusercontent.com/100200062/170906134-51e8275e-631d-4996-946a-7c2e7035b10e.png)
![image](https://user-images.githubusercontent.com/100200062/170906291-b8c9bbc8-82d8-4620-962a-829540ff140f.png)


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

![image](https://user-images.githubusercontent.com/100200062/170906469-97b4fc05-69a0-456e-81a4-db4df80b2cba.png)


Kucing Hitam
```R
qqnorm(Group2$Length)
qqline(Group2$Length)
```
![image](https://user-images.githubusercontent.com/100200062/170906477-2a712a4a-29c6-4f07-b877-b7982b700bdb.png)


Kucing Putih 
```R
qqnorm(Group3$Length)
qqline(Group3$Length)
```
![image](https://user-images.githubusercontent.com/100200062/170906494-a764bd83-548b-48aa-bf2e-b1e9500f899e.png)


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
![image](https://user-images.githubusercontent.com/100200062/170906667-048785b0-4f15-4411-8bf5-b4477a4acf13.png)

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
![image](https://user-images.githubusercontent.com/100200062/170906961-bbf3fad7-bb4f-4c4e-8baa-f065fdf5f0cb.png)

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
