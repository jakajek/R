library(readxl)
library(dplyr)

# import data
data_piloting<-read_xlsx("D:/Arna Ramadhan/Project/BAN PAUD/PILOTING_2020-SAMPLING.xlsx",
                         sheet="AKRE 18-19",col_names=TRUE)

str(data_piloting)

colnames(data_piloting)<-c("NO","NPSN","NAMALEMBAGA","SATUAN","PROGRAM","PROVINSI",
                           "KOTA","KECAMATAN","KELURAHAN","ALAMAT","AKREDITASI","TAHUN")

data_piloting<-data_piloting %>% mutate(PROVINSI=toupper(PROVINSI))

head(data_piloting)


set.seed(1)

#-------------------
#------PAUD---------
#-------------------

# sampling PAUD akreditasi A
data_paud_A<-data_piloting %>% filter(SATUAN=="PAUD", AKREDITASI=="A") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_paud_A<-sample(nrow(data_paud_A),229,replace=FALSE)

sampel_paud_A<-data_paud_A[indeks_paud_A,]

# sampling PAUD akreditasi B
data_paud_B<-data_piloting %>% filter(SATUAN=="PAUD", AKREDITASI=="B") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_paud_B<-sample(nrow(data_paud_B),1699,replace=FALSE)

sampel_paud_B<-data_paud_B[indeks_paud_B,]

# sampling PAUD akreditasi C
data_paud_C<-data_piloting %>% filter(SATUAN=="PAUD", AKREDITASI=="C") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_paud_C<-sample(nrow(data_paud_C),960,replace=FALSE)

sampel_paud_C<-data_paud_C[indeks_paud_C,]

# sampling PAUD akreditasi TT
data_paud_TT<-data_piloting %>% filter(SATUAN=="PAUD", AKREDITASI=="TT") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_paud_TT<-sample(nrow(data_paud_TT),7,replace=FALSE)

sampel_paud_TT<-data_paud_TT[indeks_paud_TT,]

# gabung data sampel PAUD
sampel_paud_total<-rbind(sampel_paud_A,sampel_paud_B,sampel_paud_C,sampel_paud_TT)
write.csv(sampel_paud_total,"D:/Arna Ramadhan/Project/BAN PAUD/Sampling PAUD.csv")


#-------------------
#------LKP----------
#-------------------

# sampling LKP akreditasi A
data_lkp_A<-data_piloting %>% filter(SATUAN=="LKP", AKREDITASI=="A") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_lkp_A<-sample(nrow(data_lkp_A),103,replace=FALSE)

sampel_lkp_A<-data_lkp_A[indeks_lkp_A,]

# sampling LKP akreditasi B
data_lkp_B<-data_piloting %>% filter(SATUAN=="LKP", AKREDITASI=="B") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_lkp_B<-sample(nrow(data_lkp_B),460,replace=FALSE)

sampel_lkp_B<-data_lkp_B[indeks_lkp_B,]

# sampling LKP akreditasi C
data_lkp_C<-data_piloting %>% filter(SATUAN=="LKP", AKREDITASI=="C") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_lkp_C<-sample(nrow(data_lkp_C),426,replace=FALSE)

sampel_lkp_C<-data_lkp_C[indeks_lkp_C,]

# sampling LKP akreditasi TT
data_lkp_TT<-data_piloting %>% filter(SATUAN=="LKP", AKREDITASI=="TT") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_lkp_TT<-sample(nrow(data_lkp_TT),76,replace=FALSE)

sampel_lkp_TT<-data_lkp_TT[indeks_lkp_TT,]

# gabung data sampel PAUD
sampel_lkp_total<-rbind(sampel_lkp_A,sampel_lkp_B,sampel_lkp_C,sampel_lkp_TT)
write.csv(sampel_lkp_total,"D:/Arna Ramadhan/Project/BAN PAUD/Sampling LKP.csv")


#-------------------
#------PKBM---------
#-------------------

# sampling PKBM akreditasi A
data_pkbm_A<-data_piloting %>% filter(SATUAN=="PKBM", AKREDITASI=="A") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_pkbm_A<-sample(nrow(data_pkbm_A),120,replace=FALSE)

sampel_pkbm_A<-data_pkbm_A[indeks_pkbm_A,]

# sampling PKBM akreditasi B
data_pkbm_B<-data_piloting %>% filter(SATUAN=="PKBM", AKREDITASI=="B") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_pkbm_B<-sample(nrow(data_pkbm_B),538,replace=FALSE)

sampel_pkbm_B<-data_pkbm_B[indeks_pkbm_B,]

# sampling PKBM akreditasi C
data_pkbm_C<-data_piloting %>% filter(SATUAN=="PKBM", AKREDITASI=="C") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_pkbm_C<-sample(nrow(data_pkbm_C),338,replace=FALSE)

sampel_pkbm_C<-data_pkbm_C[indeks_pkbm_C,]

# sampling PKBM akreditasi TT
data_pkbm_TT<-data_piloting %>% filter(SATUAN=="PKBM", AKREDITASI=="TT") %>%
  select(NO,NPSN,PROVINSI,KOTA,AKREDITASI,TAHUN)

indeks_pkbm_TT<-sample(nrow(data_pkbm_TT),44,replace=FALSE)

sampel_pkbm_TT<-data_pkbm_TT[indeks_pkbm_TT,]

# gabung data sampel PAUD
sampel_pkbm_total<-rbind(sampel_pkbm_A,sampel_pkbm_B,sampel_pkbm_C,sampel_pkbm_TT)
write.csv(sampel_pkbm_total,"D:/Arna Ramadhan/Project/BAN PAUD/Sampling PKBM.csv")
