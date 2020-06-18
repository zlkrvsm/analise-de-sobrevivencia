library(data.table)
options(datatable.print.class = TRUE)

# Sirio Libanes
aten_hsl <- fread("dados/COVID_ENCOUNTER.csv", encoding = "UTF-8")
lab_hsl <- fread("dados/COVID_LAB_RESULT.csv", encoding = "UTF-8")
pac_hsl <- fread("dados/COVID_PATIENT.csv", encoding = "UTF-8")

dim(aten_hsl)
dim(lab_hsl)
dim(pac_hsl)

str(aten_hsl)
str(lab_hsl)
str(pac_hsl)

hsl <- lab_hsl[aten_hsl, on = c("ID_PACIENTE", "ID_ATENDIMENTO")][pac_hsl, on = "ID_PACIENTE"]
dim(hsl)

fwrite(hsl, "siriolibanes.csv")

# Albert Einstein ----
lab_ae <- fread("dados/einstein_small_dataset_exames.csv", encoding = "UTF-8")
pac_ae <- fread("dados/einstein_small_dataset_paciente1.csv", encoding = "UTF-8")
fread("dados/")

dim(pac_ae)
dim(lab_ae)

names(pac_ae)
names(lab_ae)

ae <- lab_ae[pac_ae, on = "id_paciente"]

fwrite(ae, "alb-einst.csv")
