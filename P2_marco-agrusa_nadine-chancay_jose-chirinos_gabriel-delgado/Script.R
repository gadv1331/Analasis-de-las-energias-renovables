#Integrantes:
# Marco Agrusa - C.I: 25.866.681
# Nadine Chancay - C.I: 26.838.242
# José Chirinos - C.I: 22.356.796
# Gabriel Delgado - C.I: 29.553.027


# LIBRERIAS ---------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(gridExtra)


# CARGAR CSV ------------------------------------------------------------

#Dataset General sobre Consumo de Energías Renovables
ConsumoEnergiasRenovablesG <- read.csv("P2_marco-agrusa_nadine-chancay_jose-chirinos_gabriel-delgado/01 renewable-share-energy.csv")
head(ConsumoEnergiasRenovablesG,10)
colnames(ConsumoEnergiasRenovablesG) <- c("Pais", "ID", "Año", "Porcentaje_Consumo")
head(ConsumoEnergiasRenovablesG,10)

#Dataset General sobre Producción de Energías Renovables
ProduccionEnergiasRenovablesG <- read.csv("P2_marco-agrusa_nadine-chancay_jose-chirinos_gabriel-delgado/04 share-electricity-renewables.csv")
head(ProduccionEnergiasRenovablesG,10)
colnames(ProduccionEnergiasRenovablesG) <- c("Pais", "ID", "Año", "Porcentaje_Produccion")
head(ProduccionEnergiasRenovablesG,10)

#Dataset Específico sobre Cnatidad en TWh por tipo de Energías Renovables
EnergiasRenovablesE <- read.csv("P2_marco-agrusa_nadine-chancay_jose-chirinos_gabriel-delgado/03 modern-renewable-prod.csv")
head(EnergiasRenovablesE,10)
colnames(EnergiasRenovablesE) <- c("Pais", "ID", "Año", "Eólica", "Hidroelétrica", "Solar", "Otras")
head(EnergiasRenovablesE,50)

# AJUSTE DE DATOS -----------------------------------------------------
# Se ajustan los datos para tener datos consistentes en todas las tablas
# asi como el mismo tamaño.

ProduccionEnergiasRenovablesG <- semi_join(ProduccionEnergiasRenovablesG, 
                                           ConsumoEnergiasRenovablesG, 
                                           by = c("Pais", "Año")) %>% mutate_if(is.numeric, round, 4)
ConsumoEnergiasRenovablesG <- semi_join(ConsumoEnergiasRenovablesG, 
                                        ProduccionEnergiasRenovablesG, 
                                        by = c("Pais", "Año")) %>% mutate_if(is.numeric, round, 4)
EnergiasRenovablesE <- semi_join(EnergiasRenovablesE, 
                                 ConsumoEnergiasRenovablesG, 
                                 by = c("Pais", "Año")) %>% mutate_if(is.numeric, round, 4)

# CÁLCULOS ------------------------------------------------------------

EnergiasRenovablesE <- EnergiasRenovablesE %>%
  mutate(Total = Eólica + Hidroelétrica + Solar + Otras) %>%  #Total de TWh por año
  mutate(PorcentajeConsumoHidro = Hidroelétrica * ConsumoEnergiasRenovablesG$Porcentaje_Consumo / Total)  %>%
  mutate(PorcentajeConsumoEolica = Eólica * ConsumoEnergiasRenovablesG$Porcentaje_Consumo / Total)  %>%
  mutate(PorcentajeConsumoSolar = Solar * ConsumoEnergiasRenovablesG$Porcentaje_Consumo / Total)  %>%
  mutate(PorcentajeConsumoOtras = Otras * ConsumoEnergiasRenovablesG$Porcentaje_Consumo / Total)  %>%
  mutate(PorcentajeProducciónHidro = Hidroelétrica * ProduccionEnergiasRenovablesG$Porcentaje_Produccion / Total)  %>%
  mutate(PorcentajeProducciónEolica = Eólica * ProduccionEnergiasRenovablesG$Porcentaje_Produccion / Total)  %>%
  mutate(PorcentajeProducciónSolar = Solar * ProduccionEnergiasRenovablesG$Porcentaje_Produccion / Total)  %>%
  mutate(PorcentajeProducciónOtras = Otras * ProduccionEnergiasRenovablesG$Porcentaje_Produccion / Total) %>% 
  mutate_if(is.numeric, round, 4)


#Se asigna NA a los campos en blanco en ID para facilitar futuras búsquedas
ProduccionEnergiasRenovablesG$ID[ProduccionEnergiasRenovablesG$ID == ""] <- NA
ConsumoEnergiasRenovablesG$ID[ConsumoEnergiasRenovablesG$ID == ""] <- NA
EnergiasRenovablesE$ID[EnergiasRenovablesE$ID == ""] <- NA

org <- c("Africa (BP)", "Asia Pacific (BP)", "CIS (BP)", "Central America (BP)", 
         "Eastern Africa (BP)", "Europe (BP)",  "European Union (27)", 
         "High-income countries", "Lower-middle-income countries", 
         "Middle Africa (BP)", "Middle East (BP)", "Non-OECD (BP)", 
         "North America (BP)", "South and Central America (BP)", 
         "Upper-middle-income countries", "Western Africa (BP)", "OECD (BP)")




# GRÁFICAS GENERALES Y POR TIPO DE ENERGÍA RENOVABLE ----------------------

#GENERALES

# Distribución de TWh por Tipo de Energía Renovable
energia_tipo_anual <- EnergiasRenovablesE %>%
  group_by(Año) %>%
  summarise(
    Eólica = sum(Eólica, na.rm = TRUE),
    Hidroeléctrica = sum(Hidroelétrica, na.rm = TRUE),
    Solar = sum(Solar, na.rm = TRUE),
    Otras = sum(Otras, na.rm = TRUE)
  ) %>%
  gather(key = "TipoEnergía", value = "TWh", -Año)

ggplot(energia_tipo_anual, aes(x = as.factor(Año), y = TWh, fill = TipoEnergía)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) + # Ajustar el ancho de las barras
  ggtitle("Distribución de TWh por Tipo de Energía Renovable") +
  xlab("Año") +
  ylab("Energía Producida (TWh)") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotar etiquetas del eje X
  theme(axis.title.x = element_blank()) + # Eliminar título del eje X
  theme(axis.text = element_text(size = 10))

#Comparación entre el Consumo Y la Producción de Energías Renovables
#a lo largo de los años
ConsumoEnergiasRenovablesG2 <- ConsumoEnergiasRenovablesG %>%
  group_by(Año) %>%
  summarise(Promedio_Consumo = mean(Porcentaje_Consumo, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)

  

consumoG <- ggplot(ConsumoEnergiasRenovablesG2, aes(x = factor(Año), y = Promedio_Consumo)) +
  geom_bar(stat = "identity", fill = "coral", color = "black") +
  geom_text(aes(label = paste0(round(Promedio_Consumo, 1), "%"), y = Promedio_Consumo), 
            vjust = -1.0 ,hjust = -0.20, size = 2.5, angle = 45, color = "black", fontface = "bold") +
  ggtitle("Consumo de Energía Renovable por Año (Promedio)") +
  xlab("Año") +
  ylab("Porcentaje de Consumo de Energía Renovable (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ProduccionEnergiasRenovablesG2 <- ProduccionEnergiasRenovablesG %>%
  group_by(Año) %>%
  summarise(Promedio_Produccion = mean(Porcentaje_Produccion, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


produccionG <- ggplot(ProduccionEnergiasRenovablesG2, aes(x = factor(Año), y = Promedio_Produccion)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  geom_text(aes(label = paste0(round(Promedio_Produccion, 1), "%"), y = Promedio_Produccion), 
            vjust = -1.0 ,hjust = -0.20, size = 2.5, angle = 45, color = "black", fontface = "bold") +
  ggtitle("Producción de Energía Renovable por Año (Promedio)") +
  xlab("Año") +
  ylab("Porcentaje de Producción de Energía Renovable (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(consumoG, produccionG, nrow=2)

#---


# Hidroeléctrica

EnergiaHidroelectrica_agrupada <- aggregate(Hidroelétrica ~ Año, data = filter(EnergiasRenovablesE, !is.na(ID)), sum)

EnergiaHidroelectrica_agrupada_pais <- aggregate(Hidroelétrica ~ Pais, data = filter(EnergiasRenovablesE, !is.na(ID)), sum)

esa_Hidroelectrica <- head(EnergiaHidroelectrica_agrupada_pais[order(-EnergiaHidroelectrica_agrupada_pais$Hidroelétrica), ], 8)


EnergiasRenovablesE_paises_summary_Hidroelectrica <- filter(EnergiasRenovablesE, !is.na(ID)) %>%
  group_by(Año) %>%
  summarise( media = mean(Hidroelétrica, na.rm = TRUE),
             se = sd(Hidroelétrica, na.rm = TRUE)/sqrt(n())) %>%
  mutate(ci_upper = media + (1.96 * se),
         ci_lower = media - (1.96 * se))



ggplot(EnergiasRenovablesE_paises_summary_Hidroelectrica, aes(x = Año, y = media)) + 
  geom_line(color = "red") + geom_point(color = "red") + 
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "red", alpha = 0.2) +
  ggtitle("Produccion Promedio de Energia Hidroeléctrica por Año") + 
  xlab("Año") + ylab("Energia Hidroeléctrica Producida (TWh)")

ggplot(filter(EnergiasRenovablesE, is.na(ID) & !Pais %in% org), aes(x = Año, y = Hidroelétrica, colour = Pais)) + 
  geom_line() + geom_point() + 
  ggtitle("Producción Anual de Energia Hidroeléctrica por Continente") + 
  xlab("Año") + ylab("Energia Hidroeléctrica (TWh)") + 
  labs(colour = "Continentes")

ggplot(esa_Hidroelectrica, aes(reorder(Pais, Hidroelétrica), y = Hidroelétrica, fill = Pais)) + 
  geom_bar(stat = "identity")+ 
  theme_minimal() + 
  ggtitle("Top 7 Paises con Mayor Produccion de Energia Hidroeléctrica") + 
  xlab("Pais") + ylab("Energia Hidroeléctrica (TWh)")+ 
  geom_text(aes(label = Hidroelétrica), vjust = 1.0, color = "white", size = 3, fontface = "bold")


#---


# Eólica


EnergiaEolica_agrupada <- aggregate(Eólica ~ Año, data = filter(EnergiasRenovablesE, !is.na(ID)), sum)

EnergiaEolica_agrupada_pais <- aggregate(Eólica ~ Pais, data = filter(EnergiasRenovablesE, !is.na(ID)), sum)

esa_eolica <- head(EnergiaEolica_agrupada_pais[order(-EnergiaEolica_agrupada_pais$Eólica), ], 8)


EnergiasRenovablesE_paises_summary_Eolica <- filter(EnergiasRenovablesE, !is.na(ID)) %>%
  group_by(Año) %>%
  summarise( media = mean(Eólica, na.rm = TRUE),
             se = sd(Eólica, na.rm = TRUE)/sqrt(n())) %>%
  mutate(ci_upper = media + (1.96 * se),
         ci_lower = media - (1.96 * se))



ggplot(EnergiasRenovablesE_paises_summary_Eolica, aes(x = Año, y = media)) + 
  geom_line(color = "red") + geom_point(color = "red") + 
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "red", alpha = 0.2) +
  ggtitle("Produccion Promedio de Energia Eólica por Año") + 
  xlab("Año") + ylab("Energia Eólica Producida (TWh)")
  

ggplot(filter(EnergiasRenovablesE, is.na(ID) & !Pais %in% org), aes(x = Año, y = Eólica, colour = Pais)) + 
  geom_line() + geom_point() + 
  ggtitle("Producción Anual de Energia Eólica por Continente") + 
  xlab("Año") + ylab("Energia Eólica (TWh)") + 
  labs(colour = "Continentes")

ggplot(esa_eolica, aes(reorder(Pais, Eólica), y = Eólica, fill = Pais)) + 
  geom_bar(stat = "identity")+ 
  theme_minimal() + 
  ggtitle("Top 7 Paises con Mayor Produccion de Energia Eólica") + 
  xlab("Pais") + ylab("Energia Eólica (TWh)")+ 
  geom_text(aes(label = Eólica), vjust = 1.0, color = "white", size = 3, fontface = "bold")


#---


# Solar

EnergiaSolar_agrupada <- aggregate(Solar ~ Año, data = filter(EnergiasRenovablesE, !is.na(ID)), sum)

EnergiaSolar_agrupada_pais <- aggregate(Solar ~ Pais, data = filter(EnergiasRenovablesE, !is.na(ID)), sum)

esa_7 <- head(EnergiaSolar_agrupada_pais[order(-EnergiaSolar_agrupada_pais$Solar), ], 8)


EnergiasRenovablesE_paises_summary <- filter(EnergiasRenovablesE, !is.na(ID)) %>%
  group_by(Año) %>%
  summarise( media = mean(Solar, na.rm = TRUE),
             se = sd(Solar, na.rm = TRUE)/sqrt(n())) %>%
  mutate(ci_upper = media + (1.96 * se),
         ci_lower = media - (1.96 * se))



ggplot(EnergiasRenovablesE_paises_summary, aes(x = Año, y = media)) + 
  geom_line(color = "red") + geom_point(color = "red") + 
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "red", alpha = 0.2) +
  ggtitle("Produccion Promedio de Energia Solar por Año") + 
  xlab("Año") + ylab("Energia Solar Producida (TWh)")

ggplot(filter(EnergiasRenovablesE, is.na(ID) & !Pais %in% org), aes(x = Año, y = Solar, colour = Pais)) + 
  geom_line() + geom_point() + 
  ggtitle("Producción Anual de Energia Solar por Continente") + 
  xlab("Año") + ylab("Energia Solar (TWh)") + 
  labs(colour = "Continentes")


ggplot(esa_7, aes(reorder(Pais, Solar), y = Solar, fill = Pais)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  ggtitle("Top 7 Paises con Mayor Produccion de Energia Solar") + 
  xlab("Pais") + ylab("Energia Solar (TWh)") + 
  geom_text(aes(label = Solar), vjust = 1.0, color = "white", size = 3, fontface = "bold")

