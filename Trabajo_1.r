#Cargamos nuestra paqueteria
invisible( paquetes <- c("dplyr", "ggplot2", "visdat", "skimr", "insuranceData", "patchwork"))
invisible( sapply(paquetes, function(p) if (!require(p, character.only = TRUE)) install.packages(p)))
invisible(lapply(paquetes, library, character.only = TRUE))

#Cargamos nuestra base de datos "dataCar" la cual usaremos para generar nuestro reporte
data(dataCar)
str(dataCar)
summary(dataCar)
skim(dataCar)


poliza_n <- dataCar %>%
  group_by(veh_body) %>%
  summarise(total_pol = n())%>%
  arrange(desc(total_pol))
head(poliza_n,10)

poliza <- length(dataCar$numclaims)
sum(dataCar$clm>=1)
claims <- sum (dataCar$numclaims >=1)
(claims/poliza)*100

clm <- dataCar %>%
  group_by(veh_body) %>%
  summarise(totalclaims = sum(numclaims))%>%
  arrange(desc(totalclaims))
head(clm,10)

max_cost <- dataCar %>%
  group_by(veh_body, veh_value, veh_age, gender)%>%
  summarise(maax_cost = max(claimcst0))%>%
  arrange(desc(maax_cost))
head(max_cost,10)

max_veh <- dataCar %>%
  group_by(veh_body, veh_age, claimcst0, gender)%>%
  summarise(maax_veh = max(veh_value))%>%
  arrange(desc(maax_veh))
head(max_veh,10)

ggplot(max_veh, aes(x = reorder(veh_body, -claimcst0), y = claimcst0, fill = gender)) + 
  geom_bar(stat = "identity", position = "dodge" ) + 
  labs(title = "Tipos de vehiculos mas reclamados por genero",
       x = "Tipo de Vehiculo",
       y = "Reclamacion") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(max_cost, aes(x = reorder(veh_body, +veh_value), y = veh_value, fill = gender)) + 
  geom_bar(stat = "identity", position = "dodge" ) + 
  labs(title = "Tipos de vehiculos mas valor",
       x = "Tipo de Vehiculo",
       y = "Valor") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dataCar, aes(x = exposure, y = veh_value, fill = gender)) + 
  geom_point(stat = "identity" ) + 
  labs(title = "Dispersion de exposicion",
       x = "Exposicion",
       y = "Valor del vehiculo") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


a <- ggplot(dataCar, aes(x = exposure, y = veh_value, fill = gender)) + geom_point(stat = "identity") + 
  labs(title = "Grafico de dispersion Esposicion vs Tipo de vehiculo") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
b <- ggplot(dataCar, aes(x = veh_body, y = veh_value, fill = gender)) + geom_bar(stat = "identity" , position = "dodge") + 
  labs(title = "Grafico de dispersion Esposicion vs Tipo de vehiculo") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))        
c <- a+b       
print(c)


dataCar %>%
  filter(claimcst0 > 0) %>%
  ggplot(aes(x = claimcst0)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  scale_x_log10() +
  labs(
    title = "Distribución del costo de los siniestros",
    x = "Costo del siniestro (escala logarítmica)",
    y = "Frecuencia"
  ) +
  theme_minimal()

