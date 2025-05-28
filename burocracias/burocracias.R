options(scipen = 999)
#librerias

library(tidyverse)
library(readxl)
library(viridis)
library(patchwork)

#bases de datos----

#1 poblacion total por edo----

edos <- read_excel("Libro1.xlsx")

edos <- edos %>% 
  filter(tipo != "Localidad")

#2 funcionarios por edo----

rechum <- read_csv("m1s1p3_cnge2023.csv", 
                   col_types = cols(sexostt = col_double())) 

funedos <- rechum %>% 
  filter(sexostt >= 0) %>% 
  group_by(entidad_a) %>% 
  summarise(funtot = sum(sexostt)) %>% 
  mutate(funtot = round(funtot/1000, 2))

percapita <- edos %>%
  left_join(funedos, by = "entidad_a") %>% 
  mutate(clave = 1) %>% 
  select(ent, pobtot, funtot, clave)

#estados 1997 y ahora----


versus <- rechum %>% 
  filter(sexostt >= 0) %>%
  group_by(entidad_a) %>% 
  summarise(funtot = sum(sexostt))

a97 <- c(5502, 17790, 2756, 22405, 6700, 2346, 18736, 6489, NA, 3535, NA, 15787, 5083, 16550, 41684, 8987,5846, 7456, 12649, 12123, 23921, 2810, 4376, NA, 18498, 13595, NA, 5338, 19368, 48449, 5232, 765)

versus1 <- edos %>% 
  inner_join(versus) %>% 
  mutate(anio = 2022)

versus2 <- data.frame("ent" =  c(versus1$ent, versus1$ent), tot = c(a97, versus1$funtot), anio = as.factor(c(rep(1997, times = 32), versus1$anio)))

levels(versus2$anio)


g1 <- versus2 %>% 
  filter(ent %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Coahuila", "Colima", "Durango", "Estado de México", "Guerrero", "Hidalgo", "Jalisco", "Michoacán")) %>% 
  mutate(anio = factor(anio, levels = c("2022", "1997"))) %>% 
  ggplot(aes(x = fct_rev(ent), y = tot, fill = anio))+
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.4)+
  geom_text(aes(label = scales::comma(tot)), size = 3, hjust = "inward", family = "sans", fontface = "bold", color = "#222122", position = position_dodge(width = .9))+
  scale_fill_manual(values = c("#1B6B93","#4FC0D0"))+
  scale_y_continuous(labels = scales::comma_format(), expand = expansion(c(0,0.07)))+
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = NULL,
       subtitle = NULL,
       caption = NULL,
       x = NULL,
       y = "Personas contratadas")+
  coord_flip()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill = NA),
        legend.text = element_text(family = "sans", color = "#5F5D5E", face = "bold"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(face = "bold", family = "sans", color = "#222122", hjust = 0.3),
        plot.caption = element_text(family = "sans", color = "#5F5D5E"),
        axis.title = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        axis.text = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        panel.background = element_blank(),
        panel.grid.major = element_blank())

g2 <- versus2 %>% 
  filter(!ent %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Coahuila", "Colima", "Durango", "Estado de México", "Guerrero", "Hidalgo", "Jalisco", "Michoacán", "San Luis Potosí", "Tabasco", "Ciudad de México", "Guanajuato")) %>% 
  mutate(anio = factor(anio, levels = c("2022", "1997"))) %>% 
  ggplot(aes(x = fct_rev(ent), y = tot, fill = anio))+
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.4)+
  scale_fill_manual(values = c("#1B6B93","#4FC0D0"))+
  geom_text(aes(label = scales::comma(tot)), size = 3, hjust = "inward", family = "sans", fontface = "bold", color = "#222122", position = position_dodge(width = .9))+
  scale_y_continuous(labels = scales::comma_format(), expand = expansion(c(0,0.07)))+
  guides(fill = guide_legend(reverse = TRUE))+
  labs(
    x = NULL,
    y = "Personas contratadas",
    caption = NULL)+
  coord_flip()+
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.5),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(family = "sans", color = "#5F5D5E", face = "bold"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(face = "bold", family = "sans", color = "#222122", hjust = 0.3),
        plot.caption = element_text(family = "sans", color = "#5F5D5E"),
        axis.title = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        axis.text = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        panel.background = element_blank(),
        panel.grid.major = element_blank())

g1+g2+
  plot_annotation(title = "Gráfica 1. Personas contratadas en los gobiernos estatales",
                  subtitle = NULL,
                  caption = "Fuente: elaboración propia con datos de INEGI y Hernández (2008)
                  Nota: se incluyeron únicamente a los estados con disponibilidad de datos para los dos años
                  @Ivanbmata")&
  theme(text = element_text(face = "bold", family = "sans", color = "#222122", hjust = 0.3),
        plot.title = element_text(hjust = 0.5))

ggsave("versus dodge.pdf")


#3 datos paises----

pais <- read_excel("paises.xlsx", sheet = "Hoja2") %>% 
  mutate(clave = 2) %>% 
  select(pais, pob, empleados, clave)

#4 presupuesto----

presupuesto <- read_csv("m1s1p20_cnge2023.csv", 
                        col_types = cols(presup3 = col_double()))

nombres <- read_csv("entidad_a_cnge2023.csv") %>% 
  filter(entidad_a != 99) %>% 
  pull(descrip)


#grafica de barras total de funcionarios por edo y POR CADA 1000 MIL----

nemp <- data.frame("ent" = c(percapita$ent, pais$pais),
                   "pobtot" = c(percapita$pobtot, pais$pob),
                   "funtot" = c(percapita$funtot, pais$empleados),
                   "clave" = c(percapita$clave, pais$clave))

nemp <- nemp %>% 
  mutate(clave = as.factor(clave))

nemp %>% 
  filter(ent != "Panamá", clave == 1) %>% 
  mutate(funtot = funtot*1000,
         n = round((funtot/pobtot)*100000, 2)) %>% 
  summarise(mean(n))

nemp %>% 
  filter(ent != "Panamá") %>% 
  mutate(funtot = funtot*1000,
         n = round((funtot/pobtot)*100000, 2)) %>% 
  ggplot(aes(x = reorder(ent, n), y = n, fill = clave))+
  scale_fill_manual(values = c("#176B87","#64CCC5"), labels = c("Entidadades", "Países"))+
  # scale_fill_manual(values = c("#0AFA23", "#028A10"), labels = c("Entidadades", "Países"))+
  geom_bar(stat = "identity", color = "white")+
  annotate("text", x = 4, y = 2400, label= "2,225", family = "sans", color = "#222122", fontface = "bold")+
  annotate("text", x = 2, y = 2770, label="Promedio nacional", family = "sans", color = "#222122", fontface = "bold")+
  geom_hline(yintercept= 2190, linetype = "dashed", colour= "#05366A") +
  geom_text(aes(label = scales::comma(round(n, 2))),  size = 3, hjust = 0, family = "sans", fontface = "bold", color = "#222122")+
  # geom_text(aes(label = round(n, 2)), size = 3, hjust = 0, family = "sans", fontface = "bold", color = "#222122")+
  scale_y_continuous(labels = scales::comma_format(), expand = expansion(c(0,0.07)))+
  coord_flip()+
  labs(title= "Gráfica 2. Personas contratadas en el gobierno por cada 100 mil habitantes en 2022",
       x = NULL,
       y = "Total de personas funcionarias",
       caption = "Nota: los datos de los países corresponden al número de empleados en el gobierno central en 2020
    Fuente: elaboración propia con datos de INEGI, OIT y Banco Mundial
    @Ivanbmata")+
  theme(axis.title = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        axis.text = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9, 0), 
        legend.justification = c(1, -3),
        legend.text = element_text(family = "sans", color = "#5F5D5E", face = "bold"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(face = "bold", family = "sans", color = "#222122", hjust = 0.3),
        plot.caption = element_text(face = "bold", family = "sans", color = "#222122"))



#burbujas: PRESUPUESTO, personal, poblacion----
presupuesto <- read_csv("m1s1p20_cnge2023.csv", 
                        col_types = cols(presup3 = col_double()))

nombres <- read_csv("entidad_a_cnge2023.csv") %>% 
  filter(entidad_a != 99) %>% 
  pull(descrip)

presup <- presupuesto %>% 
  mutate(ent = factor(entidad_a, labels = nombres))

pretot <- presup %>% 
  group_by(ent) %>% 
  summarise(pretot = sum(presup3, na.rm = TRUE)) %>% 
  filter(pretot > 0)

burbujas <- nemp %>% 
  filter(clave == 1) %>% 
  inner_join(pretot)

burbujas %>% 
  mutate(pretot = round(pretot/1000000000, 2),
         n = round((funtot/pobtot)*100000, 2)) %>% 
  summarise(mean(pretot),
            mean(n))

burbujas %>% 
  mutate(funtot = funtot*1000,
         n = round((funtot/pobtot)*100000, 2),
         pretot = pretot/1000000,
         pretot = round((pretot/pobtot)*100000, 2),
         pobtot = round(pobtot/1000000, 2)) %>% 
  summarise(mean(pretot))

burbujas %>% 
  mutate(funtot = funtot*1000,
         n = round((funtot/pobtot)*100000, 2),
         pretot = pretot/1000000,
         pretot = round((pretot/pobtot)*100000, 2),
         pobtot = round(pobtot/1000000, 2)) %>% 
  arrange(pretot)

burbujas %>% 
  mutate(funtot = funtot*1000,
         n = round((funtot/pobtot)*100000, 2),
         pretot = pretot/1000000,
         pretot = round((pretot/pobtot)*100000, 2),
         pobtot = round(pobtot/1000000, 2)) %>%
  # summarise(mean(n),
  #           mean(pretot))
  ggplot(aes(x = n, y = pretot, color = ent, size = pobtot))+
  
  #sin estandarizar
  # mutate(pretot = round(pretot/1000000, 2),
  #        funtot = funtot*1000,
  #        pobtot = round(pobtot/1000000, 2)) %>% 
  
  #presupuesto estandarizado
  # mutate(pretot = round((pretot/pobtot)*100000, 2),
  #        funtot = funtot*1000,
  #        pretot = pretot/1000000,
  #        pobtot = round(pobtot/1000000, 2)) %>%
  
  # ggplot(aes(x = funtot, y = pretot, color = ent, size = pobtot))+
  geom_point()+
  guides(size = guide_legend(title.position = "top", direction = "horizontal"),
         color = "none")+
  geom_hline(yintercept= 1625, linetype = "dotted", colour= "#0E6655") +
  geom_vline(xintercept= 2224, linetype = "dashed", colour= "#0E6655") +
  annotate("text", x= 3800, y = 1550, label="1,625", family = "sans", color = "#0E6655", fontface = "bold")+
  annotate("text", x= 3800, y = 1300, label= str_wrap("Promedio nacional", width = 10), family = "sans", color = "#0E6655", fontface = "bold")+
  annotate("text", x= 2570, y = 2450, label = "Promedio nacional", family = "sans", color = "#0E6655", fontface = "bold")+
  annotate("text", x= 2400, y = 2620, label="2,224", family = "sans", color = "#0E6655", fontface = "bold")+
  scale_color_viridis(discrete = TRUE, option = "turbo")+
  labs(title = "Gráfica 3. Presupuesto y tamaño de los gobiernos estatales en 2022",
       x = "Número de personas contratadas por cada 100 mil habitantes",
       y = "Presupuesto en millones de pesos por cada 100 mil habitantes",
       caption = "Fuente: elaboración propia a partir de datos de INEGI
    @Ivanbmata",
       size = "Millones de habitantes")+
  scale_y_continuous(limits = c(0, 3000), labels = scales::dollar_format())+
  # scale_y_continuous(limits = c(0, 350000), labels = scales::dollar_format())+
  scale_x_continuous(labels = scales::comma_format())+
  geom_text(aes(label = ent), color = "black", vjust = "inward", hjust = "inward", size = 2.5, check_overlap = TRUE, fontface = "bold", family = "sans")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = NA),
        axis.title = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        axis.text = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        plot.caption = element_text(face = "bold", family = "sans", color = "#222122"),
        legend.title = element_text(family = "sans", face = "bold", color = "#222122"),
        legend.text = element_text(family = "sans", color = "#5F5D5E", face = "bold"),
        legend.title.align = 0.5,
        legend.position = c(0.85, 0.9),
        # legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        plot.title = element_text(face = "bold", family = "sans", color = "#222122", hjust = 0.45))

#profesionalizacion----

profe <- read_csv("m1s1p16_cnge2023.csv")

profesi <- profe %>% 
  mutate(ent = factor(entidad_a, labels = nombres),
         pro = factor(rnspni1, labels = c("No aplica", "Sí", "En proceso de integración", "No", "No identificado"))) 

secre <-  profesi %>% 
  filter(rnspni1 %in% c(1:3)) %>% 
  count(ent)


#se van Jalisco, Puebla, y zac
profesi1 <- profesi %>% 
  filter(rnspni1 %in% c(1:3)) %>% 
  group_by(ent) %>% 
  count(pro) %>% 
  mutate(total = case_when(ent == "Aguascalientes" ~ 57,
                           ent == "Baja California" ~ 61,
                           ent == "Baja California Sur" ~ 21,
                           ent == "Campeche" ~ 74,
                           ent == "Coahuila" ~ 60,
                           ent == "Colima" ~ 30,
                           ent == "Chiapas" ~ 56,
                           ent == "Chihuahua" ~ 65,
                           ent == "Ciudad de México" ~ 75,
                           ent == "Durango" ~ 52,
                           ent == "Guanajuato" ~79,
                           ent == "Guerrero" ~80,
                           ent == "Hidalgo" ~ 56,
                           ent == "Estado de México" ~103,
                           ent == "Michoacán" ~ 64,
                           ent == "Morelos" ~ 53,
                           ent == "Nayarit" ~ 57,
                           ent == "Nuevo León" ~ 90,
                           ent == "Oaxaca" ~ 42,
                           ent == "Querétaro" ~ 82,
                           ent == "Quintana Roo" ~ 70,
                           ent == "San Luis Potosí" ~75,
                           ent == "Sinaloa"~ 84,
                           ent == "Sonora" ~ 46,
                           ent == "Tabasco" ~ 61,
                           ent == "Tamaulipas" ~ 41,
                           ent == "Tlaxcala" ~ 54,
                           ent == "Veracruz" ~ 72,
                           ent == "Yucatán" ~ 68,
  ),
  porcentaje = round((n*100)/total, 1))

profesi1 %>% 
  ggplot(aes(x = fct_rev(ent), y = porcentaje, fill = pro))+
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.4, alpha = 0.9)+
  # scale_fill_manual(values = c("#7FB3D5", "#99A3A4", "#73C6B6"))+
  scale_fill_manual(values = c("#1B6B93","#4FC0D0",  "#A2FF86"))+
  geom_text(aes(label = str_c(porcentaje, "%")), position = position_stack(vjust = .5), hjust= "inward", size = 2.7, fontface = "bold", family = "sans", color = "black")+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = expansion(c(0,0.07)))+
  labs(title = str_wrap("Gráfica 4. Porcentaje de insitucioness de la administración estatal con algún esquema de profesionalización", width = 70),
       x = NULL,
       y = NULL,
       caption = "Fuente: elaboración propia con datos de INEGI
    Nota: se excluyeron aquellos estados en donde no aplican los esquemas de profesionalización")+
  coord_flip()+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = NA),
        legend.text = element_text(family = "sans", color = "#5F5D5E", face = "bold"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(face = "bold", family = "sans", color = "black", hjust = 0.5),
        plot.caption = element_text(face = "bold", family = "sans", color = "black"),
        axis.title = element_text(family = "sans", face = "bold", color = "black"),
        axis.text = element_text(family = "sans", face = "bold", color = "#5F5D5E"),
        panel.background = element_blank(),
        panel.grid.major = element_blank())

ggsave("profesionalizacion.pdf")
