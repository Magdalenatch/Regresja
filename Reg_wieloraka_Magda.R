# Ustawiam folder roboczy i wczytuję dane
setwd("C:/Users/magda/Desktop/NAD/Statystyka/PROJEKTY/Projekt 1")
dane1<-read.table("projekt_1_uzup.txt")
head(dane1)

str(dane1)
# Dane "projekt_1.txt", kolejne zmienne:
# Nr - numer, X1 - cena dzialki, X2 - odleglosc od portu, X3 - odleglosc od drogi,
# X4 - powierzchnia dzialki, X5 - wskaznik jakosci, X6 - nr tygodnia sprzedazy,
# X7 - data sprzedazy

# Pomijam 1 i 8 kolumne (nr i data)
dane<-dane1[c(2,3,4,5,6,7)]  
dane <- as.data.frame(dane)
dim(dane) # 75 wierszy 6 kolumn

# Nadaję nazw zmiennym
names(dane)=c('Cena.dzialki','odl.od.portu','odl.od.drogi','powierzchnia','wsk.jakosci','nr.tygodnia') 
head(dane)
summary(dane)
str(dane)


# Wyliczam współczynnik zmienności dla każdej kolumny # na.rm+TRUE - ignoruje, pomija NA
for (i in names(dane)) {
  sd_mean_ratio <- (sd(dane[[i]], na.rm = TRUE) / mean(dane[[i]], na.rm = TRUE)) * 100
  sd_mean_ratio <- round(sd_mean_ratio, 2)
  cat(paste("SD/mean dla", i, ":", sd_mean_ratio, "%", "\n"))
}

# Box ploty dla każdej zmiennej:
windows() # pierwszy wykres
par(mfrow=c(3,2))

for (i in seq_along(dane)) {
  boxplot(dane[[i]], horizontal = TRUE, ylab = names(dane)[i])
}



# 2. sposób:
lapply(seq_along(dane), function(i) {
  boxplot(dane[[i]], horizontal = TRUE, ylab = names(dane)[i])
})


# Biblioteki
library(ggplot2)
library(tidyverse)
library(readxl)

# Zależność Y od poszczególnych zmiennych (pętla)
for (i in names(dane)[-1]) { # [-1] bo pomijamy y :)
  model <- lm(dane[[1]] ~ dane[[i]], data = dane)
  cat("\nRegresja dla zmiennej:", i, "\n")
  print(summary(model))
}

# Wykresy zależności
library(ggplot2)
plots <- list()
for (i in names(dane)[-1]) {
  model <- lm(Cena.dzialki ~ dane[[i]], data = dane)
  
  r_squared <- summary(model)$r.squared
  r_squared <- round(r_squared, 3)
  
  p <- ggplot(dane, aes_string(x = i, y = "Cena.dzialki")) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab(i) + 
    ylab("Cena działki") +
    theme_bw() +
    annotate("text", x = Inf, y = Inf, label = paste("R² =", r_squared), hjust = 1.1, vjust = 2, size = 5)
  
  plots[[i]] <- p
}


library(gridExtra)
grid.arrange(grobs = plots, ncol = 2, nrow = 3)

# sprawdzam zależności zmiennych zależnych, macierz korelacji:
library(corrplot)
kor=round(cor(dane), 3) 
kor
par(mfrow = c(1, 1))
corrplot(kor, method = "circle")
corrplot(kor, method = "number")

# Usuwam 5 kolumnę wskaźnik jakości - brak korelacji z Y
dane <- dane[-5]
dane
dim(dane)
attach(dane)
summary(dane)
# wyszukuje rekordy do usuniecia, usuwam i tworze plik dane2
r1 <- which(odl.od.portu==min(odl.od.portu))
r1
r2 <- which(odl.od.portu==max(odl.od.portu))
r2
r3 <- which(odl.od.drogi==max(odl.od.drogi))
r3
r4 <- which(odl.od.drogi==max(odl.od.drogi[-c(r3)]))
r4
indeksy_do_usuniecia <- c(r1,r2, r3, r4)
indeksy_do_usuniecia
dane2 <- dane[-indeksy_do_usuniecia, ]
dim(dane2)
dane2
attach(dane2)
# Powtarzam wykresy zależności i box ploty na nowych danych:
# Box ploty dla każdej zmiennej:
windows() 
par(mfrow=c(3,2))

for (i in seq_along(dane2)) {
  boxplot(dane2[[i]], horizontal = TRUE, ylab = names(dane2)[i])
}
# Zależność Y od poszczególnych zmiennych (pętla)
for (i in names(dane2)[-1]) { # [-1] bo pomijamy y :)
  model <- lm(dane2[[1]] ~ dane2[[i]], data = dane2)
  cat("\nRegresja dla zmiennej:", i, "\n")
  print(summary(model))
}

# Wykresy zależności
library(ggplot2)
plots <- list()
for (i in names(dane2)[-1]) {
  model <- lm(Cena.dzialki ~ dane2[[i]], data = dane2)
  
  r_squared <- summary(model)$r.squared
  r_squared <- round(r_squared, 3)
  
  p <- ggplot(dane2, aes_string(x = i, y = "Cena.dzialki")) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab(i) + 
    ylab("Cena działki") +
    theme_bw() +
    annotate("text", x = Inf, y = Inf, label = paste("R² =", r_squared), hjust = 1.1, vjust = 2, size = 5)
  
  plots[[i]] <- p
}
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2, nrow = 3)

# Model regresji wielorakiej na danych bez 5 kolumny i punktów odstających:
model2 <- lm(Cena.dzialki~ ., dane2)
model2

summary(model2)
AIC(model2)
library(olsrr)
wszystkie_modele <- ols_step_all_possible(model2)
wszystkie_modele
summary(wszystkie_modele)
ols_step_both_aic(model2) # Krokowo dodaje - co robi tutaj anova? porównuje nasz final model do czego?

ols_plot_resid_hist(model2)
ols_test_normality(model2)
ols_plot_resid_qq(model2)
dim(dane2)

reszty <- model2$residuals
teoret <- model2$fitted.values

par(mfrow=c(2,2))
plot(reszty,col=1,ylab='Reszty modelu');
abline(h=0,lty=1.5)
par(mfrow=c(1,1))
plot(dane2[ ,1],teoret,col=1,xlab='Empiryczna cena działki',ylab='Teoretyczna cena działki');

cook <- cooks.distance(model2)
cook
sort(cook)
stud<-rstandard(model2)
stud
sort(stud)

# punkty influance:
inf<-influence.measures(model2);inf
summary(inf)
install.packages("car")
library(car)
influencePlot(model2)# ten wykres świetny
cook <- ols_plot_cooksd_bar(model2)

plot(model2, which = 1:3) # Wykresy diagnostyczne, super




# Model końcowy. Pomijamy rekordy:
indeksy_do_usuniecia <- sort(c(11, 48, 72), decreasing = TRUE)
indeksy_do_usuniecia
dane2[c(11,48, 72), ] # teraz działa :)

dane2
dim(dane2)
dane3 <- dane2[-indeksy_do_usuniecia, ]
dane3
dim(dane3)

dane3# Model final
attach(dane3)
model_final <- lm(Cena.dzialki~ ., dane3)
model_final
AIC(model_final)
summary(model_final)
confint(model_final)
pred <- predict(model_final,
                newdata=dane3,
                interval = "prediction",
                level=0.95)
pred
ols_step_both_aic(model_final)
ols_plot_resid_hist(model_final)
ols_test_normality(model_final)
ols_plot_resid_qq(model_final)

plot(model_final, which = 1:3)

reszty <- model_final$residuals
teoret <- model_final$fitted.values

par(mfrow=c(1,1))
plot(reszty,col=1,ylab='Reszty modelu');
abline(h=0,lty=1.5)
hist(reszty,10,main='',xlab='Reszty modelu')
shapiro_test <- shapiro.test(residuals(model_final)) # reszty pochodzą z rozkładu normalnego
shapiro_test
plot(dane3[ ,1],teoret,col=1,xlab='Empiryczna cena działki',ylab='Teoretyczna cena działki');

ols_vif_tol(model_final) # nie wystepuje współliniowość VIF ~1
cook <- ols_plot_cooksd_bar(model_final)
stud3 <- ols_plot_resid_stud(model_final)
stud2 <- ols_plot_resid_stud_fit(model_final)
ols_test_normality(model_final)


# w ostatecznym modelu nadal pokazuje jakieś wpływowe, powinnam usunąć i ponownie utworzyć model???
stud<-rstandard(model_final)
stud
sort(stud)

pairs(dane3) # o co w tych wykresach chodzi???
library(tidyverse)          # Pipe operator (%>%) and other commands
library(caret)              # Random split of data/cross validation
library(olsrr)              # Heteroscedasticity Testing (ols_test_score)
library(car)                # Muticolinearity detection (vif)
library(broom)              # Diagnostic Metric Table (augment)
# Sprawdzenie założeń regresji liniowej:
# Liniowość Wykresy reszt względem wartości przewidywanych.
plot(model_final ,1) # reszty

# Homoskedastyczność
ols_test_score(model_final) # p>> alfa więc nasze reszty sa homoskedaktyczne - jednorodnośc wariancji :)

# Autokorrelacja reszt
durbinWatsonTest(model_final) # nie ma dowodów na obecność autokorelacji reszt :)

# Normalność reszt
shapiro.test(model_final$residuals) # i znowu super, H0 :)

# współliniowość
vif(model_final) # i znowu super, wszystkie blisko 1 :)

# Mój model jest ok?

dt_importance <- varImp(model_final)
dt_importance


# Upewnienie się, że dane są w odpowiednim formacie

library(caret)
# Konwertuj dt_importance do ramki danych i dodaj nazwy zmiennych
importance_df <- as.data.frame(dt_importance)
importance_df$Variable <- rownames(importance_df)
rownames(importance_df) <- NULL

# Stwórz wykres słupkowy zamiast pudełkowego
ggplot(data = importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Obrócenie wykresu o 90 stopni
  labs(title = "Variable Importance: Regression Model", x = "Variables", y = "Importance") +
  theme_light()
summary(model_final)

## czy da sie taki wykres zrobić "Pareto" jak ten wyżej tylko na X zaznaczony jest poziom istotności np. 0.05 taką pionową linią???
## kiedys takie gdzieś widziałam

#############################################################




# Wyodrębnij współczynniki i wartości p
coefficients_df <- as.data.frame(summary(model_final)$coefficients)
coefficients_df$Variable <- rownames(coefficients_df)
rownames(coefficients_df) <- NULL

# Kolumny w dataframe: Variable, Estimate, Std. Error, t value, Pr(>|t|)

# Ustal poziom istotności (próg wartości p)
p_threshold <- 0.05

# Przekształć dane dla ggplot2
coefficients_df$Significant <- coefficients_df$`Pr(>|t|)` < p_threshold
coefficients_df <- coefficients_df[order(coefficients_df$`Pr(>|t|)`), ]  # Sortowanie po wartości p
coefficients_df$Variable <- factor(coefficients_df$Variable, levels = coefficients_df$Variable)  # Ustalanie kolejności zmiennych
library(ggplot2)

# Stwórz wykres Pareto z pionową linią poziomu istotności
ggplot(data = coefficients_df, aes(x = Variable, y = -log10(`Pr(>|t|)`), fill = Significant)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = -log10(p_threshold), color = "red", linetype = "dashed", size = 0.5) +  # Linia poziomu istotności
  coord_flip() +  # Obrócenie wykresu o 90 stopni
  labs(title = "Variable Importance: Linear Regression Model", x = "Variables", y = "-log10(p-value)") +
  theme_light() +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey"))
