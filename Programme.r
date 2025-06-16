# Charger les données
data <- read.csv("donnees_medicales.csv")

# Convertir Result en valeurs numériques (0 = negative, 1 = positive)
data$Result_numeric <- ifelse(data$Result == "positive", 1, 0)

# Sélection des variables numériques (en gardant les noms exacts du CSV)
num_data <- data[, c("Age", "Gender", "Heart.rate", "Systolic.blood.pressure",
                     "Diastolic.blood.pressure", "Blood.sugar",
                     "CK.MB", "Troponin", "Result_numeric")]

# Créer une grille de graphiques (3 colonnes, 3 lignes pour 9 variables)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))

# Boxplots individuels avec échelles adaptées
for (col in names(num_data)) {
  # Calculer les statistiques pour définir l'échelle
  col_data <- num_data[[col]]
  col_data <- col_data[!is.na(col_data)]  # Supprimer les NA
  
  # Calculer les quartiles et IQR pour définir des limites adaptées
  Q1 <- quantile(col_data, 0.25)
  Q3 <- quantile(col_data, 0.75)
  IQR <- Q3 - Q1
  
  # Limites adaptées (1.5 * IQR au-delà des quartiles)
  lower_limit <- max(min(col_data), Q1 - 1.5 * IQR)
  upper_limit <- min(max(col_data), Q3 + 1.5 * IQR)
  
  # Ajouter une marge de 10% pour une meilleure visualisation
  range_data <- upper_limit - lower_limit
  ylim_min <- lower_limit - 0.1 * range_data
  ylim_max <- upper_limit + 0.1 * range_data
  
  # Créer le boxplot avec échelle adaptée
  # Traitement spécial pour Result_numeric (variable binaire)
  if (col == "Result_numeric") {
    boxplot(col_data,
            main = "Boxplot - Result (0=neg, 1=pos)",
            ylab = "Result",
            col = "lightcoral",
            ylim = c(-0.2, 1.2),
            outline = TRUE)
    
    # Ajouter les proportions
    prop_pos <- mean(col_data)
    text(x = 1.3, y = 0.9, 
         labels = paste("Positifs:", round(prop_pos * 100, 1), "%"), 
         cex = 0.8, adj = 0)
  } else {
    boxplot(col_data,
            main = paste("Boxplot -", col),
            ylab = col,
            col = "lightblue",
            ylim = c(ylim_min, ylim_max),
            outline = TRUE)
    
    # Afficher les statistiques sur le graphique
    text(x = 1.3, y = ylim_max * 0.9, 
         labels = paste("Médiane:", round(median(col_data), 2)), 
         cex = 0.8, adj = 0)
  }
}

# Remettre les paramètres graphiques par défaut
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# Alternative : Boxplots individuels en pleine page (un par un)
cat("Pour voir les boxplots un par un en pleine page, utilisez ce code :\n\n")

# Code pour boxplots individuels en pleine page
for (col in names(num_data)) {
  # Pause pour voir chaque graphique (décommentez si nécessaire)
  # readline(prompt = paste("Appuyez sur Entrée pour voir", col, "..."))
  
  col_data <- num_data[[col]]
  col_data <- col_data[!is.na(col_data)]
  
  # Statistiques pour l'échelle
  stats <- boxplot.stats(col_data)
  
  boxplot(col_data,
          main = paste("Distribution de", col),
          ylab = col,
          col = "lightblue",
          border = "darkblue",
          outline = TRUE)
  
  # Ajouter des informations statistiques
  mtext(paste("Min:", round(min(col_data), 2), 
              "| Max:", round(max(col_data), 2),
              "| Médiane:", round(median(col_data), 2)),
        side = 3, line = 0.5, cex = 0.8)
}