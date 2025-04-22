# Tema: Questão 2 Artemis - Data Modelling
# Autor: Marcello Filgueiras
Sys.Date()

library(tidyverse)
library(ggcorrplot)
library(GGally)
library(broom)
library(pROC)
library(car)

# 1. Importação e Preparação dos Dados -------------------------------------

df <- read_csv("data/breast_cancer.csv") %>%
  select(mean_concave_points, mean_perimeter, mean_fractal_dimension,
         worst_perimeter, worst_texture, worst_area, target) %>%
  mutate(target_fact = as.factor(target),
         target_num = as.numeric(target) - 1)

df_long <- df %>%
  pivot_longer(names_to = "variavel", values_to = "valor", -c(target, target_fact, target_num))

# 2. Análise Exploratória de Dados ----------------------------------------

# 2.1 Estatísticas descritivas
cat("\nEstatísticas Descritivas:\n")
print(summary(df))

# 2.2 Visualizações

# Boxplots
df_long %>%
  ggplot(aes(x = target_fact, y = valor, fill = target_fact)) +
  geom_boxplot() +
  facet_wrap(~variavel, scales = "free") +
  labs(title = "Distribuição das Variáveis por Tipo de Tumor",
       x = "Tipo de Tumor",
       y = "Valor",
       fill = "Tipo") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  theme_minimal()

# Histogramas
df_long %>%
  ggplot(aes(x = valor, fill = target_fact)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  facet_wrap(~variavel, scales = "free", ncol = 3) +
  labs(title = "Distribuição das Variáveis por Tipo de Tumor",
       x = "Valor da Variável",
       y = "Frequência",
       fill = "Tipo") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  theme_minimal()

# Matriz de correlação
cor_matrix <- df %>%
  select(-target, -target_fact, -target_num) %>%
  cor()

ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           title = "Matriz de Correlação") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pairplot
ggpairs(df, columns = 1:7, aes(color = target_fact, alpha = 0.5)) +
  labs(title = "Relações entre Variáveis") +
  theme_minimal()

# Comentários da EDA:

# Variáveis como 'worst_perimeter' e 'worst_area' mostram forte correlação negativa com target
# 'mean_fractal_dimension' parece ter pouca discriminação entre classes
#  forte correlação entre variáveis. Quando aumenta perímetro aumenta área.

# 3. Regressão Linear -----------------------------------------------------

# 3.1 Modelo completo
model_ols <- lm(target ~ mean_concave_points + mean_perimeter + 
                  mean_fractal_dimension + worst_perimeter + 
                  worst_texture + worst_area, 
                data = df)

#Resultados
summary(model_ols)
ols_results <- broom::tidy(model_ols)
print(ols_results)



# Comentários OLS:
cat("\nComentários da Regressão Linear:
- Variáveis significativas (p < 0.05):", 
    ols_results$term[ols_results$p.value < 0.05], "
- R² ajustado:", summary(model_ols)$adj.r.squared, "
- Worst_perimeter tem o maior coeficiente negativo, indicando forte relação com malignidade\n")


# Verificação de multicolinearidade
"Fator de Inflação de Variância (VIF)"

print(car::vif(model_ols))


# 3.2 Modelo com uma variável
modelo_simples <- lm(target ~ mean_concave_points, data = df)

cat("\n=== Modelo Linear Simples ===\n")
summary(modelo_simples)
ols_simples <- broom::tidy(modelo_simples)
print(ols_simples)

# Gráfico do modelo simples

#Alta relação
ggplot(df, aes(x = mean_concave_points, y = target)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(-0.1, 1.1) +
  labs(title = "Relação Linear: Target ~ Mean Concave Points",
       x = "Mean Concave Points",
       y = "Target (0=Benigno, 1=Maligno)")

#Baixa relação

modelo_simples2 <- lm(target ~ mean_fractal_dimension, data = df)

cat("\n=== Modelo Linear Simples ===\n")
summary(modelo_simples2)
ols_simples2 <- broom::tidy(modelo_simples2)
print(ols_simples)

ggplot(df, aes(x = mean_fractal_dimension, y = target)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(-0.1, 1.1) +
  labs(title = "Relação Linear: Target ~ mean_fractal_dimension",
       x = "Mean Fractal Dimension",
       y = "Target (0=Benigno, 1=Maligno)")

# 3.3 Modelo Simplificado - Final

modelo_final <- lm(target ~ mean_concave_points +
                     worst_perimeter + mean_fractal_dimension, 
                   data = df)

summary(modelo_final)
ols_final <- broom::tidy(modelo_final)
print(ols_final)

# Visualização dos coeficientes
ggplot(ols_final[-1, ], aes(x = term, y = estimate)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = estimate - std.error, 
                    ymax = estimate + std.error),
                width = 0.2) +
  labs(title = "Coeficientes do Modelo Linear Simplificado",
       x = "Variável",
       y = "Coeficiente") +
  theme_minimal()

# quando associada a mean_convic
# mean_fractal_dimension na verdade é induzida a f


# 4. Regressão Logística --------------------------------------------------

# 4.1 Modelo completo
model_logit_full <- glm(target ~ mean_concave_points + mean_perimeter + 
                          mean_fractal_dimension + worst_perimeter + 
                          worst_texture + worst_area,
                        family = binomial(link = "logit"), 
                        data = df)

summary(model_logit_full)
logit_full_results <- broom::tidy(model_logit_full)
print(logit_full_results)



# 4.2 Modelo simplificado
model_logit_final <- glm(target ~ mean_concave_points + worst_perimeter,
                         family = binomial(link = "logit"), 
                         data = df)

cat("\n=== Modelo Logístico Simplificado ===\n")
summary(model_logit_final)
logit_final_results <- broom::tidy(model_logit_final)
print(logit_final_results)

# Curva de Regressão Logística para mean_concave_points
ggplot(df, aes(x = mean_concave_points, y = target)) +
  geom_point(alpha = 0.4, position = position_jitter(height = 0.02, width = 0), 
             color = "#0072B2") +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = TRUE, 
              color = "#D55E00", 
              fill = "#E69F00") +
  labs(title = "Curva de Regressão Logística: Probabilidade de Tumor Maligno",
       subtitle = "Em função de Mean Concave Points",
       x = "Mean Concave Points", 
       y = "Probabilidade de Tumor Maligno",
       caption = "A curva mostra a relação não-linear estimada pelo modelo logístico") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank())


# Odds Ratio com intervalo de confiança
cat("\nOdds Ratio com IC 95%:\n")
print(exp(cbind(OR = coef(model_logit_final), confint(model_logit_final))))

# 4.3 Avaliação do modelo
prob <- predict(model_logit_final, type = "response")
roc_obj <- roc(df$target, prob)

plot(roc_obj, 
     main = paste("Curva ROC - AUC =", round(auc(roc_obj), 3)),
     col = "darkorange", lwd = 2)
abline(a = 0, b = 1, col = "navy", lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 3)))

# 5. Comparação Final -----------------------------------------------------

# Tabela comparativa
resultados <- data.frame(
  Modelo = c("Linear Completo", "Linear Simplificado", 
             "Logístico Completo", "Logístico Simplificado"),
  Variáveis = c("6", "2", "6", "2"),
  R2_AIC = c(summary(model_ols)$adj.r.squared, 
             summary(modelo_final)$adj.r.squared,
             AIC(model_logit_full),
             AIC(model_logit_final)),
  AUC = c(NA, NA,
          auc(roc(df$target, predict(model_logit_full, type = "response"))),
          auc(roc_obj))
)

cat("\nResumo Comparativo dos Modelos:\n")
print(resultados)

# 6. Função para Previsão -------------------------------------------------

prever_risco <- function(concave_points, perimeter) {
  new_data <- data.frame(
    mean_concave_points = concave_points,
    worst_perimeter = perimeter
  )
  predict(model_logit_final, newdata = new_data, type = "response")
}

# Exemplo de uso
cat("\nExemplo de Previsão:\n")
cat("Probabilidade para mean_concave_points=0.1 e worst_perimeter=150:",
    prever_risco(1,50), "\n")


# Preparar dados para a curva
logit_curve_data <- data.frame(
  mean_concave_points = seq(min(df$mean_concave_points), 
                            max(df$mean_concave_points), 
                            length.out = 100)
)

# Calcular predições e intervalos de confiança
predictions <- predict(model_logit_final, 
                       newdata = logit_curve_data,
                       type = "link",
                       se.fit = TRUE)

logit_curve_data <- logit_curve_data %>%
  mutate(
    prob = plogis(predictions$fit),
    lower = plogis(predictions$fit - 1.96 * predictions$se.fit),
    upper = plogis(predictions$fit + 1.96 * predictions$se.fit)
  )


# gráfico detalhado -------------------------------------------------------


# Versão corrigida - criando dados para ambas as variáveis

# 1. Criar sequência para mean_concave_points mantendo worst_perimeter constante (na média)
logit_curve_data <- data.frame(
  mean_concave_points = seq(min(df$mean_concave_points), 
                            max(df$mean_concave_points), 
                            length.out = 100),
  worst_perimeter = mean(df$worst_perimeter)  # Mantemos constante na média
)

# 2. Calcular predições
predictions <- predict(model_logit_final, 
                       newdata = logit_curve_data,
                       type = "link",
                       se.fit = TRUE)

# 3. Calcular probabilidades e IC
logit_curve_data <- logit_curve_data %>%
  mutate(
    prob = plogis(predictions$fit),
    lower = plogis(predictions$fit - 1.96 * predictions$se.fit),
    upper = plogis(predictions$fit + 1.96 * predictions$se.fit)
  )

# 4. Gráfico corrigido
ggplot(logit_curve_data, aes(x = mean_concave_points, y = prob)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  geom_line(color = "red", size = 1.5) +
  geom_point(data = df, aes(y = target), alpha = 0.4) +
  labs(title = "Curva de Regressão Logística (worst_perimeter fixo na média)",
       x = "Mean Concave Points",
       y = "Probabilidade de Tumor Maligno") +
  theme_minimal()
# Gráfico detalhado
ggplot() +
  # Dados observados
  geom_point(data = df, 
             aes(x = mean_concave_points, y = target),
             alpha = 0.3, color = "#0072B2", shape = 16) +
  
  # Curva de regressão
  geom_ribbon(data = logit_curve_data,
              aes(x = mean_concave_points, ymin = lower, ymax = upper),
              fill = "#E69F00", alpha = 0.3) +
  
  geom_line(data = logit_curve_data,
            aes(x = mean_concave_points, y = prob),
            color = "#D55E00", size = 1.5) +
  
  # Linha de decisão (threshold = 0.5)
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "#999999") +
  
  # Texto explicativo
  geom_text(aes(x = quantile(df$mean_concave_points, 0.7), y = 0.9,
                label = "Maior probabilidade\nde tumor maligno"),
            size = 3.5, color = "#D55E00") +
  
  geom_text(aes(x = quantile(df$mean_concave_points, 0.3), y = 0.1,
                label = "Maior probabilidade\nde tumor benigno"),
            size = 3.5, color = "#0072B2") +
  
  labs(title = "Relação entre Mean Concave Points e Risco de Tumor Maligno",
       subtitle = "Modelo Logístico com Intervalo de Confiança de 95%",
       x = "Mean Concave Points", 
       y = "Probabilidade Estimada de Tumor Maligno",
       caption = paste("AUC =", round(auc(roc_obj), 3))) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        panel.grid.minor = element_blank())



