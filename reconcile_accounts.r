library(tidyverse)

reconcile_accounts <- function(df1, df2) {
  # Converter datas para formato Date e remover ponto e vírgula
  df1 <- df1 %>%
    mutate(date = as.Date(date),
           beneficiary = str_remove(beneficiary, ";$"))
  
  df2 <- df2 %>%
    mutate(date = as.Date(date),
           beneficiary = str_remove(beneficiary, ";$"))
  
  # Adicionar coluna de status
  df1$status <- "MISSING"
  df2$status <- "MISSING"
  
  # Parear transações
  for (i in 1:nrow(df1)) {
    # Encontrar correspondências válidas
    matches <- which(
      df2$dept == df1$dept[i] &  # Departamento
        df2$value == df1$value[i] &  # Valor
        df2$beneficiary == df1$beneficiary[i] &  # Beneficiário
        abs(as.numeric(df2$date - df1$date[i])) <= 1 &  # Data ±1 dia
        df2$status == "MISSING"  # Ainda não pareada
    )
    
    if (length(matches) > 0) {
      # Pegar a correspondência mais antiga
      oldest_match <- matches[which.min(df2$date[matches])]
      df1$status[i] <- "FOUND"
      df2$status[oldest_match] <- "FOUND"
    }
  }
  
  # Retornar os data frames sem modificações de tipo
  list(df1, df2)
}

# Carregar os dados corretamente
t1 <- read_csv("data/transactions1.csv",
               col_names = c("date", "dept", "value", "beneficiary")) %>% as_tibble()
t2 <- read_csv("data/transactions2.csv", col_names = c("date", "dept", "value", "beneficiary")) %>% as_tibble()

# Executar reconciliação
result <- reconcile_accounts(t1, t2)

# Visualizar resultados
print(result[[1]])  # transactions1 com status
print(result[[2]])  # transactions2 com status

 t1 %>%
  mutate(status2= if_else(
         t1$dept == t2$dept & t1$date == t2$date & t1$value == t2$value & t1$beneficiary == t2$beneficiary
         , true = "FOUND",
        false= "MISSING"))
t2

t1
print(t1)


reconcile_simple <- function(t1, t2) {
  prep <- function(df) df %>%
    mutate(date=ymd(date),
           id=row_number(),
           beneficiary=str_remove(beneficiary, ";$"))
  
  t1p <- prep(t1);
  
  t2p <- prep(t2)
  
  m <- inner_join(t1p, t2p, by=c("dept","value","beneficiary")) %>%
    filter(abs(date.x-date.y)<=1)
  
  list(
    t1p %>% mutate(status=ifelse(id %in% m$id.x, "FOUND", "MISSING")) %>% select(-id),
    t2p %>% mutate(status=ifelse(id %in% m$id.y, "FOUND", "MISSING")) %>% select(-id)
  )
}

reconcile_simple(t1,t2)
