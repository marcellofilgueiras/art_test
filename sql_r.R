reticulate::repl_python()


import sqlite3
import pandas as pd

# Conectar ao arquivo enviado
con = sqlite3.connect("/mnt/data/Loja.sqlite")

# Listar tabelas disponíveis
tabelas = pd.read_sql_query("SELECT name FROM sqlite_master WHERE type='table';", con)

tabelas
#  1ª Função --------------------------------------------------------------

## 1 - Escreva uma consulta SQL que mostre a lista de todos os clientes
# e seus respectivos pedidos, mesmo que o cliente não tenha feito nenhum pedido. 
# A tabela resultante deve conter as colunas: Nome, PedidoID, DataPedido e ValorTotal.


