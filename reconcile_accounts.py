# -*- coding: utf-8 -*-
"""reconcile_accounts.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1MUv-MmIEYP9XcPQmujq21If-tAF1drVz
"""



from google.colab import drive

# montar o drive pra ter acesso a
# arquivos do drive

drive.mount('/content/drive')

import pandas as pd


t1 = pd.read_csv("/content/drive/MyDrive/py/data/transactions1.csv",
                 names=['date', 'dept', 'value', 'beneficiary'],
                 header=None)
t2 = pd.read_csv("/content/drive/MyDrive/py/data/transactions2.csv",
                 names=['date', 'dept', 'value', 'beneficiary'],
                 header=None)


print(t1)
print(t2)

"""reconcile_accounts
Escreva uma função que faça a conciliação de dois grupos de transações financeiras. Sua função, reconcile_accounts, deve receber duas listas de listas (representando as linhas dos dados financeiros) e deve devolver cópias dessas duas listas de listas com uma nova coluna acrescentada à direita das demais, que designará se a transação pôde ser encontrada (FOUND) na outra lista ou não (MISSING).
As listas de listas representarão os dados em quatro colunas tipo string:

• Data (em formato yyyy-mm-dd)

• Departamento

• Valor

• Beneficiário

Dados o arquivo transactions1.csv :
2020-12-04,Tecnologia,16.00,Bitbucket 2020-12-04,Jurídico,60.00,LinkSquares 2020-12-05,Tecnologia,50.00,AWS
E o arquivo transactions2.csv :
2020-12-04,Tecnologia,16.00,Bitbucket 2020-12-05,Tecnologia,49.99,AWS
2020-12-04,Jurídico,60.00,LinkSquares
Sua função deve funcionar do seguinte modo:
```
>>> import csv
>>> from pathlib import Path
>>> from pprint import pprint
>>> transactions1 = list(csv.reader(Path('transactions1.csv').open()))
>>> transactions2 = list(csv.reader(Path('transactions2.csv').open()))
>>> out1, out2 = reconcile_accounts(transactions1, transactions2)
>>> pprint(out1)
[['2020-12-04', 'Tecnologia', '16.00', 'Bitbucket', 'FOUND'],
['2020-12-04', 'Jurídico', '60.00', 'LinkSquares', 'FOUND'],
['2020-12-05', 'Tecnologia', '50.00', 'AWS', 'MISSING']]


```

Um jeito MUITO SIMPLES, que talvez não fosse o ideal em um problema mais complexo, eu faria algo desse jeito.
"""

def reconcile(t1, t2):
    prep = lambda df: df.assign(date=pd.to_datetime(df.date), id=df.index)
    t1p, t2p = prep(t1), prep(t2)
    m = pd.merge(t1p, t2p, on=['dept','value','beneficiary'])
    m = m[(m.date_x - m.date_y).abs().dt.days <= 1]

    return (
        t1.assign(status=['FOUND' if i in m.id_x else 'MISSING' for i in t1p.id]),
        t2.assign(status=['FOUND' if i in m.id_y else 'MISSING' for i in t2p.id])
    )

reconcile(t1,t2)

"""Assim, seria a função conforme solicitado"""

import pandas as pd
from datetime import datetime

def reconcile_accounts(t1, t2):
    # Função de preparação dos dados
    def prep(df):
        df = df.copy()
        df['date'] = pd.to_datetime(df['date'])
        df['id'] = df.reset_index().index
        df['beneficiary'] = df['beneficiary'].str.rstrip(';')
        return df

    # Preparar os dataframes
    t1p = prep(t1)
    t2p = prep(t2)

    # Encontrar correspondências válidas
    m = pd.merge(t1p, t2p, on=['dept', 'value', 'beneficiary'])
    m = m[abs((m['date_x'] - m['date_y']).dt.days) <= 1]

    # Atualizar status
    result1 = t1p.copy()
    result1['status'] = ['FOUND' if x in m['id_x'].values else 'MISSING' for x in t1p['id']]
    result1 = result1.drop(columns=['id'])

    result2 = t2p.copy()
    result2['status'] = ['FOUND' if x in m['id_y'].values else 'MISSING' for x in t2p['id']]
    result2 = result2.drop(columns=['id'])

    return result1, result2

reconcile_accounts(t1,t2)