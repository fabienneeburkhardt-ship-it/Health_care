from ucimlrepo import fetch_ucirepo

# fetch dataset
heart_disease = fetch_ucirepo(id=45)

# data (as pandas dataframes)
X = heart_disease.data.features
y = heart_disease.data.targets

# metadata
print(heart_disease.metadata)

# variable information
print(heart_disease.variables)

import pandas as pd

# Features und Zielvariable zusammenführen
df = pd.concat([X, y], axis=1)

# Als CSV speichern
df.to_csv("heart_disease.csv", index=False)

print("CSV gespeichert!")