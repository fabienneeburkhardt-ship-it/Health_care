# Erstellung des Dataframes mit den Original-Beschreibungen (UCI)
heart_disease_info <- data.frame(
  Variable = c(
    "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
    "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num"
  ),
  Description = c(
    "age in years",
    "sex (1 = male; 0 = female)",
    "chest pain type (1: typical angina, 2: atypical angina, 3: non-anginal pain, 4: asymptomatic)",
    "resting blood pressure (in mm Hg on admission to the hospital)",
    "serum cholestoral in mg/dl",
    "(fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)",
    "resting electrocardiographic results (0: normal, 1: ST-T wave abnormality, 2: probable or definite left ventricular hypertrophy)",
    "maximum heart rate achieved",
    "exercise induced angina (1 = yes; 0 = no)",
    "ST depression induced by exercise relative to rest",
    "the slope of the peak exercise ST segment (1: upsloping, 2: flat, 3: downsloping)",
    "number of major vessels (0-3) colored by flourosopy",
    "thal: 3 = normal; 6 = fixed defect; 7 = reversable defect",
    "diagnosis of heart disease (angiographic disease status)"
  )
)

# Anzeige der Tabelle
print(heart_disease_info)