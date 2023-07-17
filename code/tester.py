#setup 
import pandas as pd
import matplotlib.pyplot as plt

# Assuming you have a DataFrame named 'data' with columns 'important_var' and 'target_var'
data = pd.read_csv('data/daymet/finalWeatherandRegions.csv')

# Visualize the relationship between important_var and target_var
plt.scatter(data['important_var'], data['target_var'])
plt.xlabel('Important Variable')
plt.ylabel('Target Variable')
plt.title('Relationship between Important Variable and Target Variable')
plt.show()

