import numpy as np
import pandas as pd

import matplotlib.pyplot as plt

df = pd.read_csv("owid-covid-data.csv")

df.dropna(subset=["total_vaccinations"], inplace=True)

column_names = ['location',
                'date',
                'total_cases',
                'new_cases',
                'total_deaths',
                'new_deaths',
                'total_vaccinations']

df['date'] = pd.to_datetime(df['date'], format='%Y-%m-%d')
df.drop(df.columns.difference(column_names), axis=1, inplace=True)
df = df[df['total_vaccinations'] > 1000]
df = df[df['location'] != 'World']
df = df[df['new_cases'] >= 0]
df = df[df['new_deaths'] >= 0]
df = df[df['date'] >= "2021-1-1"]
df = df[df['date'] < "2021-10-1"]
df.dropna(inplace=True)

total_deaths = df['total_deaths']
new_cases = df['new_cases']
new_deaths = df['new_deaths']
total_vaccinations = df['total_vaccinations']

df.to_csv('preprocessed_covid_data.csv', index=False)

def preprocess_month(df, filename, n_month, n_days):
    df_month = df[df['date'].dt.month == n_month]
    df_month = df_month.groupby('location').filter(lambda x: len(x) == n_days)
    filename = 'covid_data_per_month/' + filename
    df_month.to_csv(filename, index=False)
    return df_month

def preprocess_complete_countries(df, country_name, filename):
    df_country = df[df['location'] == country_name]
    df_country.drop(['location'], axis=1, inplace=True)
    filename = 'covid_data_per_country/' + filename
    df_country.to_csv(filename, index=False)
    return df_country

month_filenames = ['covid_data_january.csv',
                   'covid_data_february.csv',
                   'covid_data_march.csv',
                   'covid_data_april.csv',
                   'covid_data_may.csv',
                   'covid_data_june.csv',
                   'covid_data_july.csv',
                   'covid_data_august.csv',
                   'covid_data_september.csv']

month_n_days = [31, 28, 31, 30, 31, 30, 31, 31, 30]

total = np.sum(month_n_days)

month_names = ['January',
               'February',
               'March',
               'April',
               'May',
               'June',
               'July',
               'August',
               'September']

df_months = []

for n_month, [filename, n_day] in enumerate(zip(month_filenames, month_n_days)):
    df_months.append(preprocess_month(df, filename, n_month+1, n_day))

countries = []

for df_month in df_months:
    countries.append(df_month['location'].unique())

test = np.concatenate(countries)
uniques, counts = np.unique(test, return_counts=True)

complete_countries = []

for unique, count in zip(uniques, counts):
    if count == 9:
        country_dict = {'country_name': unique,
                        'filename': "covid_data_" + unique.lower().replace(' ', '_') + '.csv'}
        complete_countries.append(country_dict)

df_countries = []

for country in complete_countries:
    df_countries.append(preprocess_complete_countries(df, country['country_name'], country['filename']))
        
    
fig, axs = plt.subplots(9, 2, figsize=(40,40))

for n_month, df_month in enumerate(df_months):
    df_month = df_month[df['location'] == 'Asia']
    axs[n_month, 0].scatter(df_month['total_vaccinations'], df_month['new_cases'])
    
    axs[n_month, 1].scatter(df_month['total_vaccinations'], df_month['new_deaths'])

plt.show()