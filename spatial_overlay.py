import pandas as pd
import geopandas as gpd
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors

# Read the results dataframe
results = pd.read_excel('E:/Eeshan/evaluation/results/results_without_background_DJF.xlsx',
                        sheet_name='Paper')

# Extract text inside parentheses using regular expressions
results['site'] = results['site'].str.extract(r'\((.*?)\)')

# Forward fill NaN values with previous non-null value
results['site'].fillna(method='ffill', inplace=True)

# List of columns to drop
columns_to_drop = ['obs', 'sim', 'NMB(%)', 'NME(%)']

# Drop the columns from the DataFrame
results = results.drop(columns_to_drop, axis=1)

# Read the site lat lon dataframe
site_list = pd.read_csv('E:/Eeshan/evaluation/site_information.csv')

# List of columns to drop
columns_to_drop = ['Full Name', 'State']
site_list = site_list.drop(columns_to_drop, axis=1)
# Rename the column
site_list = site_list.rename(columns={'Site ID': 'site'})
# Capitalize the column values
site_list['site'] = site_list['site'].str.upper()

# Join the DataFrames based on the common column 'ID'
df = pd.merge(results, site_list, on='site')

# Group the DataFrame by the "dataset" column
grouped = df.groupby('dataset')

# Create separate DataFrames based on unique values of the "dataset" column
df_EDGAR = grouped.get_group('EDGAR')
df_FFDAS = grouped.get_group('FFDAS')
df_ODIAC = grouped.get_group('ODIAC')
df_VULCAN = grouped.get_group('VULCAN')

# Read the shapefile for USA boundaries
usa_shapefile = 'E:/Eeshan/domain_info/us-state-boundaries.shp'
usa = gpd.read_file(usa_shapefile)

# Create a GeoDataFrame for df_EDGAR
geometry = gpd.points_from_xy(df_EDGAR['Latitude'], df_EDGAR['Longitude'])
gdf_EDGAR = gpd.GeoDataFrame(df_EDGAR, geometry=geometry)

# Set the CRS of gdf_EDGAR to match the CRS of the usa GeoDataFrame
gdf_EDGAR.crs = usa.crs

# Create the plot
fig = plt.figure(figsize=(10, 8))
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())

# Add the USA boundaries
ax.add_geometries(usa['geometry'], crs=ccrs.PlateCarree(), facecolor='none', edgecolor='black')

# Define the colormap from blue to white to red
colors = ['blue', 'white', 'red']
cmap = mcolors.LinearSegmentedColormap.from_list('custom_cmap', colors, N=61)

# Set the range for the scatter dots
cbar_min = -30
cbar_max = 30

# Plot the 'MB(ppm)' values as circles with color based on the MB values (custom colormap)
scatter = ax.scatter(gdf_EDGAR['geometry'].x, gdf_EDGAR['geometry'].y, s=50,
                     c=gdf_EDGAR['MB(ppm)'], cmap=cmap, alpha=1,
                     vmin=cbar_min, vmax=cbar_max, edgecolor='black', transform=ccrs.PlateCarree())

# Set the extent of the plot to focus on the USA region
ax.set_extent([-125, -66.5, 20, 50], crs=ccrs.PlateCarree())

# Add the country boundary
ax.coastlines()

# Set the plot title and labels
ax.set_title('Spatial Overlay Plot of MB Values')
ax.set_xlabel('Longitude')
ax.set_ylabel('Latitude')

# Create a colorbar based on the scatter plot
sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(vmin=cbar_min, vmax=cbar_max))
sm.set_array([])  # Set an empty array since we only want the colorbar for the colors

# Add the colorbar
cbar = plt.colorbar(sm, ax=ax, orientation='vertical', fraction=0.03, pad=0.04)
cbar.set_label('MB(ppm)')

# Show the plot
plt.show()
