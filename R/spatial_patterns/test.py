import numpy as np
import rasterio
import os

RASTER_PATH = os.getenv("RASTER_PATH")
PERCENTAGE = float(os.getenv("PERCENTAGE"))

print("RASTER_PATH:", RASTER_PATH)
print("PERCENTAGE:", PERCENTAGE)

with rasterio.open(RASTER_PATH) as src:
    data = src.read(1)
    profile = src.profile

print("Raster loaded, start flattening")

flat = data.flatten()

p = PERCENTAGE

n = flat.size
k = int((p / 100) * n)

# choose 10% of indices
idx = np.random.choice(n, k, replace=False)

# shuffle those values
shuffled = flat[idx].copy()
np.random.shuffle(shuffled)

flat[idx] = shuffled

print("{p} % of pixels shuffled.")

# reshape back
new_data = flat.reshape(data.shape)

print("Raster rebuild.")

# create new filename in same folder
folder = os.path.dirname(RASTER_PATH)
new_path = os.path.join(folder, "modified_raster.tif")

# write output
with rasterio.open(new_path, "w", **profile) as dst:
    dst.write(new_data, 1)

print("Export succesfull.")
