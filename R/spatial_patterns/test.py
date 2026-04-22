import numpy as np
import rasterio

with rasterio.open("input.tif") as src:
    data = src.read(1)

flat = data.flatten()

n = flat.size
k = int(0.1 * n)

# choose 10% of indices
idx = np.random.choice(n, k, replace=False)

# shuffle those values
shuffled = flat[idx].copy()
np.random.shuffle(shuffled)

flat[idx] = shuffled

# reshape back
new_data = flat.reshape(data.shape)