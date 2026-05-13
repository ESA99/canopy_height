import numpy as np
import rasterio
import os

RASTER_PATH = os.getenv("RASTER_PATH")
PERCENTAGE = float(os.getenv("PERCENTAGE"))

print("RASTER_PATH:", RASTER_PATH)
print("PERCENTAGE:", PERCENTAGE)



class ShuffleRaster(object):
    """Shuffle a percentage of pixel positions while keeping spectral bands intact."""

    def __init__(self, percentage=10):
        self.percentage = percentage

    def __call__(self, x):
        if self.percentage <= 0:
            return x

        h, w, c = x.shape

        # reshape: each pixel is a vector of length C
        pixels = x.reshape(-1, c)

        n = pixels.shape[0]
        k = int((self.percentage / 100) * n)

        if k <= 0:
            return x

        idx = np.random.choice(n, k, replace=False)

        shuffled = pixels[idx].copy()
        np.random.shuffle(shuffled)

        pixels[idx] = shuffled

        return pixels.reshape(h, w, c)

