import numpy as np
import rasterio
import os

##### Shuffle single pixels #####

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


##### Shuffle global or "local" (within subtiles) #####

class ShuffleRaster(object):
    """
    Shuffle a percentage of pixel positions while keeping
    spectral bands intact.

    If tile_size is given, shuffling is performed independently
    inside each subtile.
    """

    def __init__(self, percentage=10, tile_size=None):
        self.percentage = percentage
        self.tile_size = tile_size

    def _shuffle_pixels(self, pixels):
        """
        Shuffle percentage of rows in a (N, C) pixel array.
        """
        n = pixels.shape[0]
        k = int((self.percentage / 100) * n)

        if k <= 0:
            return pixels

        idx = np.random.choice(n, k, replace=False)

        shuffled = pixels[idx].copy()
        np.random.shuffle(shuffled)

        pixels[idx] = shuffled

        return pixels

    def __call__(self, x):

        if self.percentage <= 0:
            return x

        h, w, c = x.shape

        # ---------------------------------------------------
        # GLOBAL SHUFFLE
        # ---------------------------------------------------
        if self.tile_size is None:

            pixels = x.reshape(-1, c)

            pixels = self._shuffle_pixels(pixels)

            return pixels.reshape(h, w, c)

        # ---------------------------------------------------
        # TILE-LOCAL SHUFFLE
        # ---------------------------------------------------
        tile = self.tile_size

        out = x.copy()

        for y in range(0, h, tile):
            for z in range(0, w, tile):

                y_end = min(y + tile, h)
                z_end = min(z + tile, w)

                subtile = out[y:y_end, z:z_end, :]

                pixels = subtile.reshape(-1, c)

                pixels = self._shuffle_pixels(pixels)

                out[y:y_end, z:z_end, :] = pixels.reshape(
                    y_end - y,
                    z_end - z,
                    c
                )

        return out