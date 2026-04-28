import numpy as np
import rasterio
import os

def shuffle_raster(raster_path, percentage, out_name="modified_raster.tif"):
    """
    Randomly shuffles a percentage of raster pixel values.

    Parameters
    ----------
    raster_path : str
        Path to input raster
    percentage : float
        Percentage of pixels to shuffle (0–100)
    out_name : str
        Output filename (saved in same folder)

    Returns
    -------
    str
        Path to output raster
    """

    print(f"Loading raster: {raster_path}")
    print(f"Shuffling {percentage}% of pixels")

    with rasterio.open(raster_path) as src:
        data = src.read(1)
        profile = src.profile

    flat = data.flatten()

    n = flat.size
    k = int((percentage / 100) * n)

    if k <= 0:
        raise ValueError("Percentage too small → no pixels selected")

    idx = np.random.choice(n, k, replace=False)

    shuffled = flat[idx].copy()
    np.random.shuffle(shuffled)

    flat[idx] = shuffled

    new_data = flat.reshape(data.shape)

    folder = os.path.dirname(raster_path)
    out_path = os.path.join(folder, out_name)

    with rasterio.open(out_path, "w", **profile) as dst:
        dst.write(new_data, 1)

    print(f"Export successful: {out_path}")

    return out_path



###### 2D & 3D pixel shuffle class function ######

class ShuffleRaster(object):
    """Randomly shuffle a percentage of values in a raster array."""

    def __init__(self, percentage=10):
        self.percentage = percentage

    def __call__(self, x):
        """
        Parameters
        ----------
        x : np.ndarray
            2D raster array (single band)

        Returns
        -------
        np.ndarray
            Modified raster array
        """

        if self.percentage is None:
            return x

        flat = x.flatten()

        n = flat.size
        k = int((self.percentage / 100) * n)

        if k <= 0:
            return x

        idx = np.random.choice(n, k, replace=False)

        shuffled = flat[idx].copy()
        np.random.shuffle(shuffled)

        flat[idx] = shuffled

        return flat.reshape(x.shape)

























####### 3D Pixel Shuffle class function #####


class ShuffleRaster(object):
    """Shuffle a percentage of pixels in selected bands of a 3D raster."""

    def __init__(self, percentage=10, bands=None):
        self.percentage = percentage
        self.bands = bands  # None = all bands

    def _shuffle_band(self, band):
        flat = band.flatten()

        n = flat.size
        k = int((self.percentage / 100) * n)

        if k <= 0:
            return band

        idx = np.random.choice(n, k, replace=False)

        shuffled = flat[idx].copy()
        np.random.shuffle(shuffled)

        flat[idx] = shuffled

        return flat.reshape(band.shape)

    def __call__(self, x):
        if self.percentage <= 0:
            return x

        # ensure we have 3D input
        if x.ndim != 3:
            raise ValueError("Expected 3D array (H, W, C)")

        bands = self.bands if self.bands is not None else range(x.shape[2])

        for b in bands:
            x[:, :, b] = self._shuffle_band(x[:, :, b])

        return x















######### IMPORTANT!!!! Keep consistency across BANDS!!! #########

import numpy as np

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