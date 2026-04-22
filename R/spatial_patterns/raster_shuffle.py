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
