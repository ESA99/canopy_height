import numpy as np

class ModifyImage(object):
    """ Perturb an image by a percentage. """

    def __init__(self, percentage=None, decrease=True):
        self.percentage = percentage
        self.decrease=decrease

    def __call__(self, x):
        if self.percentage is None:
            return x
        if self.decrease:
            x = x * (1-self.percentage)
        else:
            x = x * (1+self.percentage)
        return x

class ModifyBands(object):
    """ Perturb bands by a percentage. """

    def __init__(self, bands, percentage=0.05, decrease=True):
        self.bands = bands
        self.percentage = percentage
        self.decrease = decrease

    def __call__(self, x):
        if self.percentage is None:
            return x
        if self.decrease:
            x[:,:,self.bands] = x[:,:,self.bands] * (1-self.percentage)
        else:
            x[:,:,self.bands] = x[:,:,self.bands] * (1+self.percentage)
        return x

class ShiftLatitude:
    """
    Shift Latitude of subtiles, via transforms by X km to North or south.
    """
    def __init__(self, distance_km = 0, direction = "S"):
        self.distance_km = distance_km
        self.direction = direction.upper()
    
        if direction not in ["N", "S"]:
                raise ValueError("shift_direction must be 'N' or 'S'")
    
    def __call__(self, inputs):

        if self.distance_km == 0:
                return inputs

        sign = 1 if self.direction == "N" else -1

        # km -> degrees latitude
        # lat_shift_deg = sign * (self.distance_km / 111.32)

        inputs = inputs.copy()  # Saftey Net - Needed?
        lat_channel = inputs.shape[-1] - 3 #LAT: Third channel from the back in the first dimension (channels)
        # mean latitude of the patch
        mean_lat = np.mean(inputs[..., lat_channel])
        lat_rad = np.deg2rad(mean_lat)

        meters_per_degree = (
            111132.92
            - 559.82 * np.cos(2 * lat_rad)
            + 1.175 * np.cos(4 * lat_rad)
            - 0.0023 * np.cos(6 * lat_rad)
        )
        lat_shift_deg = sign * ( (self.distance_km * 1000.0) / meters_per_degree)

        ### Debugging Print
        # print(
        #     f"[ShiftLatitude] Shifting latitude by "
        #     f"{self.distance_km} km {self.direction} "
        #     f"at {mean_lat:.2f}° -> "
        #     f"{lat_shift_deg:.6f}°"
        # )

        inputs[..., lat_channel] += lat_shift_deg

        return inputs


# class ShuffleRaster:
#     """
#     Applies controlled patch-based shuffling across a raster image.
#     X % of pixels are shuffled as patches of size y, optionally inside 
#     of subtiles of 512x512px (if None across the whole scene).
    
#     Edge remainder pixels are excluded from shuffling but still inside the
#     output image, keeping geographical size and attributes

#     """

#     def __init__(self, percentage=20, patch_size=1, subtile_size=None):
#         self.percentage = percentage
#         self.patch_size = patch_size

#         # If None or 0 → global operation
#         self.subtile_size = subtile_size if subtile_size else None

#     # -------------------------------------------------------------
#     # Helper: apply shuffle inside a given image region
#     # -------------------------------------------------------------
#     def _shuffle_region(self, region):
#         """
#         Applies patch shuffle to a cropped region (H, W, C).
#         Returns shuffled region of same size.
#         """

#         h, w, c = region.shape
#         p = self.patch_size

#         # Crop to full patch grid (ignore leftovers)
#         hh = (h // p) * p
#         ww = (w // p) * p

#         region_core = region[:hh, :ww].copy()

#         # Extract patches
#         patches = []
#         positions = []

#         for y in range(0, hh, p):
#             for x in range(0, ww, p):
#                 patches.append(region_core[y:y+p, x:x+p].copy())
#                 positions.append((y, x))

#         patches = np.array(patches, dtype=object)
#         n = len(patches)

#         # how many patches to shuffle
#         k = int((self.percentage / 100) * n)

#         # nothing to do
#         if k <= 1:
#             out = region.copy()
#             out[:hh, :ww] = region_core
#             return out

#         # Select random subset of patches
#         idx = np.random.choice(n, k, replace=False)

#         # ---------------------------------------------
#         # Derangement helper -> prevents random shuffle to original position
#         # ---------------------------------------------
#         def random_derangement(arr):
#             arr = arr.copy()

#             while True:
#                 perm = arr.copy()
#                 np.random.shuffle(perm)

#                 if not np.any(perm == arr):
#                     return perm

#         shuffled_idx = random_derangement(idx)

#         # apply shuffle mapping
#         new_patches = patches.copy()
#         new_patches[idx] = patches[shuffled_idx]

#         # ---------------------------------------------
#         # Reconstruct region
#         out = region.copy()

#         for patch, (y, x) in zip(new_patches, positions):
#             out[y:y+p, x:x+p] = patch

#         return out

#     # -------------------------------------------------------------
#     # Main call
#     # -------------------------------------------------------------
#     def __call__(self, x):

#         if self.percentage <= 0:
#             return x

#         H, W, C = x.shape

#         # =========================================================
#         # CASE 1: NO SUB-TILING → global shuffle
#         # =========================================================
#         if self.subtile_size is None:
#             return self._shuffle_region(x)

#         # =========================================================
#         # CASE 2: SUB-TILING ACTIVE
#         # =========================================================
#         s = self.subtile_size

#         # crop to full tile grid
#         HH = (H // s) * s
#         WW = (W // s) * s

#         x_core = x[:HH, :WW].copy()
#         out = x.copy()

#         # iterate over tiles
#         for ty in range(0, HH, s):
#             for tx in range(0, WW, s):

#                 tile = x_core[ty:ty+s, tx:tx+s]

#                 # apply patch shuffle independently per tile
#                 shuffled_tile = self._shuffle_region(tile)

#                 out[ty:ty+s, tx:tx+s] = shuffled_tile

#         return out

class ShuffleRaster:
    """
    Applies a controlled spatial patch shuffle consistently 
    across all spectral bands, preserving exact pixel values,
    switching a percentage of patches with each other.
    """

    def __init__(self, percentage=10, shuffle_patch_size=1):
        self.percentage = percentage
        self.shuffle_patch_size = shuffle_patch_size

    def __call__(self, x):

        if self.percentage <= 0:
            return x

        h, w, c = x.shape
        p = self.shuffle_patch_size

        # -------------------------------------------------
        # 1. CROP TO FULL PATCH GRID ONLY
        # -------------------------------------------------
        hh = (h // p) * p
        ww = (w // p) * p

        x_core = x[:hh, :ww].copy()

        # -------------------------------------------------
        # 2. EXTRACT PATCHES WITH EXPLICIT INDEXING
        # -------------------------------------------------
        patches = []
        positions = []

        patch_id = 0

        for y in range(0, hh, p):
            for x_ in range(0, ww, p):

                patches.append(x_core[y:y+p, x_:x_+p].copy())
                positions.append((y, x_))

                patch_id += 1

        patches = np.array(patches, dtype=object)

        n = len(patches)
        k = int((self.percentage / 100) * n)

        if k <= 1:
            out = x.copy()
            out[:hh, :ww] = x_core
            return out

        # -------------------------------------------------
        # 3. SELECT PATCH INDICES
        # -------------------------------------------------
        idx = np.random.choice(n, k, replace=False)

        # true permutation inside selected subset
        shuffled_idx = idx.copy()
        np.random.shuffle(shuffled_idx)

        # -------------------------------------------------
        # 4. APPLY DERANGEMENT (NO FIXED POINTS)
        # -------------------------------------------------

        def random_derangement(arr):
            """
            Returns a permutation of arr with no fixed points.
            """
            arr = arr.copy()
            n = len(arr)

            while True:
                perm = arr.copy()
                np.random.shuffle(perm)

                # check no fixed points
                if not np.any(perm == arr):
                    return perm


        shuffled_idx = random_derangement(idx)

        new_patches = patches.copy()
        new_patches[idx] = patches[shuffled_idx]

        # -------------------------------------------------
        # 5. RECONSTRUCT IMAGE USING ORIGINAL POSITIONS
        # -------------------------------------------------
        out = x.copy()

        for patch, (y, x_) in zip(new_patches, positions):

            out[y:y+p, x_:x_+p] = patch

        return out

class Transformer(object):
    """Calls a list of transforms on an image"""

    def __init__(self, transforms):
        self.transforms = transforms

    def __call__(self, x):
        for transform in self.transforms:
            x = transform(x)
        return x


class Normalize(object):
    """ Normalize tensor with mean and std. """

    def __init__(self, mean, std):
        self.mean = mean
        self.std = std

    def __call__(self, x):
        x = x - self.mean
        x = x / self.std
        return x


class NormalizeVariance(object):
    """ Normalize variance tensor with std. """

    def __init__(self, std):
        self.var = std ** 2

    def __call__(self, x):
        x = x / self.var
        return x


def denormalize(x, mean, std):
    """ Denormalize normalized numpy array with mean and std. """
    x = x * std
    x = x + mean
    return x


def denormalize_variance(x, std):
    """ Denormalize normalized numpy array (representing a variance) with std. """
    x = x * std ** 2
    return x

