import numpy as np

class ShuffleRaster:
    """
    Applies controlled patch-based shuffling across a raster image.

    Improvements over original:
    ------------------------------------
    1. Edge handling:
       - Instead of ignoring leftover pixels, we now CROP them away.
       - Only full patch grids are processed.

    2. Optional sub-tiling:
       - Image can be split into larger tiles (e.g. 512x512).
       - Patch shuffling is applied independently within each tile.
       - Tiles at edges are cropped if they don't fit exactly.

    3. Patch shuffling remains identical inside each region:
       - same derangement logic
       - same percentage behavior
    """

    def __init__(self, percentage=10, patch_size=1, subtile_size=None):
        self.percentage = percentage
        self.patch_size = patch_size

        # If None or 0 → global operation
        self.subtile_size = subtile_size if subtile_size else None

    # -------------------------------------------------------------
    # Helper: apply shuffle inside a given image region
    # -------------------------------------------------------------
    def _shuffle_region(self, region):
        """
        Applies patch shuffle to a cropped region (H, W, C).
        Returns shuffled region of same size.
        """

        h, w, c = region.shape
        p = self.patch_size

        # ---------------------------------------------
        # Crop to full patch grid (ignore leftovers)
        # ---------------------------------------------
        hh = (h // p) * p
        ww = (w // p) * p

        region_core = region[:hh, :ww].copy()

        # ---------------------------------------------
        # Extract patches
        # ---------------------------------------------
        patches = []
        positions = []

        for y in range(0, hh, p):
            for x in range(0, ww, p):
                patches.append(region_core[y:y+p, x:x+p].copy())
                positions.append((y, x))

        patches = np.array(patches, dtype=object)
        n = len(patches)

        # how many patches to shuffle
        k = int((self.percentage / 100) * n)

        # nothing to do
        if k <= 1:
            out = region.copy()
            out[:hh, :ww] = region_core
            return out

        # ---------------------------------------------
        # Select random subset of patches
        # ---------------------------------------------
        idx = np.random.choice(n, k, replace=False)

        # ---------------------------------------------
        # Derangement helper (no fixed points)
        # ---------------------------------------------
        def random_derangement(arr):
            arr = arr.copy()

            while True:
                perm = arr.copy()
                np.random.shuffle(perm)

                if not np.any(perm == arr):
                    return perm

        shuffled_idx = random_derangement(idx)

        # apply shuffle mapping
        new_patches = patches.copy()
        new_patches[idx] = patches[shuffled_idx]

        # ---------------------------------------------
        # Reconstruct region
        # ---------------------------------------------
        out = region.copy()

        for patch, (y, x) in zip(new_patches, positions):
            out[y:y+p, x:x+p] = patch

        return out

    # -------------------------------------------------------------
    # Main call
    # -------------------------------------------------------------
    def __call__(self, x):

        if self.percentage <= 0:
            return x

        H, W, C = x.shape

        # =========================================================
        # CASE 1: NO SUB-TILING → global shuffle
        # =========================================================
        if self.subtile_size is None:
            return self._shuffle_region(x)

        # =========================================================
        # CASE 2: SUB-TILING ACTIVE
        # =========================================================
        s = self.subtile_size

        # crop to full tile grid
        HH = (H // s) * s
        WW = (W // s) * s

        x_core = x[:HH, :WW].copy()
        out = x.copy()

        # iterate over tiles
        for ty in range(0, HH, s):
            for tx in range(0, WW, s):

                tile = x_core[ty:ty+s, tx:tx+s]

                # apply patch shuffle independently per tile
                shuffled_tile = self._shuffle_region(tile)

                out[ty:ty+s, tx:tx+s] = shuffled_tile

        return out







# # Patch sized shuffle to X %, edge pixels are ignored but included
# class ShuffleRaster:
#     """
#     Applies a controlled spatial patch shuffle consistently 
#     across all spectral bands, preserving exact pixel values,
#     switching a percentage of patches with each other.
#     """

#     def __init__(self, percentage=10, patch_size=1):
#         self.percentage = percentage
#         self.patch_size = patch_size

#     def __call__(self, x):

#         if self.percentage <= 0:
#             return x

#         h, w, c = x.shape
#         p = self.patch_size

#         # -------------------------------------------------
#         # 1. CROP TO FULL PATCH GRID ONLY
#         # -------------------------------------------------
#         hh = (h // p) * p
#         ww = (w // p) * p

#         x_core = x[:hh, :ww].copy()

#         # -------------------------------------------------
#         # 2. EXTRACT PATCHES WITH EXPLICIT INDEXING
#         # -------------------------------------------------
#         patches = []
#         positions = []

#         patch_id = 0

#         for y in range(0, hh, p):
#             for x_ in range(0, ww, p):

#                 patches.append(x_core[y:y+p, x_:x_+p].copy())
#                 positions.append((y, x_))

#                 patch_id += 1

#         patches = np.array(patches, dtype=object)

#         n = len(patches)
#         k = int((self.percentage / 100) * n)

#         if k <= 1:
#             out = x.copy()
#             out[:hh, :ww] = x_core
#             return out

#         # -------------------------------------------------
#         # 3. SELECT PATCH INDICES
#         # -------------------------------------------------
#         idx = np.random.choice(n, k, replace=False)

#         # true permutation inside selected subset
#         shuffled_idx = idx.copy()
#         np.random.shuffle(shuffled_idx)

#         # -------------------------------------------------
#         # 4. APPLY DERANGEMENT (NO FIXED POINTS)
#         # -------------------------------------------------

#         def random_derangement(arr):
#             """
#             Returns a permutation of arr with no fixed points.
#             """
#             arr = arr.copy()
#             n = len(arr)

#             while True:
#                 perm = arr.copy()
#                 np.random.shuffle(perm)

#                 # check no fixed points
#                 if not np.any(perm == arr):
#                     return perm


#         shuffled_idx = random_derangement(idx)

#         new_patches = patches.copy()
#         new_patches[idx] = patches[shuffled_idx]

#         # -------------------------------------------------
#         # 5. RECONSTRUCT IMAGE USING ORIGINAL POSITIONS
#         # -------------------------------------------------
#         out = x.copy()

#         for patch, (y, x_) in zip(new_patches, positions):

#             out[y:y+p, x_:x_+p] = patch

#         return out

# class Transformer(object):
#     """Calls a list of transforms on an image"""

#     def __init__(self, transforms):
#         self.transforms = transforms

#     def __call__(self, x):
#         for transform in self.transforms:
#             x = transform(x)
#         return x








# class ShuffleRaster:
#     """
#     Fully correct, bijective patch shuffle.

#     GUARANTEES:
#     - No pixel creation or loss
#     - Exact patch permutation
#     - No ordering ambiguity
#     - No edge corruption inside valid patch grid
#     - Strict 1-to-1 mapping of patches
#     """

#     def __init__(self, percentage=10, patch_size=1):
#         self.percentage = percentage
#         self.patch_size = patch_size

#     def __call__(self, x):

#         if self.percentage <= 0:
#             return x

#         h, w, c = x.shape
#         p = self.patch_size

#         # -------------------------------------------------
#         # 1. CROP TO FULL PATCH GRID ONLY
#         # -------------------------------------------------
#         hh = (h // p) * p
#         ww = (w // p) * p

#         x_core = x[:hh, :ww].copy()

#         # -------------------------------------------------
#         # 2. EXTRACT PATCHES WITH EXPLICIT INDEXING
#         # -------------------------------------------------
#         patches = []
#         positions = []

#         patch_id = 0

#         for y in range(0, hh, p):
#             for x_ in range(0, ww, p):

#                 patches.append(x_core[y:y+p, x_:x_+p].copy())
#                 positions.append((y, x_))

#                 patch_id += 1

#         patches = np.array(patches, dtype=object)

#         n = len(patches)
#         k = int((self.percentage / 100) * n)

#         if k <= 1:
#             out = x.copy()
#             out[:hh, :ww] = x_core
#             return out

#         # -------------------------------------------------
#         # 3. SELECT PATCH INDICES
#         # -------------------------------------------------
#         idx = np.random.choice(n, k, replace=False)

#         # true permutation inside selected subset
#         shuffled_idx = idx.copy()
#         np.random.shuffle(shuffled_idx)

#         # -------------------------------------------------
#         # 4. APPLY DERANGEMENT (NO FIXED POINTS)
#         # -------------------------------------------------

#         def random_derangement(arr):
#             """
#             Returns a permutation of arr with no fixed points.
#             """
#             arr = arr.copy()
#             n = len(arr)

#             while True:
#                 perm = arr.copy()
#                 np.random.shuffle(perm)

#                 # check no fixed points
#                 if not np.any(perm == arr):
#                     return perm


#         shuffled_idx = random_derangement(idx)

#         new_patches = patches.copy()
#         new_patches[idx] = patches[shuffled_idx]

#         # -------------------------------------------------
#         # 5. RECONSTRUCT IMAGE USING ORIGINAL POSITIONS
#         # -------------------------------------------------
#         out = x.copy()

#         for patch, (y, x_) in zip(new_patches, positions):

#             out[y:y+p, x_:x_+p] = patch

#         return out










# class ShuffleRaster:
#     """
#     Patch shuffle that preserves full image shape,
#     including edge (partial) patches.
#     """

#     def __init__(self, percentage=10, patch_size=1):
#         self.percentage = percentage
#         self.patch_size = patch_size

#     def __call__(self, x):

#         if self.percentage <= 0:
#             return x

#         h, w, c = x.shape
#         p = self.patch_size

#         patches = []
#         positions = []

#         # -------------------------------------------------
#         # EXTRACT PATCHES (INCLUDING EDGES)
#         # -------------------------------------------------
#         for y in range(0, h, p):
#             for x_ in range(0, w, p):

#                 patch = x[y:y+p, x_:x_+p, :]

#                 patches.append(patch)
#                 positions.append((y, x_))

#         patches = np.array(patches, dtype=object)

#         n = len(patches)
#         k = int((self.percentage / 100) * n)

#         if k <= 1:
#             return x

#         # -------------------------------------------------
#         # SELECT PATCHES TO SHUFFLE
#         # -------------------------------------------------
#         idx = np.random.choice(n, k, replace=False)

#         shuffled_idx = idx.copy()
#         np.random.shuffle(shuffled_idx)

#         # -------------------------------------------------
#         # SWAP PATCHES
#         # -------------------------------------------------
#         patches[idx] = patches[shuffled_idx].copy()

#         # -------------------------------------------------
#         # REBUILD IMAGE
#         # -------------------------------------------------
#         out = np.zeros_like(x)

#         for patch, (y, x_) in zip(patches, positions):
#             ph, pw = patch.shape[:2]
#             out[y:y+ph, x_:x_+pw, :] = patch

#         return out