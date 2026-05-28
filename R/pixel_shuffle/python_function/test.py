import numpy as np


class ShuffleRaster:
    """
    Fully correct, bijective patch shuffle.

    GUARANTEES:
    - No pixel creation or loss
    - Exact patch permutation
    - No ordering ambiguity
    - No edge corruption inside valid patch grid
    - Strict 1-to-1 mapping of patches
    """

    def __init__(self, percentage=10, patch_size=1):
        self.percentage = percentage
        self.patch_size = patch_size

    def __call__(self, x):

        if self.percentage <= 0:
            return x

        h, w, c = x.shape
        p = self.patch_size

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