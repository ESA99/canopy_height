import numpy as np
import matplotlib.pyplot as plt
import sys

# sys.path.append("/home/emilio/canopy_height")
# from R.pixel_shuffle.python_function.test import ShuffleRaster
from gchm.utils.transforms import ShuffleRaster
from gchm.utils.transforms import ModifyBands

CONFIG = {
    # "mode": "shuffle",
    "mode": "spectral",

    # shuffle params
    "shuffle_percentage": 30,
    "patch_size": 16,

    # spectral params
    "spectral_bands": [1, 2],
    "spectral_percentage": 0.5,
    "spectral_decrease": True,
}

# 1. SYNTHETIC TEST IMAGE
def create_test_image(h=1000, w=1000, c=3):
    img = np.zeros((h, w, c), dtype=np.uint8)
    # Band 0: horizontal gradient
    img[:, :, 0] = np.tile(
        np.linspace(0, 255, w, dtype=np.uint8),
        (h, 1)
    )
    # Band 1: vertical gradient
    img[:, :, 1] = np.tile(
        np.linspace(0, 255, h, dtype=np.uint8).reshape(h, 1),
        (1, w)
    )
    # Band 2: checkerboard
    for i in range(h):
        for j in range(w):
            img[i, j, 2] = ((i // 10 + j // 10) % 2) * 255
    return img


# 2. VISUALIZATION HELPERS
def show_images(original, transformed, percentage, patch_size):

    fig, axes = plt.subplots(1, 2, figsize=(10, 5))

    axes[0].imshow(original)
    axes[0].set_title("Original")
    axes[0].axis("off")

    axes[1].imshow(transformed)
    if CONFIG["mode"] == "shuffle":
        axes[1].set_title(
            f"Shuffled {percentage}% | Patch {patch_size}×{patch_size}")
    else:
        axes[1].set_title(f"Bands {CONFIG["spectral_bands"]} increased by factor {CONFIG["spectral_percentage"]}")

    axes[1].axis("off")

    plt.tight_layout()
    plt.show()

def show_difference_raster(original, transformed):

    diff = np.linalg.norm(
        transformed.astype(np.float32) - original.astype(np.float32),
        axis=2
    )

    plt.figure(figsize=(6, 5))
    plt.imshow(diff, cmap="magma")
    plt.colorbar(label="Per-pixel L2 difference")
    plt.title("Difference Raster (Generalized Change Map)")
    plt.axis("off")
    plt.show()

    return diff

def show_relative_change(original, transformed):

    eps = 1e-6

    orig = original.astype(np.float32)
    trans = transformed.astype(np.float32)

    rel = np.abs(trans - orig) / (np.abs(orig) + eps)

    plt.figure(figsize=(6,5))
    plt.imshow(rel.mean(axis=2), cmap="viridis")
    plt.colorbar(label="Relative change")
    plt.title("Relative Spectral Change")
    plt.axis("off")
    plt.show()

    return rel

def show_displacement_mask(original, transformed):

    mask = np.any(original != transformed, axis=2)

    plt.figure(figsize=(5, 5))
    plt.imshow(mask, cmap="gray")
    plt.title("Pixel Displacement Mask (True spatial change)")
    plt.axis("off")
    plt.show()

    return mask


# 3. STATISTICAL CHECKS
def verify_pixel_identity(original, transformed, mode):
    orig_sorted = np.sort(original.reshape(-1))
    trans_sorted = np.sort(transformed.reshape(-1))

    if mode == "shuffle":
        assert np.array_equal(orig_sorted, trans_sorted), \
            "ERROR: Shuffle must preserve pixel multiset!"
        print("✔ Shuffle: pixel values preserved")

    elif mode == "spectral":
        # spectral transforms intentionally change values
        diff = np.abs(original.astype(np.float32) - transformed.astype(np.float32))
        print("✔ Spectral: pixel values modified as expected")
        print("Mean absolute change:", diff.mean())

    else:
        raise ValueError(f"Unknown mode: {mode}")


def patch_level_change(original, transformed, patch_size):

    h, w, _ = original.shape
    p = patch_size

    hh = (h // p) * p
    ww = (w // p) * p

    changed_patches = 0
    total_patches = 0

    for y in range(0, hh, p):
        for x in range(0, ww, p):

            o = original[y:y+p, x:x+p]
            t = transformed[y:y+p, x:x+p]

            if not np.array_equal(o, t):
                changed_patches += 1

            total_patches += 1

    print("Patch-level change:")
    print("  changed patches:", changed_patches)
    print("  total patches:", total_patches)
    print("  ratio:", changed_patches / total_patches)


def pixel_level_change(mask):

    changed = mask.sum()
    total = mask.size

    print("Pixel-level change:")
    print("  changed pixels:", changed)
    print("  total pixels:", total)
    print("  ratio:", changed / total)


def build_transformer(cfg):

    if cfg["mode"] == "shuffle":
        return ShuffleRaster(
            percentage=cfg["shuffle_percentage"],
            patch_size=cfg["patch_size"]
        )

    elif cfg["mode"] == "spectral":
        return ModifyBands(
            bands=cfg["spectral_bands"],
            percentage=cfg["spectral_percentage"],
            decrease=cfg["spectral_decrease"]
        )

    else:
        raise ValueError(f"Unknown mode: {cfg['mode']}")

# ------------------------------------------------------------
# 4. MAIN
# ------------------------------------------------------------
def main():

    np.random.seed(42)

    img = create_test_image()

    # percentage = 30
    # patch_size = 16

    transformer = build_transformer(CONFIG)
    shuffled = transformer(img.copy())

    print("\n--- TRANSFORM CONFIG ---")
    for k, v in CONFIG.items():
        print(f"{k}: {v}")
    print("------------------------")
    
    print("Original shape:", img.shape)
    print("Transformed shape:", shuffled.shape)

    # CHECK 1: pixel identity
    verify_pixel_identity(img, shuffled, CONFIG["mode"])

    # CHECK 2: exact match (should be False)
    print("Exact equality (should be False):", np.all(img == shuffled))

    # CHECK 3: patch-level correctness
    if CONFIG["mode"] == "shuffle":
        patch_level_change(img, shuffled, transformer.patch_size)

    # CHECK 4: pixel-level displacement
    # mask = show_displacement_mask(img, shuffled)
    # pixel_level_change(mask)

    # diff = show_difference_raster(img, shuffled)
    diff = show_relative_change(img, shuffled)

    print("Difference stats:")
    print("  mean:", diff.mean())
    print("  max:", diff.max())
    print("  std:", diff.std())

    # VISUALIZATION
    if CONFIG["mode"] == "shuffle":
        show_images(
            img,
            shuffled,
            transformer.percentage,
            CONFIG["patch_size"]
        )
    else:
        show_images(
            img,
            shuffled,
            transformer.percentage,
            None
        )


if __name__ == "__main__":
    main()
