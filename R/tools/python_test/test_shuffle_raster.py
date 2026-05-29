import numpy as np
import matplotlib.pyplot as plt
import sys

sys.path.append("/home/emilio/canopy_height")
from R.pixel_shuffle.python_function.test import ShuffleRaster
from gchm.utils.transforms import ModifyBands

# CONFIG
CONFIG = {

    # --------------------------------------------------------
    # MODE
    # "mode": "shuffle",
    "mode": "spectral",

    "shuffle_percentage": 30,
    "patch_size": 64,
    # SUBTILING
        # None or 0 = whole image shuffled globally
        # Integer (e.g. 512) = image split into independent tiles
    # "subtile_size": 512,
    "subtile_size": None,

    # --------------------------------------------------------
    # TEST IMAGE SIZE
    "image_height": 1030,
    "image_width": 1030,

    # SPECTRAL SETTINGS
    "spectral_bands": [1, 2],
    "spectral_percentage": 0.5,
    "spectral_decrease": True,
}


# 1. SYNTHETIC TEST IMAGE
# ------------------------------------------------------------
def create_test_image(h=1000, w=1000, c=3):

    img = np.zeros((h, w, c), dtype=np.uint8)

    # Band 0 -> horizontal gradient
    img[:, :, 0] = np.tile(
        np.linspace(0, 255, w, dtype=np.uint8),
        (h, 1)
    )
    # Band 1 -> vertical gradient
    img[:, :, 1] = np.tile(
        np.linspace(0, 255, h, dtype=np.uint8).reshape(h, 1),
        (1, w)
    )
    # Band 2 -> checkerboard
    for i in range(h):
        for j in range(w):
            img[i, j, 2] = ((i // 10 + j // 10) % 2) * 255

    return img


# 2. VISUALIZATION HELPERS
# ------------------------------------------------------------
def show_images(original, transformed):

    fig, axes = plt.subplots(1, 2, figsize=(12, 6))

    axes[0].imshow(original)
    axes[0].set_title("Original")
    axes[0].axis("off")

    axes[1].imshow(transformed)

    if CONFIG["mode"] == "shuffle":

        title = (
            f"Shuffle {CONFIG['shuffle_percentage']}% | "
            f"Patch {CONFIG['patch_size']}x{CONFIG['patch_size']}"
        )

        # add subtile info to title
        if CONFIG["subtile_size"]:
            title += f" | Subtile {CONFIG['subtile_size']}"

        else:
            title += " | Global"

        axes[1].set_title(title)

    else:

        axes[1].set_title(
            f"Bands {CONFIG['spectral_bands']} modified"
        )

    axes[1].axis("off")

    plt.tight_layout()
    plt.show()


def show_difference_raster(original, transformed):

    diff = np.linalg.norm(
        transformed.astype(np.float32)
        - original.astype(np.float32),
        axis=2
    )

    plt.figure(figsize=(6, 5))

    plt.imshow(diff, cmap="magma")

    plt.colorbar(label="Per-pixel L2 difference")

    plt.title("Difference Raster")

    plt.axis("off")

    plt.show()

    return diff


def show_relative_change(original, transformed):

    eps = 1e-6

    orig = original.astype(np.float32)
    trans = transformed.astype(np.float32)

    rel = np.abs(trans - orig) / (np.abs(orig) + eps)

    plt.figure(figsize=(6, 5))

    plt.imshow(rel.mean(axis=2), cmap="viridis")

    plt.colorbar(label="Relative change")

    plt.title("Relative Spectral Change")

    plt.axis("off")

    plt.show()

    return rel


def show_displacement_mask(original, transformed):

    mask = np.any(original != transformed, axis=2)

    plt.figure(figsize=(6, 6))

    plt.imshow(mask, cmap="gray")

    plt.title("Pixel Displacement Mask")

    plt.axis("off")

    plt.show()

    return mask


# ------------------------------------------------------------
# VISUALIZE CROPPED AREAS
# ------------------------------------------------------------
def visualize_crop_regions(img_shape, patch_size, subtile_size):

    """
    Shows:
    - valid processed region
    - cropped borders

    Very useful for debugging edge behavior.
    """

    H, W = img_shape[:2]

    # --------------------------------------------------------
    # NO SUBTILING
    # --------------------------------------------------------
    if not subtile_size:

        HH = (H // patch_size) * patch_size
        WW = (W // patch_size) * patch_size

    # --------------------------------------------------------
    # WITH SUBTILING
    # --------------------------------------------------------
    else:

        HH = (H // subtile_size) * subtile_size
        WW = (W // subtile_size) * subtile_size

    mask = np.zeros((H, W), dtype=np.uint8)

    mask[:HH, :WW] = 1

    plt.figure(figsize=(6, 6))

    plt.imshow(mask, cmap="gray")

    plt.title(
        "White = processed region\n"
        "Black = cropped / untouched edges"
    )

    plt.axis("off")

    plt.show()


# ------------------------------------------------------------
# 3. STATISTICAL CHECKS
# ------------------------------------------------------------
def verify_pixel_identity(original, transformed, mode):

    orig_sorted = np.sort(original.reshape(-1))
    trans_sorted = np.sort(transformed.reshape(-1))

    if mode == "shuffle":

        # ----------------------------------------------------
        # IMPORTANT:
        # Since edge pixels are untouched,
        # total histogram should remain identical.
        # ----------------------------------------------------
        assert np.array_equal(orig_sorted, trans_sorted), \
            "ERROR: Shuffle must preserve pixel multiset!"

        print("✔ Shuffle: pixel values preserved")

    elif mode == "spectral":

        diff = np.abs(
            original.astype(np.float32)
            - transformed.astype(np.float32)
        )

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

    print("\nPatch-level change:")
    print("  changed patches:", changed_patches)
    print("  total patches:", total_patches)
    print("  ratio:", changed_patches / total_patches)


def pixel_level_change(mask):

    changed = mask.sum()
    total = mask.size

    print("\nPixel-level change:")
    print("  changed pixels:", changed)
    print("  total pixels:", total)
    print("  ratio:", changed / total)


# ------------------------------------------------------------
# PRINT CROPPING INFO
# ------------------------------------------------------------
def print_cropping_info(cfg):

    H = cfg["image_height"]
    W = cfg["image_width"]

    p = cfg["patch_size"]
    s = cfg["subtile_size"]

    print("\n--- CROPPING INFO ---")

    # --------------------------------------------------------
    # GLOBAL MODE
    # --------------------------------------------------------
    if not s:

        HH = (H // p) * p
        WW = (W // p) * p

        print("Mode: GLOBAL SHUFFLE")
        print(f"Patch size: {p}")

        print(f"Input shape: ({H}, {W})")
        print(f"Usable region: ({HH}, {WW})")

        print(
            f"Cropped bottom pixels: {H - HH}"
        )

        print(
            f"Cropped right pixels: {W - WW}"
        )

    # --------------------------------------------------------
    # SUBTILE MODE
    # --------------------------------------------------------
    else:

        HH = (H // s) * s
        WW = (W // s) * s

        tile_hh = (s // p) * p
        tile_ww = (s // p) * p

        print("Mode: SUBTILED SHUFFLE")

        print(f"Subtile size: {s}")
        print(f"Patch size: {p}")

        print(f"\nInput shape: ({H}, {W})")

        print(
            f"Region after subtile crop: ({HH}, {WW})"
        )

        print(
            f"Outer cropped border:"
        )

        print(
            f"  bottom: {H - HH}"
        )

        print(
            f"  right: {W - WW}"
        )

        print("\nInside each subtile:")

        print(
            f"  usable patch region: ({tile_hh}, {tile_ww})"
        )

        print(
            f"  cropped per tile:"
        )

        print(
            f"    bottom: {s - tile_hh}"
        )

        print(
            f"    right: {s - tile_ww}"
        )


# ------------------------------------------------------------
# 4. BUILD TRANSFORMER
# ------------------------------------------------------------
def build_transformer(cfg):

    if cfg["mode"] == "shuffle":

        return ShuffleRaster(
            percentage=cfg["shuffle_percentage"],
            patch_size=cfg["patch_size"],
            subtile_size=cfg["subtile_size"]
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
# 5. MAIN
# ------------------------------------------------------------
def main():

    np.random.seed(42)

    # --------------------------------------------------------
    # CREATE TEST IMAGE
    # --------------------------------------------------------
    img = create_test_image(
        h=CONFIG["image_height"],
        w=CONFIG["image_width"]
    )

    # --------------------------------------------------------
    # BUILD TRANSFORM
    # --------------------------------------------------------
    transformer = build_transformer(CONFIG)

    transformed = transformer(img.copy())

    # --------------------------------------------------------
    # PRINT CONFIG
    # --------------------------------------------------------
    print("\n--- TRANSFORM CONFIG ---")

    for k, v in CONFIG.items():
        print(f"{k}: {v}")

    print("------------------------")

    print("\nOriginal shape:", img.shape)
    print("Transformed shape:", transformed.shape)

    # --------------------------------------------------------
    # PRINT CROPPING DEBUG INFO
    # --------------------------------------------------------
    if CONFIG["mode"] == "shuffle":
        print_cropping_info(CONFIG)

    # --------------------------------------------------------
    # CHECKS
    # --------------------------------------------------------
    verify_pixel_identity(
        img,
        transformed,
        CONFIG["mode"]
    )

    print(
        "\nExact equality (should usually be False):",
        np.all(img == transformed)
    )

    if CONFIG["mode"] == "shuffle":

        patch_level_change(
            img,
            transformed,
            transformer.patch_size
        )

    # --------------------------------------------------------
    # VISUALIZATION
    # --------------------------------------------------------

    # uncomment if needed
    # visualize_crop_regions(
    #     img.shape,
    #     CONFIG["patch_size"],
    #     CONFIG["subtile_size"]
    # )

    mask = show_displacement_mask(img, transformed)

    pixel_level_change(mask)

    diff = show_difference_raster(img, transformed)

    print("\nDifference stats:")
    print("  mean:", diff.mean())
    print("  max:", diff.max())
    print("  std:", diff.std())

    show_images(img, transformed)


if __name__ == "__main__":
    main()