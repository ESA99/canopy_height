import numpy as np
import matplotlib.pyplot as plt
import sys
import os
sys.path.append("/home/emilio/canopy_height")
from gchm.utils.transforms import ShuffleRaster

def create_test_image(h=100, w=100, c=3):
    """
    Create a synthetic image with clear spatial structure
    so shuffling is visually obvious.
    """
    img = np.zeros((h, w, c), dtype=np.uint8)

    # Band 0: horizontal gradient
    img[:, :, 0] = np.tile(np.linspace(0, 255, w, dtype=np.uint8), (h, 1))

    # Band 1: vertical gradient
    img[:, :, 1] = np.tile(np.linspace(0, 255, h, dtype=np.uint8).reshape(h, 1), (1, w))

    # Band 2: checkerboard pattern
    for i in range(h):
        for j in range(w):
            img[i, j, 2] = ((i // 10 + j // 10) % 2) * 255

    return img


def show_images(original, transformed, percentage):
    fig, axes = plt.subplots(1, 2, figsize=(10, 5))

    axes[0].imshow(original)
    axes[0].set_title("Original")
    axes[0].axis("off")

    axes[1].imshow(transformed)
    axes[1].set_title(f"Shuffled ({percentage}%)")
    axes[1].axis("off")

    plt.tight_layout()
    plt.show()


def main():
    np.random.seed(42)  # for reproducibility

    img = create_test_image()

    transformer = ShuffleRaster(percentage=80)
    shuffled_img = transformer(img.copy())

    # Basic sanity checks
    print("Original shape:", img.shape)
    print("Shuffled shape:", shuffled_img.shape)
    
    original_mean = img.mean(axis=(0, 1))
    shuffled_mean = shuffled_img.mean(axis=(0, 1))

    print("Original per-band mean:", original_mean)
    print("Shuffled per-band mean:", shuffled_mean)
    print("Difference:", np.abs(original_mean - shuffled_mean))


    # Check that values are preserved (same pixels, rearranged)
    assert sorted(img.flatten()) == sorted(shuffled_img.flatten()), "Pixel values changed!"

    print("Pixel values preserved ✔")

    show_images(img, shuffled_img, transformer.percentage)


if __name__ == "__main__":
    main()
