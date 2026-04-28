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

