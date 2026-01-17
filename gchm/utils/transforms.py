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

