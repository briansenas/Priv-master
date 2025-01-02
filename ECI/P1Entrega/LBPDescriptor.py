import numpy as np
import cv2
from torchvision import transforms
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
from sklearn.preprocessing import StandardScaler
import polars as pl
from HogSVM import get_train_test_dataset


class LBPDescriptor:
    def __init__(self, radius=1, neighbours=8, stride=(1, 1), block=None):
        """
        The idea is to do a LBP pass through in the compute method using the parameters.
        :param radius: Select neighbours further away from the center point.
        :param neighbours: Limit the amount of neighbours
        :param stride: Whether to do sequential or "jumpy" strides.
        :param block: Whether to use the full descriptor or concatenated local patches.
        """
        self.radius = radius
        self.neighbours = neighbours
        # The number of points the window contains depends on the radius and neighbours?
        self.points = self.radius * self.neighbours
        # The window size depends of the radius
        self.windowsize = 2 * self.radius + 1
        # The step size is the angle step we will do in the circle given the amount of points to sample
        self.step = 2 * np.pi / self.neighbours
        # We can pre-compute the angles since it is always the same
        self.angles = [n * self.step - np.pi / 2 for n in range(self.neighbours)]
        # We can pre-compute the dx for the neighbouring points
        self.dx = (
            np.round(self.radius * np.cos(self.angles)).astype(np.intp) + self.radius
        )
        self.dy = (
            np.round(self.radius * np.sin(self.angles)).astype(np.intp) + self.radius
        )
        # We can choose whether to skip pixels or to use local patches.
        self.stride = stride
        self.block = block

    def compute(self, img):
        # We fetch the dimension of the image
        height, width, *_ = img.shape
        # We pad the image to make it easier to work with in the edges.
        padded = cv2.copyMakeBorder(
            img,
            top=self.radius,
            left=self.radius,
            right=self.radius,
            bottom=self.radius,
            borderType=cv2.BORDER_CONSTANT,
            value=0,
        )
        # We create a copy of what a lbp image would be filled with zeros
        lbp = np.zeros_like(img)
        # We create a base_2 numpy array for the bitwise operation using np.dot
        base_2 = 2 ** np.arange(self.neighbours)
        # We need to slide the window through the image and do the operations
        for x in range(0, height, self.stride[0]):
            for y in range(0, width, self.stride[1]):
                # We use the padded image to read the values using or dx/dy vectors.
                # With the patch, we compare it with the center value and do the bit operation <<
                lbp[x, y] = np.dot(
                    padded[x + self.dx, y + self.dy] >= img[x, y], base_2
                )
        # We fetch the pixels we computed by the stride
        lbp = lbp[0 : height : self.stride[0], 0 : width : self.stride[1]]
        lbp_height, lbp_width, *_ = lbp.shape
        # Now we need to calculate the histogramas.
        descriptors = []
        size_neigh = 2**self.neighbours
        if not self.block:
            block = (lbp_height, lbp_width)
        else:
            block = self.block
        bins = np.arange(size_neigh)
        # We slide the window again (if block = (h, w) we use the whole image)
        for x in range(0, lbp_height, block[0]):
            for y in range(0, lbp_width, block[1]):
                # We compute the histogram
                hist, _ = np.histogram(
                    lbp[x : x + block[0], y : y + block[1]].flatten(),
                    bins=bins,
                    range=(0, size_neigh),
                )
                # Normalize the histogram
                hist = hist.astype("float32") / hist.sum()
                descriptors.append(hist)
        # We know concatenate all the histogram into 1 flat vector.
        global_descriptor = np.concatenate(descriptors)
        # We return the lbp image (for debugging purpose) and the (histogram)
        return lbp, global_descriptor


if __name__ == "__main__":
    transform = transforms.Compose(
        [
            # By default, I'll set the size to 128x128 for now.
            transforms.Resize((128, 128)),
            transforms.Grayscale(),
            transforms.ToTensor(),
            # transforms.Normalize((0.4921,), (0.2924,)),
        ]
    )
    # Initialize the dataset
    root_dir = "emotions_classification"  # Replace accordingly
    desired_classes = ["sad", "happy"]
    full_dataset, train_dataset, test_dataset = get_train_test_dataset(
        root_dir, transform, desired_classes, descriptor=None
    )

    mylbp = LBPDescriptor()

    svm = SVC()
    scaler = StandardScaler()
    params = {
        "C": [0.1, 1, 10],  # Regularization strength
        "kernel": [
            "linear",
            "rbf",
            "poly",
            "sigmoid",
        ],  # Different kernels (Non linear)
    }
    gs = GridSearchCV(
        svm,
        params,
        refit="f1",
        cv=5,
        scoring=["accuracy", "f1"],
        # return_train_score=True,
    )
    X, y = [], []
    for i in train_dataset.indices:
        image, label, descriptor = full_dataset[i]
        image = np.asarray(image).transpose(1, 2, 0)
        image = (image.reshape(image.shape[:-1]) * 255).astype(np.uint8)
        X += [mylbp.compute(image)[1]]
        y += [label]
    X = scaler.fit_transform(X)
    gs.fit(X, y)
    print(gs.best_score_, gs.best_params_)
    # Create a DataFrame with the results
    results = pl.DataFrame(gs.cv_results_, strict=False)
    results = results.with_columns(
        pl.col("mean_test_f1").round(3), pl.col("mean_test_accuracy").round(3)
    )
    results_sum = results.sort(["param_kernel", "param_C"])[
        "param_kernel", "param_C", "mean_test_f1", "mean_test_accuracy"
    ]
    results_sum.write_csv("csvs/LBPDescriptor.csv")
    print(results_sum)
