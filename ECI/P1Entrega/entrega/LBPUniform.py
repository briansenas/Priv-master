import numpy as np
import cv2
from torchvision import transforms
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
from sklearn.preprocessing import StandardScaler
import polars as pl
from HogSVM import get_train_test_dataset


# NOTE: We can probably clean up the clases by using hierarchy or even setting the lookup table for basic one as lambda x: x
class LBPUniform:
    def __init__(self, radius=1, neighbours=8, stride=(1, 1), block=None):
        """
        The idea is to do a LBP pass through in the compute method using the parameters.
        :param radius: Select neighbours further away from the center point.
        :param neighbours: Limit the amount of neighbours
        :param stride: Whether to do sequential or "jumpy" strides.
        :param block: Whether to use the fulggl descriptor or concatenated local patches.
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
        # We generate a vector where each position correspond to a unique identifier (incremental id).
        self.lookup = self.generate_lookup_table()

    def generate_lookup_table(self):
        # NOTE: How do we scale the number 58 for neighbours > 8?
        lookup_table = np.full((2**self.neighbours), 58)
        cont = 0
        for i in range(2**self.neighbours):
            bits = np.asarray(list(f"{i:0{self.neighbours}b}")).astype(np.uint8)
            transitions = np.sum(bits[:-1] != bits[1:])
            if transitions == 0 or transitions <= 2:
                lookup_table[i] = cont
                cont += 1
        return lookup_table

    def compute(self, img):
        height, width, *_ = img.shape
        padded = cv2.copyMakeBorder(
            img,
            top=self.radius,
            left=self.radius,
            right=self.radius,
            bottom=self.radius,
            borderType=cv2.BORDER_CONSTANT,
            value=0,
        )
        lbp = np.zeros_like(img)
        # TODO: We can avoid the computation of the decimal number if we create a hash lookup table instead of a list.
        base_2 = 2 ** np.arange(self.neighbours)
        for x in range(0, height, self.stride[0]):
            for y in range(0, width, self.stride[1]):
                lbp[x, y] = self.lookup[
                    np.dot(padded[x + self.dx, y + self.dy] >= img[x, y], base_2)
                ]
        lbp = lbp[0 : height : self.stride[0], 0 : width : self.stride[1]]
        lbp_height, lbp_width, *_ = lbp.shape
        descriptors = []
        if not self.block:
            block = (lbp_height, lbp_width)
        else:
            block = self.block
        # TODO: How does the number of bins scale with the size of neighbours?
        bins = np.arange(59)
        size_neigh = 59
        for x in range(0, lbp_height, block[0]):
            for y in range(0, lbp_width, block[1]):
                hist, _ = np.histogram(
                    lbp[x : x + block[0], y : y + block[1]].flatten(),
                    bins=bins,
                    range=(0, size_neigh),
                )
                # Normalize the histogram
                hist = hist.astype("float32") / hist.sum()
                descriptors.append(hist)
        global_descriptor = np.concatenate(descriptors)
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

    mylbp = LBPUniform()

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
    results_sum.write_csv("csvs/LBPUniform.csv")
    print(results_sum)
