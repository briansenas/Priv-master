import numpy as np
import cv2
import os
from torch.utils.data import Dataset
from torchvision import transforms
from torchvision.datasets.folder import ImageFolder
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
from sklearn.preprocessing import StandardScaler
import polars as pl
from torch.utils.data import Subset
from sklearn.model_selection import train_test_split


# https://github.com/pytorch/vision/issues/4633
class ImageFolderFiltered(ImageFolder):
    def __init__(self, root, transform=None, desired_classes=None, descriptor=None):
        self.desired_classes = desired_classes or []
        super().__init__(root, transform=transform)
        # Precompute descriptors for all images and store them
        self.descriptors = None
        if descriptor is not None:
            self.descriptors = [
                descriptor(
                    self.__getitem__(i)[0].numpy().astype(np.uint8).transpose(1, 2, 0)
                )
                for i in range(len(self.samples))
            ]

    def find_classes(self, directory):
        if self.desired_classes:
            classes = [
                d.name
                for d in os.scandir(directory)
                if d.is_dir() and any(x in d.name for x in self.desired_classes)
            ]
        else:
            classes = [d.name for d in os.scandir(directory) if d.is_dir()]
        classes.sort()  # Sorting classes
        class_to_idx = {cls: i for i, cls in enumerate(classes)}
        return classes, class_to_idx

    def __getitem__(self, index):
        image, label = super().__getitem__(index)
        # Get the precomputed descriptor
        descriptor = -1
        if self.descriptors is not None:
            descriptor = self.descriptors[index]
        return image, label, descriptor


class EmotionsDataset(Dataset):
    def __init__(self, root_dir, transform=None, desired_classes=None, descriptor=None):
        # Instantiate the super class
        super(Dataset, self).__init__()
        self.root_dir = root_dir
        self.transform = transform
        # Instantiate our version of find_classes()
        self.dataset = ImageFolderFiltered(
            root=root_dir,
            transform=transform,
            desired_classes=desired_classes,
            descriptor=descriptor,
        )

    def __len__(self):
        return len(self.dataset)

    def __getitem__(self, idx):
        image, label, descriptor = self.dataset[idx]
        return image, label, descriptor


def get_train_test_dataset(
    root_dir,
    transform,
    desired_classes,
    descriptor,
):
    # Load the entire dataset
    full_dataset = EmotionsDataset(
        root_dir,
        transform=transform,
        desired_classes=desired_classes,
        descriptor=descriptor,
    )
    # Create a list of indexes to later use to subset our dataset
    indices = list(range(len(full_dataset)))
    train_idx, test_idx = train_test_split(
        indices,
        test_size=0.2,
        stratify=full_dataset.dataset.targets,
        random_state=140421,
    )
    # Create Subsets for training and testing
    train_dataset = Subset(full_dataset, train_idx)
    test_dataset = Subset(full_dataset, test_idx)
    return full_dataset, train_dataset, test_dataset


if __name__ == "__main__":
    hog = cv2.HOGDescriptor()
    transform = transforms.Compose(
        [
            # By default, I'll set the size to 128x128 for now.
            transforms.Resize((128, 128)),
            transforms.Grayscale(),
            transforms.ToTensor(),
            # transforms.Normalize((0.4921,), (0.2924,)),
        ]
    )
    root_dir = (
        "emotions_classification"  # Replace with the actual path to your image dataset
    )
    desired_classes = ["sad", "happy"]
    full_dataset, train_dataset, test_dataset = get_train_test_dataset(
        root_dir, transform, desired_classes, descriptor=hog.compute
    )
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
        X += [descriptor]
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
    results_sum.write_csv("csvs/HOG+SVM.csv")
    results_sum