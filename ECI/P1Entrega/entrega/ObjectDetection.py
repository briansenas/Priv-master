import numpy as np
import cv2
from torchvision import transforms
from torch.utils.data import Subset
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
from sklearn.preprocessing import StandardScaler
import polars as pl
from HogSVM import EmotionsDataset
import matplotlib.pyplot as plt


# https://pyimagesearch.com/2016/11/07/intersection-over-union-iou-for-object-detection/
def compute_iou(box1, box2):
    # Coordinates for box1: (x1, y1, x2, y2)
    x1, y1 = box1[0]
    fx1, fy1 = box1[1]
    x2, y2 = box2[0]
    fx2, fy2 = box2[1]

    # Coordinates for intersection
    inter_x1 = max(x1, x2)
    inter_y1 = max(y1, y2)
    inter_x2 = min(fx1, fx2)
    inter_y2 = min(fy1, fy2)

    # If there is no intersection
    if inter_x1 >= inter_x2 or inter_y1 >= inter_y2:
        return 0.0

    # Calculate the area of intersection
    inter_area = (inter_x2 - inter_x1) * (inter_y2 - inter_y1)

    # Calculate the area of both bounding boxes
    area1 = (y1 - fy1) * (x1 - fx1)
    area2 = (y2 - fy2) * (x2 - fx2)

    # Calculate IoU
    union_area = area1 + area2 - inter_area
    iou = inter_area / union_area
    return iou


# Refine bounding boxes based on IoU and confidence
def refine_bounding_boxes(bboxes, iou_threshold=0.5):
    # Sort bounding boxes by confidence, in descending order
    bboxes = sorted(bboxes, key=lambda x: x[1], reverse=True)
    refined_boxes = []
    for bbox in bboxes:
        _, _, (x, y, fx, fy), (w, h) = bbox
        # Keep the current bbox if it does not overlap with any already refined bbox
        add_box = True
        for refined_bbox in refined_boxes:
            _, _, (rx, ry, rfx, rfy), (rw, rh) = refined_bbox
            iou = compute_iou(((x, y), (fx, fy)), ((rx, ry), (rfx, rfy)))
            if iou > iou_threshold:
                # If overlap is high, only keep the one with greater confidence
                add_box = False
                break

        if add_box:
            refined_boxes.append(bbox)

    return refined_boxes


def draw_bboxes(normalized, bboxes):
    copy = cv2.resize(
        normalized, (sizes[0], sizes[0]), interpolation=cv2.INTER_LANCZOS4
    )
    copy = cv2.cvtColor(copy, cv2.COLOR_GRAY2RGB)
    for bbox in bboxes:
        # Extract data from the list
        class_id, confidence, (x, y, fx, fy), _ = bbox
        # Draw the bounding box (using the class label and confidence)
        color = (255, 0, 0)  # Green color for the bounding box
        thickness = 1  # Thickness of the bounding box lines
        cv2.rectangle(copy, (y, x), (fy, fx), color, thickness)
        # Prepare the text (class label and confidence)
        label = f"{train_dataset.dataset.classes[class_id]}({confidence:.2f})"
        # Set text position
        text_position = (y, x - 2)  # Just above the bbox
        # Set font, size, and color for the text
        font = cv2.FONT_HERSHEY_SIMPLEX
        font_scale = 0.3
        font_color = (255, 255, 255)  # White text
        font_thickness = 1
        # Add the text to the image
        cv2.putText(
            copy, label, text_position, font, font_scale, font_color, font_thickness
        )
    # Display the image (if you want to check the result)
    plt.imshow(copy, cmap="gray")
    plt.show()


if __name__ == "__main__":
    # Load the hog descriptor with the correct configuration for 28x28 images
    hog = cv2.HOGDescriptor(
        _winSize=(28, 28),
        _cellSize=(4, 4),
        _blockSize=(8, 8),
        _blockStride=(2, 2),
        _nbins=9,
    )
    # Filter the desired clases
    mnist_desired_classes = ["two", "six", "nine", "background"]

    # TODO: Better way to subsample train_dataset to use less than 50000 instances for training.
    # NOTE: using a 0.2% of the training (validation_set) as training is sketchy
    MNIST_transform = transforms.Compose(
        [
            transforms.ToTensor(),
            transforms.Grayscale(),  # Just in case I guess
            transforms.Normalize((0.1307,), (0.3081,)),
        ]
    )
    # Load the entire dataset
    train_dataset = EmotionsDataset(
        "mnist_data_with_background/mnist_data_with_background/train",
        transform=MNIST_transform,
        desired_classes=mnist_desired_classes,
        descriptor=None,
    )
    indices = list(range(len(train_dataset)))
    train_idx, reduced_train_idx = train_test_split(
        indices,
        test_size=0.05,  # Approx 1500
        stratify=train_dataset.dataset.targets,
        random_state=140421,
    )
    print(len(reduced_train_idx))
    # Create Subsets for training and testing
    reduced_train_dataset = Subset(train_dataset, reduced_train_idx)
    svm = SVC(probability=True, kernel="sigmoid", C=1)
    scaler = StandardScaler()
    params = {
        "C": [0.1, 1, 10],
        "kernel": ["linear", "rbf", "poly", "sigmoid"],
        # 'decision_function_shape': ["ovo", "ovr"]
    }
    gs = GridSearchCV(
        svm,
        params,
        refit="f1_macro",
        cv=5,
        scoring=["accuracy", "f1_macro"],
        # return_train_score=True,
    )
    X, y = [], []
    for i in reduced_train_dataset.indices:
        image, label, descriptor = train_dataset[i]
        X += [hog.compute(image.numpy().astype(np.uint8).transpose(1, 2, 0))]
        y += [label]
    X = scaler.fit_transform(X)
    # gs.fit(X, y)
    # print(gs.best_score_, gs.best_params_)
    # best_svm = gs.best_estimator_
    best_svm = svm.fit(X, y)
    #  Handwritten image 
    image = cv2.imread("HandWritten.jpeg")
    image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    # Inverse binary threshold
    rotated = cv2.rotate(image, cv2.ROTATE_90_COUNTERCLOCKWISE)
    rotated = rotated[25:-25, 25:-25]
    _, binary_image = cv2.threshold(rotated, 50, 255, cv2.THRESH_BINARY_INV)
    # Apply morphological dilation to enlarge the lines
    kernel = np.ones((5, 5), np.uint8)  # Square kernel of size 5x5
    dilated_image = cv2.dilate(binary_image, kernel, iterations=1)
    # Smooth the edges after dilation if needed
    smoothed_image = cv2.GaussianBlur(dilated_image, (3, 3), 0)

    def normalize_image(img):
        mean = 0.1307
        std = 0.3081
        normalized_image = (img - mean) / std

        return np.clip(normalized_image, 0, 255).astype(np.uint8)

    normalized = normalize_image(smoothed_image)
    plt.imshow(normalized)
    plt.show()
    copy = normalized.copy()
    block_size = (28, 28)
    block_stride = (9, 9)
    bboxes = []
    thresh = 0.80
    target = copy
    sizes = [256, 128, 64]
    og_h, og_w = (sizes[0], sizes[0])
    for i, size in enumerate(sizes):
        # Resize the image 
        target = cv2.resize(copy, (size, size), interpolation=cv2.INTER_LANCZOS4)
        height, width, *_ = target.shape
        # Calculate the ratio to scale the bbox
        scx, scy = og_h / height, og_w / width
        for x in range(0, height, block_stride[0]):
            for y in range(0, width, block_stride[1]):
                # Slide the window
                selected = target[x : x + block_size[0], y : y + block_size[1]]
                dh, dw, *_ = selected.shape
                dh = block_size[0] - dh
                dw = block_size[1] - dw
                # Pad the image
                selected = cv2.copyMakeBorder(
                    selected,
                    top=0,
                    left=0,
                    bottom=dh,
                    right=dw,
                    borderType=cv2.BORDER_CONSTANT,
                    value=0,
                )
                selected_descriptor = hog.compute(selected)
                selected_descriptor = scaler.transform(selected_descriptor.reshape(1, -1))
                log_pb = best_svm.predict_proba(selected_descriptor.reshape(1, -1))[0]
                maxi = np.argmax(log_pb)
                # Filter by threshold and exclude class background
                if (
                    log_pb[maxi] >= thresh
                    and maxi != train_dataset.dataset.class_to_idx["background"]
                ):
                    sx, sy = int(x * scx), int(y * scy)
                    bboxes += [
                        (
                            maxi,
                            log_pb[maxi],
                            (
                                sx,
                                sy,
                                int((x + block_size[0]) * scx),
                                int((y + block_size[1]) * scy),
                            ),
                            (height, width),
                        )
                    ]
    # print(bboxes)
    bboxes = sorted(bboxes, key=lambda x: x[1], reverse=True)
    draw_bboxes(normalized.copy(), bboxes)
    pre_boxes = refine_bounding_boxes(bboxes, iou_threshold=0.1)
    draw_bboxes(normalized.copy(), pre_boxes)