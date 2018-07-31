# MonFkNN - Monotonic Fuzzy k-Nearest Neighbors 

Monotonic Fuzzy k-Nearest Neighbors is a adaptation of the original Fuzzy k-NN for Monotonic Classification. 

This repository includes the following files:
- *skeel/classification/lazy_learning/MonFuzzyKNN:* Main class of Monotonic Fuzzy k-NN. It extends MonKNN.
- *skeel/classification/lazy_learning/FuzzyKNN:* Implementation of the original Fuzzy k-NN 
- *skeel/classification/lazy_learning/KNN:* Class implementing k-Nearest Neighbors for standard classification. It is needed by MonKNN class.
- *skeel/classification/lazy_learning/MonKNN:* Class of KNN for Monotonic classification. It extends KNN. It is needed by MonFuzzyKNN class.
- *skeel/classification/Classifier:* Scala trait to set the basics for every classifier.
- *skeel/utils/Distance:* Factory to compute the distance between two instances.
- *skeel/utils/KeelDataset:* Class to read and parse a dataset in KEEL format.
- *skeel/utils/MonKeelDataset:* Class to read and parse a dataset for monotonic classification in KEEL format.
- *skeel/utils/Score:* Class to compute the confusion matrix and different measurements giving the real and predicted class labels.
- *skeel/utils/MonotonicScore:* Class to compute different measurements of monotonic classification giving the real and predicted class labels. It extends Score class.

