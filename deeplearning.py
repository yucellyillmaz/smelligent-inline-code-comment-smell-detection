# Input : output/smell_label_list.txt  -> clean_comment;label
# Output:
#   output/deeplearning_fold_results.csv
#   output/deeplearning_results.csv

import os
import time
import random
import argparse
import tracemalloc
import numpy as np
import pandas as pd
import tensorflow as tf
import psutil

from sklearn.model_selection import StratifiedKFold
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import (accuracy_score, precision_score, recall_score, f1_score, matthews_corrcoef, cohen_kappa_score)
from sklearn.utils.class_weight import compute_class_weight
from tensorflow.keras import layers, models, callbacks

parser = argparse.ArgumentParser()
parser.add_argument("--input", default="output/smell_label_list.txt")
parser.add_argument("--output_dir", default="output")
parser.add_argument("--seeds", type=int, default=1)
parser.add_argument("--folds", type=int, default=5)
parser.add_argument("--epochs", type=int, default=15)
parser.add_argument("--batch_size", type=int, default=64)
parser.add_argument("--patience", type=int, default=3)
parser.add_argument("--max_tokens", type=int, default=20000)
parser.add_argument("--sequence_length", type=int, default=60)
parser.add_argument("--embedding_dim", type=int, default=128)
args = parser.parse_args()

INPUT_FILE = args.input
OUTPUT_DIR = args.output_dir
N_SEEDS = args.seeds
N_SPLITS = args.folds
EPOCHS = args.epochs
BATCH_SIZE = args.batch_size
PATIENCE = args.patience
MAX_TOKENS = args.max_tokens
SEQUENCE_LENGTH = args.sequence_length
EMBEDDING_DIM = args.embedding_dim
BASE_RANDOM_STATE = 42
os.makedirs(OUTPUT_DIR, exist_ok=True)

def set_seed(seed):
    random.seed(seed)
    np.random.seed(seed)
    tf.random.set_seed(seed)

def get_memory_mb():
    process = psutil.Process(os.getpid())
    return process.memory_info().rss / 1024 / 1024

def load_dataset(path):
    if not os.path.exists(path):
        raise FileNotFoundError(f"Input file not found: {path}")
    df = pd.read_csv(path, sep=";", encoding="utf-8")
    if "clean_comment" not in df.columns or "label" not in df.columns:
        raise ValueError("Input file must contain clean_comment and label columns.")
    df = df.dropna(subset=["clean_comment", "label"])
    df["clean_comment"] = df["clean_comment"].astype(str).str.strip()
    df["label"] = df["label"].astype(str).str.strip()
    df = df[df["clean_comment"] != ""]
    df = df[df["label"] != ""]
    df = df[df["label"] != "-"]
    return df.reset_index(drop=True)

def build_textcnn(num_classes):
    model = models.Sequential(name="TextCNN")
    model.add(layers.Input(shape=(SEQUENCE_LENGTH,)))
    model.add(layers.Embedding(input_dim=MAX_TOKENS,output_dim=EMBEDDING_DIM))
    model.add(layers.Conv1D( filters=128, kernel_size=3, activation="relu", padding="same"))
    model.add(layers.GlobalMaxPooling1D())
    model.add(layers.Dropout(0.4))
    model.add(layers.Dense(64, activation="relu"))
    model.add(layers.Dropout(0.3))
    model.add(layers.Dense(num_classes, activation="softmax"))
    model.compile(optimizer="adam", loss="sparse_categorical_crossentropy", metrics=["accuracy"])
    return model

def build_cnn_bilstm(num_classes):
    model = models.Sequential(name="CNN_BiLSTM")
    model.add(layers.Input(shape=(SEQUENCE_LENGTH,)))
    model.add(layers.Embedding(input_dim=MAX_TOKENS, output_dim=EMBEDDING_DIM))
    model.add(layers.Conv1D(filters=128, kernel_size=3, activation="relu", padding="same"))
    model.add(layers.MaxPooling1D(pool_size=2))
    model.add(layers.Bidirectional(layers.LSTM(64, return_sequences=False)))
    model.add(layers.Dropout(0.4))
    model.add(layers.Dense(64, activation="relu"))
    model.add(layers.Dropout(0.3))
    model.add(layers.Dense(num_classes, activation="softmax"))
    model.compile(optimizer="adam", loss="sparse_categorical_crossentropy", metrics=["accuracy"])
    return model

def calculate_metrics(y_true, y_pred):
    return {
        "accuracy": accuracy_score(y_true, y_pred),
        "precision_macro": precision_score(y_true, y_pred, average="macro", zero_division=0),
        "recall_macro": recall_score(y_true, y_pred, average="macro", zero_division=0),
        "f1_macro": f1_score(y_true, y_pred, average="macro", zero_division=0),
        "mcc": matthews_corrcoef(y_true, y_pred),
        "kappa": cohen_kappa_score(y_true, y_pred)
    }

def run_cross_validation(model_name, build_model_fn, texts, labels, num_classes, seed_id):
    skf = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=seed_id)
    fold_results = []
    for fold, (train_idx, test_idx) in enumerate(skf.split(texts, labels), start=1):
        print(f"\n[{model_name}] Seed {seed_id} | Fold {fold}/{N_SPLITS}")
        set_seed(seed_id + fold)
        x_train_raw = texts[train_idx]
        x_test_raw = texts[test_idx]
        y_train = labels[train_idx]
        y_test = labels[test_idx]
        memory_before_mb = get_memory_mb()
        tracemalloc.start()
        total_start = time.time()
        vectorizer = layers.TextVectorization(max_tokens=MAX_TOKENS, output_mode="int", output_sequence_length=SEQUENCE_LENGTH, standardize="lower_and_strip_punctuation")
        vectorizer.adapt(x_train_raw)
        x_train = vectorizer(x_train_raw).numpy()
        x_test = vectorizer(x_test_raw).numpy()
        class_weights_values = compute_class_weight(class_weight="balanced", classes=np.unique(y_train), y=y_train)
        class_weights = {
            int(cls): float(weight)
            for cls, weight in zip(np.unique(y_train), class_weights_values)
        }
        model = build_model_fn(num_classes)
        early_stop = callbacks.EarlyStopping(monitor="val_loss", patience=PATIENCE, restore_best_weights=True)
        train_start = time.time()
        history = model.fit(x_train, y_train, validation_split=0.1, epochs=EPOCHS, batch_size=BATCH_SIZE, class_weight=class_weights, callbacks=[early_stop], verbose=1)
        train_time_sec = time.time() - train_start
        predict_start = time.time()
        y_prob = model.predict(x_test, batch_size=BATCH_SIZE, verbose=0)
        predict_time_sec = time.time() - predict_start
        y_pred = np.argmax(y_prob, axis=1)
        total_time_sec = time.time() - total_start
        current_mem, peak_mem = tracemalloc.get_traced_memory()
        tracemalloc.stop()
        memory_after_mb = get_memory_mb()
        memory_delta_mb = memory_after_mb - memory_before_mb
        python_peak_memory_mb = peak_mem / 1024 / 1024
        metrics = calculate_metrics(y_test, y_pred)
        result = {
            "model": model_name,
            "seed": seed_id,
            "fold": fold,
            "epochs_trained": len(history.history["loss"]),
            "train_size": len(y_train),
            "test_size": len(y_test),
            "total_time_sec": round(total_time_sec, 4),
            "train_time_sec": round(train_time_sec, 4),
            "predict_time_sec": round(predict_time_sec, 4),
            "predict_time_ms_per_sample": round((predict_time_sec / len(y_test)) * 1000, 6),
            "memory_before_mb": round(memory_before_mb, 4),
            "memory_after_mb": round(memory_after_mb, 4),
            "memory_delta_mb": round(memory_delta_mb, 4),
            "python_peak_memory_mb": round(python_peak_memory_mb, 4),
            **metrics
        }
        fold_results.append(result)
        print(
            f"Accuracy={metrics['accuracy']:.4f}, "
            f"Macro-F1={metrics['f1_macro']:.4f}, "
            f"MCC={metrics['mcc']:.4f}, "
            f"Kappa={metrics['kappa']:.4f}, "
            f"Total Time={total_time_sec:.2f}s, "
            f"Memory Delta={memory_delta_mb:.2f}MB"
        )
        tf.keras.backend.clear_session()
    return fold_results

def main():
    set_seed(BASE_RANDOM_STATE)
    df = load_dataset(INPUT_FILE)
    print("Dataset loaded.")
    print("Total samples:", len(df))
    print("\nClass distribution:")
    print(df["label"].value_counts())
    label_encoder = LabelEncoder()
    y = label_encoder.fit_transform(df["label"].values)
    texts = df["clean_comment"].values
    num_classes = len(label_encoder.classes_)
    print("\nClasses:")
    for i, cls in enumerate(label_encoder.classes_):
        print(i, "->", cls)
    all_fold_results = []
    model_list = [("TextCNN", build_textcnn), ("CNN-BiLSTM", build_cnn_bilstm)]

    for seed_no in range(1, N_SEEDS + 1):
        seed_id = BASE_RANDOM_STATE + seed_no - 1
        for model_name, model_fn in model_list:
            results = run_cross_validation(model_name=model_name, build_model_fn=model_fn, texts=texts, labels=y, num_classes=num_classes, seed_id=seed_id)
            all_fold_results.extend(results)
    fold_df = pd.DataFrame(all_fold_results)
    fold_output_path = os.path.join(OUTPUT_DIR, "deeplearning_fold_results.csv")
    fold_df.to_csv(fold_output_path, index=False, encoding="utf-8-sig")

    summary_df = fold_df.groupby("model").agg(
        accuracy_mean=("accuracy", "mean"),
        accuracy_sd=("accuracy", "std"),
        precision_macro_mean=("precision_macro", "mean"),
        precision_macro_sd=("precision_macro", "std"),
        recall_macro_mean=("recall_macro", "mean"),
        recall_macro_sd=("recall_macro", "std"),
        f1_macro_mean=("f1_macro", "mean"),
        f1_macro_sd=("f1_macro", "std"),
        mcc_mean=("mcc", "mean"),
        mcc_sd=("mcc", "std"),
        kappa_mean=("kappa", "mean"),
        kappa_sd=("kappa", "std"),
        total_time_sec_mean=("total_time_sec", "mean"),
        total_time_sec_sd=("total_time_sec", "std"),
        train_time_sec_mean=("train_time_sec", "mean"),
        train_time_sec_sd=("train_time_sec", "std"),
        predict_time_sec_mean=("predict_time_sec", "mean"),
        predict_time_sec_sd=("predict_time_sec", "std"),
        predict_time_ms_per_sample_mean=("predict_time_ms_per_sample", "mean"),
        predict_time_ms_per_sample_sd=("predict_time_ms_per_sample", "std"),
        memory_delta_mb_mean=("memory_delta_mb", "mean"),
        memory_delta_mb_sd=("memory_delta_mb", "std"),
        memory_after_mb_mean=("memory_after_mb", "mean"),
        memory_after_mb_sd=("memory_after_mb", "std"),
        python_peak_memory_mb_mean=("python_peak_memory_mb", "mean"),
        python_peak_memory_mb_sd=("python_peak_memory_mb", "std")
    ).reset_index()

    summary_output_path = os.path.join(OUTPUT_DIR, "deeplearning_results.csv")
    summary_df.to_csv(summary_output_path, index=False, encoding="utf-8-sig")
    print("\nSaved fold results to:", fold_output_path)
    print("Saved summary results to:", summary_output_path)
    print("\nSummary:")
    print(summary_df)

if __name__ == "__main__":
    main()