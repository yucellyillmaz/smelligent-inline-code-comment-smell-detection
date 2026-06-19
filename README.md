# Smelligent: An Automated Labeling and Analysis Framework for Inline Code Comment Smells

This repository contains the experimental code and data used in the study.

The goal of this work is to identify poor-quality and misleading inline code comments through automated analysis techniques.

The proposed framework automatically generates inline code comment smell labels using a weakly supervised, rule-based labeling mechanism and evaluates their semantic consistency through embedding-based machine learning and deep learning models.

---

## Overview

- Extraction and preprocessing of inline code comments from real-world software projects
- Representation of comments using classical and contextual embedding methods
- Classification of comment smells using machine learning and deep learning models
- Evaluation using repeated experiments, five-fold cross-validation, and multiple performance metrics including Accuracy, Macro F1-score, MCC, Cohen's kappa, runtime, and memory usage.

All experiments are designed to be fully reproducible. The repository contains the complete implementation, embedding generation procedures, machine learning classifiers, deep learning models, and evaluation scripts used in the study.

---

## Experimental Setup

- **Embedding generation and deep learning experiments:** Python 3.13.5  
- **Classification and statistical evaluation:** R 4.4.1  
- **Hardware:** Intel Core i7-4510 @ 2.00 GHz, 8 GB RAM  
- **OS:** Windows 10 (64-bit)  
- **GPU:** Not used  

The deep learning experiments were conducted using Google Colab to avoid local hardware limitations and to ensure consistent execution across folds and random seeds.

---

## Experimental Projects

The `inputs/` directory is reserved **exclusively** for subject project directories.  

Each subdirectory under `inputs/` is treated as an independent software project and is processed automatically by the analysis pipeline.

To keep this repository lightweight and to respect upstream licenses, third-party projects are **not redistributed** here. Instead, the subject projects used in the experiments can be obtained directly from their original GitHub repositories.

### Obtaining the subject projects

You can automatically download all subject projects by running:

```bash
bash scripts/fetch_inputs.sh
```

This script will populate the `inputs/` directory with the required project folders.
Subject projects used in the study

| # | Project      | Language | Repository                                                                                   |
| - | ------------ | -------- | -------------------------------------------------------------------------------------------- |
| 1 | Anki-Android | Java     | [https://github.com/ankidroid/Anki-Android](https://github.com/ankidroid/Anki-Android)       |
| 2 | Jitsi        | Java     | [https://github.com/jitsi/jitsi](https://github.com/jitsi/jitsi)                             |
| 3 | Moshi        | Java     | [https://github.com/square/moshi](https://github.com/square/moshi)                           |
| 4 | Light-4j     | Java     | [https://github.com/networknt/light-4j](https://github.com/networknt/light-4j)               |
| 5 | Requests     | Python   | [https://github.com/psf/requests](https://github.com/psf/requests)                           |
| 6 | Scrapy       | Python   | [https://github.com/scrapy/scrapy](https://github.com/scrapy/scrapy)                         |
| 7 | Kivy         | Python   | [https://github.com/kivy/kivy](https://github.com/kivy/kivy)                                 |
| 8 | Scikit-learn | Python   | [https://github.com/scikit-learn/scikit-learn](https://github.com/scikit-learn/scikit-learn) |

> Users may replace these projects with their own repositories by placing them under the `inputs/` directory, provided that the same directory structure is preserved.
