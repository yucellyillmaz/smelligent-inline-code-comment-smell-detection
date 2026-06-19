import sys
import os
import pandas as pd
import numpy as np

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
from sklearn.decomposition import PCA
from sklearn.preprocessing import Normalizer
from sklearn.preprocessing import StandardScaler

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(BASE_DIR, "output")
INPUT_FILE = os.path.join(OUTPUT_DIR, "smell_label_list.txt")
os.makedirs(OUTPUT_DIR, exist_ok=True)

EMBED_DIM = 6
RANDOM_STATE = 42

def log(msg):
    print(f"[EMBED] {msg}", flush=True)

def apply_pca(X):
    log(f"Applying PCA -> {EMBED_DIM} dims")
    X = StandardScaler().fit_transform(X)
    pca = PCA(n_components=EMBED_DIM, random_state=RANDOM_STATE)
    return pca.fit_transform(X)

##### LSA
def embed_lsa(texts):
    log("Computing LSA embeddings")
    vectorizer = TfidfVectorizer(ngram_range=(1, 2), min_df=2, max_df=0.9, stop_words=None)
    X_tfidf = vectorizer.fit_transform(texts)
    
    svd = TruncatedSVD(n_components=EMBED_DIM, random_state=RANDOM_STATE)
    
    X_lsa = svd.fit_transform(X_tfidf)
    
    return Normalizer().fit_transform(X_lsa)

##### WORD2VEC 
def embed_word2vec(texts):
    from gensim.models import Word2Vec
    from gensim.utils import simple_preprocess
    
    log("Training Word2Vec")
    corpus = [simple_preprocess(t) for t in texts]

    model = Word2Vec(sentences=corpus, vector_size=100, window=5, min_count=1, workers=4, seed=RANDOM_STATE)

    vectors = []
    for tokens in corpus:
        v = [model.wv[w] for w in tokens if w in model.wv]
        vectors.append(np.mean(v, axis=0) if v else np.zeros(100))

    return apply_pca(np.array(vectors))

##### FASTTEXT
def embed_fasttext(texts):
    from gensim.models import FastText
    from gensim.utils import simple_preprocess

    log("Training FastText")
    corpus = [simple_preprocess(t) for t in texts]

    model = FastText(sentences=corpus, vector_size=100, window=5, min_count=1, workers=4, seed=RANDOM_STATE)

    vectors = []
    for tokens in corpus:
        v = [model.wv[w] for w in tokens]
        vectors.append(np.mean(v, axis=0) if v else np.zeros(100))

    return apply_pca(np.array(vectors))

##### SBERT 
def embed_sbert(texts):
    from sentence_transformers import SentenceTransformer
    log("Loading SBERT (MiniLM-L6)")
    
    model = SentenceTransformer("sentence-transformers/all-MiniLM-L6-v2")
    
    log("Encoding texts with SBERT")
    X = model.encode(texts, show_progress_bar=True)
    
    return apply_pca(X)

##### CODEBERT
def embed_codebert(texts):
    from sentence_transformers import SentenceTransformer
    import torch

    log("Loading CodeBERT (Sentence-Transformers)")
    torch.set_num_threads(4)

    model = SentenceTransformer("microsoft/codebert-base")
    model.max_seq_length = 64
    #model.max_seq_length = 128
    
    log("Encoding texts with CodeBERT (batched)")
    X = model.encode(texts, batch_size=16, show_progress_bar=True, convert_to_numpy=True)

    return apply_pca(X)
    
##### CODET5
def embed_codet5(texts):
    import torch
    import numpy as np
    from transformers import AutoTokenizer, T5EncoderModel

    log("Loading CodeT5-small (encoder-only)")
    torch.set_num_threads(4)

    tokenizer = AutoTokenizer.from_pretrained("Salesforce/codet5-small")
    model = T5EncoderModel.from_pretrained("Salesforce/codet5-small")
    model.eval()

    all_embeddings = []

    batch_size = 8
    max_length = 128

    log("Encoding texts with CodeT5 (batched, encoder-only)")
    with torch.no_grad():
        for i in range(0, len(texts), batch_size):
            batch = texts[i:i + batch_size]
            inputs = tokenizer(batch, padding=True, truncation=True, max_length=max_length, return_tensors="pt")

            outputs = model(**inputs)
            emb = outputs.last_hidden_state.mean(dim=1)
            all_embeddings.append(emb.cpu().numpy())
    X = np.vstack(all_embeddings)
    return apply_pca(X)

def main(method):
    method = method.lower()

    log(f"Reading input: {INPUT_FILE}")
    df = pd.read_csv(INPUT_FILE, sep=";", encoding="utf-8")

    df = df[df["label"] != "NOSMELL"].reset_index(drop=True)
    texts = df["clean_comment"].astype(str).tolist()

    if method == "lsa":
        X = embed_lsa(texts)
    elif method == "word2vec":
        X = embed_word2vec(texts)
    elif method == "fasttext":
        X = embed_fasttext(texts)
    elif method == "sbert":
        X = embed_sbert(texts)
    elif method == "codebert":
        X = embed_codebert(texts)
    elif method == "codet5":
        X = embed_codet5(texts)
    else:
        raise ValueError("Method must be one of: lsa | word2vec | fasttext | sbert | codebert | codet5")

    cols = [f"v{i+1}" for i in range(EMBED_DIM)]
    out_df = pd.DataFrame(X, columns=cols)
    out_df.insert(0, "clean_comment", texts)
    out_df["label"] = df["label"].values
    out_path = os.path.join(OUTPUT_DIR, f"py_comments_v6_{method}.txt")
    log(f"Saving output: {out_path}")
    out_df.to_csv(out_path, sep=";", index=False, encoding="utf-8")
    if not os.path.exists(out_path):
        raise RuntimeError("Output file was NOT written!")
    log("DONE")

if __name__ == "__main__":
    
    if len(sys.argv) != 2:
        print("Usage: python embedding_python.py [lsa|word2vec|fasttext|sbert|codebert|codet5]")
        sys.exit(1)
    
    main(sys.argv[1])
