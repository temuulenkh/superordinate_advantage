#!/usr/bin/env python3
"""
Estimate contextual probabilities using T5 model.
"""

import re
import os
import argparse
import torch
import torch.distributed as dist
import torch.nn.functional as F
from pathlib import Path
import pandas as pd
from transformers import AutoTokenizer, T5ForConditionalGeneration
from tqdm.auto import tqdm
import datetime as dt

# The script directory
SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR
DATA_DIR = PROJECT_ROOT / "data"
MODEL_CACHE_DIR = PROJECT_ROOT / "models"

# Model configuration
MODEL_NAME = "google-t5/t5-large"

# File paths
DATA_PATH = DATA_DIR / "all_context_sorted.csv"

# Initialize distributed training
def init_distributed():
    if "RANK" in os.environ and "WORLD_SIZE" in os.environ:
        rank = int(os.environ["RANK"])
        world_size = int(os.environ["WORLD_SIZE"])
        local_rank = int(os.environ["LOCAL_RANK"])

        torch.cuda.set_device(local_rank)
        device = torch.device(f"cuda:{local_rank}")

        dist.init_process_group(
            backend="nccl",
            timeout=dt.timedelta(hours=6),  # <-- increase from 10 minutes
        )

        return True, rank, world_size, local_rank, device
    else:
        device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        return False, 0, 1, 0, device

IS_DISTRIBUTED, RANK, WORLD_SIZE, LOCAL_RANK, DEVICE = init_distributed()


def create_probe_words_list(data_path):
    """Create a list of probe words."""
    print("Creating list of probe words...")
    
    all_data = pd.read_csv(data_path)
 
    target_words = all_data['target_word']
    
    probe_words = sorted(target_words.dropna().unique().tolist())
    
    print(f"Found {len(probe_words)} unique probe words")
    return probe_words


def prepare_sentences(data_path):
    """Load input sentences."""
    print("Loading sentences...")
    
    all_data = pd.read_csv(data_path)

    print(f"Prepared {len(all_data)} rows")
    return all_data


def split_context(text: str, target_word: str):
    """Split context around target word."""
    text = str(text)
    target = str(target_word).strip()
    sentinel = f"**{target}**"
    parts = text.split(sentinel, 1)
    return parts[0].rstrip(), parts[1].lstrip()


@torch.no_grad()
def t5_logp_targets_given_context(left: str, targets: list[str], right: str, batch_size: int = 32):
    """Compute log probabilities for targets given context using T5 model."""
    left_clean = left.rstrip()
    right_clean = right.lstrip()
    
    enc_text = f"{left_clean} <extra_id_0> {right_clean}"
    enc_text = re.sub(r"\s+", " ", enc_text).strip()
    
    enc = tokenizer(
        enc_text,
        return_tensors="pt",
        padding=False,
        add_special_tokens=True,
    )
    
    enc_input_ids = enc["input_ids"].to(DEVICE)
    
    logp_dict = {}
    
    for start in range(0, len(targets), batch_size):
        # batching logic for efficiency
        batch_targets = targets[start:start + batch_size]
        
        dec_texts = [f"<extra_id_0> {t} <extra_id_1>" for t in batch_targets]
        
        tokenizer.padding_side = "right"
        
        lab = tokenizer(
            dec_texts,
            return_tensors="pt",
            padding=True,
            add_special_tokens=False,
        )
        
        labels = lab["input_ids"].to(DEVICE)
        B, _ = labels.shape
        
        input_ids = enc_input_ids.expand(B, -1)
        
        outputs = model(input_ids=input_ids, labels=labels)
        logits = outputs.logits
        log_probs = F.log_softmax(logits, dim=-1)
        
        pad_id = tokenizer.pad_token_id
        
        for b_idx, tgt in enumerate(batch_targets):
            label_ids = labels[b_idx]
            total_logp = 0.0
            
            for pos, tok_id in enumerate(label_ids.tolist()):
                if tok_id == pad_id:
                    continue
                tok_str = tokenizer.convert_ids_to_tokens(tok_id)
                if tok_str.startswith("<extra_id_") or tok_str.strip() == "":
                    continue
                total_logp += log_probs[b_idx, pos, tok_id].item()
            
            logp_dict[tgt] = total_logp
        
        if DEVICE.type == "cuda":
            torch.cuda.empty_cache()
    
    return logp_dict


def safe_t5_logp_targets_given_context(
    left: str,
    targets: list[str],
    right: str,
    batch_sizes=(32, 16, 8, 4),
):
    """
    Try multiple batch sizes in case of CUDA out-of-memory errors.
    For each row we start from the largest batch size again.
    """
    last_err = None
    for bs in batch_sizes:
        try:
            return t5_logp_targets_given_context(left, targets, right, batch_size=bs)
        except RuntimeError as e:
            msg = str(e).lower()
            if ("out of memory" in msg) or ("cuda" in msg and "memory" in msg):
                print(f"CUDA OOM with batch_size={bs}. Trying smaller batch...")
                if DEVICE.type == "cuda":
                    torch.cuda.empty_cache()
                last_err = e
            else:
                raise
    raise last_err if last_err is not None else RuntimeError("All batch sizes failed.")


def main():
    """Main execution function."""
    # Parse command-line arguments (only rank 0 parses)
    parser = argparse.ArgumentParser(description="Estimate probabilities using T5 model")
    parser.add_argument(
        "--input", 
        type=str, 
        default=str(DATA_DIR / "all_context_sorted.csv"),
        help="Path to input CSV file (default: data/all_context_sorted.csv)"
    )
    parser.add_argument(
        "--output", 
        type=str, 
        default=str(DATA_DIR / "all_probs.csv"),
        help="Path to output CSV file (default: data/all_probs.csv)"
    )
    
    args = parser.parse_args()
    
    # Convert to Path objects
    INPUT_PATH = Path(args.input)
    OUT_PATH = Path(args.output)
    
    if RANK == 0:
        print("=" * 60)
        print("T5 Probability Estimation Script")
        print("=" * 60)
        if IS_DISTRIBUTED:
            print(f"Running in distributed mode with {WORLD_SIZE} processes")
        print(f"Process {RANK}/{WORLD_SIZE-1} using device: {DEVICE}")
        print(f"Input file: {INPUT_PATH}")
        print(f"Output file: {OUT_PATH}")
        print()
    
    # Load model (will use cache if already downloaded)
    if RANK == 0:
        print("Loading T5 model (using cache if available)...")
        print(f"Model cache directory: {MODEL_CACHE_DIR}")
    
    global tokenizer, model
    tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME, cache_dir=str(MODEL_CACHE_DIR))
    model = T5ForConditionalGeneration.from_pretrained(MODEL_NAME, cache_dir=str(MODEL_CACHE_DIR)).to(DEVICE)
    model.eval()
    
    if RANK == 0:
        print("Model loaded successfully!")
        print()
    
    # Create probe words list (all ranks need this)
    probe_words = create_probe_words_list(INPUT_PATH)
    if RANK == 0:
        print()
    
    # Prepare sentences (all ranks need this)
    all_data = prepare_sentences(INPUT_PATH)
    if RANK == 0:
        print()
    
    # Check for existing output file (all ranks check to know what's done)
    if os.path.exists(OUT_PATH):
        done = pd.read_csv(OUT_PATH, usecols=["domain", "row_id", "target_word"])
        done_pairs = set(zip(done["domain"], done["row_id"], done["target_word"]))
        already_processed = len(done_pairs)
        if RANK == 0:
            print(f"Found existing output file: {OUT_PATH}")
            print(f"Already processed rows: {already_processed}")
    else:
        done_pairs = set()
        already_processed = 0
        if RANK == 0:
            print("No existing output file found. Starting from scratch.")
    
    # Synchronize all processes before processing
    if IS_DISTRIBUTED:
        dist.barrier()
    
    processed = already_processed
    
    # Only keep rows that still need to be processed
    rows_to_process = [
        row for row in all_data.itertuples(index=False)
        if (row.domain, row.row_id, row.target_word) not in done_pairs
    ]
    
    # Split rows across processes
    if IS_DISTRIBUTED:
        chunk_size = len(rows_to_process) // WORLD_SIZE
        start_idx = RANK * chunk_size
        if RANK == WORLD_SIZE - 1:
            # Last rank gets remaining rows
            end_idx = len(rows_to_process)
        else:
            end_idx = start_idx + chunk_size
        rows_to_process = rows_to_process[start_idx:end_idx]
    
    if RANK == 0:
        print(f"➡ Total rows to process: {len(rows_to_process) * WORLD_SIZE if IS_DISTRIBUTED else len(rows_to_process)}")
        if IS_DISTRIBUTED:
            print(f"   Process {RANK} will process {len(rows_to_process)} rows")
        print()
    
    # Each rank writes to a temporary file to avoid conflicts
    if IS_DISTRIBUTED:
        rank_out_path = OUT_PATH.parent / f"{OUT_PATH.stem}_rank{RANK}{OUT_PATH.suffix}"
    else:
        rank_out_path = OUT_PATH
    
    # Process rows
    for row in tqdm(rows_to_process, desc=f"Rank {RANK} scoring rows", unit="row", disable=(RANK != 0)):
        processed += 1
        
        sent = row.input
        target = row.target_word
        left, right = split_context(sent, target)
        
        # Use safe wrapper (will try 32, then 16, 8, 4 if needed)
        logp_dict = safe_t5_logp_targets_given_context(left, probe_words, right)
        
        logps_tensor = torch.tensor(list(logp_dict.values()), dtype=torch.float32)
        max_lp = torch.max(logps_tensor)
        exps = torch.exp(logps_tensor - max_lp)
        probs = exps / torch.sum(exps)
        
        row_df = pd.DataFrame({
            "domain": row.domain,
            "row_id": row.row_id,
            "doc_type": row.doc_type,
            "input_sent": sent,
            "target_word": target,
            "probe_word": list(logp_dict.keys()),
            "log_prob": logps_tensor.tolist(),
            "prob_norm": probs.tolist(),
        })
        
        header = not os.path.exists(rank_out_path)
        row_df.to_csv(rank_out_path, mode="a", index=False, header=header)
    
    if RANK == 0:
        print()
        print(f"Process {RANK} done! Processed {len(rows_to_process)} rows.")
    
    # Wait for all processes to finish
    if IS_DISTRIBUTED:
        dist.barrier()
        
        # Rank 0 combines all output files
        if RANK == 0:
            print("Combining results from all processes...")
            all_dfs = []
            for r in range(WORLD_SIZE):
                rank_file = OUT_PATH.parent / f"{OUT_PATH.stem}_rank{r}{OUT_PATH.suffix}"
                if os.path.exists(rank_file):
                    df = pd.read_csv(rank_file)
                    all_dfs.append(df)
                    os.remove(rank_file)  # Clean up temp file
            
            if all_dfs:
                combined_df = pd.concat(all_dfs, ignore_index=True)
                # Check if main output exists and append or create
                if os.path.exists(OUT_PATH):
                    existing_df = pd.read_csv(OUT_PATH)
                    combined_df = pd.concat([existing_df, combined_df], ignore_index=True)
                combined_df.to_csv(OUT_PATH, index=False)
                print(f"Combined results saved to: {OUT_PATH}")
                print(f"Total rows processed: {len(combined_df)}")
        dist.barrier()
    else:
        print(f"File saved to: {OUT_PATH}")
    
    if IS_DISTRIBUTED:
        dist.destroy_process_group()


if __name__ == "__main__":
    main()
