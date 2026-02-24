#!/usr/bin/env python3
"""Verify BF16 model in both input modes and optionally against RTL.

For every generated vector, this checks:
1) exp_bf16_bits(x_bits, rm)
2) exp_bf16_float(bf16_bits_to_float(x_bits), rm)

The two outputs must match exactly. The checked vectors are then emitted in the
same format consumed by BF16ModelBitExactSpec for RTL bit-exact checking.
"""

from __future__ import annotations

import argparse
import os
import random
import subprocess
import sys
from pathlib import Path

from bf16_exp_model import (
    bf16_bits_to_float,
    exp_bf16_bits,
    exp_bf16_float,
    float_to_bf16_bits_trunc,
)


def _is_nan_bits(x_bits: int) -> bool:
    exp = (x_bits >> 7) & 0xFF
    frac = x_bits & 0x7F
    return exp == 0xFF and frac != 0


def _roundtrip_float_stable(x_bits: int) -> bool:
    # NaN payloads are not stable through Python float, so exclude them here.
    if _is_nan_bits(x_bits):
        return False
    return float_to_bf16_bits_trunc(bf16_bits_to_float(x_bits)) == x_bits


def _generate_vectors(count: int, seed: int) -> list[tuple[int, int, int]]:
    rng = random.Random(seed)

    edges = [
        0x0000, 0x8000, 0x0001, 0x0002, 0x007F, 0x0080,
        0x3F80, 0x4000, 0x4049, 0x3F00, 0xBF80, 0xC000,
        0x7F7F, 0x7F80, 0xFF80, 0x42B1, 0x42B2, 0xC2B1, 0xC2B2,
    ]

    x_values: list[int] = [x for x in edges if _roundtrip_float_stable(x)]

    while len(x_values) < count:
        x = rng.randrange(0, 1 << 16)
        if _roundtrip_float_stable(x):
            x_values.append(x)

    vectors: list[tuple[int, int, int]] = []
    for x_bits in x_values:
        x_float = bf16_bits_to_float(x_bits)
        for rm in range(8):
            out_bits = exp_bf16_bits(x_bits, rm)
            out_float_mode = exp_bf16_float(x_float, rm)
            if out_bits != out_float_mode:
                raise AssertionError(
                    f"mode mismatch x=0x{x_bits:04x} rm={rm}: "
                    f"bits=0x{out_bits:04x}, float=0x{out_float_mode:04x}"
                )
            vectors.append((x_bits, rm, out_bits))

    return vectors


def _write_vectors(path: Path, vectors: list[tuple[int, int, int]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
        f.write("# x_bits_hex rounding_mode expected_bits_hex\n")
        for x_bits, rm, out_bits in vectors:
            f.write(f"{x_bits:04x}\t{rm}\t{out_bits:04x}\n")


def main() -> None:
    parser = argparse.ArgumentParser(description="Verify BF16 model in bit+float modes and optionally RTL.")
    parser.add_argument("--count", type=int, default=20000, help="Number of stable BF16 x values to generate.")
    parser.add_argument("--seed", type=int, default=0xC0DEC0DE, help="RNG seed.")
    parser.add_argument("--vectors-out", default="/tmp/bf16_model_vectors.tsv", help="Output vector file path.")
    parser.add_argument("--run-rtl", action="store_true", help="Also run BF16ModelBitExactSpec against emitted vectors.")
    args = parser.parse_args()

    vectors = _generate_vectors(args.count, args.seed)
    out_path = Path(args.vectors_out).resolve()
    _write_vectors(out_path, vectors)

    print(f"[ok] model bit-mode == float-mode for {len(vectors)} vectors")
    print(f"[ok] wrote vectors to {out_path}")

    if not args.run_rtl:
        return

    env = os.environ.copy()
    env["BF16_MODEL_VECTORS"] = str(out_path)
    cmd = ["./mill", "--no-server", "test.testOnly", "fpex.BF16ModelBitExactSpec"]
    print(f"[run] {' '.join(cmd)}")
    proc = subprocess.run(cmd, env=env)
    if proc.returncode != 0:
        sys.exit(proc.returncode)
    print("[ok] RTL bit-exact check passed")


if __name__ == "__main__":
    main()
