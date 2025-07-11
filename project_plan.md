Project Plan

Title: Implementation and Evaluation of FIFO and LFU Cache Replacement Policies for L1 I-Cache and D-Cache in Shakti C-Class Core    


General Project Flow

1. Understanding the existing replacement policies 
   Study the structure of `replacement.bsv` and existing policies (`RANDOM`, `RRBIN`, `PLRU`).

2. Implement FIFO and LFU algorithms
   - Modify `replacement.bsv` to add two new policies with unique `alg` codes.

3. Simulate using Shakti benchmarks  

4. Compare the performance across all policies  
   - Measure miss rates, cycles, and memory traffic.

5. Document the results in tabular form

---

Modules or files to change

| File | Purpose |
|------|---------|
| `caches_mmu/src/dcache/replacement.bsv` | Add new replacement logic for FIFO and LFU |
| `caches_mmu/src/icache_1rw/replacement.bsv` |  Same policies in I-Cache |

---

Methods to Implement: 

Implement FIFO (alg==3) and LFU (alg==4) in replacement.bsv using head pointer and access counters.

---

Testing and Evaluation Plan

- Use Shakti’s provided `make run` flow and run with at least:
  - `REPLACEMENT_POLICY` = 0 (RANDOM)
  - 1 (RRBIN)
  - 2 (PLRU)
  - 3 (FIFO)
  - 4 (LFU)

- Collect stats from:
  - Simulation logs 
  - Cycle count, memory reads/writes, cache miss rate

- Record data in:
  - CSV table or Markdown

---

Final Output: 

- Modified RTL files pushed to GitHub
- Benchmarks results + comparison document
- Final README describing:
  - New policies
  - Integration steps
  - Summary of performance findings