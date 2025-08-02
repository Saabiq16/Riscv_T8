# CF-RV-25-08

## Implementation of Two New Cache Replacement Policies (LFU and FIFO) in I-Cache and D-Cache of Shakti C-Class

This project was developed as part of the **CEG Fabless RISC-V Internship**, a collaborative initiative between **College of Engineering, Guindy (CEG)**, **Vyoma Systems**, and the **Shakti Processor Team (IIT Madras)**.

---

### Domain – RTL Core/Debug Team

---

## Project Overview

Our objective was to **design and integrate two new cache replacement policies** into both the **Instruction Cache (I-Cache)** and **Data Cache (D-Cache)** of the **Shakti C-Class processor**.

This enhancement allows the processor to handle different types of memory access patterns more efficiently, helping improve overall cache hit rate depending on application behavior.

---

## What is a Cache?

In computer architecture, a **cache** is a small, fast memory located close to the CPU. It stores copies of frequently accessed data from main memory (RAM), helping reduce latency and improve performance.

Since caches have **limited size**, they cannot hold all data. When the cache is full and a new block needs to be loaded, an existing block must be **evicted**—this is where **replacement policies** come in.

---

## Why Do We Need Replacement Policies?

A **cache replacement policy** determines which block to remove from the cache when new data must be loaded. The choice of policy can **significantly affect cache performance**, especially in programs with complex memory access patterns.

Common policies include:

- **LRU (Least Recently Used)**
- **Random**
- **FIFO (First In First Out)**
- **LFU (Least Frequently Used)**

---

## Our Contribution: Added Policies

### 🔁 FIFO (First In First Out)

- The **oldest** cache line (the one that entered first) is removed when space is needed.
- Simple to implement.
- Works reasonably well when all cache lines are equally important.

### 📊 LFU (Least Frequently Used)

- Tracks **how often** each cache line is accessed.
- Removes the **least accessed** block when replacing.
- Ideal for workloads with strong temporal locality (i.e., frequently used data should stay).

These policies were designed and tested within the **Bluespec SystemVerilog**-based RTL framework of the Shakti C-Class core. Extensive testing was done using RTL simulations and benchmarks to evaluate performance impact.

---

## References

- [Shakti Processor Documentation](https://shakti.org.in)
- [RISC-V ISA Specifications](https://riscv.org/specifications/ratified//)
- [Cache Replacement Policy – Wikipedia](https://en.wikipedia.org/wiki/Cache_replacement_policies)

---

## Contributors

- **Saabiq U A** – 2023105514 
- **Thiruvikesh B** – 2023105502

ECE, 3rd Year  
College of Engineering, Guindy  
Anna University, Chennai

---

## Acknowledgement

This project was made possible by the support and guidance from:

- **CEG Fabless Internship Program**
- **Vyoma Systems**
- **Shakti Processor Team, IIT Madras**

