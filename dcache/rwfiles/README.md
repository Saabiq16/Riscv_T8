This folder contains modified versions of the original Shakti dcache1rw.bsv and dcache2rw.bsv files.
The changes are focused on enabling support for two new cache replacement policies:

      drepl == 3 → FIFO (First-In-First-Out)
      drepl == 4 → LFU (Least Frequently Used)

These two new policies were integrated into the replacement update logic without altering unrelated logic in the original files.

✅ The updates are minimal and localized — only the relevant replacement policy if-else sections have been edited.

📸 Screenshots below highlight the modified code regions, showing exactly where and how the new logic was inserted.
![dcache1rw modified part](<dcache1rw.png>)
![dcache2rw modified part](<dcache2rw>)
